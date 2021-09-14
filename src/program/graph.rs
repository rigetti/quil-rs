//! Utilities for analysis of the dependency graph of a Quil Program

/**
 * Copyright 2021 Rigetti Computing
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 **/
use std::collections::{HashMap, HashSet};
use std::fmt;

use indexmap::IndexMap;
use petgraph::graphmap::GraphMap;
use petgraph::Directed;

use crate::instruction::{
    FrameIdentifier, Instruction, Jump, JumpUnless, JumpWhen, Label, MeasureCalibrationDefinition,
    MemoryReference,
};
use crate::{instruction::InstructionRole, program::Program};

pub use super::memory::MemoryAccessType;

#[derive(Debug, Clone)]
pub enum ScheduleErrorVariant {
    DuplicateLabel,
    UncalibratedInstruction,
    UnschedulableInstruction,
    // Note: these may be restored once enforced
    // DurationNotRealConstant,
    // DurationNotApplicable,
    // InvalidFrame,
}

#[derive(Debug, Clone)]
pub struct ScheduleError {
    instruction: Instruction,
    variant: ScheduleErrorVariant,
}

pub type ScheduleResult<T> = Result<T, ScheduleError>;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Hash, Ord)]
pub enum ScheduledGraphNode {
    BlockStart,
    InstructionIndex(usize),
    BlockEnd,
}

impl Eq for ScheduledGraphNode {}

/// A MemoryAccessQueue expresses the current state of memory accessors at the time of
/// an instruction's execution.
///
/// Quil uses a multiple-reader, single-writer concurrency model for memory access.
#[derive(Debug, Default, Clone)]
struct MemoryAccessQueue {
    pending_capture: Option<ScheduledGraphNode>,
    pending_reads: Vec<ScheduledGraphNode>,
    pending_write: Option<ScheduledGraphNode>,
}

/// A MemoryAccessDependency expresses a dependency that one node has on another to complete
/// some type of memory access prior to the dependent node's execution.
#[derive(Clone, Debug)]
struct MemoryAccessDependency {
    /// What type of memory access must complete prior to the downstream instruction.
    // NOTE: This must remain the first field for ordering to work as expected.
    pub access_type: MemoryAccessType,

    /// Which node is using the given `access_type`.
    pub node_id: ScheduledGraphNode,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum ExecutionDependency {
    /// The downstream instruction must wait for the given operation to complete.
    AwaitMemoryAccess(MemoryAccessType),

    /// The instructions share a reference frame
    ReferenceFrame,

    /// The ordering between these two instructions must remain unchanged
    StableOrdering,
}

/// A data structure to be used in the serializing of access to a memory region.
/// This utility helps guarantee strong consistency in a single-writer, multiple-reader model.
impl MemoryAccessQueue {
    pub fn flush(mut self) -> Vec<MemoryAccessDependency> {
        self.get_blocking_nodes(ScheduledGraphNode::BlockEnd, &MemoryAccessType::Capture)
    }

    /// Register that a node wants access of the given type, while returning which accesses block
    /// the requested access.
    ///
    /// Captures and writes may not happen concurrently with any other access; multiple reads may
    /// occur concurrently.
    ///
    /// Thus, if the caller requests Read access, and there are no pending captures or writes, then
    /// there will be no blocking nodes.
    ///
    /// However, if there is a pending capture or write, that dependency will be expressed in the
    /// return value.
    ///
    /// If the caller requests a capture or a write, then all pending calls - reads, writes, and captures -
    /// will be returned as "blocking" the capture or write.
    ///
    /// A capture or write remains blocking until the next capture or write.
    pub fn get_blocking_nodes(
        &mut self,
        node_id: ScheduledGraphNode,
        access: &MemoryAccessType,
    ) -> Vec<MemoryAccessDependency> {
        use MemoryAccessType::*;

        let mut result = vec![];
        if let Some(node_id) = self.pending_write {
            result.push(MemoryAccessDependency {
                node_id,
                access_type: Write,
            });
        }
        if let Some(node_id) = self.pending_capture {
            result.push(MemoryAccessDependency {
                node_id,
                access_type: Capture,
            });
        }

        match access {
            // Mark the given node as reading from this memory region. If there was a write pending,
            // return it to be used as a dependency.
            Read => {
                self.pending_reads.push(node_id);
                result
            }
            // Mark the given node as writing to this memory region. If there were any reads or another
            // write or capture pending, return those as a dependency list.
            Capture | Write => {
                for upstream_node_id in self.pending_reads.iter() {
                    result.push(MemoryAccessDependency {
                        node_id: *upstream_node_id,
                        access_type: Read,
                    });
                }

                match access {
                    Capture => {
                        self.pending_capture = Some(node_id);
                        self.pending_write = None;
                    }
                    Write => {
                        self.pending_capture = None;
                        self.pending_write = Some(node_id);
                    }
                    _ => panic!("expected Capture or Write memory dependency"),
                }

                result
            }
        }
    }
}

/// Add a dependency to an edge on the graph, whether that edge currently exists or not.
macro_rules! add_dependency {
    ($graph:expr, $source:expr => $target:expr, $dependency:expr) => {
        match $graph.edge_weight_mut($source, $target) {
            Some(edge) => {
                edge.insert($dependency);
            }
            None => {
                let mut edge = HashSet::new();
                edge.insert($dependency);
                $graph.add_edge($source.clone(), $target.clone(), edge);
            }
        }
    };
}

pub type DependencyGraph = GraphMap<ScheduledGraphNode, HashSet<ExecutionDependency>, Directed>;

/// An InstructionBlock of a ScheduledProgram is a group of instructions, identified by a string label,
/// which include no control flow instructions aside from an (optional) terminating control
/// flow instruction.
#[derive(Clone, Debug)]
pub struct InstructionBlock {
    pub instructions: Vec<Instruction>,
    graph: DependencyGraph,
    pub terminator: BlockTerminator,
}

impl InstructionBlock {
    pub fn build(
        instructions: Vec<Instruction>,
        terminator: Option<BlockTerminator>,
        program: &Program,
    ) -> ScheduleResult<Self> {
        let mut graph: DependencyGraph = GraphMap::new();
        // Root node
        graph.add_node(ScheduledGraphNode::BlockStart);

        let mut last_classical_instruction = ScheduledGraphNode::BlockStart;

        // Store the instruction index of the last instruction to block that frame
        let mut last_instruction_by_frame: HashMap<FrameIdentifier, ScheduledGraphNode> =
            HashMap::new();

        // Store memory access reads and writes. Key is memory region name.
        // NOTE: this may be refined to serialize by memory region offset rather than by entire region.
        let mut pending_memory_access: HashMap<String, MemoryAccessQueue> = HashMap::new();

        for (index, instruction) in instructions.iter().enumerate() {
            let node = graph.add_node(ScheduledGraphNode::InstructionIndex(index));

            let instruction_role = InstructionRole::from(instruction);
            match instruction_role {
                // Classical instructions must be strongly ordered by appearance in the program
                InstructionRole::ClassicalCompute => {
                    add_dependency!(graph, last_classical_instruction => node, ExecutionDependency::StableOrdering);

                    last_classical_instruction = node;
                    Ok(())
                }
                InstructionRole::RFControl => {
                    let frames = match program.get_frames_for_instruction(instruction, true) {
                        Some(frames) => frames,
                        None => vec![],
                    };

                    // Mark a dependency on the last instruction which executed in the context of each target frame
                    for frame in frames {
                        let previous_node_id = last_instruction_by_frame
                            .entry(frame.clone())
                            .or_insert(ScheduledGraphNode::BlockStart);
                        add_dependency!(graph, *previous_node_id => node, ExecutionDependency::ReferenceFrame);
                        last_instruction_by_frame.insert(frame.clone(), node);
                    }
                    Ok(())
                }
                InstructionRole::ControlFlow => Err(ScheduleError {
                    instruction: instruction.clone(),
                    variant: ScheduleErrorVariant::UnschedulableInstruction,
                }),
                InstructionRole::ProgramComposition => Err(ScheduleError {
                    instruction: instruction.clone(),
                    variant: ScheduleErrorVariant::UnschedulableInstruction,
                }),
            }?;

            // FIXME: This will handle reads, writes, and captures in arbitrary order, which is a bug.
            // Must be handled as reads -> (writes / captures). Instructions read all values prior to any
            // writes they make to those values.
            let accesses = instruction.get_memory_accesses();
            for (regions, access_type) in [
                (accesses.reads, MemoryAccessType::Read),
                (accesses.writes, MemoryAccessType::Write),
                (accesses.captures, MemoryAccessType::Capture),
            ] {
                for region in regions {
                    let memory_dependencies = pending_memory_access
                        .entry(region.clone())
                        .or_default()
                        .get_blocking_nodes(node, &access_type);
                    for memory_dependency in memory_dependencies {
                        // Test to make sure that no instructions depend directly on themselves
                        if memory_dependency.node_id != node {
                            let execution_dependency = ExecutionDependency::AwaitMemoryAccess(
                                memory_dependency.access_type.clone(),
                            );
                            add_dependency!(graph, memory_dependency.node_id => node, execution_dependency);
                        }
                    }
                }
            }
        }

        // Link all pending dependency nodes to the end of the block, to ensure that the block
        // does not terminate until these are complete
        add_dependency!(graph, last_classical_instruction => ScheduledGraphNode::BlockEnd, ExecutionDependency::StableOrdering);

        for (_, last_instruction) in last_instruction_by_frame {
            add_dependency!(graph, last_instruction => ScheduledGraphNode::BlockEnd, ExecutionDependency::ReferenceFrame);
        }

        // Examine all "pending" memory operations for all regions
        let remaining_dependencies = pending_memory_access
            .drain()
            .flat_map(|(_, queue)| queue.flush())
            .collect::<Vec<MemoryAccessDependency>>();

        // For each dependency, insert or overwrite an edge in the graph connecting the node pending that
        // operation to the end of the graph.
        for dependency in remaining_dependencies {
            let execution_dependency =
                ExecutionDependency::AwaitMemoryAccess(dependency.access_type);

            add_dependency!(graph, dependency.node_id => ScheduledGraphNode::BlockEnd, execution_dependency);
        }

        Ok(InstructionBlock {
            graph,
            instructions,
            terminator: terminator.unwrap_or(BlockTerminator::Continue),
        })
    }

    pub fn get_dependency_graph(&self) -> &DependencyGraph {
        &self.graph
    }

    /// Write a DOT-formatted string to the provided writer for use with GraphViz.
    /// This output can be used within a `subgraph` or at the top level of a `digraph`.
    ///
    /// Parameters:
    ///
    /// * line_prefix: The prefix for each new line in the output. This can be used to indent this
    ///   output for readability in a larger definition.
    /// * element_prefix: The prefix for each graph element (node and edge). This can be used to
    ///   namespace this block when used with other blocks which may have conflicting labels.
    pub fn write_dot_format(
        &self,
        f: &mut fmt::Formatter,
        line_prefix: &str,
        element_prefix: &str,
    ) -> fmt::Result {
        self.graph.nodes().try_for_each(|node| {
            match &node {
                ScheduledGraphNode::BlockEnd => {
                    writeln!(
                        f,
                        "{}\"{}end\" [ label=end, shape=circle ]",
                        line_prefix, element_prefix
                    )
                }
                ScheduledGraphNode::BlockStart => {
                    writeln!(
                        f,
                        "{}\"{}start\" [ label=start, shape=circle ]",
                        line_prefix, element_prefix
                    )
                }
                ScheduledGraphNode::InstructionIndex(index) => {
                    write!(
                        f,
                        "{}\"{}{}\" [label=\"",
                        line_prefix, element_prefix, index
                    )?;
                    write_escaped(f, &format!("{}", self.instructions.get(*index).unwrap()))?;
                    writeln!(f, "\"]")
                }
            }?;
            self.graph.edges(node).try_for_each(|(src, dest, edge)| {
                match &src {
                    ScheduledGraphNode::BlockEnd => {
                        write!(f, "{}\"{}end\"", line_prefix, element_prefix)
                    }
                    ScheduledGraphNode::BlockStart => {
                        write!(f, "{}\"{}start\"", line_prefix, element_prefix)
                    }
                    ScheduledGraphNode::InstructionIndex(index) => {
                        write!(f, "{}\"{}{}\"", line_prefix, element_prefix, index)
                    }
                }?;
                write!(f, " -> ")?;
                match &dest {
                    ScheduledGraphNode::BlockEnd => write!(f, "\"{}end\"", element_prefix),
                    ScheduledGraphNode::BlockStart => {
                        write!(f, "\"{}start\"", element_prefix)
                    }
                    ScheduledGraphNode::InstructionIndex(index) => {
                        write!(f, "\"{}{}\"", element_prefix, index)
                    }
                }?;
                let mut labels = edge
                    .iter()
                    .map(|dependency| match dependency {
                        ExecutionDependency::AwaitMemoryAccess(access_type) => match access_type {
                            MemoryAccessType::Read => "await read",
                            MemoryAccessType::Write => "await write",
                            MemoryAccessType::Capture => "await capture",
                        },
                        ExecutionDependency::ReferenceFrame => "frame",
                        ExecutionDependency::StableOrdering => "ordering",
                    })
                    .collect::<Vec<&str>>();

                // We sort them so that graph output is deterministic; iterating over the set
                // without sorting would cause flaky tests.
                labels.sort_unstable();
                let label = labels.join("\n");
                writeln!(f, " [ label=\"{}\" ]", label)
            })
        })?;
        Ok(())
    }

    /// Return a particular-indexed instruction (if present).
    pub fn get_instruction(&self, node_id: usize) -> Option<&Instruction> {
        self.instructions.get(node_id)
    }

    /// Return the count of executable instructions in this block.
    pub fn len(&self) -> usize {
        self.instructions.len()
    }

    /// Return true if this block contains no executable instructions.
    pub fn is_empty(&self) -> bool {
        self.instructions.is_empty()
    }

    pub fn set_exit_condition(&mut self, terminator: BlockTerminator) {
        self.terminator = terminator
    }
}

#[derive(Clone, Debug)]
pub enum BlockTerminator {
    Conditional {
        condition: MemoryReference,
        target: String,
        jump_if_condition_true: bool,
    },
    Unconditional {
        target: String,
    },
    Continue,
    Halt,
}

/// Escape strings for use as DOT format quoted ID's
fn write_escaped(f: &mut fmt::Formatter, s: &str) -> fmt::Result {
    for c in s.chars() {
        write_char(f, c)?;
    }
    Ok(())
}

/// Escape a single character for use within a DOT format quoted ID.
fn write_char(f: &mut fmt::Formatter, c: char) -> fmt::Result {
    use std::fmt::Write;
    match c {
        '"' | '\\' => f.write_char('\\')?,
        // \l is for left justified linebreak
        '\n' => return f.write_str("\\l"),
        _ => {}
    }
    f.write_char(c)
}

#[derive(Clone, Debug)]
pub struct ScheduledProgram {
    /// All blocks within the ScheduledProgram, keyed on string label.
    pub blocks: IndexMap<String, InstructionBlock>,
}

macro_rules! terminate_working_block {
    ($terminator:expr, $working_instructions:ident, $blocks:ident, $working_label:ident, $program: ident) => {{
        // If this "block" has no instructions and no terminator, it's not worth storing - skip it
        if $working_instructions.is_empty() && $terminator.is_none() && $working_label.is_none() {
            $working_label = None
        } else {
            let block = InstructionBlock::build(
                $working_instructions.iter().map(|el| el.clone()).collect(),
                $terminator,
                $program,
            )?;
            let label =
                $working_label.unwrap_or_else(|| Self::generate_autoincremented_label(&$blocks));

            match $blocks.insert(label.clone(), block) {
                Some(_) => Err(ScheduleError {
                    instruction: Instruction::Label(Label(label.clone())),
                    variant: ScheduleErrorVariant::DuplicateLabel,
                }), // Duplicate label
                None => Ok(()),
            }?;
            $working_instructions = vec![];
            $working_label = None
        }
        Ok(())
    }};
}

impl ScheduledProgram {
    /// Structure a sequential program
    #[allow(unused_assignments)]
    pub fn from_program(program: &Program) -> ScheduleResult<Self> {
        let mut working_label = None;
        let mut working_instructions: Vec<Instruction> = vec![];
        let mut blocks = IndexMap::new();

        let instructions = program.to_instructions(false);

        for instruction in instructions {
            match instruction {
                Instruction::Arithmetic(_)
                | Instruction::Capture(_)
                | Instruction::Delay(_)
                | Instruction::Fence(_)
                | Instruction::Move(_)
                | Instruction::Exchange(_)
                | Instruction::Load(_)
                | Instruction::Store(_)
                | Instruction::Pulse(_)
                | Instruction::SetFrequency(_)
                | Instruction::SetPhase(_)
                | Instruction::SetScale(_)
                | Instruction::ShiftFrequency(_)
                | Instruction::ShiftPhase(_)
                | Instruction::SwapPhases(_)
                | Instruction::RawCapture(_)
                | Instruction::Reset(_) => {
                    working_instructions.push(instruction);
                    Ok(())
                }
                Instruction::Gate(_) | Instruction::Measurement(_) => Err(ScheduleError {
                    instruction: instruction.clone(),
                    variant: ScheduleErrorVariant::UncalibratedInstruction,
                }),
                Instruction::CalibrationDefinition(_)
                | Instruction::CircuitDefinition(_)
                | Instruction::Declaration(_)
                | Instruction::GateDefinition(_)
                | Instruction::FrameDefinition(_)
                | Instruction::MeasureCalibrationDefinition(MeasureCalibrationDefinition {
                    ..
                })
                | Instruction::WaveformDefinition(_) => Err(ScheduleError {
                    instruction: instruction.clone(),
                    variant: ScheduleErrorVariant::UnschedulableInstruction,
                }),

                Instruction::Pragma(_) => {
                    // TODO: Handle pragmas. Here, we just silently discard them, but certain
                    // pragmas must be supported.
                    Ok(())
                }
                // _ => Err(()), // Unimplemented
                Instruction::Label(Label(value)) => {
                    terminate_working_block!(
                        None as Option<BlockTerminator>,
                        working_instructions,
                        blocks,
                        working_label,
                        program
                    )?;

                    working_label = Some(value.clone());
                    Ok(())
                }
                Instruction::Jump(Jump { target }) => {
                    terminate_working_block!(
                        Some(BlockTerminator::Unconditional {
                            target: target.clone(),
                        }),
                        working_instructions,
                        blocks,
                        working_label,
                        program
                    )?;
                    Ok(())
                }
                Instruction::JumpWhen(JumpWhen { target, condition }) => {
                    terminate_working_block!(
                        Some(BlockTerminator::Conditional {
                            target: target.clone(),
                            condition: condition.clone(),
                            jump_if_condition_true: true,
                        }),
                        working_instructions,
                        blocks,
                        working_label,
                        program
                    )?;
                    Ok(())
                }
                Instruction::JumpUnless(JumpUnless { target, condition }) => {
                    terminate_working_block!(
                        Some(BlockTerminator::Conditional {
                            target: target.clone(),
                            condition: condition.clone(),
                            jump_if_condition_true: false,
                        }),
                        working_instructions,
                        blocks,
                        working_label,
                        program
                    )
                }
                Instruction::Halt => {
                    terminate_working_block!(
                        Some(BlockTerminator::Halt),
                        working_instructions,
                        blocks,
                        working_label,
                        program
                    )
                }
            }?;
        }

        terminate_working_block!(
            None as Option<BlockTerminator>,
            working_instructions,
            blocks,
            working_label,
            program
        )?;

        Ok(ScheduledProgram { blocks })
    }

    fn generate_autoincremented_label(block_labels: &IndexMap<String, InstructionBlock>) -> String {
        let mut suffix = 0;
        let mut label = format!("block_{}", suffix);
        while block_labels.get(&label).is_some() {
            suffix += 1;
            label = format!("block_{}", suffix);
        }
        label
    }

    /// Write a DOT format string to the provided writer for use with Graphviz.
    ///
    /// This outputs a `digraph` object with a `subgraph` for each block to inform the layout engine.
    /// Each `subgraph` ID is prefixed with `cluster_` which instructs some supporting layout engines
    /// to enclose the subgraph with a border. This improves readability of the graph.
    ///
    /// Lines on the graph indicate scheduling dependencies within blocks and control flow among blocks.
    /// Each node representing an instruction is labeled with the contents of that instruction.
    pub fn write_dot_format(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "digraph {{")?;

        let mut iter = self.blocks.iter().peekable();

        writeln!(f, "\tentry [label=\"Entry Point\"]")?;

        if let Some((first_label, _)) = iter.peek() {
            writeln!(f, "\tentry -> \"{}_start\"", first_label)?;
        }

        while let Some((label, block)) = iter.next() {
            writeln!(f, "\tsubgraph \"cluster_{}\" {{", label)?;
            writeln!(f, "\t\tlabel=\"{}\"", label)?;
            writeln!(f, "\t\tnode [ style=\"filled\" ]")?;

            let line_prefix = "\t\t";
            // let element_prefix = format!("b{}_", index);
            let element_prefix = format!("{}_", label);

            block.write_dot_format(f, line_prefix, &element_prefix)?;
            writeln!(f, "\t}}")?;

            let next_block_label = iter.peek().map(|(next_label, _)| (*next_label).clone());
            match &block.terminator {
                BlockTerminator::Conditional {
                    condition,
                    target,
                    jump_if_condition_true,
                } => {
                    let equality_operators = if *jump_if_condition_true {
                        ("==", "!=")
                    } else {
                        ("!=", "==")
                    };
                    writeln!(
                        f,
                        "\"{}_end\" -> \"{}_start\" [label=\"if {} {} 0\"]",
                        label, target, condition, equality_operators.0,
                    )?;
                    if let Some(next_label) = next_block_label {
                        writeln!(
                            f,
                            "\"{}_end\" -> \"{}_start\" [label=\"if {} {} 0\"]",
                            label, next_label, condition, equality_operators.1
                        )?;
                    };
                }
                BlockTerminator::Unconditional { target } => {
                    writeln!(
                        f,
                        "\"{}_end\" -> \"{}_start\" [label=\"always\"]",
                        label, target
                    )?;
                }
                BlockTerminator::Continue => {
                    if let Some(next_label) = next_block_label {
                        writeln!(
                            f,
                            "\"{}_end\" -> \"{}_start\" [label=\"always\"]",
                            label, next_label
                        )?;
                    };
                }
                BlockTerminator::Halt => {}
            }
        }
        writeln!(f, "}}")
    }
}

#[cfg(test)]
mod tests {
    mod graph {
        use std::str::FromStr;

        use crate::program::Program;

        use super::super::ScheduledProgram;

        /// Build a test case which compiles the input program, builds the dot-format string from the program,
        /// and then compares that to a "correct" snapshot of that dot format. This makes diffs easy to compare and
        /// understand; if a test is failing, you can copy the snapshot contents out to your preferred Graphviz
        /// viewer to help understand why.
        ///
        /// NOTE: because this relies on direct string comparison, it will be brittle against changes in the way
        /// that the `write_dot_format` methods work. If _all_ or _most_ of these tests are failing, examine the
        /// diffs closely to determine if it's only a matter of reformatting.
        macro_rules! build_dot_format_snapshot_test_case {
            ($name: ident, $input:expr) => {
                #[test]
                fn $name() {
                    use std::fmt;
                    const FRAME_DEFINITIONS: &'static str = "
DEFFRAME 0 \"rf\":
    INITIAL-FREQUENCY: 1e6
DEFFRAME 1 \"rf\":
    INITIAL-FREQUENCY: 1e6
DEFFRAME 2 \"rf\":
    INITIAL-FREQUENCY: 1e6
DEFFRAME 0 \"ro_rx\":
    INITIAL-FREQUENCY: 1e6
DEFFRAME 0 \"ro_tx\":
    INITIAL-FREQUENCY: 1e6
";

                    let program =
                        Program::from_str(&format!("{}\n{}", FRAME_DEFINITIONS, $input)).unwrap();
                    let scheduled_program = ScheduledProgram::from_program(&program).unwrap();

                    for block in scheduled_program.blocks.values() {
                        let graph = block.get_dependency_graph();
                        assert!(
                            !petgraph::algo::is_cyclic_directed(graph),
                            "cycle in graph: {:?}",
                            graph
                        );
                    }

                    struct ProgramDebugWrapper<'a> {
                        pub program: &'a ScheduledProgram,
                    }

                    impl<'a> fmt::Debug for ProgramDebugWrapper<'a> {
                        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                            self.program.write_dot_format(f)
                        }
                    }

                    insta::assert_debug_snapshot!(ProgramDebugWrapper {
                        program: &scheduled_program
                    });
                }
            };
        }

        build_dot_format_snapshot_test_case!(
            single_instruction,
            "PULSE 0 \"rf\" test(duration: 1e6)"
        );

        build_dot_format_snapshot_test_case!(
            single_dependency,
            "
PULSE 0 \"rf\" test(duration: 1e6)
PULSE 0 \"rf\" test(duration: 1e6)
"
        );

        build_dot_format_snapshot_test_case!(
            chained_pulses,
            "
PULSE 0 \"rf\" test(duration: 1e6)
PULSE 0 \"rf\" test(duration: 1e6)
PULSE 0 \"rf\" test(duration: 1e6)
PULSE 0 \"rf\" test(duration: 1e6)
PULSE 0 \"rf\" test(duration: 1e6)
"
        );

        build_dot_format_snapshot_test_case!(
            different_frames_blocking,
            "
PULSE 0 \"rf\" test(duration: 1e6)
PULSE 1 \"rf\" test(duration: 1e6)
PULSE 2 \"rf\" test(duration: 1e6)
"
        );

        build_dot_format_snapshot_test_case!(
            different_frames_nonblocking,
            "
NONBLOCKING PULSE 0 \"rf\" test(duration: 1e6)
NONBLOCKING PULSE 1 \"rf\" test(duration: 1e6)
NONBLOCKING PULSE 2 \"rf\" test(duration: 1e6)
"
        );

        build_dot_format_snapshot_test_case!(
            fence_all_with_nonblocking_pulses,
            "
NONBLOCKING PULSE 0 \"rf\" test(duration: 1e6)
NONBLOCKING PULSE 1 \"rf\" test(duration: 1e6)
FENCE
NONBLOCKING PULSE 0 \"rf\" test(duration: 1e6)
NONBLOCKING PULSE 1 \"rf\" test(duration: 1e6)
"
        );
        build_dot_format_snapshot_test_case!(fence_all, "FENCE");

        build_dot_format_snapshot_test_case!(
            jump,
            "DECLARE ro BIT
LABEL @first-block
PULSE 0 \"rf\" test(duration: 1e6)
JUMP-UNLESS @third-block ro[0]
LABEL @second-block
PULSE 0 \"rf\" test(duration: 1e6)
LABEL @third-block
PULSE 0 \"rf\" test(duration: 1e6)
"
        );

        build_dot_format_snapshot_test_case!(
            active_reset_single_frame,
            "DECLARE ro BIT
LABEL @measure
NONBLOCKING PULSE 0 \"ro_tx\" test(duration: 1e6)
NONBLOCKING CAPTURE 0 \"ro_rx\" test(duration: 1e6) ro
JUMP-WHEN @end ro[0]
LABEL @feedback
PULSE 0 \"rf\" test(duration: 1e6)
JUMP @measure
LABEL @end
"
        );

        build_dot_format_snapshot_test_case!(
            labels_only,
            "LABEL @a
LABEL @b
LABEL @c
"
        );

        // assert that read and write memory dependencies are expressed correctly
        build_dot_format_snapshot_test_case!(
            simple_memory_access,
            "DECLARE a INTEGER
DECLARE b INTEGER
MOVE a 1
MOVE b 2
ADD a b
"
        );

        // assert that a block "waits" for a capture to complete
        build_dot_format_snapshot_test_case!(
            simple_capture,
            "DECLARE ro BIT
CAPTURE 0 \"ro_rx\" test ro"
        );

        // assert that a block "waits" for a capture to complete even with a pulse after it
        build_dot_format_snapshot_test_case!(
            pulse_after_capture,
            "DECLARE ro BIT
CAPTURE 0 \"ro_rx\" test ro
PULSE 0 \"rf\" test"
        );

        // assert that a block "waits" for a capture to complete
        build_dot_format_snapshot_test_case!(
            parametric_pulse,
            "DECLARE ro BIT
DECLARE param REAL
PULSE 0 \"rf\" test(a: param[0])
CAPTURE 0 \"ro_rx\" test(a: param[0]) ro"
        );

        // Assert that all pulses following a capture block on that capture, until the next capture
        build_dot_format_snapshot_test_case!(
            parametric_pulses_using_capture_results,
            "DECLARE ro BIT
DECLARE param REAL
CAPTURE 0 \"ro_rx\" test(a: param[0]) ro
NONBLOCKING PULSE 0 \"rf\" test(a: ro[0])
NONBLOCKING PULSE 1 \"rf\" test(a: ro[0])
CAPTURE 0 \"ro_rx\" test(a: param[0]) ro
NONBLOCKING PULSE 0 \"rf\" test(a: ro[0])
NONBLOCKING PULSE 1 \"rf\" test(a: ro[0])"
        );

        build_dot_format_snapshot_test_case!(
            multiple_classical_instructions,
            "DECLARE ro INTEGER[2]\nMOVE ro[0] 1\nMOVE ro[1] 0\nADD ro[0] 5\nSUB ro[1] ro[0]"
        );
    }
}
