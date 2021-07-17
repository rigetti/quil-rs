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

use petgraph::graphmap::GraphMap;
use petgraph::Directed;

use crate::instruction::{FrameIdentifier, Instruction, MemoryReference};
use crate::{instruction::InstructionRole, program::Program};

use indexmap::IndexMap;

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

struct MemoryAccess {
    pub regions: HashSet<String>,
    pub access_type: MemoryAccessType,
}

/// Express a mode of memory access.
#[derive(Clone, Debug)]
enum MemoryAccessType {
    /// Write to a memory location using readout (`CAPTURE` and `RAW-CAPTURE` instructions)
    Capture,

    /// Read from a memory location
    Read,

    /// Write to a memory location using classical instructions
    Write,
}

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
    pub node_id: ScheduledGraphNode,
    // What type of memory access must complete prior to the downstream instruction
    pub access_type: MemoryAccessType,
}

#[derive(Clone, Debug)]
pub enum ExecutionDependency {
    /// The downstream instruction must wait for the capture (asynchronous write) to complete.
    AwaitCapture,
    /// The downstream instruction can execute as soon as the upstream completes.
    Immediate,
}

/// A data structure to be used in the serializing of access to a memory region.
/// This utility helps guarantee strong consistency in a single-writer, multiple-reader model.
impl MemoryAccessQueue {
    pub fn flush(mut self) -> Vec<MemoryAccessDependency> {
        self.get_blocking_nodes(ScheduledGraphNode::BlockEnd, &MemoryAccessType::Capture)
    }

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
        self.pending_write = None;
        if let Some(node_id) = self.pending_capture {
            result.push(MemoryAccessDependency {
                node_id,
                access_type: Capture,
            });
        }
        self.pending_capture = None;

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
                let mut result = vec![];
                if !self.pending_reads.is_empty() {
                    for upstream_node_id in self.pending_reads.iter() {
                        result.push(MemoryAccessDependency {
                            node_id: *upstream_node_id,
                            access_type: Read,
                        });
                    }
                }

                match access {
                    Capture => {
                        self.pending_capture = Some(node_id);
                    }
                    Write => {
                        self.pending_write = Some(node_id);
                    }
                    _ => panic!("expected Capture or Write memory dependency"),
                }

                result
            }
        }
    }
}

pub type DependencyGraph = GraphMap<ScheduledGraphNode, ExecutionDependency, Directed>;
pub type DependencyGraphEdgeRef<'a> = (
    ScheduledGraphNode,
    ScheduledGraphNode,
    &'a ExecutionDependency,
);
pub type DependencyGraphNodeRef<'a> = (ScheduledGraphNode, &'a ScheduledGraphNode);

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
        let mut graph = GraphMap::new();
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
                InstructionRole::ClassicalCompute => {
                    graph.add_edge(
                        last_classical_instruction,
                        node,
                        ExecutionDependency::Immediate,
                    );
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
                        graph.add_edge(*previous_node_id, node, ExecutionDependency::Immediate);
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

            if let Some(memory_accesses) = Self::get_memory_accesses(instruction) {
                for region in memory_accesses.regions {
                    let memory_dependencies = pending_memory_access
                        .entry(region)
                        .or_default()
                        .get_blocking_nodes(node, &memory_accesses.access_type);
                    for memory_dependency in memory_dependencies {
                        // If this instruction follows one which performed a capture, we have to wait for the
                        // capture to complete before proceeding. Otherwise, this is just a simple ordering
                        // dependency.
                        let execution_dependency = match memory_dependency.access_type {
                            MemoryAccessType::Capture => ExecutionDependency::AwaitCapture,
                            _ => ExecutionDependency::Immediate,
                        };
                        graph.add_edge(memory_dependency.node_id, node, execution_dependency);
                    }
                }
            }
        }

        // Link all pending dependency nodes to the end of the block, to ensure that the block
        // does not terminate until these are complete

        graph.add_edge(
            last_classical_instruction,
            ScheduledGraphNode::BlockEnd,
            ExecutionDependency::Immediate,
        );

        for (_, last_instruction) in last_instruction_by_frame {
            graph.add_edge(
                last_instruction,
                ScheduledGraphNode::BlockEnd,
                ExecutionDependency::Immediate,
            );
        }

        for (_, memory_access_queue) in pending_memory_access {
            let remaining_dependencies = memory_access_queue.flush();
            for dependency in remaining_dependencies {
                let execution_dependency = match dependency.access_type {
                    MemoryAccessType::Capture => ExecutionDependency::AwaitCapture,
                    _ => ExecutionDependency::Immediate,
                };
                graph.add_edge(
                    dependency.node_id,
                    ScheduledGraphNode::BlockEnd,
                    execution_dependency,
                );
            }
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
                match edge {
                    ExecutionDependency::AwaitCapture => {
                        writeln!(f, " [ label=\"await capture\" ]")
                    }
                    ExecutionDependency::Immediate => {
                        writeln!(f, " [ label=\"immediate\" ]")
                    }
                }
            })
        })?;
        Ok(())
    }

    /// Return a particular-indexed instruction (if present).
    pub fn get_instruction(&self, node_id: usize) -> Option<&Instruction> {
        self.instructions.get(node_id)
    }

    /// Return all memory accesses by the instruction - in expressions, captures, and memory manipulation
    fn get_memory_accesses(_instruction: &Instruction) -> Option<MemoryAccess> {
        None
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
        if $working_instructions.is_empty() && $terminator.is_none() {
            $working_label = Self::generate_autoincremented_label(&$blocks);
        } else {
            let block = InstructionBlock::build(
                $working_instructions.iter().map(|el| el.clone()).collect(),
                $terminator,
                $program,
            )?;
            match $blocks.insert($working_label.clone(), block) {
                Some(_) => Err(ScheduleError {
                    instruction: Instruction::Label($working_label.clone()),
                    variant: ScheduleErrorVariant::DuplicateLabel,
                }), // Duplicate label
                None => Ok(()),
            }?;
            $working_instructions = vec![];
            $working_label = Self::generate_autoincremented_label(&$blocks);
        }
        Ok(())
    }};
}

impl ScheduledProgram {
    /// Structure a sequential program
    #[allow(unused_assignments)]
    pub fn from_program(program: &Program) -> ScheduleResult<Self> {
        let mut working_label = "start".to_owned();
        let mut working_instructions: Vec<Instruction> = vec![];
        let mut blocks = IndexMap::new();

        let instructions = program.to_instructions(false);

        for instruction in instructions {
            match instruction {
                Instruction::Arithmetic { .. }
                | Instruction::Capture { .. }
                | Instruction::Delay { .. }
                | Instruction::Fence { .. }
                | Instruction::Move { .. }
                | Instruction::Exchange { .. }
                | Instruction::Load { .. }
                | Instruction::Store { .. }
                | Instruction::Pulse { .. }
                | Instruction::SetFrequency { .. }
                | Instruction::SetPhase { .. }
                | Instruction::SetScale { .. }
                | Instruction::ShiftFrequency { .. }
                | Instruction::ShiftPhase { .. }
                | Instruction::SwapPhases { .. }
                | Instruction::RawCapture { .. }
                | Instruction::Reset { .. } => {
                    working_instructions.push(instruction);
                    Ok(())
                }
                Instruction::Gate { .. } | Instruction::Measurement { .. } => Err(ScheduleError {
                    instruction: instruction.clone(),
                    variant: ScheduleErrorVariant::UncalibratedInstruction,
                }),
                Instruction::CalibrationDefinition { .. }
                | Instruction::CircuitDefinition { .. }
                | Instruction::Declaration { .. }
                | Instruction::GateDefinition { .. }
                | Instruction::FrameDefinition { .. }
                | Instruction::MeasureCalibrationDefinition { .. }
                | Instruction::WaveformDefinition { .. } => Err(ScheduleError {
                    instruction: instruction.clone(),
                    variant: ScheduleErrorVariant::UnschedulableInstruction,
                }),

                Instruction::Pragma { .. } => {
                    // TODO: Handle pragmas. Here, we just silently discard them, but certain
                    // pragmas must be supported.
                    Ok(())
                }
                // _ => Err(()), // Unimplemented
                Instruction::Label(value) => {
                    terminate_working_block!(
                        None as Option<BlockTerminator>,
                        working_instructions,
                        blocks,
                        working_label,
                        program
                    )?;

                    working_label = value.clone();
                    Ok(())
                }
                Instruction::Jump { target } => {
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
                Instruction::JumpWhen { target, condition } => {
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
                Instruction::JumpUnless { target, condition } => {
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
        let mut label = format!("auto-label-{}", suffix);
        while block_labels.get(&label).is_some() {
            suffix += 1;
            label = format!("auto-label-{}", suffix);
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
    mod blocks {

        use super::super::ScheduledProgram;
        use crate::program::Program;
        use std::str::FromStr;

        #[test]
        fn without_control_flow() {
            let input = "
DEFFRAME 0 \"rx\":
    INITIAL-FREQUENCY: 1e6
PULSE 0 \"rx\" test(duration: 1e6)
DELAY 0 1.0
";
            let program = Program::from_str(input).unwrap();
            let scheduled_program = ScheduledProgram::from_program(&program).unwrap();
            assert_eq!(scheduled_program.blocks.len(), 1)
        }

        #[test]
        fn with_control_flow() {
            let input = "
DEFFRAME 0 \"rx\":
    INITIAL-FREQUENCY: 1e6
PULSE 0 \"rx\" test(duration: 1e-6)
PULSE 0 \"rx\" test(duration: 1e-6)
DELAY 0 1.0
LABEL @part2
PULSE 0 \"rx\" test(duration: 1e-6)
DELAY 0 2.0
LABEL @part3
DELAY 0 3.0
JUMP @part2
DELAY 0 4.0
JUMP @block5
HALT
LABEL @block5
DELAY 0 5.0
HALT
";
            let program = Program::from_str(input).unwrap();
            let scheduled_program = ScheduledProgram::from_program(&program).unwrap();
            println!("{:?}", scheduled_program.blocks);
            assert_eq!(scheduled_program.blocks.len(), 7);
        }
    }

    mod graph {
        use super::super::ScheduledProgram;
        use crate::program::Program;
        use std::str::FromStr;

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
DEFFRAME 0 \"rx\":
    INITIAL-FREQUENCY: 1e6
DEFFRAME 1 \"rx\":
    INITIAL-FREQUENCY: 1e6
DEFFRAME 2 \"rx\":
    INITIAL-FREQUENCY: 1e6
DEFFRAME 0 \"ro_rx\":
    INITIAL-FREQUENCY: 1e6
";

                    let program =
                        Program::from_str(&format!("{}\n{}", FRAME_DEFINITIONS, $input)).unwrap();
                    let scheduled_program = ScheduledProgram::from_program(&program).unwrap();

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
            "PULSE 0 \"rx\" test(duration: 1e6)"
        );

        build_dot_format_snapshot_test_case!(
            single_dependency,
            "
PULSE 0 \"rx\" test(duration: 1e6)
PULSE 0 \"rx\" test(duration: 1e6)
"
        );

        build_dot_format_snapshot_test_case!(
            chained_pulses,
            "
PULSE 0 \"rx\" test(duration: 1e6)
PULSE 0 \"rx\" test(duration: 1e6)
PULSE 0 \"rx\" test(duration: 1e6)
PULSE 0 \"rx\" test(duration: 1e6)
PULSE 0 \"rx\" test(duration: 1e6)
"
        );

        build_dot_format_snapshot_test_case!(
            different_frames_blocking,
            "
PULSE 0 \"rx\" test(duration: 1e6)
PULSE 1 \"rx\" test(duration: 1e6)
PULSE 2 \"rx\" test(duration: 1e6)
"
        );

        build_dot_format_snapshot_test_case!(
            different_frames_nonblocking,
            "
NONBLOCKING PULSE 0 \"rx\" test(duration: 1e6)
NONBLOCKING PULSE 1 \"rx\" test(duration: 1e6)
NONBLOCKING PULSE 2 \"rx\" test(duration: 1e6)
"
        );

        build_dot_format_snapshot_test_case!(
            fence_all_with_nonblocking_pulses,
            "
NONBLOCKING PULSE 0 \"rx\" test(duration: 1e6)
NONBLOCKING PULSE 1 \"rx\" test(duration: 1e6)
FENCE
NONBLOCKING PULSE 0 \"rx\" test(duration: 1e6)
NONBLOCKING PULSE 1 \"rx\" test(duration: 1e6)
"
        );
        build_dot_format_snapshot_test_case!(fence_all, "FENCE");

        build_dot_format_snapshot_test_case!(
            jump,
            "DECLARE ro BIT
LABEL @first-block
PULSE 0 \"rx\" test(duration: 1e6)
JUMP-UNLESS @third-block ro[0]
LABEL @second-block
PULSE 0 \"rx\" test(duration: 1e6)
LABEL @third-block
PULSE 0 \"rx\" test(duration: 1e6)
"
        );
    }
}
