//! Utilities for analysis of the dependency graph of a Quil Program

// Copyright 2021 Rigetti Computing
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

use std::collections::{HashMap, HashSet};

use indexmap::IndexMap;
use petgraph::graphmap::GraphMap;
use petgraph::Directed;

use crate::instruction::{
    FrameIdentifier, Instruction, InstructionHandler, Jump, JumpUnless, JumpWhen, Label,
    MemoryReference, Target,
};
use crate::{instruction::InstructionRole, program::Program};

pub use super::memory::MemoryAccessType;

#[derive(Debug, Clone)]
pub enum ScheduleErrorVariant {
    DuplicateLabel,
    UncalibratedInstruction,
    UnschedulableInstruction,
}

#[derive(Debug, Clone)]
pub struct ScheduleError {
    pub instruction_index: Option<usize>,
    pub instruction: Instruction,
    pub variant: ScheduleErrorVariant,
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

    /// The schedule of the downstream instruction depends on the upstream instruction.
    /// Per the Quil-T specification, the downstream instruction begins execution at
    /// the time that its latest upstream neighbor completes.
    Scheduled,

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

        self.pending_capture = None;
        self.pending_write = None;

        match access {
            Read => {
                self.pending_reads.push(node_id);
            }
            Capture => {
                for upstream_node_id in self.pending_reads.iter() {
                    result.push(MemoryAccessDependency {
                        node_id: *upstream_node_id,
                        access_type: Read,
                    });
                }

                self.pending_reads = vec![];
                self.pending_capture = Some(node_id);
            }

            Write => {
                for upstream_node_id in self.pending_reads.iter() {
                    result.push(MemoryAccessDependency {
                        node_id: *upstream_node_id,
                        access_type: Read,
                    });
                }

                self.pending_reads = vec![];
                self.pending_write = Some(node_id);
            }
        }

        result
    }
}

/// Add a dependency to an edge on the graph, whether that edge currently exists or not.
macro_rules! add_dependency {
    ($graph:expr, $source:expr => $target:expr, $dependency:expr) => {{
        let source = $source;
        let target = $target;
        let dependency = $dependency;
        match $graph.edge_weight_mut(source, target) {
            Some(edge) => {
                edge.insert(dependency);
            }
            None => {
                let mut edge = HashSet::new();
                edge.insert(dependency);
                $graph.add_edge(source, target, edge);
            }
        }
    }};
}

pub type DependencyGraph = GraphMap<ScheduledGraphNode, HashSet<ExecutionDependency>, Directed>;

/// An InstructionBlock of a ScheduledProgram is a group of instructions, identified by a string label,
/// which include no control flow instructions aside from an (optional) terminating control
/// flow instruction.
#[derive(Clone, Debug)]
pub struct InstructionBlock<'a> {
    pub instructions: Vec<&'a Instruction>,
    pub(super) graph: DependencyGraph,
    pub terminator: BlockTerminator<'a>,
}

impl<'a> Default for InstructionBlock<'a> {
    fn default() -> Self {
        Self {
            instructions: Default::default(),
            graph: Default::default(),
            terminator: BlockTerminator::Continue,
        }
    }
}

/// PreviousNodes is a structure which helps maintain ordering among instructions which operate on a given frame.
/// It works similarly to a multiple-reader-single-writer queue, where an instruction which "uses" a frame is like
/// a writer and an instruction which blocks that frame is like a reader. Multiple instructions may concurrently
/// block a frame, but an instruction may not use a frame while it is concurrently used or blocked.
///
/// ## Examples
///
/// Note that "depends on" is equivalent to "must execute at or after completion of." The interpretation of
/// "at or after" depends on the type of dependency and the compiler.
///
/// ```text
/// user --> user # a second user takes a dependency on the first
///
/// user --> blocker # multiple blockers take a dependency on the most recent user
///      \-> blocker
///      \-> blocker
///
/// blocker --> user --> blocker # users and blockers take dependencies on one another,
///                              # but blockers do not depend on other blocking instructions
/// ```
struct PreviousNodes {
    using: Option<ScheduledGraphNode>,
    blocking: HashSet<ScheduledGraphNode>,
}

impl Default for PreviousNodes {
    /// The default value for [PreviousNodes] is useful in that, if no previous nodes have been recorded
    /// as using a frame, we should consider that the start of the instruction block "uses" of that frame
    ///
    /// In other words, no instruction can be scheduled prior to the start of the instruction block
    /// and all scheduled instructions within the block depend on the block's start time, at least indirectly.
    fn default() -> Self {
        Self {
            using: Some(ScheduledGraphNode::BlockStart),
            blocking: HashSet::new(),
        }
    }
}

impl PreviousNodes {
    /// Register a node as using a frame, and return the instructions on which it should depend/wait for scheduling (if any).
    ///
    /// A node which uses a frame will block on any previous user or blocker of the frame, much like a writer in a read-write lock.
    fn get_dependencies_for_next_user(
        &mut self,
        node: ScheduledGraphNode,
    ) -> HashSet<ScheduledGraphNode> {
        let mut result = std::mem::take(&mut self.blocking);
        if let Some(previous_user) = self.using.replace(node) {
            result.insert(previous_user);
        }

        result
    }

    /// Register a node as blocking a frame, and return the instructions on which it should depend/wait for scheduling (if any).
    ///
    /// A node which blocks a frame will block on any previous user of the frame, but not concurrent blockers.
    ///
    /// If the frame is currently blocked by other nodes, it will add itself to the list of blockers,
    /// much like a reader in a read-write lock.
    fn get_dependency_for_next_blocker(
        &mut self,
        node: ScheduledGraphNode,
    ) -> Option<ScheduledGraphNode> {
        self.blocking.insert(node);
        self.using
    }

    /// Consume the [PreviousNodes] and return all nodes within.
    pub fn into_hashset(mut self) -> HashSet<ScheduledGraphNode> {
        if let Some(using) = self.using {
            self.blocking.insert(using);
        }
        self.blocking
    }
}

impl<'a> InstructionBlock<'a> {
    pub fn build(
        // The set of instructions in the block, a subset of the `program`.
        instructions: Vec<&'a Instruction>,
        terminator: Option<BlockTerminator<'a>>,
        program: &'a Program,
        custom_handler: &mut InstructionHandler,
    ) -> ScheduleResult<Self> {
        let mut graph: DependencyGraph = GraphMap::new();
        // Root node
        graph.add_node(ScheduledGraphNode::BlockStart);

        let mut last_classical_instruction = ScheduledGraphNode::BlockStart;

        // Store the instruction index of the last instruction to block that frame
        let mut last_instruction_by_frame: HashMap<FrameIdentifier, PreviousNodes> = HashMap::new();
        let mut last_timed_instruction_by_frame: HashMap<FrameIdentifier, PreviousNodes> =
            HashMap::new();

        // Store memory access reads and writes. Key is memory region name.
        // NOTE: this may be refined to serialize by memory region offset rather than by entire region.
        let mut pending_memory_access: HashMap<String, MemoryAccessQueue> = HashMap::new();

        for (index, &instruction) in instructions.iter().enumerate() {
            let node = graph.add_node(ScheduledGraphNode::InstructionIndex(index));

            match custom_handler.role_for_instruction(instruction) {
                // Classical instructions must be ordered by appearance in the program
                InstructionRole::ClassicalCompute => {
                    add_dependency!(graph, last_classical_instruction => node, ExecutionDependency::StableOrdering);

                    last_classical_instruction = node;
                    Ok(())
                }
                InstructionRole::RFControl => {
                    let matched_frames = custom_handler.matching_frames(instruction, program);
                    let is_scheduled = custom_handler.is_scheduled(instruction);

                    if let Some(matched_frames) = matched_frames {
                        for frame in matched_frames.used() {
                            if is_scheduled {
                                let previous_node_ids = last_timed_instruction_by_frame
                                    .entry((*frame).clone())
                                    .or_default()
                                    .get_dependencies_for_next_user(node);

                                for previous_node_id in previous_node_ids {
                                    add_dependency!(graph, previous_node_id => node, ExecutionDependency::Scheduled);
                                }
                            }

                            let previous_node_ids = last_instruction_by_frame
                                .entry((*frame).clone())
                                .or_default()
                                .get_dependencies_for_next_user(node);

                            for previous_node_id in previous_node_ids {
                                add_dependency!(graph, previous_node_id => node, ExecutionDependency::StableOrdering);
                            }
                        }

                        for frame in matched_frames.blocked() {
                            if is_scheduled {
                                if let Some(previous_node_id) = last_timed_instruction_by_frame
                                    .entry((*frame).clone())
                                    .or_default()
                                    .get_dependency_for_next_blocker(node)
                                {
                                    add_dependency!(graph, previous_node_id => node, ExecutionDependency::Scheduled);
                                }
                            }

                            if let Some(previous_node_id) = last_instruction_by_frame
                                .entry((*frame).clone())
                                .or_default()
                                .get_dependency_for_next_blocker(node)
                            {
                                add_dependency!(graph, previous_node_id => node, ExecutionDependency::StableOrdering);
                            }
                        }
                    }

                    Ok(())
                }
                InstructionRole::ControlFlow => Err(ScheduleError {
                    instruction_index: Some(index),
                    instruction: instruction.clone(),
                    variant: ScheduleErrorVariant::UnschedulableInstruction,
                }),
                InstructionRole::ProgramComposition => Err(ScheduleError {
                    instruction_index: Some(index),
                    instruction: instruction.clone(),
                    variant: ScheduleErrorVariant::UnschedulableInstruction,
                }),
            }?;

            let accesses = custom_handler.memory_accesses(instruction);
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
                                memory_dependency.access_type,
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

        for previous_nodes in last_timed_instruction_by_frame.into_values() {
            for node in previous_nodes.into_hashset() {
                add_dependency!(graph, node => ScheduledGraphNode::BlockEnd, ExecutionDependency::Scheduled);
            }
        }

        for previous_nodes in last_instruction_by_frame.into_values() {
            for node in previous_nodes.into_hashset() {
                add_dependency!(graph, node => ScheduledGraphNode::BlockEnd, ExecutionDependency::StableOrdering);
            }
        }

        // Examine all "pending" memory operations for all regions
        let remaining_dependencies = pending_memory_access
            .into_iter()
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

    /// Return a particular-indexed instruction (if present).
    pub fn get_instruction(&self, node_id: usize) -> Option<&Instruction> {
        self.instructions.get(node_id).copied()
    }

    /// Return the count of executable instructions in this block.
    pub fn len(&self) -> usize {
        self.instructions.len()
    }

    /// Return true if this block contains no executable instructions.
    pub fn is_empty(&self) -> bool {
        self.instructions.is_empty()
    }

    pub fn set_exit_condition(&mut self, terminator: BlockTerminator<'a>) {
        self.terminator = terminator;
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum BlockTerminator<'a> {
    Conditional {
        condition: &'a MemoryReference,
        target: &'a str,
        jump_if_condition_true: bool,
    },
    Unconditional {
        target: &'a str,
    },
    Continue,
    Halt,
}

#[derive(Clone, Debug)]
pub struct ScheduledProgram<'a> {
    /// All blocks within the ScheduledProgram, keyed on string label.
    pub blocks: IndexMap<String, InstructionBlock<'a>>,
}

/// Builds an [`InstructionBlock`] from provided instructions, terminator, and program, then tracks
/// the block and its label, and resets the instruction list and label for a future block to use.
///
/// The "working block" is that being traversed within the program, accumulating instructions located
/// between control instructions (such as `LABEL` and 'JUMP`). When such a control instruction is reached,
/// this function performs the work to close out and store the instructions in the current "working block"
/// and then reset the state to prepare for the next block.
///
/// # Errors
///
/// If an error occurs, `working_instructions` and/or `working_label` may have been emptied and
/// cannot be relied on to be unchanged.
fn terminate_working_block<'a>(
    terminator: Option<BlockTerminator<'a>>,
    working_instructions: &mut Vec<&'a Instruction>,
    blocks: &mut IndexMap<String, InstructionBlock<'a>>,
    working_label: &mut Option<&'a str>,
    program: &'a Program,
    instruction_index: Option<usize>,
    custom_handler: &mut InstructionHandler,
) -> ScheduleResult<()> {
    // If this "block" has no instructions and no terminator, it's not worth storing - skip it
    if working_instructions.is_empty() && terminator.is_none() && working_label.is_none() {
        return Ok(());
    }

    // This leaves working_instructions and working_label in their respective "empty" states.
    let block = InstructionBlock::build(
        std::mem::take(working_instructions),
        terminator,
        program,
        custom_handler,
    )?;

    let label = working_label
        .take()
        .map(String::from)
        .unwrap_or_else(|| ScheduledProgram::generate_autoincremented_label(blocks));

    if blocks.insert(label.clone(), block).is_some() {
        return Err(ScheduleError {
            instruction_index,
            instruction: Instruction::Label(Label {
                target: Target::Fixed(label),
            }),
            variant: ScheduleErrorVariant::DuplicateLabel,
        });
    }

    Ok(())
}

impl<'a> ScheduledProgram<'a> {
    /// Structure a sequential program
    #[allow(unused_assignments)]
    pub fn from_program(
        program: &'a Program,
        custom_handler: &mut InstructionHandler,
    ) -> ScheduleResult<Self> {
        let mut working_label = None;
        let mut working_instructions: Vec<&'a Instruction> = vec![];
        let mut blocks = IndexMap::new();

        for (index, instruction) in program.body_instructions().enumerate() {
            let instruction_index = Some(index);
            match instruction {
                Instruction::Arithmetic(_)
                | Instruction::Comparison(_)
                | Instruction::BinaryLogic(_)
                | Instruction::Convert(_)
                | Instruction::UnaryLogic(_)
                | Instruction::Capture(_)
                | Instruction::Delay(_)
                | Instruction::Fence(_)
                | Instruction::Include(_)
                | Instruction::Move(_)
                | Instruction::Nop
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
                | Instruction::Reset(_)
                | Instruction::Wait => {
                    working_instructions.push(instruction);
                }
                Instruction::CalibrationDefinition(_)
                | Instruction::CircuitDefinition(_)
                | Instruction::Declaration(_)
                | Instruction::GateDefinition(_)
                | Instruction::FrameDefinition(_)
                | Instruction::MeasureCalibrationDefinition(_)
                | Instruction::WaveformDefinition(_) => {}
                Instruction::Pragma(_) => {
                    working_instructions.push(instruction);
                }
                Instruction::Label(Label {
                    target: Target::Fixed(value),
                }) => {
                    terminate_working_block(
                        None as Option<BlockTerminator>,
                        &mut working_instructions,
                        &mut blocks,
                        &mut working_label,
                        program,
                        instruction_index,
                        custom_handler,
                    )?;

                    working_label = Some(value);
                }
                Instruction::Jump(Jump {
                    target: Target::Fixed(target),
                }) => {
                    terminate_working_block(
                        Some(BlockTerminator::Unconditional { target }),
                        &mut working_instructions,
                        &mut blocks,
                        &mut working_label,
                        program,
                        instruction_index,
                        custom_handler,
                    )?;
                }
                Instruction::JumpWhen(JumpWhen {
                    target: Target::Fixed(target),
                    condition,
                }) => {
                    terminate_working_block(
                        Some(BlockTerminator::Conditional {
                            target,
                            condition,
                            jump_if_condition_true: true,
                        }),
                        &mut working_instructions,
                        &mut blocks,
                        &mut working_label,
                        program,
                        instruction_index,
                        custom_handler,
                    )?;
                }
                Instruction::JumpUnless(JumpUnless {
                    target: Target::Fixed(target),
                    condition,
                }) => {
                    terminate_working_block(
                        Some(BlockTerminator::Conditional {
                            target,
                            condition,
                            jump_if_condition_true: false,
                        }),
                        &mut working_instructions,
                        &mut blocks,
                        &mut working_label,
                        program,
                        instruction_index,
                        custom_handler,
                    )?;
                }
                Instruction::Halt => terminate_working_block(
                    Some(BlockTerminator::Halt),
                    &mut working_instructions,
                    &mut blocks,
                    &mut working_label,
                    program,
                    instruction_index,
                    custom_handler,
                )?,
                Instruction::Gate(_)
                | Instruction::Measurement(_)
                | Instruction::Label(Label {
                    target: Target::Placeholder(_),
                })
                | Instruction::Jump(_)
                | Instruction::JumpWhen(_)
                | Instruction::JumpUnless(_) => {
                    return Err(ScheduleError {
                        instruction_index,
                        instruction: instruction.clone(),
                        variant: ScheduleErrorVariant::UncalibratedInstruction,
                    })
                }
            };
        }

        terminate_working_block(
            None as Option<BlockTerminator>,
            &mut working_instructions,
            &mut blocks,
            &mut working_label,
            program,
            None,
            custom_handler,
        )?;

        Ok(ScheduledProgram { blocks })
    }

    fn generate_autoincremented_label(block_labels: &IndexMap<String, InstructionBlock>) -> String {
        let mut suffix = 0;
        let mut label = format!("block_{suffix}");
        while block_labels.get(&label).is_some() {
            suffix += 1;
            label = format!("block_{suffix}");
        }
        label
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::Pragma;
    use crate::program::{MatchedFrames, MemoryAccesses};

    #[cfg(feature = "graphviz-dot")]
    mod custom_handler {
        use super::*;

        use crate::instruction::PragmaArgument;
        use crate::program::frame::FrameMatchCondition;
        use crate::program::graphviz_dot::tests::build_dot_format_snapshot_test_case;

        /// Generates a custom [`InstructionHandler`] that specially handles two `PRAGMA` instructions:
        ///
        /// - `NO-OP` is considered a `ClassicalCompute` instruction that does nothing
        /// - `RAW-INSTRUCTION` is an `RFControl` instruction that is scheduled on all frames by default
        ///   or the frame names specified as arguments, and reads from `ro`.
        ///
        /// Note that any program being tested must define at least one frame for `RAW-INSTRUCTION` to
        /// have any effect.
        fn get_custom_handler() -> InstructionHandler {
            const NO_OP: &str = "NO-OP";
            const RAW_INSTRUCTION: &str = "RAW-INSTRUCTION";

            InstructionHandler::default()
                .set_is_scheduled(|instruction| match instruction {
                    Instruction::Pragma(Pragma { name, .. }) if name == NO_OP => Some(false),
                    Instruction::Pragma(Pragma { name, .. }) if name == RAW_INSTRUCTION => {
                        Some(true)
                    }
                    _ => None,
                })
                .set_role_for_instruction(|instruction| match instruction {
                    Instruction::Pragma(Pragma { name, .. }) if name == NO_OP => {
                        Some(InstructionRole::ClassicalCompute)
                    }
                    Instruction::Pragma(Pragma { name, .. }) if name == RAW_INSTRUCTION => {
                        Some(InstructionRole::RFControl)
                    }
                    _ => None,
                })
                .set_matching_frames(|instruction, program| match instruction {
                    Instruction::Pragma(Pragma { name, .. }) if name == NO_OP => Some(None),
                    Instruction::Pragma(Pragma {
                        name, arguments, ..
                    }) if name == RAW_INSTRUCTION => Some(Some({
                        let frame_condition = if arguments.is_empty() {
                            FrameMatchCondition::All
                        } else {
                            FrameMatchCondition::AnyOfNames(
                                arguments
                                    .iter()
                                    .filter_map(|arg| match arg {
                                        PragmaArgument::Identifier(name) => Some(name.as_str()),
                                        PragmaArgument::Integer(_) => None,
                                    })
                                    .collect(),
                            )
                        };

                        let used = program
                            .frames
                            .get_matching_keys_for_condition(frame_condition);

                        MatchedFrames {
                            used,
                            blocked: HashSet::new(),
                        }
                    })),
                    _ => None,
                })
                .set_memory_accesses(|instruction| match instruction {
                    Instruction::Pragma(Pragma { name, .. }) if name == NO_OP => {
                        Some(MemoryAccesses::default())
                    }
                    Instruction::Pragma(Pragma { name, .. }) if name == RAW_INSTRUCTION => Some({
                        MemoryAccesses {
                            captures: HashSet::new(),
                            reads: [String::from("ro")].into(),
                            writes: HashSet::new(),
                        }
                    }),
                    _ => None,
                })
        }

        build_dot_format_snapshot_test_case! {
            only_pragmas_without_frames,
            r#"
DEFFRAME 0 "quux":
    SAMPLE-RATE: 1.0
    INITIAL-FREQUENCY: 1e8
PRAGMA NO-OP
PRAGMA RAW-INSTRUCTION
PRAGMA RAW-INSTRUCTION
PRAGMA NO-OP
PRAGMA RAW-INSTRUCTION
"#,
            &mut get_custom_handler(),
        }

        build_dot_format_snapshot_test_case! {
            only_pragmas_with_frames,
            r#"
DEFFRAME 0 "foo":
    SAMPLE-RATE: 1.0
    INITIAL-FREQUENCY: 1e8
DEFFRAME 1 "bar":
    SAMPLE-RATE: 1.0
    INITIAL-FREQUENCY: 1e8

PRAGMA NO-OP
PRAGMA RAW-INSTRUCTION foo
PRAGMA RAW-INSTRUCTION bar
PRAGMA NO-OP
PRAGMA RAW-INSTRUCTION foo bar
PRAGMA NO-OP
PRAGMA RAW-INSTRUCTION foo
"#,
            &mut get_custom_handler(),
        }

        build_dot_format_snapshot_test_case! {
            mixed_pragmas_and_pulses,
            r#"
DEFFRAME 0 "foo":
    SAMPLE-RATE: 1.0
    INITIAL-FREQUENCY: 1e8
DEFFRAME 1 "bar":
    SAMPLE-RATE: 1.0
    INITIAL-FREQUENCY: 1e8

PRAGMA NO-OP
PRAGMA RAW-INSTRUCTION foo
PULSE 1 "bar" gaussian(duration: 1, fwhm: 2, t0: 3)
PRAGMA RAW-INSTRUCTION foo bar
PRAGMA NO-OP
PULSE 0 "foo" gaussian(duration: 1, fwhm: 2, t0: 3)
PRAGMA RAW-INSTRUCTION bar
PULSE 0 "foo" gaussian(duration: 1, fwhm: 2, t0: 3)
PULSE 1 "bar" gaussian(duration: 1, fwhm: 2, t0: 3)
PRAGMA NO-OP
PRAGMA RAW-INSTRUCTION foo
"#,
            &mut get_custom_handler(),
        }
    }
}
