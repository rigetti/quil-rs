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

use petgraph::graphmap::GraphMap;
use petgraph::Directed;

use crate::instruction::{
    ExternSignatureMap, FrameIdentifier, Instruction, InstructionHandler, Target,
};
use crate::program::analysis::{
    BasicBlock, BasicBlockOwned, BasicBlockTerminator, ControlFlowGraph,
};
use crate::{instruction::InstructionRole, program::Program, quil::Quil};

pub use crate::program::memory::MemoryAccessType;

#[derive(Debug, Clone, Copy)]
pub enum ScheduleErrorVariant {
    DuplicateLabel,
    Extern,
    UncalibratedInstruction,
    UnresolvedCallInstruction,
    UnschedulableInstruction,
}

#[derive(Debug, Clone, thiserror::Error)]
#[error("Error scheduling instruction {}: {}: {variant:?}", .instruction_index.map(|i| i.to_string()).unwrap_or(String::from("")), .instruction.to_quil_or_debug())]
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

/// A [`ScheduledBasicBlock`] is a wrapper around a [`BasicBlock`] which includes a graph expressing the vector clock
/// among the instructions according to the Quil specification.
///
/// If instruction A blocks instruction B (because of shared use of a frame), then there will be an edge from A to B
/// in the graph.
#[derive(Clone, Debug)]
pub struct ScheduledBasicBlock<'a> {
    basic_block: BasicBlock<'a>,
    pub(super) graph: DependencyGraph,
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

impl<'a> ScheduledBasicBlock<'a> {
    /// Build a scheduled basic block from a basic block and a program.
    pub fn build(
        basic_block: BasicBlock<'a>,
        program: &'a Program,
        custom_handler: &mut InstructionHandler,
    ) -> ScheduleResult<Self> {
        let mut graph: DependencyGraph = GraphMap::new();
        // Root node
        graph.add_node(ScheduledGraphNode::BlockStart);

        // The set of classical instructions that do not have outgoing edges (i.e. there are no
        // downstream instructions that depend on them). After iterating over all instructions,
        // the set of trailing classical instructions will need an outgoing edge to the block end.
        let mut trailing_classical_instructions: HashSet<ScheduledGraphNode> = HashSet::new();

        // Store the instruction index of the last instruction to block that frame
        let mut last_instruction_by_frame: HashMap<FrameIdentifier, PreviousNodes> = HashMap::new();
        let mut last_timed_instruction_by_frame: HashMap<FrameIdentifier, PreviousNodes> =
            HashMap::new();

        // Store memory access reads and writes. Key is memory region name.
        // NOTE: this may be refined to serialize by memory region offset rather than by entire region.
        let mut pending_memory_access: HashMap<String, MemoryAccessQueue> = HashMap::new();

        let extern_signature_map = ExternSignatureMap::try_from(program.extern_pragma_map.clone())
            .map_err(|(pragma, _)| ScheduleError {
                instruction_index: None,
                instruction: Instruction::Pragma(pragma),
                variant: ScheduleErrorVariant::Extern,
            })?;
        for (index, &instruction) in basic_block.instructions().iter().enumerate() {
            let node = graph.add_node(ScheduledGraphNode::InstructionIndex(index));

            let accesses = custom_handler
                .memory_accesses(instruction, &extern_signature_map)
                .map_err(|_| ScheduleError {
                    instruction_index: Some(index),
                    instruction: instruction.clone(),
                    variant: ScheduleErrorVariant::UnresolvedCallInstruction,
                })?;

            let memory_dependencies = [
                (accesses.reads, MemoryAccessType::Read),
                (accesses.writes, MemoryAccessType::Write),
                (accesses.captures, MemoryAccessType::Capture),
            ]
            .iter()
            .flat_map(|(regions, access_type)| {
                regions
                    .iter()
                    .flat_map(|region| {
                        pending_memory_access
                            .entry(region.clone())
                            .or_default()
                            // NOTE: This mutates the underlying `MemoryAccessQueue` by registering
                            // the instruction node.
                            .get_blocking_nodes(node, access_type)
                    })
                    // Collecting is necessary to avoid "captured variable cannot escape FnMut closure body" errors
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();
            let has_memory_dependencies = !memory_dependencies.is_empty();
            for memory_dependency in memory_dependencies {
                // Test to make sure that no instructions depend directly on themselves
                if memory_dependency.node_id != node {
                    let execution_dependency =
                        ExecutionDependency::AwaitMemoryAccess(memory_dependency.access_type);
                    add_dependency!(graph, memory_dependency.node_id => node, execution_dependency);
                    // This memory dependency now has an outgoing edge, so it is no longer a trailing classical
                    // instruction. If the memory dependency is not a classical instruction, this
                    // has no effect.
                    trailing_classical_instructions.remove(&memory_dependency.node_id);
                }
            }

            match custom_handler.role_for_instruction(instruction) {
                // Classical instructions must be ordered by appearance in the program
                InstructionRole::ClassicalCompute => {
                    // If this instruction has no memory dependencies, it is a leading classical
                    // instruction and needs an incoming edge from the block start.
                    if !has_memory_dependencies {
                        add_dependency!(graph, ScheduledGraphNode::BlockStart => node, ExecutionDependency::StableOrdering);
                    }
                    trailing_classical_instructions.insert(node);
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
        }

        // Link all pending dependency nodes to the end of the block, to ensure that the block
        // does not terminate until these are complete
        for trailing_classical_instruction in trailing_classical_instructions {
            add_dependency!(graph, trailing_classical_instruction => ScheduledGraphNode::BlockEnd, ExecutionDependency::StableOrdering);
        }

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

        // Maintain the invariant that the block start node has a connecting path to the block end node.
        if basic_block.instructions().is_empty() {
            add_dependency!(graph, ScheduledGraphNode::BlockStart => ScheduledGraphNode::BlockEnd, ExecutionDependency::StableOrdering);
        }

        Ok(ScheduledBasicBlock { graph, basic_block })
    }

    pub fn get_dependency_graph(&self) -> &DependencyGraph {
        &self.graph
    }

    pub fn instructions(&'a self) -> &[&'a Instruction] {
        self.basic_block.instructions()
    }

    /// Return a particular-indexed instruction (if present).
    pub fn get_instruction(&self, node_id: usize) -> Option<&Instruction> {
        self.instructions().get(node_id).copied()
    }

    pub fn label(&self) -> Option<&Target> {
        self.basic_block.label()
    }

    /// Return the count of executable instructions in this block.
    pub fn len(&self) -> usize {
        self.instructions().len()
    }

    /// Return true if this block contains no executable instructions.
    pub fn is_empty(&self) -> bool {
        self.instructions().is_empty()
    }

    pub fn terminator(&self) -> &BasicBlockTerminator {
        self.basic_block.terminator()
    }

    pub fn basic_block(&self) -> &BasicBlock<'a> {
        &self.basic_block
    }
}

#[derive(Clone, Debug)]
pub struct ScheduledProgram<'a> {
    basic_blocks: Vec<ScheduledBasicBlock<'a>>,
}

impl<'a> ScheduledProgram<'a> {
    /// Structure a sequential program
    #[allow(unused_assignments)]
    pub fn from_program(
        program: &'a Program,
        custom_handler: &mut InstructionHandler,
    ) -> ScheduleResult<Self> {
        let control_flow_graph = ControlFlowGraph::from(program);
        Ok(Self {
            basic_blocks: control_flow_graph
                .into_blocks()
                .into_iter()
                .map(|block| ScheduledBasicBlock::build(block, program, custom_handler))
                .collect::<ScheduleResult<Vec<_>>>()?,
        })
    }

    pub fn basic_blocks(&self) -> &[ScheduledBasicBlock<'_>] {
        self.basic_blocks.as_ref()
    }

    pub fn into_basic_blocks(self) -> Vec<ScheduledBasicBlock<'a>> {
        self.basic_blocks
    }
}

#[derive(Clone, Debug)]
pub struct ScheduledBasicBlockOwned {
    basic_block: BasicBlockOwned,
    graph: DependencyGraph,
}

impl<'a> From<&'a ScheduledBasicBlockOwned> for ScheduledBasicBlock<'a> {
    fn from(block: &'a ScheduledBasicBlockOwned) -> Self {
        Self {
            basic_block: (&block.basic_block).into(),
            graph: block.graph.clone(),
        }
    }
}

impl<'a> From<ScheduledBasicBlock<'a>> for ScheduledBasicBlockOwned {
    fn from(block: ScheduledBasicBlock) -> Self {
        Self {
            basic_block: block.basic_block.into(),
            graph: block.graph.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[cfg(feature = "graphviz-dot")]
    use crate::program::scheduling::graphviz_dot::tests::build_dot_format_snapshot_test_case;

    #[cfg(feature = "graphviz-dot")]
    mod custom_handler {
        use super::*;

        use crate::instruction::Pragma;
        use crate::instruction::PragmaArgument;
        use crate::program::frame::FrameMatchCondition;
        use crate::program::{MatchedFrames, MemoryAccesses};

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

    // Because any instruction that reads a particular region must be preceded by any earlier instructions that write to/ capture that memory region,
    // we expect an edge from the first load to the second (0 -> 1).
    #[cfg(feature = "graphviz-dot")]
    build_dot_format_snapshot_test_case! {
        classical_write_read_load_load,
        r#"
DECLARE params1 REAL[1]
DECLARE params2 REAL[1]
DECLARE params3 REAL[1]
DECLARE integers INTEGER[1]
LOAD params2[0] params3 integers[0] # writes params2 
LOAD params1[0] params2 integers[0] # reads params2
"#
    }

    // Because any instruction that reads a particular region must be preceded by any earlier instructions that write to/ capture that memory region,
    // we expect an edge from the mul to the load (0 -> 1).
    #[cfg(feature = "graphviz-dot")]
    build_dot_format_snapshot_test_case! {
        classical_write_read_mul_load,
        r#"
DECLARE params1 REAL[1]
DECLARE params2 REAL[1]
DECLARE integers INTEGER[1]

MUL params2[0] 2 # reads and writes params2
LOAD params1[0] params2 integers[0] # just reads params2
"#
    }

    // Because any instruction that reads a particular region must be preceded by any earlier instructions that write to/ capture that memory region,
    // we expect an edge from the mul to the add (0 -> 1).
    #[cfg(feature = "graphviz-dot")]
    build_dot_format_snapshot_test_case! {
        classical_write_read_add_mul,
        r#"
DECLARE params1 REAL[1]
DECLARE params2 REAL[1]
DECLARE integers INTEGER[1]

ADD params1[0] 1 # this reads and writes params1
MUL params1[0] 2 # this reads and writes params1
"#
    }

    // Because any instruction that reads a particular region must precede any later instructions that write to/ capture that memory region,
    // we expect an edge from the first load to the second (0, 1).
    #[cfg(feature = "graphviz-dot")]
    build_dot_format_snapshot_test_case! {
        classical_read_write_load_load,
        r#"
DECLARE params1 REAL[1]
DECLARE params2 REAL[1]
DECLARE integers INTEGER[1]

LOAD params1[0] params2 integers[0] # reads params2
LOAD params2[0] params3 integers[0] # writes params2
"#
    }

    // Because any instruction that reads a particular region must precede any later instructions that write to/ capture that memory region,
    // we expect an edge from the load to the mul (0, 1).
    #[cfg(feature = "graphviz-dot")]
    build_dot_format_snapshot_test_case! {
        classical_read_write_load_mul,
        r#"
DECLARE params1 REAL[1]
DECLARE params2 REAL[1]
DECLARE integers INTEGER[1]

LOAD params1[0] params2 integers[0] # reads params2
MUL params2[0] 2                    # reads and writes params2
"#
    }

    // Because memory reading and writing dependencies also apply to RfControl instructions, we
    // expect edges from the first load to the first shift-phase (0 -> 1), the first shift-phase
    // to the second load (1 -> 2), and the second load to the second shift-phase (2 -> 3).
    #[cfg(feature = "graphviz-dot")]
    build_dot_format_snapshot_test_case! {
        quantum_write_parameterized_operations,
        r#"
DEFFRAME 0 "rx":
    INITIAL-FREQUENCY: 1e8
DEFFRAME 1 "rx":
    INITIAL-FREQUENCY: 1e8

DECLARE params1 REAL[1]
DECLARE params2 REAL[1]
DECLARE integers INTEGER[1]

LOAD params2[0] params1 integers[0] # writes params2
SHIFT-PHASE 0 "rf" params2[0]       # reads params2
LOAD params2[0] params1 integers[1] # writes params2
SHIFT-PHASE 1 "rf" params2[0]       # reads params2
"#
    }

    // Because a pragma by default will have no memory accesses, it should only have edges from the block start and to the block end.
    #[cfg(feature = "graphviz-dot")]
    build_dot_format_snapshot_test_case! {
        classical_no_memory_pragma,
        r#"PRAGMA example"#
    }

    #[cfg(feature = "graphviz-dot")]
    build_dot_format_snapshot_test_case! {
        write_capture_read,
        r#"
DECLARE bits BIT[1]
DECLARE integers INTEGER[1]
LOAD bits[0] bits2 integers[0] # write
NONBLOCKING CAPTURE 0 "Transmon-0_readout_rx" flat(duration: 2.0000000000000003e-06, iq: 1.0, scale: 1.0, phase: 0.8745492960861506, detuning: 0.0) bits[0] # capture
LOAD bits3[0] bits integers[0] # read
"#
    }

    #[cfg(feature = "graphviz-dot")]
    build_dot_format_snapshot_test_case! {
        write_write_read,
        r#"
DECLARE bits BIT[1]
DECLARE bits2 BIT[1]
DECLARE bits3 BIT[1]
DECLARE integers INTEGER[1]
LOAD bits[0] bits2 integers[0] # write
LOAD bits[0] bits3 integers[0] # write
LOAD bits4[0] bits integers[0] # read
"#
    }
}
