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

use self::dependency_queue::DependencyQueue;
use crate::instruction::{
    ExternSignatureMap, FrameIdentifier, Instruction, InstructionHandler, Target,
};
use crate::program::analysis::{
    BasicBlock, BasicBlockOwned, BasicBlockTerminator, ControlFlowGraph,
};
use crate::{instruction::InstructionRole, program::Program, quil::Quil};

pub use crate::program::memory::MemoryAccessType;

mod dependency_queue;

#[derive(Debug, Clone, Copy)]
pub enum ScheduleErrorVariant {
    DuplicateLabel,
    Extern,
    UncalibratedInstruction,
    UnresolvedCallInstruction,
    ControlFlowNotBlockTerminator,
    UnschedulableInstruction,
}

#[derive(Debug, Clone, thiserror::Error)]
#[error(
    "Error scheduling {}: {}: {variant:?}",
    .instruction_node.map_or_else(||  "an instruction".to_string(), |node| node.to_string()),
    .instruction.to_quil_or_debug(),
)]
pub struct ScheduleError {
    pub instruction_node: Option<ScheduledGraphNode>,
    pub instruction: Instruction,
    pub variant: ScheduleErrorVariant,
}

pub type ScheduleResult<T> = Result<T, ScheduleError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Hash, Ord)]
pub enum ScheduledGraphNode {
    BlockStart,
    InstructionIndex(usize),
    BlockEnd,
}

impl std::fmt::Display for ScheduledGraphNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BlockStart => write!(f, "the start of the block"),
            Self::InstructionIndex(ix) => write!(f, "instruction {ix}"),
            Self::BlockEnd => write!(f, "the end-of-block terminator"),
        }
    }
}

/// A MemoryAccessDependency expresses a dependency that one node has on another to complete
/// some type of memory access prior to the dependent node's execution.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
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

/// How an instruction affects a frame.
///
/// An instruction may do one of two things:
///
/// 1. It may [*block*][Self::Blocking] the frame, which indicates that while it does not play on
///    that frame itself, it must prevent any other instructions from playing on it.
///
/// 2. It may [*use*][Self::Using] the frame, which indicates that the instruction play on or
///    modifies that frame.
///
/// These may be thought of as reads and writes of a frame, respectively: blocking does not have an
/// affect on the frame but must be ordered with respect to uses, while uses must happen in a
/// specific order.  This is relevant for [`dependency_queue::Access`].
///
/// See also [`crate::program::MatchedFrames`] for the type that keeps track of the specific frames
/// that are blocked or used by an instruction.
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum InstructionFrameInteraction {
    Blocking,
    Using,
}

impl<'a> ScheduledBasicBlock<'a> {
    /// Build a scheduled basic block from a basic block and a program.
    pub fn build<H: InstructionHandler>(
        basic_block: BasicBlock<'a>,
        program: &'a Program,
        handler: &H,
    ) -> ScheduleResult<Self> {
        let mut graph: DependencyGraph = GraphMap::new();
        // Root node
        graph.add_node(ScheduledGraphNode::BlockStart);

        // The set of classical instructions that do not have outgoing edges (i.e. there are no
        // downstream instructions that depend on them). After iterating over all instructions,
        // the set of trailing classical instructions will need an outgoing edge to the block end.
        let mut trailing_classical_instructions: HashSet<ScheduledGraphNode> = HashSet::new();

        // Store the instruction index of the last instruction to block that frame
        let mut last_instruction_by_frame: HashMap<
            FrameIdentifier,
            DependencyQueue<InstructionFrameInteraction>,
        > = HashMap::new();

        let mut last_timed_instruction_by_frame: HashMap<
            FrameIdentifier,
            DependencyQueue<InstructionFrameInteraction>,
        > = HashMap::new();

        // Keep track of memory accesses (reads/writes) to each region, so that they can be
        // serialized.  This serialization is done by memory region; that is, writes to `region[0]`
        // are not allowed to be concurrent with writes to `region[1]`.
        //
        // This could be refined to serialize writes to each memory region *and offset*, so writes
        // to `region[0]` could happen in parallel with writes to `region[1]`, although this would
        // require further refinement of [`Instruction::get_memory_accesses`] and the
        // [`crate::program::memory::MemoryAccesses`] type.  In particular, we would need to capture
        // accesses that read from/write to potentially an entire region, such as `LOAD` and
        // `STORE`, as well as accesses that only access a statically known index.
        let mut pending_memory_access: HashMap<String, DependencyQueue<MemoryAccessType>> =
            HashMap::new();

        let extern_signature_map = ExternSignatureMap::try_from(program.extern_pragma_map.clone())
            .map_err(|(pragma, _)| ScheduleError {
                instruction_node: None,
                instruction: Instruction::Pragma(pragma),
                variant: ScheduleErrorVariant::Extern,
            })?;

        let terminator = basic_block.terminator().clone().into_instruction();
        let terminator_ref = terminator.as_ref();

        let instructions_iter = basic_block
            .instructions()
            .iter()
            .enumerate()
            .map(|(index, instr)| (ScheduledGraphNode::InstructionIndex(index), *instr))
            .chain(terminator_ref.map(|instr| (ScheduledGraphNode::BlockEnd, instr)));

        for (node, instruction) in instructions_iter {
            graph.add_node(node);

            let accesses = handler
                .memory_accesses(&extern_signature_map, instruction)
                .map_err(|_| ScheduleError {
                    instruction_node: Some(node),
                    instruction: instruction.clone(),
                    variant: ScheduleErrorVariant::UnresolvedCallInstruction,
                })?;

            let memory_dependencies = [
                (accesses.reads, MemoryAccessType::Read),
                (accesses.writes, MemoryAccessType::Write),
                (accesses.captures, MemoryAccessType::Capture),
            ]
            .into_iter()
            .flat_map(|(regions, access_type)| {
                regions
                    .into_iter()
                    .flat_map(|region| {
                        pending_memory_access
                            .entry(region)
                            .or_default()
                            // NOTE: This mutates the underlying `DependencyQueue` by registering
                            // the instruction node.
                            .record_access_and_get_dependencies(node, access_type)
                    })
                    // We have to `collect` into a `Vec` to finish our accesses to
                    // `pending_memory_access`.
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

            // Does this instruction have no incoming edges?  (Only used if this instruction is
            // classical.)
            let mut leading_instruction = true;

            for memory_dependency in memory_dependencies {
                // Test to make sure that no instructions depend directly on themselves
                if memory_dependency.node_id != node {
                    let execution_dependency =
                        ExecutionDependency::AwaitMemoryAccess(memory_dependency.access_type);

                    add_dependency!(graph, memory_dependency.node_id => node, execution_dependency);

                    // This instruction now has an incoming edge, so it is not leading.
                    leading_instruction = false;

                    // This memory dependency now has an outgoing edge, so it is no longer a
                    // trailing classical instruction. If the memory dependency is not a classical
                    // instruction, this has no effect.
                    trailing_classical_instructions.remove(&memory_dependency.node_id);
                }
            }

            // We're done defining `leading_instruction`, it's no longer `mut`.
            let leading_instruction = leading_instruction;

            match handler.role(instruction) {
                // Classical instructions must be ordered by appearance in the program
                InstructionRole::ClassicalCompute => {
                    // All classical instructions must occur after the block start.
                    if leading_instruction {
                        add_dependency!(graph, ScheduledGraphNode::BlockStart => node, ExecutionDependency::StableOrdering);
                    }
                    trailing_classical_instructions.insert(node);
                    Ok(())
                }
                InstructionRole::RFControl => {
                    let matched_frames = handler.matching_frames(program, instruction);
                    let is_scheduled = handler.is_scheduled(instruction);

                    if let Some(matched_frames) = matched_frames {
                        for frame in matched_frames.used {
                            if is_scheduled {
                                let previous_node_ids = last_timed_instruction_by_frame
                                    .entry((*frame).clone())
                                    .or_default()
                                    .record_access_and_get_dependencies(
                                        node,
                                        InstructionFrameInteraction::Using,
                                    );

                                for previous_node_id in previous_node_ids {
                                    add_dependency!(graph, previous_node_id => node, ExecutionDependency::Scheduled);
                                }
                            }

                            let previous_node_ids = last_instruction_by_frame
                                .entry((*frame).clone())
                                .or_default()
                                .record_access_and_get_dependencies(
                                    node,
                                    InstructionFrameInteraction::Using,
                                );

                            for previous_node_id in previous_node_ids {
                                add_dependency!(graph, previous_node_id => node, ExecutionDependency::StableOrdering);
                            }
                        }

                        for frame in matched_frames.blocked {
                            if is_scheduled {
                                let previous_node_ids = last_timed_instruction_by_frame
                                    .entry((*frame).clone())
                                    .or_default()
                                    .record_access_and_get_dependencies(
                                        node,
                                        InstructionFrameInteraction::Blocking,
                                    );
                                for previous_node_id in previous_node_ids {
                                    add_dependency!(graph, previous_node_id => node, ExecutionDependency::Scheduled);
                                }
                            }

                            let previous_node_ids = last_instruction_by_frame
                                .entry((*frame).clone())
                                .or_default()
                                .record_access_and_get_dependencies(
                                    node,
                                    InstructionFrameInteraction::Blocking,
                                );
                            for previous_node_id in previous_node_ids {
                                add_dependency!(graph, previous_node_id => node, ExecutionDependency::StableOrdering);
                            }
                        }
                    }

                    Ok(())
                }
                InstructionRole::ControlFlow => {
                    if let ScheduledGraphNode::BlockEnd = node {
                        Ok(())
                    } else {
                        Err(ScheduleError {
                            instruction_node: Some(node),
                            instruction: instruction.clone(),
                            variant: ScheduleErrorVariant::ControlFlowNotBlockTerminator,
                        })
                    }
                }
                InstructionRole::ProgramComposition => Err(ScheduleError {
                    instruction_node: Some(node),
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

        for node in last_timed_instruction_by_frame
            .into_values()
            .flat_map(DependencyQueue::into_pending_dependencies)
        {
            add_dependency!(graph, node => ScheduledGraphNode::BlockEnd, ExecutionDependency::Scheduled);
        }

        for node in last_instruction_by_frame
            .into_values()
            .flat_map(DependencyQueue::into_pending_dependencies)
        {
            add_dependency!(graph, node => ScheduledGraphNode::BlockEnd, ExecutionDependency::StableOrdering);
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

    pub fn instructions(&'a self) -> &'a [&'a Instruction] {
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

    pub fn terminator(&self) -> &BasicBlockTerminator<'a> {
        self.basic_block.terminator()
    }

    pub fn basic_block(&self) -> &BasicBlock<'a> {
        &self.basic_block
    }
}

/// A program broken down into its [`ScheduledBasicBlock`]s.  All instruction-level scheduling in a
/// program is intra-block; the only dependencies between basic blocks are those resulting from
/// execution flow.  For instance, we do *not* track memory dependencies from a write in one block to
/// a read in a subsequent block.
#[derive(Clone, Debug)]
pub struct ScheduledProgram<'a> {
    basic_blocks: Vec<ScheduledBasicBlock<'a>>,
}

impl<'a> ScheduledProgram<'a> {
    /// Structure a sequential program
    pub fn from_program<H: InstructionHandler>(
        program: &'a Program,
        handler: &H,
    ) -> ScheduleResult<Self> {
        let control_flow_graph = ControlFlowGraph::from(program);
        Ok(Self {
            basic_blocks: control_flow_graph
                .into_blocks()
                .into_iter()
                .map(|block| ScheduledBasicBlock::build(block, program, handler))
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

impl From<ScheduledBasicBlock<'_>> for ScheduledBasicBlockOwned {
    fn from(block: ScheduledBasicBlock) -> Self {
        Self {
            basic_block: block.basic_block.into(),
            graph: block.graph.clone(),
        }
    }
}

#[cfg(all(test, feature = "graphviz-dot"))]
mod graphviz_dot_tests {
    use super::*;

    use crate::program::scheduling::graphviz_dot::tests::build_dot_format_snapshot_test_case;

    mod custom_handler {
        use super::*;

        use crate::instruction::DefaultHandler;
        use crate::instruction::Pragma;
        use crate::instruction::PragmaArgument;
        use crate::program::frame::FrameMatchCondition;
        use crate::program::MemoryAccessesError;
        use crate::program::{MatchedFrames, MemoryAccesses};

        /// A custom [`InstructionHandler`] that specially handles two `PRAGMA` instructions:
        ///
        /// - `NO-OP` is considered a `ClassicalCompute` instruction that does nothing
        /// - `RAW-INSTRUCTION` is an `RFControl` instruction that is scheduled on all frames by default
        ///   or the frame names specified as arguments, and reads from `ro`.
        ///
        /// Note that any program being tested must define at least one frame for `RAW-INSTRUCTION` to
        /// have any effect.
        struct CustomHandler;

        const NO_OP: &str = "NO-OP";
        const RAW_INSTRUCTION: &str = "RAW-INSTRUCTION";

        impl InstructionHandler for CustomHandler {
            fn is_scheduled(&self, instruction: &Instruction) -> bool {
                match instruction {
                    Instruction::Pragma(Pragma { name, .. }) if name == NO_OP => false,
                    Instruction::Pragma(Pragma { name, .. }) if name == RAW_INSTRUCTION => true,
                    _ => DefaultHandler.is_scheduled(instruction),
                }
            }

            fn role(&self, instruction: &Instruction) -> InstructionRole {
                match instruction {
                    Instruction::Pragma(Pragma { name, .. }) if name == NO_OP => {
                        InstructionRole::ClassicalCompute
                    }
                    Instruction::Pragma(Pragma { name, .. }) if name == RAW_INSTRUCTION => {
                        InstructionRole::RFControl
                    }
                    _ => DefaultHandler.role(instruction),
                }
            }

            fn matching_frames<'p>(
                &self,
                program: &'p Program,
                instruction: &Instruction,
            ) -> Option<MatchedFrames<'p>> {
                match instruction {
                    Instruction::Pragma(Pragma { name, .. }) if name == NO_OP => None,
                    Instruction::Pragma(Pragma {
                        name, arguments, ..
                    }) if name == RAW_INSTRUCTION => {
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

                        Some(MatchedFrames {
                            used,
                            blocked: HashSet::new(),
                        })
                    }
                    _ => DefaultHandler.matching_frames(program, instruction),
                }
            }

            fn memory_accesses(
                &self,
                extern_signature_map: &ExternSignatureMap,
                instruction: &Instruction,
            ) -> Result<MemoryAccesses, MemoryAccessesError> {
                match instruction {
                    Instruction::Pragma(Pragma { name, .. }) if name == NO_OP => {
                        Ok(MemoryAccesses::none())
                    }
                    Instruction::Pragma(Pragma { name, .. }) if name == RAW_INSTRUCTION => {
                        Ok(MemoryAccesses {
                            reads: ["ro".to_owned()].into(),
                            writes: HashSet::new(),
                            captures: HashSet::new(),
                        })
                    }
                    _ => DefaultHandler.memory_accesses(extern_signature_map, instruction),
                }
            }
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
            &CustomHandler,
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
            &CustomHandler,
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
            &CustomHandler,
        }
    }

    // Because any instruction that reads a particular region must be preceded by any earlier instructions that write to/ capture that memory region,
    // we expect an edge from the first load to the second (0 -> 1).
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
    // expect edges from the first load to the first shift-phase (0 -> 1) as well as to the
    // second load (0 -> 2), from the first shift-phase to the second load (1 -> 2), and from the
    // second load to the second shift-phase (2 -> 3).
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
    build_dot_format_snapshot_test_case! {
        classical_no_memory_pragma,
        r#"PRAGMA example"#
    }

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

    build_dot_format_snapshot_test_case! {
        memory_dependency_not_in_block_terminator,
        r#"
DECLARE ro BIT
DECLARE depends_on_ro BIT

NONBLOCKING CAPTURE 0 "ro_rx" flat(duration: 2.0000000000000003e-06, iq: 1.0, scale: 1.0, phase: 0.8745492960861506, detuning: 0.0) ro
MOVE depends_on_ro ro
JUMP @eq
LABEL @eq
PULSE 0 "ro_tx" gaussian(duration: 1, fwhm: 2, t0: 3)
"#
    }

    build_dot_format_snapshot_test_case! {
        memory_dependency_in_block_terminator,
        r#"
DECLARE ro BIT

NONBLOCKING CAPTURE 0 "ro_rx" flat(duration: 2.0000000000000003e-06, iq: 1.0, scale: 1.0, phase: 0.8745492960861506, detuning: 0.0) ro
JUMP-WHEN @eq ro
LABEL @eq
PULSE 0 "ro_tx" gaussian(duration: 1, fwhm: 2, t0: 3)
"#
    }

    build_dot_format_snapshot_test_case! {
        no_memory_dependency_across_blocks,
        r#"
DECLARE ro BIT
DECLARE depends_on_ro BIT

NONBLOCKING CAPTURE 0 "ro_rx" flat(duration: 2.0000000000000003e-06, iq: 1.0, scale: 1.0, phase: 0.8745492960861506, detuning: 0.0) ro
JUMP @eq
LABEL @eq
MOVE depends_on_ro ro
PULSE 0 "ro_tx" gaussian(duration: 1, fwhm: 2, t0: 3)
"#
    }

    build_dot_format_snapshot_test_case! {
        memory_dependency_one_variable,
        r#"DEFFRAME 0 "frame0":
    DIRECTION: "tx"
    INITIAL-FREQUENCY: 6864214214.214214
    CENTER-FREQUENCY: 7250000000.0
    HARDWARE-OBJECT: "{\"instrument_name\": \"tsunami00\", \"card_index\": 0, \"channel_type\": \"QGSx2Channel\", \"channel_index\": 0, \"sequencer_index\": 0, \"nco_index\": 0}"
    SAMPLE-RATE: 1000000000.0
DEFFRAME 1 "frame1":
    DIRECTION: "tx"
    INITIAL-FREQUENCY: 6864214214.214214
    CENTER-FREQUENCY: 7250000000.0
    HARDWARE-OBJECT: "{\"instrument_name\": \"tsunami00\", \"card_index\": 1, \"channel_type\": \"QGSx2Channel\", \"channel_index\": 0, \"sequencer_index\": 0, \"nco_index\": 0}"
    SAMPLE-RATE: 1000000000.0

DELAY 2e-8

DECLARE phase REAL
MOVE phase 0.1

SET-PHASE 0 "frame0" 2*pi*phase
SET-PHASE 1 "frame1" 2*pi*phase

PULSE 0 "frame0" flat(iq: 1, duration: 4e-9)
PULSE 1 "frame1" flat(iq: 1, duration: 4e-9)
"#
    }

    build_dot_format_snapshot_test_case! {
        memory_dependency_array,
        r#"DEFFRAME 0 "frame0":
    DIRECTION: "tx"
    INITIAL-FREQUENCY: 6864214214.214214
    CENTER-FREQUENCY: 7250000000.0
    HARDWARE-OBJECT: "{\"instrument_name\": \"tsunami00\", \"card_index\": 0, \"channel_type\": \"QGSx2Channel\", \"channel_index\": 0, \"sequencer_index\": 0, \"nco_index\": 0}"
    SAMPLE-RATE: 1000000000.0
DEFFRAME 1 "frame1":
    DIRECTION: "tx"
    INITIAL-FREQUENCY: 6864214214.214214
    CENTER-FREQUENCY: 7250000000.0
    HARDWARE-OBJECT: "{\"instrument_name\": \"tsunami00\", \"card_index\": 1, \"channel_type\": \"QGSx2Channel\", \"channel_index\": 0, \"sequencer_index\": 0, \"nco_index\": 0}"
    SAMPLE-RATE: 1000000000.0

DELAY 2e-8

DECLARE phase REAL[2]
MOVE phase[0] 0.1
MOVE phase[1] 0.1

SET-PHASE 0 "frame0" 2*pi*phase[0]
SET-PHASE 1 "frame1" 2*pi*phase[1]

PULSE 0 "frame0" flat(iq: 1, duration: 4e-9)
PULSE 1 "frame1" flat(iq: 1, duration: 4e-9)
"#
    }

    build_dot_format_snapshot_test_case! {
        memory_dependency_two_variables,
        r#"DEFFRAME 0 "frame0":
    DIRECTION: "tx"
    INITIAL-FREQUENCY: 6864214214.214214
    CENTER-FREQUENCY: 7250000000.0
    HARDWARE-OBJECT: "{\"instrument_name\": \"tsunami00\", \"card_index\": 0, \"channel_type\": \"QGSx2Channel\", \"channel_index\": 0, \"sequencer_index\": 0, \"nco_index\": 0}"
    SAMPLE-RATE: 1000000000.0
DEFFRAME 1 "frame1":
    DIRECTION: "tx"
    INITIAL-FREQUENCY: 6864214214.214214
    CENTER-FREQUENCY: 7250000000.0
    HARDWARE-OBJECT: "{\"instrument_name\": \"tsunami00\", \"card_index\": 1, \"channel_type\": \"QGSx2Channel\", \"channel_index\": 0, \"sequencer_index\": 0, \"nco_index\": 0}"
    SAMPLE-RATE: 1000000000.0

DELAY 2e-8

DECLARE phase0 REAL
DECLARE phase1 REAL
MOVE phase0 0.1
MOVE phase1 0.1

SET-PHASE 0 "frame0" 2*pi*phase0
SET-PHASE 1 "frame1" 2*pi*phase1

PULSE 0 "frame0" flat(iq: 1, duration: 4e-9)
PULSE 1 "frame1" flat(iq: 1, duration: 4e-9)
"#
    }
}
