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
    pub(super) graph: DependencyGraph,
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
                    let used_frames = match program.get_frames_for_instruction(instruction, false) {
                        Some(frames) => frames,
                        None => vec![],
                    };
                    let blocked_frames = match program.get_frames_for_instruction(instruction, true) {
                        Some(frames) => frames,
                        None => vec![],
                    };

                    // Take a dependency on any previous instructions to _block_ or _use_ a frame which this instruction _uses_.
                    for frame in used_frames {
                        let previous_node_id = last_instruction_by_frame
                            .entry(frame.clone())
                            .or_insert(ScheduledGraphNode::BlockStart);
                        add_dependency!(graph, *previous_node_id => node, ExecutionDependency::ReferenceFrame);
                        last_instruction_by_frame.insert(frame.clone(), node);
                    }

                    // We mark all "blocked" frames as such for later instructions to take a dependency on
                    for frame in blocked_frames {
                        last_instruction_by_frame.insert(frame.clone(), node);
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

#[derive(Clone, Debug)]
pub struct ScheduledProgram {
    /// All blocks within the ScheduledProgram, keyed on string label.
    pub blocks: IndexMap<String, InstructionBlock>,
}

macro_rules! terminate_working_block {
    ($terminator:expr, $working_instructions:ident, $blocks:ident, $working_label:ident, $program: ident, $instruction_index: ident) => {{
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
                    instruction_index: $instruction_index,
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

        for (index, instruction) in instructions.into_iter().enumerate() {
            let instruction_index = Some(index);
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
                    instruction_index,
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
                | Instruction::WaveformDefinition(_) => Ok(()),
                Instruction::Pragma(_) => {
                    working_instructions.push(instruction);
                    Ok(())
                }
                Instruction::Label(Label(value)) => {
                    terminate_working_block!(
                        None as Option<BlockTerminator>,
                        working_instructions,
                        blocks,
                        working_label,
                        program,
                        instruction_index
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
                        program,
                        instruction_index
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
                        program,
                        instruction_index
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
                        program,
                        instruction_index
                    )
                }
                Instruction::Halt => {
                    terminate_working_block!(
                        Some(BlockTerminator::Halt),
                        working_instructions,
                        blocks,
                        working_label,
                        program,
                        instruction_index
                    )
                }
            }?;
        }

        terminate_working_block!(
            None as Option<BlockTerminator>,
            working_instructions,
            blocks,
            working_label,
            program,
            None
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
}
