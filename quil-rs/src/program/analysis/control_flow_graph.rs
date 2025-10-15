//! Construction and analysis of a control flow graph (CFG) for a Quil program.

// Copyright 2024 Rigetti Computing
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

use std::{
    collections::{BTreeMap, HashMap},
    fmt::Debug,
};

#[cfg(not(feature = "python"))]
use optipy::strip_pyo3;
#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::gen_stub_pyclass;

use crate::{
    instruction::{
        Instruction, InstructionHandler, Jump, JumpUnless, JumpWhen, Label, MemoryReference, Target,
    },
    program::{
        scheduling::{
            schedule::{ComputedScheduleError, ComputedScheduleItem, Schedule, TimeSpan, Zero},
            ScheduleError, ScheduledBasicBlock, Seconds,
        },
        ProgramError,
    },
    Program,
};

/// A control flow graph (CFG) is a representation of a program's control flow as a directed graph.
/// Each node in the graph is a basic block, a sequence of instructions with a single entry point
/// and a single exit point. The edges in the graph represent control flow between basic blocks.
#[derive(Clone, Debug, Default)]
pub struct ControlFlowGraph<'p> {
    blocks: Vec<BasicBlock<'p>>,
}

impl<'p> ControlFlowGraph<'p> {
    /// Returns `true` if the program contains dynamic control flow, i.e. `JUMP-WHEN` or `JUMP-UNLESS`
    pub fn has_dynamic_control_flow(&self) -> bool {
        self.blocks
            .iter()
            .any(|block| block.terminator().is_dynamic())
    }

    /// Returns the basic blocks in the control flow graph.
    pub fn into_blocks(self) -> Vec<BasicBlock<'p>> {
        self.blocks
    }
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(name = "ControlFlowGraph", module = "quil.program", subclass, frozen)
)]
pub struct ControlFlowGraphOwned {
    pub(crate) blocks: Vec<BasicBlockOwned>,
}

impl From<ControlFlowGraph<'_>> for ControlFlowGraphOwned {
    fn from(value: ControlFlowGraph) -> Self {
        let blocks = value
            .blocks
            .into_iter()
            .map(BasicBlockOwned::from)
            .collect();
        ControlFlowGraphOwned { blocks }
    }
}

impl<'p> From<&'p ControlFlowGraphOwned> for ControlFlowGraph<'p> {
    fn from(value: &'p ControlFlowGraphOwned) -> Self {
        let blocks = value.blocks.iter().map(BasicBlock::from).collect();
        ControlFlowGraph { blocks }
    }
}

#[derive(Clone, Debug, Default)]
pub struct BasicBlock<'p> {
    /// The label of the basic block, if any. An unlabeled basic block cannot be a target of a jump, but can
    /// be entered by a [`BasicBlockTerminator::Continue`] from the preceding block or program start.
    label: Option<&'p Target>,

    /// The instructions within the basic block, not including its terminator.
    instructions: Vec<&'p Instruction>,

    /// The offset of the start of this block from the containing program, in instruction count.
    /// For the first block in the program, this is `0`. This counts only "body" instructions, not
    /// `DEFCAL`, `DEFFRAME`, et al.
    ///
    /// This is intended for use in debugging and source mapping.
    instruction_index_offset: usize,

    /// The terminator of the basic block, which determines the control flow to the next basic block.
    terminator: BasicBlockTerminator<'p>,
}

impl<'p> BasicBlock<'p> {
    pub fn label(&self) -> Option<&'p Target> {
        self.label
    }

    pub fn instruction_index_offset(&self) -> usize {
        self.instruction_index_offset
    }

    pub fn instructions(&self) -> &[&'p Instruction] {
        self.instructions.as_ref()
    }

    pub fn terminator(&self) -> &BasicBlockTerminator<'p> {
        &self.terminator
    }

    /// Compute the flattened schedule for this [`BasicBlock`] in terms of seconds,
    /// using a default built-in calculation for the duration of scheduled instructions.
    /// See [`BasicBlockOwned::as_schedule`].
    ///
    /// # Arguments
    ///
    /// * `program` - The program containing this basic block.
    ///   This is used to retrieve frame and calibration information.
    ///   Generally, this should be the program
    ///   from which the block was extracted.
    ///
    /// # How it Works
    ///
    /// * Expanding each instruction within the block using the program's calibration definitions
    /// * Resolving the `ScheduleSeconds` of the expanded instructions
    /// * Mapping calibrated instructions back to the original instructions within this block,
    ///   such that the block's instruction is represented as a timespan encompassing all of its expanded instructions
    ///
    /// # Notes
    ///
    /// If the basic block contains gates,
    /// the program must contain corresponding `DEFCAL`s for those gates.
    /// Gates do not inherently have durations,
    /// but rather inherit them from the `PULSE`, `CAPTURE`, `DELAY`,
    /// and other instructions within their calibrations.
    /// Without a calibration, a gate's duration cannot be computed.
    ///
    /// # Python Example
    ///
    /// For Python users, the following example demonstrates construction
    /// of such a schedule for a simple program
    /// without explicit control flow (and thus with only one basic block):
    ///
    /// ```python
    /// from quil.program import Program
    ///
    /// program = Program.parse("CZ 0 1; CZ 0 2")
    /// print(program.to_quil())
    ///
    /// control_flow_graph = program.control_flow_graph()
    /// assert control_flow_graph.has_dynamic_control_flow() == False
    ///
    /// basic_blocks = control_flow_graph.basic_blocks()
    /// assert len(basic_blocks) == 1
    ///
    /// schedule = blocks[0].as_schedule_seconds(program)
    /// print(f"Duration = {schedule.duration()}")
    /// print(schedule.items())
    /// ```
    pub fn as_schedule_seconds<H: InstructionHandler>(
        &self,
        program: &Program,
        handler: &H,
    ) -> Result<Schedule<Seconds>, BasicBlockScheduleError> {
        self.as_schedule(
            program,
            |prog, instr| ScheduledBasicBlock::instruction_duration_seconds(prog, instr, handler),
            handler,
        )
    }

    /// Compute the schedule for this [`BasicBlock`] in terms of a generic unit of time,
    /// using a provided function to calculate the duration of scheduled instructions in that unit.
    ///
    /// # Arguments
    ///
    /// * `program` - The program containing this basic block. This is used to retrieve frame
    ///   and calibration information.
    /// * `get_duration` - A function that takes a program and an instruction and returns the
    ///   duration of the instruction in the desired time unit, or `None` if the instruction's
    ///   duration is not known.
    ///
    /// Note: when an instruction is expanded, the "time" of that original instruction includes
    /// the times of all of the resulting instructions. This may cause gate times to be
    /// longer than a user might expect.
    ///
    /// To understand why, consider a program like this:
    ///
    /// ```text
    /// # One-qubit frame
    /// DEFFRAME 0 "a":
    ///     ATTRIBUTE: 1
    ///
    /// # Two-qubit frame
    /// DEFFRAME 0 1 "b":
    ///     ATTRIBUTE: 1
    ///
    /// DEFCAL A 0:
    ///     PULSE 0 "a" flat(duration: 1.0)
    ///
    /// DEFCAL B 0 1:
    ///     FENCE 1
    ///     PULSE 0 1 "b" flat(duration: 1.0)
    ///
    /// A 0
    /// B 0 1
    /// ```
    ///
    /// `B 0` will be scheduled from time 0 to time 2, because its inner `FENCE` is scheduled for time 0.
    /// This may be unexpected if the user expects to see only the timing of the inner `PULSE`.
    pub fn as_schedule<H, F, Time>(
        &self,
        program: &'p Program,
        get_duration: F,
        handler: &H,
    ) -> Result<Schedule<Time>, BasicBlockScheduleError>
    where
        H: InstructionHandler,
        F: Fn(&Program, &Instruction) -> Option<Time>,
        Time: Clone
            + Debug
            + PartialOrd
            + std::ops::Add<Time, Output = Time>
            + std::ops::Sub<Time, Output = Time>
            + Zero,
    {
        // 1: expand calibrations and track the source mapping
        let mut calibrated_to_uncalibrated_instruction_source_mapping = BTreeMap::new();
        let mut calibrated_block_instructions = Vec::new();

        for (uncalibrated_instruction_index, instruction) in self.instructions.iter().enumerate() {
            let first_calibrated_instruction_index = calibrated_block_instructions.len();
            if let Some(expanded) = program.calibrations.expand(instruction, &[])? {
                calibrated_block_instructions.extend(expanded.into_iter());
            } else {
                calibrated_block_instructions.push((*instruction).clone());
            }
            calibrated_to_uncalibrated_instruction_source_mapping.insert(
                first_calibrated_instruction_index,
                uncalibrated_instruction_index,
            );
        }

        let calibrated_block = BasicBlock {
            label: self.label,
            instructions: calibrated_block_instructions.iter().collect(),
            instruction_index_offset: self.instruction_index_offset,
            terminator: self.terminator.clone(),
        };

        // 2: attempt to schedule the newly-expanded block
        let scheduled_self = ScheduledBasicBlock::build(calibrated_block, program, handler)?;
        let schedule = scheduled_self.as_schedule(program, get_duration)?;

        // 3: map that schedule back to the original instructions from this basic block using the source mapping
        let uncalibrated_schedule_items_by_instruction_index = schedule
            .into_items()
            .into_iter()
            .fold(HashMap::<usize, TimeSpan<Time>>::new(), |mut map, item| {
                if let Some((_, uncalibrated_instruction_index)) =
                    calibrated_to_uncalibrated_instruction_source_mapping
                        .range(..=item.instruction_index)
                        .next_back()
                {
                    if let Some(existing_time_span) = map.get_mut(uncalibrated_instruction_index) {
                        *existing_time_span = existing_time_span.clone().union(item.time_span);
                    } else {
                        map.insert(*uncalibrated_instruction_index, item.time_span.clone());
                    }
                }

                map
            });

        let schedule_items = uncalibrated_schedule_items_by_instruction_index
            .into_iter()
            .map(|(instruction_index, time_span)| ComputedScheduleItem {
                instruction_index,
                time_span,
            })
            .collect::<Vec<_>>();

        let schedule = Schedule::from(schedule_items);
        Ok(schedule)
    }
}

// TODO (#453): Address large error types.
#[allow(clippy::large_enum_variant)]
#[allow(clippy::enum_variant_names)]
#[derive(Debug, thiserror::Error)]
pub enum BasicBlockScheduleError {
    #[error(transparent)]
    ScheduleError(#[from] ScheduleError),

    #[error(transparent)]
    ComputedScheduleError(#[from] ComputedScheduleError),

    #[error(transparent)]
    ProgramError(#[from] ProgramError),
}

// TODO (#472): The conversions to/from these Owned types and their non-Owned counterparts
// involves a lot of Cloning, and they're the types exposed by the Python bindings.
// Can we combine their relevant methods or otherwise avoid the costly conversions?
#[derive(Clone, Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(name = "BasicBlock", module = "quil.program", subclass)
)]
#[cfg_attr(not(feature = "python"), strip_pyo3)]
pub struct BasicBlockOwned {
    /// The label of the block, if any.
    /// This is used to target this block in control flow.
    #[pyo3(get)]
    label: Option<Target>,
    /// A list of the instructions in the block, in order of definition.
    ///
    /// This does not include the label or terminator instructions.
    #[pyo3(get)]
    instructions: Vec<Instruction>,
    instruction_index_offset: usize,
    pub(crate) terminator: BasicBlockTerminatorOwned,
}

impl From<BasicBlock<'_>> for BasicBlockOwned {
    fn from(value: BasicBlock) -> Self {
        let label = value.label.cloned();
        let instructions = value.instructions.into_iter().cloned().collect();
        let instruction_index_offset = value.instruction_index_offset;
        let terminator = value.terminator.into();
        BasicBlockOwned {
            label,
            instructions,
            instruction_index_offset,
            terminator,
        }
    }
}

impl<'b> From<&'b BasicBlockOwned> for BasicBlock<'b> {
    fn from(value: &'b BasicBlockOwned) -> Self {
        let label = value.label.as_ref();
        let instructions = value.instructions.iter().collect();
        let instruction_index_offset = value.instruction_index_offset;
        let terminator = (&value.terminator).into();
        BasicBlock {
            label,
            instructions,
            instruction_index_offset,
            terminator,
        }
    }
}

/// The terminator of a basic block, which determines the control flow to the next basic block.
#[derive(Clone, Debug, Default)]
pub enum BasicBlockTerminator<'p> {
    ConditionalJump {
        condition: &'p MemoryReference,
        target: &'p Target,
        jump_if_condition_zero: bool,
    },
    #[default]
    Continue,
    Jump {
        target: &'p Target,
    },
    Halt,
}

impl BasicBlockTerminator<'_> {
    /// Returns `true` if the terminator is dynamic, i.e. `JUMP-WHEN` or `JUMP-UNLESS`.
    ///
    /// Dynamic terminators are those that can change the control flow based on the state of the
    /// program at runtime, as opposed to static terminators like `JUMP` and `HALT`.
    pub fn is_dynamic(&self) -> bool {
        matches!(self, BasicBlockTerminator::ConditionalJump { .. })
    }

    pub fn into_instruction(self) -> Option<Instruction> {
        match self {
            BasicBlockTerminator::ConditionalJump {
                condition,
                target,
                jump_if_condition_zero,
            } => {
                if jump_if_condition_zero {
                    Some(Instruction::JumpUnless(JumpUnless {
                        condition: condition.clone(),
                        target: target.clone(),
                    }))
                } else {
                    Some(Instruction::JumpWhen(JumpWhen {
                        condition: condition.clone(),
                        target: target.clone(),
                    }))
                }
            }
            BasicBlockTerminator::Continue => None,
            BasicBlockTerminator::Jump { target } => Some(Instruction::Jump(Jump {
                target: target.clone(),
            })),
            BasicBlockTerminator::Halt => Some(Instruction::Halt()),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub enum BasicBlockTerminatorOwned {
    ConditionalJump {
        condition: MemoryReference,
        target: Target,
        jump_if_condition_zero: bool,
    },
    #[default]
    Continue,
    Jump {
        target: Target,
    },
    Halt,
}

impl From<BasicBlockTerminator<'_>> for BasicBlockTerminatorOwned {
    fn from(value: BasicBlockTerminator) -> Self {
        match value {
            BasicBlockTerminator::ConditionalJump {
                condition,
                target,
                jump_if_condition_zero,
            } => BasicBlockTerminatorOwned::ConditionalJump {
                condition: condition.clone(),
                target: target.clone(),
                jump_if_condition_zero,
            },
            BasicBlockTerminator::Continue => BasicBlockTerminatorOwned::Continue,
            BasicBlockTerminator::Jump { target } => BasicBlockTerminatorOwned::Jump {
                target: target.clone(),
            },
            BasicBlockTerminator::Halt => BasicBlockTerminatorOwned::Halt,
        }
    }
}

impl<'p> From<&'p BasicBlockTerminatorOwned> for BasicBlockTerminator<'p> {
    fn from(value: &'p BasicBlockTerminatorOwned) -> Self {
        match value {
            BasicBlockTerminatorOwned::ConditionalJump {
                condition,
                target,
                jump_if_condition_zero,
            } => BasicBlockTerminator::ConditionalJump {
                condition,
                target,
                jump_if_condition_zero: *jump_if_condition_zero,
            },
            BasicBlockTerminatorOwned::Continue => BasicBlockTerminator::Continue,
            BasicBlockTerminatorOwned::Jump { target } => BasicBlockTerminator::Jump { target },
            BasicBlockTerminatorOwned::Halt => BasicBlockTerminator::Halt,
        }
    }
}

impl<'p> From<&'p Program> for ControlFlowGraph<'p> {
    fn from(value: &'p Program) -> Self {
        let mut graph = ControlFlowGraph::default();

        let mut current_label = None;
        let mut current_block_instructions = Vec::new();
        let mut instruction_index_offset = 0;
        for instruction in &value.instructions {
            match instruction {
                Instruction::Arithmetic(_)
                | Instruction::BinaryLogic(_)
                | Instruction::Call(_)
                | Instruction::Capture(_)
                | Instruction::Convert(_)
                | Instruction::Comparison(_)
                | Instruction::Delay(_)
                | Instruction::Fence(_)
                | Instruction::Exchange(_)
                | Instruction::Gate(_)
                | Instruction::Load(_)
                | Instruction::Pragma(_)
                | Instruction::Measurement(_)
                | Instruction::Move(_)
                | Instruction::Nop()
                | Instruction::Pulse(_)
                | Instruction::RawCapture(_)
                | Instruction::Reset(_)
                | Instruction::SetFrequency(_)
                | Instruction::SetPhase(_)
                | Instruction::SetScale(_)
                | Instruction::ShiftFrequency(_)
                | Instruction::ShiftPhase(_)
                | Instruction::Store(_)
                | Instruction::SwapPhases(_)
                | Instruction::UnaryLogic(_)
                | Instruction::Wait() => current_block_instructions.push(instruction),

                Instruction::CalibrationDefinition(_)
                | Instruction::CircuitDefinition(_)
                | Instruction::Declaration(_)
                | Instruction::FrameDefinition(_)
                | Instruction::GateDefinition(_)
                | Instruction::Include(_)
                | Instruction::MeasureCalibrationDefinition(_)
                | Instruction::WaveformDefinition(_) => {}

                Instruction::Label(Label { target }) => {
                    if !current_block_instructions.is_empty() || current_label.is_some() {
                        let block = BasicBlock {
                            label: current_label.take(),
                            instructions: std::mem::take(&mut current_block_instructions),
                            instruction_index_offset,
                            terminator: BasicBlockTerminator::Continue,
                        };
                        // +1 for the label
                        instruction_index_offset += block.instructions.len() + 1;
                        graph.blocks.push(block);
                    }

                    current_label = Some(target);
                }

                Instruction::Jump(_)
                | Instruction::JumpUnless(_)
                | Instruction::JumpWhen(_)
                | Instruction::Halt() => {
                    let terminator = match instruction {
                        Instruction::Jump(jump) => BasicBlockTerminator::Jump {
                            target: &jump.target,
                        },
                        Instruction::JumpUnless(jump_unless) => {
                            BasicBlockTerminator::ConditionalJump {
                                condition: &jump_unless.condition,
                                target: &jump_unless.target,
                                jump_if_condition_zero: true,
                            }
                        }
                        Instruction::JumpWhen(jump_when) => BasicBlockTerminator::ConditionalJump {
                            condition: &jump_when.condition,
                            target: &jump_when.target,
                            jump_if_condition_zero: false,
                        },
                        Instruction::Halt() => BasicBlockTerminator::Halt,
                        _ => unreachable!(),
                    };
                    let block = BasicBlock {
                        label: current_label.take(),
                        instructions: std::mem::take(&mut current_block_instructions),
                        instruction_index_offset,
                        terminator,
                    };

                    let label_instruction_offset = if block.label().is_some() { 1 } else { 0 };
                    // +1 for this terminator instruction
                    instruction_index_offset +=
                        block.instructions.len() + 1 + label_instruction_offset;

                    graph.blocks.push(block);
                }
            }
        }

        if !current_block_instructions.is_empty() || current_label.is_some() {
            let block = BasicBlock {
                label: current_label.take(),
                instructions: current_block_instructions,
                instruction_index_offset,
                terminator: BasicBlockTerminator::Continue,
            };
            graph.blocks.push(block);
        }

        graph
    }
}

impl<'a> TryFrom<&'a Program> for BasicBlock<'a> {
    type Error = ProgramEmptyOrContainsMultipleBasicBlocks;

    fn try_from(value: &'a Program) -> Result<Self, Self::Error> {
        let blocks = ControlFlowGraph::from(value).blocks;
        if blocks.len() == 1 {
            Ok(blocks.into_iter().next().unwrap())
        } else {
            Err(ProgramEmptyOrContainsMultipleBasicBlocks)
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("Program is empty or contains multiple basic blocks")]
pub struct ProgramEmptyOrContainsMultipleBasicBlocks;

#[cfg(test)]
mod tests {
    use crate::instruction::DefaultHandler;
    use crate::Program;
    use rstest::rstest;

    use super::*;

    use super::super::test_programs::*;

    #[rstest]
    #[case(QUIL_AS_TREE)]
    #[case(QUIL_AS_INVERSE_TREE)]
    #[case(QUIL_AS_LINEAR)]
    #[case(QUIL_WITH_DIAMOND)]
    #[case(QUIL_WITH_SWAP)]
    #[case(KITCHEN_SINK_QUIL)]
    fn expect_single_basic_block(#[case] input: &str) {
        let program: Program = input.parse().unwrap();
        let _: BasicBlock = (&program).try_into().unwrap();
    }

    #[rstest]
    #[case(QUIL_AS_TREE, false)]
    #[case(QUIL_AS_INVERSE_TREE, false)]
    #[case(QUIL_AS_LINEAR, false)]
    #[case(QUIL_WITH_DIAMOND, false)]
    #[case(KITCHEN_SINK_QUIL, false)]
    #[case(QUIL_WITH_JUMP, false)]
    #[case(QUIL_WITH_JUMP_WHEN, true)]
    #[case(QUIL_WITH_JUMP_UNLESS, true)]
    fn has_dynamic_control_flow(#[case] input: &str, #[case] expected: bool) {
        let program: Program = input.parse().unwrap();
        let graph = ControlFlowGraph::from(&program);
        let dynamic = graph.has_dynamic_control_flow();
        assert_eq!(expected, dynamic);
    }

    #[rstest]
    #[case(r#"LABEL @a
JUMP @a
LABEL @b
JUMP @b
LABEL @c
JUMP @c
"#, vec![0, 2, 4])]
    #[case(r#"LABEL @a
LABEL @b
LABEL @c
JUMP @c
"#, vec![0, 1, 2])]
    #[case(r#"DEFFRAME 0 "rf":
    ATTRIBUTE: 1
DEFCAL X 0:
    Y 0
X 0
"#, vec![0])]
    fn instruction_index_offset(#[case] input: &str, #[case] expected_block_offsets: Vec<usize>) {
        let program: Program = input.parse().unwrap();
        let graph = ControlFlowGraph::from(&program);
        let blocks = graph.into_blocks();
        println!("blocks: {blocks:#?}");
        let actual = blocks
            .iter()
            .map(|block| block.instruction_index_offset())
            .collect::<Vec<_>>();

        assert_eq!(expected_block_offsets, actual);
    }

    #[test]
    fn schedule_uncalibrated() {
        let input = r#"DEFFRAME 0 "a":
    ATTRIBUTE: 1

DEFFRAME 0 "b":
    ATTRIBUTE: 1

DEFCAL A 0:
    NONBLOCKING PULSE 0 "a" flat(duration: 1.0)
    NONBLOCKING PULSE 0 "a" flat(duration: 1.0)
    NONBLOCKING PULSE 0 "a" flat(duration: 1.0)

DEFCAL B 0:
    NONBLOCKING PULSE 0 "b" flat(duration: 10.0)

A 0
B 0
FENCE
B 0
PULSE 0 "a" flat(duration: 1.0)
"#;

        let program: Program = input.parse().unwrap();
        let graph = ControlFlowGraph::from(&program);
        let blocks = graph.into_blocks();
        println!("blocks: {blocks:#?}");

        let schedule = blocks[0]
            .as_schedule_seconds(&program, &DefaultHandler)
            .unwrap();
        println!("schedule: {schedule:#?}");
        assert_eq!(schedule.duration().0, 21.0);
        let schedule_items = schedule.into_items();

        let schedule_items_by_instruction_index = schedule_items
            .iter()
            .map(|item| (item.instruction_index, item.clone()))
            .collect::<HashMap<_, _>>();

        // `A 0` backed by multiple consecutive instructions should capture all of their times
        assert_eq!(
            schedule_items_by_instruction_index[&0]
                .time_span
                .start_time()
                .0,
            0.0
        );
        assert_eq!(
            schedule_items_by_instruction_index[&0]
                .time_span
                .duration()
                .0,
            3.0
        );

        // `B 0` should be scheduled concurrently with `A 0`
        assert_eq!(
            schedule_items_by_instruction_index[&1]
                .time_span
                .start_time()
                .0,
            0.0
        );
        assert_eq!(
            schedule_items_by_instruction_index[&1]
                .time_span
                .duration()
                .0,
            10.0
        );

        // `FENCE` should be scheduled after `A 0` and `B 0` and take no time.
        // It is not included in the schedule items because it has zero duration.

        // `B 0` should be scheduled after `FENCE`
        assert_eq!(
            schedule_items_by_instruction_index[&3]
                .time_span
                .start_time()
                .0,
            10.0
        );
        assert_eq!(
            schedule_items_by_instruction_index[&3]
                .time_span
                .duration()
                .0,
            10.0
        );

        // `PULSE 0 "a" flat(duration: 1.0)` should be scheduled after `B 0` as a blocking pulse.
        assert_eq!(
            schedule_items_by_instruction_index[&4]
                .time_span
                .start_time()
                .0,
            20.0
        );
        assert_eq!(
            schedule_items_by_instruction_index[&4]
                .time_span
                .duration()
                .0,
            1.0
        );
    }

    #[test]
    fn schedule_uncalibrated_cz() {
        let input = r#"DEFFRAME 0 "flux_tx_cz":
    TEST: 1

DEFFRAME 1 "flux_tx_iswap":
    TEST: 1

DEFFRAME 1 "flux_tx_cz":
    TEST: 1

DEFFRAME 1 "flux_tx_iswap":
    TEST: 1

DEFFRAME 2 "flux_tx_cz":
    TEST: 1

DEFFRAME 2 "flux_tx_iswap":
    TEST: 1

DEFFRAME 3 "flux_tx_cz":
    TEST: 1

DEFFRAME 3 "flux_tx_iswap":
    TEST: 1

# Simplified version
DEFCAL CZ q0 q1:
    FENCE q0 q1
    SET-PHASE q0 "flux_tx_cz" 0.0
    SET-PHASE q1 "flux_tx_iswap" 0.0
    NONBLOCKING PULSE q0 "flux_tx_cz" erf_square(duration: 6.000000000000001e-08, pad_left: 0, pad_right: 0)
    NONBLOCKING PULSE q1 "flux_tx_iswap" erf_square(duration: 6.000000000000001e-08, pad_left: 0, pad_right: 0)
    SHIFT-PHASE q0 "flux_tx_cz" 1.0
    SHIFT-PHASE q1 "flux_tx_iswap" 1.0
    FENCE q0 q1

CZ 0 1
CZ 2 3
CZ 0 2
"#;

        let program: Program = input.parse().unwrap();
        let graph = ControlFlowGraph::from(&program);
        let blocks = graph.into_blocks();
        println!("blocks: {blocks:#?}");

        let schedule = blocks[0]
            .as_schedule_seconds(&program, &DefaultHandler)
            .unwrap();
        let mut schedule_items = schedule.into_items();
        schedule_items.sort_by_key(|item| item.instruction_index);

        assert_eq!(schedule_items.len(), 3);

        insta::assert_debug_snapshot!(schedule_items);
    }
}
