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

use crate::{
    instruction::{Instruction, Jump, JumpUnless, JumpWhen, Label, MemoryReference, Target},
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
pub struct ControlFlowGraphOwned {
    blocks: Vec<BasicBlockOwned>,
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
}

#[derive(Clone, Debug)]
pub struct BasicBlockOwned {
    label: Option<Target>,
    instructions: Vec<Instruction>,
    instruction_index_offset: usize,
    terminator: BasicBlockTerminatorOwned,
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
            BasicBlockTerminator::Halt => Some(Instruction::Halt),
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

impl<'p> From<BasicBlockTerminator<'p>> for BasicBlockTerminatorOwned {
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
                condition: &condition,
                target: &target,
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
                | Instruction::Nop
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
                | Instruction::Wait => current_block_instructions.push(instruction),

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
                | Instruction::Halt => {
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
                        Instruction::Halt => BasicBlockTerminator::Halt,
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
        println!("blocks: {:#?}", blocks);
        let actual = blocks
            .iter()
            .map(|block| block.instruction_index_offset())
            .collect::<Vec<_>>();

        assert_eq!(expected_block_offsets, actual);
    }
}
