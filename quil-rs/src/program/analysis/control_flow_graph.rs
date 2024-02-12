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
    instruction::{Instruction, Jump, JumpUnless, JumpWhen, Label, Target},
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

#[derive(Clone, Debug, Default)]
pub struct BasicBlock<'p> {
    /// The label of the basic block, if any. An unlabeled basic block cannot be a target of a jump, but can
    /// be entered by a [`BasicBlockTerminator::Continue`] from the preceding block or program start.
    label: Option<&'p Target>,

    /// The instructions within the basic block, not including its terminator.
    instructions: Vec<&'p Instruction>,

    /// The terminator of the basic block, which determines the control flow to the next basic block.
    terminator: BasicBlockTerminator<'p>,
}

impl<'p> BasicBlock<'p> {
    pub fn label(&self) -> Option<&'p Target> {
        self.label
    }

    pub fn instructions(&self) -> &[&'p Instruction] {
        self.instructions.as_ref()
    }

    pub fn terminator(&self) -> &BasicBlockTerminator<'p> {
        &self.terminator
    }
}

/// The terminator of a basic block, which determines the control flow to the next basic block.
#[derive(Clone, Debug, Default)]
pub enum BasicBlockTerminator<'p> {
    #[default]
    Continue,
    Jump(&'p Jump),
    JumpWhen(&'p JumpWhen),
    JumpUnless(&'p JumpUnless),
    Halt,
}

impl BasicBlockTerminator<'_> {
    /// Returns `true` if the terminator is dynamic, i.e. `JUMP-WHEN` or `JUMP-UNLESS`.
    ///
    /// Dynamic terminators are those that can change the control flow based on the state of the
    /// program at runtime, as opposed to static terminators like `JUMP` and `HALT`.
    pub fn is_dynamic(&self) -> bool {
        matches!(
            self,
            BasicBlockTerminator::JumpWhen(_) | BasicBlockTerminator::JumpUnless(_)
        )
    }
}

impl<'p> From<&'p Program> for ControlFlowGraph<'p> {
    fn from(value: &'p Program) -> Self {
        let mut graph = ControlFlowGraph::default();

        let mut current_label = None;
        let mut current_block_instructions = Vec::new();
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

                Instruction::Jump(_)
                | Instruction::JumpUnless(_)
                | Instruction::JumpWhen(_)
                | Instruction::Label(_)
                | Instruction::Halt => {
                    let (terminator, new_label) = match instruction {
                        Instruction::Jump(jump) => (BasicBlockTerminator::Jump(jump), None),
                        Instruction::JumpUnless(jump_unless) => {
                            (BasicBlockTerminator::JumpUnless(jump_unless), None)
                        }
                        Instruction::JumpWhen(jump_when) => {
                            (BasicBlockTerminator::JumpWhen(jump_when), None)
                        }
                        Instruction::Label(Label { target }) => {
                            (BasicBlockTerminator::Continue, Some(target))
                        }
                        Instruction::Halt => (BasicBlockTerminator::Halt, None),
                        _ => unreachable!(),
                    };
                    if !current_block_instructions.is_empty() {
                        let block = BasicBlock {
                            label: current_label.take(),
                            instructions: std::mem::take(&mut current_block_instructions),
                            terminator,
                        };
                        graph.blocks.push(block);
                        current_block_instructions = Vec::new();
                    }
                    current_label = new_label;
                }
            }
        }

        if !current_block_instructions.is_empty() || current_label.is_some() {
            let block = BasicBlock {
                label: current_label.take(),
                instructions: current_block_instructions,
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
}
