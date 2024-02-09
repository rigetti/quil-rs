use crate::{
    instruction::{Instruction, Jump, JumpUnless, JumpWhen, Label, Target},
    Program,
};

#[derive(Clone, Debug, Default)]
pub struct ControlFlowGraph<'p> {
    blocks: Vec<BasicBlock<'p>>,
}

impl<'p> ControlFlowGraph<'p> {
    pub fn into_blocks(self) -> Vec<BasicBlock<'p>> {
        self.blocks
    }
}

#[derive(Clone, Debug, Default)]
pub struct BasicBlock<'p> {
    label: Option<&'p Target>,
    instructions: Vec<&'p Instruction>,
    terminator: BasicBlockTerminator<'p>,
}

impl<'p> BasicBlock<'p> {
    pub fn label(&self) -> Option<&Target> {
        self.label
    }

    pub fn instructions(&self) -> &[&Instruction] {
        self.instructions.as_ref()
    }

    pub fn terminator(&self) -> &BasicBlockTerminator<'p> {
        &self.terminator
    }
}

#[derive(Clone, Debug, Default)]
pub enum BasicBlockTerminator<'p> {
    #[default]
    Continue,
    Jump(&'p Jump),
    JumpWhen(&'p JumpWhen),
    JumpUnless(&'p JumpUnless),
    Halt,
}

impl Program {
    pub fn as_control_flow_graph(&self) -> ControlFlowGraph {
        let mut graph = ControlFlowGraph::default();

        let mut current_label = None;
        let mut current_block_instructions = Vec::new();
        for instruction in &self.instructions {
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

    pub fn get_first_basic_block(&self) -> Option<BasicBlock> {
        self.as_control_flow_graph().blocks.into_iter().next()
    }

    pub fn get_only_basic_block(&self) -> Result<BasicBlock, ()> {
        let blocks = self.as_control_flow_graph().blocks;
        if blocks.len() == 1 {
            Ok(blocks.into_iter().next().unwrap())
        } else {
            Err(())
        }
    }
}
