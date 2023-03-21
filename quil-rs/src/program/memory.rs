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

use std::collections::HashSet;

use serde::{Deserialize, Serialize};

use crate::expression::{Expression, FunctionCallExpression, InfixExpression, PrefixExpression};
use crate::instruction::{
    Arithmetic, ArithmeticOperand, BinaryLogic, BinaryOperand, Capture, CircuitDefinition,
    Comparison, ComparisonOperand, Delay, Exchange, Gate, GateDefinition, GateSpecification,
    Instruction, Jump, JumpUnless, JumpWhen, Label, Load, MeasureCalibrationDefinition,
    Measurement, MemoryReference, Move, Pulse, RawCapture, SetPhase, SetScale, Sharing, ShiftPhase,
    Store, UnaryLogic, Vector, WaveformInvocation,
};

#[derive(Clone, Debug, Hash, PartialEq, Deserialize, Serialize)]
pub struct MemoryRegion {
    pub size: Vector,
    pub sharing: Option<Sharing>,
}

impl MemoryRegion {
    pub fn new(size: Vector, sharing: Option<Sharing>) -> Self {
        Self { size, sharing }
    }
}

impl Eq for MemoryRegion {}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct MemoryAccess {
    pub regions: HashSet<String>,
    pub access_type: MemoryAccessType,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct MemoryAccesses {
    pub captures: HashSet<String>,
    pub reads: HashSet<String>,
    pub writes: HashSet<String>,
}

/// Express a mode of memory access.
#[derive(Clone, Debug, Eq, Hash, PartialEq, Deserialize, Serialize)]
pub enum MemoryAccessType {
    /// Read from a memory location
    Read,

    /// Write to a memory location using classical instructions
    Write,

    /// Write to a memory location using readout (`CAPTURE` and `RAW-CAPTURE` instructions)
    Capture,
}

macro_rules! merge_sets {
    ($left:expr, $right:expr) => {
        $left.union(&$right).cloned().collect::<HashSet<String>>()
    };
}

/// Build a HashSet<String> from a Vec<&str> by cloning
macro_rules! set_from_reference_vec {
    ($vec:expr) => {
        $vec.into_iter()
            .map(|el| el.clone())
            .collect::<HashSet<String>>()
    };
}

/// Build a HashSet<String> from an Option<&MemoryReference>
macro_rules! set_from_optional_memory_reference {
    ($reference:expr) => {
        set_from_reference_vec![$reference.map_or(vec![], |reference| vec![reference.name.clone()])]
    };
}

/// Build a HashSet<String> from a Vec<&MemoryReference>
macro_rules! set_from_memory_references {
    ($references:expr) => {
        set_from_reference_vec![$references.iter().map(|reference| reference.name.clone())]
    };
}

impl Instruction {
    /// Return all memory accesses by the instruction - in expressions, captures, and memory manipulation
    pub fn get_memory_accesses(&self) -> MemoryAccesses {
        match self {
            Instruction::Comparison(Comparison { operands, .. }) => {
                let mut reads = HashSet::from([operands.1.name.clone()]);
                let writes = HashSet::from([operands.0.name.clone()]);
                if let ComparisonOperand::MemoryReference(mem) = &operands.2 {
                    reads.insert(mem.name.clone());
                }

                MemoryAccesses {
                    reads,
                    writes,
                    ..Default::default()
                }
            }
            Instruction::BinaryLogic(BinaryLogic { operands, .. }) => {
                let mut reads = HashSet::new();
                let mut writes = HashSet::new();
                reads.insert(operands.0.name.clone());
                writes.insert(operands.0.name.clone());
                if let BinaryOperand::MemoryReference(mem) = &operands.1 {
                    reads.insert(mem.name.clone());
                }

                MemoryAccesses {
                    reads,
                    writes,
                    ..Default::default()
                }
            }
            Instruction::UnaryLogic(UnaryLogic { operand, .. }) => MemoryAccesses {
                reads: HashSet::from([operand.name.clone()]),
                writes: HashSet::from([operand.name.clone()]),
                ..Default::default()
            },
            Instruction::Arithmetic(Arithmetic {
                destination,
                source,
                ..
            })
            | Instruction::Move(Move {
                destination,
                source,
            }) => MemoryAccesses {
                writes: set_from_optional_memory_reference![destination.get_memory_reference()],
                reads: set_from_optional_memory_reference![source.get_memory_reference()],
                ..Default::default()
            },
            Instruction::CalibrationDefinition(definition) => {
                let references: Vec<&MemoryReference> = definition
                    .parameters
                    .iter()
                    .flat_map(|expr| expr.get_memory_references())
                    .collect();
                MemoryAccesses {
                    reads: set_from_memory_references![references],
                    ..Default::default()
                }
            }
            Instruction::Capture(Capture {
                memory_reference,
                waveform,
                ..
            }) => MemoryAccesses {
                captures: set_from_memory_references!(vec![memory_reference]),
                reads: set_from_memory_references!(waveform.get_memory_references()),
                ..Default::default()
            },
            Instruction::CircuitDefinition(CircuitDefinition { instructions, .. })
            | Instruction::MeasureCalibrationDefinition(MeasureCalibrationDefinition {
                instructions,
                ..
            }) => instructions.iter().fold(Default::default(), |acc, el| {
                let el_accesses = el.get_memory_accesses();
                MemoryAccesses {
                    reads: merge_sets!(acc.reads, el_accesses.reads),
                    writes: merge_sets!(acc.writes, el_accesses.writes),
                    captures: merge_sets!(acc.captures, el_accesses.captures),
                }
            }),
            Instruction::Delay(Delay { duration, .. }) => MemoryAccesses {
                reads: set_from_memory_references!(duration.get_memory_references()),
                ..Default::default()
            },
            Instruction::Exchange(Exchange { left, right }) => MemoryAccesses {
                writes: merge_sets![
                    set_from_optional_memory_reference!(left.get_memory_reference()),
                    set_from_optional_memory_reference!(right.get_memory_reference())
                ],
                ..Default::default()
            },
            Instruction::Gate(Gate { parameters, .. }) => MemoryAccesses {
                reads: set_from_memory_references!(parameters
                    .iter()
                    .flat_map(|param| param.get_memory_references())
                    .collect::<Vec<&MemoryReference>>()),
                ..Default::default()
            },
            Instruction::GateDefinition(GateDefinition { specification, .. }) => {
                if let GateSpecification::Matrix(matrix) = specification {
                    let references = matrix
                        .iter()
                        .flat_map(|row| row.iter().flat_map(|cell| cell.get_memory_references()))
                        .collect::<Vec<&MemoryReference>>();
                    MemoryAccesses {
                        reads: set_from_memory_references!(references),
                        ..Default::default()
                    }
                } else {
                    Default::default()
                }
            }
            Instruction::JumpWhen(JumpWhen {
                target: _,
                condition,
            })
            | Instruction::JumpUnless(JumpUnless {
                target: _,
                condition,
            }) => MemoryAccesses {
                reads: set_from_memory_references!(vec![condition]),
                ..Default::default()
            },
            Instruction::Load(Load {
                destination,
                source,
                offset,
            }) => MemoryAccesses {
                writes: set_from_memory_references![vec![destination]],
                reads: set_from_reference_vec![vec![source, &offset.name]],
                ..Default::default()
            },
            Instruction::Measurement(Measurement { target, .. }) => MemoryAccesses {
                captures: set_from_optional_memory_reference!(target.as_ref()),
                ..Default::default()
            },
            Instruction::Pulse(Pulse { waveform, .. }) => MemoryAccesses {
                reads: set_from_memory_references![waveform.get_memory_references()],
                ..Default::default()
            },
            Instruction::RawCapture(RawCapture {
                duration,
                memory_reference,
                ..
            }) => MemoryAccesses {
                reads: set_from_memory_references![duration.get_memory_references()],
                captures: set_from_memory_references![vec![memory_reference]],
                ..Default::default()
            },
            Instruction::SetPhase(SetPhase { phase: expr, .. })
            | Instruction::SetScale(SetScale { scale: expr, .. })
            | Instruction::ShiftPhase(ShiftPhase { phase: expr, .. }) => MemoryAccesses {
                reads: set_from_memory_references!(expr.get_memory_references()),
                ..Default::default()
            },
            Instruction::Store(Store {
                destination,
                offset,
                source,
            }) => MemoryAccesses {
                reads: merge_sets![
                    set_from_memory_references!(vec![offset]),
                    set_from_optional_memory_reference!(source.get_memory_reference())
                ],
                writes: set_from_reference_vec![vec![destination]],
                ..Default::default()
            },
            Instruction::Convert(_)
            | Instruction::Declaration(_)
            | Instruction::Fence(_)
            | Instruction::FrameDefinition(_)
            | Instruction::Halt
            | Instruction::Wait
            | Instruction::Include(_)
            | Instruction::Jump(Jump { target: _ })
            | Instruction::Label(Label(_))
            | Instruction::Nop
            | Instruction::Pragma(_)
            | Instruction::Reset(_)
            | Instruction::SetFrequency(_)
            | Instruction::ShiftFrequency(_)
            | Instruction::SwapPhases(_)
            | Instruction::WaveformDefinition(_) => Default::default(),
        }
    }
}

impl ArithmeticOperand {
    pub fn get_memory_reference(&self) -> Option<&MemoryReference> {
        match self {
            ArithmeticOperand::LiteralInteger(_) => None,
            ArithmeticOperand::LiteralReal(_) => None,
            ArithmeticOperand::MemoryReference(reference) => Some(reference),
        }
    }
}

impl Expression {
    /// Return, if any, the memory references contained within this Expression.
    pub fn get_memory_references(&self) -> Vec<&MemoryReference> {
        match self {
            Expression::Address(reference) => vec![reference],
            Expression::FunctionCall(FunctionCallExpression { expression, .. }) => {
                expression.get_memory_references()
            }
            Expression::Infix(InfixExpression { left, right, .. }) => {
                let mut result = left.get_memory_references();
                result.extend(right.get_memory_references());
                result
            }
            Expression::Number(_) => vec![],
            Expression::PiConstant => vec![],
            Expression::Prefix(PrefixExpression { expression, .. }) => {
                expression.get_memory_references()
            }
            Expression::Variable(_) => vec![],
        }
    }
}

impl WaveformInvocation {
    /// Return, if any, the memory references contained within this WaveformInvocation.
    pub fn get_memory_references(&self) -> Vec<&MemoryReference> {
        let mut result = vec![];

        for expression in self.parameters.values() {
            result.extend(expression.get_memory_references());
        }

        result
    }
}
