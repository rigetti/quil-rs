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

use crate::expression::{Expression, FunctionCallExpression, InfixExpression, PrefixExpression};
use crate::instruction::{
    Arithmetic, ArithmeticOperand, BinaryLogic, BinaryOperand, CallResolutionError, Capture,
    CircuitDefinition, Comparison, ComparisonOperand, Convert, Delay, Exchange, ExternSignatureMap,
    Gate, GateDefinition, GateSpecification, Instruction, JumpUnless, JumpWhen, Load,
    MeasureCalibrationDefinition, Measurement, MemoryReference, Move, Pulse, RawCapture,
    SetFrequency, SetPhase, SetScale, Sharing, ShiftFrequency, ShiftPhase, Store, UnaryLogic,
    Vector, WaveformInvocation,
};

#[derive(Clone, Debug, Hash, PartialEq)]
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

#[derive(Clone, Debug)]
pub struct MemoryAccess {
    pub regions: HashSet<String>,
    pub access_type: MemoryAccessType,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct MemoryAccesses {
    pub captures: HashSet<String>,
    pub reads: HashSet<String>,
    pub writes: HashSet<String>,
}

/// Express a mode of memory access.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
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
        set_from_reference_vec![$reference.map_or_else(Vec::new, |reference| vec![&reference.name])]
    };
}

/// Build a HashSet<&String> from a Vec<&MemoryReference>
macro_rules! set_from_memory_references {
    ($references:expr) => {
        set_from_reference_vec![$references.iter().map(|reference| &reference.name)]
    };
}

#[derive(thiserror::Error, Debug, PartialEq, Clone)]
pub enum MemoryAccessesError {
    #[error(transparent)]
    CallResolution(#[from] CallResolutionError),
}

pub type MemoryAccessesResult = Result<MemoryAccesses, MemoryAccessesError>;

impl Instruction {
    /// Return all memory accesses by the instruction - in expressions, captures, and memory manipulation.
    ///
    /// This will fail if the program contains [`Instruction::Call`] instructions that cannot
    /// be resolved against a signature in the provided [`ExternSignatureMap`] (either because
    /// they call functions that don't appear in the map or because the types of the parameters
    /// are wrong).
    pub fn get_memory_accesses(
        &self,
        extern_signature_map: &ExternSignatureMap,
    ) -> MemoryAccessesResult {
        Ok(match self {
            Instruction::Convert(Convert {
                source,
                destination,
            }) => MemoryAccesses {
                reads: set_from_memory_references![[source]],
                writes: set_from_memory_references![[destination]],
                ..Default::default()
            },
            Instruction::Call(call) => call.get_memory_accesses(extern_signature_map)?,
            Instruction::Comparison(Comparison {
                destination,
                lhs,
                rhs,
                operator: _,
            }) => {
                let mut reads = HashSet::from([lhs.name.clone()]);
                let writes = HashSet::from([destination.name.clone()]);
                if let ComparisonOperand::MemoryReference(mem) = &rhs {
                    reads.insert(mem.name.clone());
                }

                MemoryAccesses {
                    reads,
                    writes,
                    ..Default::default()
                }
            }
            Instruction::BinaryLogic(BinaryLogic {
                destination,
                source,
                operator: _,
            }) => {
                let mut reads = HashSet::new();
                let mut writes = HashSet::new();
                reads.insert(destination.name.clone());
                writes.insert(destination.name.clone());
                if let BinaryOperand::MemoryReference(mem) = &source {
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
            }) => MemoryAccesses {
                writes: HashSet::from([destination.name.clone()]),
                reads: set_from_optional_memory_reference![source.get_memory_reference()],
                ..Default::default()
            },
            Instruction::Move(Move {
                destination,
                source,
            }) => MemoryAccesses {
                writes: set_from_memory_references![[destination]],
                reads: set_from_optional_memory_reference![source.get_memory_reference()],
                ..Default::default()
            },
            Instruction::CalibrationDefinition(definition) => {
                let references: Vec<&MemoryReference> = definition
                    .identifier
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
                captures: set_from_memory_references!([memory_reference]),
                reads: set_from_memory_references!(waveform.get_memory_references()),
                ..Default::default()
            },
            Instruction::CircuitDefinition(CircuitDefinition { instructions, .. })
            | Instruction::MeasureCalibrationDefinition(MeasureCalibrationDefinition {
                instructions,
                ..
            }) => instructions.iter().try_fold(
                Default::default(),
                |acc: MemoryAccesses, el| -> MemoryAccessesResult {
                    let el_accesses = el.get_memory_accesses(extern_signature_map)?;
                    Ok(MemoryAccesses {
                        reads: merge_sets!(acc.reads, el_accesses.reads),
                        writes: merge_sets!(acc.writes, el_accesses.writes),
                        captures: merge_sets!(acc.captures, el_accesses.captures),
                    })
                },
            )?,
            Instruction::Delay(Delay { duration, .. }) => MemoryAccesses {
                reads: set_from_memory_references!(duration.get_memory_references()),
                ..Default::default()
            },
            Instruction::Exchange(Exchange { left, right }) => MemoryAccesses {
                reads: set_from_memory_references![[left, right]],
                writes: set_from_memory_references![[left, right]],
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
                reads: set_from_memory_references!([condition]),
                ..Default::default()
            },
            Instruction::Load(Load {
                destination,
                source,
                offset,
            }) => MemoryAccesses {
                writes: set_from_memory_references![[destination]],
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
                captures: set_from_memory_references![[memory_reference]],
                ..Default::default()
            },
            Instruction::SetPhase(SetPhase { phase: expr, .. })
            | Instruction::SetScale(SetScale { scale: expr, .. })
            | Instruction::ShiftPhase(ShiftPhase { phase: expr, .. }) => MemoryAccesses {
                reads: set_from_memory_references!(expr.get_memory_references()),
                ..Default::default()
            },
            Instruction::SetFrequency(SetFrequency { frequency, .. })
            | Instruction::ShiftFrequency(ShiftFrequency { frequency, .. }) => MemoryAccesses {
                reads: set_from_memory_references!(frequency.get_memory_references()),
                ..Default::default()
            },
            Instruction::Store(Store {
                destination,
                offset,
                source,
            }) => {
                let mut reads = vec![&offset.name];
                if let Some(source) = source.get_memory_reference() {
                    reads.push(&source.name);
                }
                MemoryAccesses {
                    reads: set_from_reference_vec![reads],
                    writes: set_from_reference_vec![vec![destination]],
                    ..Default::default()
                }
            }
            Instruction::Declaration(_)
            | Instruction::Fence(_)
            | Instruction::FrameDefinition(_)
            | Instruction::Halt
            | Instruction::Wait
            | Instruction::Include(_)
            | Instruction::Jump(_)
            | Instruction::Label(_)
            | Instruction::Nop
            | Instruction::Pragma(_)
            | Instruction::Reset(_)
            | Instruction::SwapPhases(_)
            | Instruction::WaveformDefinition(_) => Default::default(),
        })
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
        self.parameters
            .values()
            .flat_map(Expression::get_memory_references)
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use rstest::rstest;

    use crate::expression::Expression;
    use crate::instruction::{
        ArithmeticOperand, Convert, Exchange, ExternSignatureMap, FrameIdentifier, Instruction,
        MemoryReference, Qubit, SetFrequency, ShiftFrequency, Store,
    };
    use crate::program::MemoryAccesses;
    use std::collections::HashSet;

    #[rstest]
    #[case(
        Instruction::Store(Store {
            destination: "destination".to_string(),
            offset: MemoryReference {
                name: "offset".to_string(),
                index: Default::default()
            },
            source: ArithmeticOperand::MemoryReference(MemoryReference {
                name: "source".to_string(),
                index: Default::default()
            }),
        }),
        MemoryAccesses {
            captures: HashSet::new(),
            reads: ["source", "offset"].iter().cloned().map(String::from).collect(),
            writes: ["destination"].iter().cloned().map(String::from).collect(),
        }
    )]
    #[case(
        Instruction::Convert(Convert {
            destination: MemoryReference {
                name: "destination".to_string(),
                index: Default::default()
            },
            source: MemoryReference {
                name: "source".to_string(),
                index: Default::default()
            },
        }),
        MemoryAccesses {
            captures: HashSet::new(),
            reads: ["source"].iter().cloned().map(String::from).collect(),
            writes: ["destination"].iter().cloned().map(String::from).collect(),
        }
    )]
    #[case(
        Instruction::Exchange(Exchange {
            left: MemoryReference {
                name: "left".to_string(),
                index: Default::default()
            },
            right: MemoryReference {
                name: "right".to_string(),
                index: Default::default()
            },
        }),
        MemoryAccesses {
            captures: HashSet::new(),
            reads: ["left", "right"].iter().cloned().map(String::from).collect(),
            writes: ["left", "right"].iter().cloned().map(String::from).collect(),
        }
    )]
    #[case(
        Instruction::SetFrequency(SetFrequency {
            frequency: Expression::Address(MemoryReference {
                name: "frequency".to_string(),
                index: Default::default()
            }),
            frame: FrameIdentifier {
                name: "frame".to_string(),
                qubits: vec![Qubit::Fixed(0)]
            }
        }),
        MemoryAccesses {
            captures: HashSet::new(),
            reads: ["frequency"].iter().cloned().map(String::from).collect(),
            writes: HashSet::new(),
        }
    )]
    #[case(
        Instruction::ShiftFrequency(ShiftFrequency {
            frequency: Expression::Address(MemoryReference {
                name: "frequency".to_string(),
                index: Default::default()
            }),
            frame: FrameIdentifier {
                name: "frame".to_string(),
                qubits: vec![Qubit::Fixed(0)]
            }
        }),
        MemoryAccesses {
            captures: HashSet::new(),
            reads: ["frequency"].iter().cloned().map(String::from).collect(),
            writes: HashSet::new(),
        }
    )]
    fn test_instruction_accesses(
        #[case] instruction: Instruction,
        #[case] expected: MemoryAccesses,
    ) {
        let memory_accesses = instruction
            .get_memory_accesses(&ExternSignatureMap::default())
            .expect("must be able to get memory accesses");
        assert_eq!(memory_accesses.captures, expected.captures);
        assert_eq!(memory_accesses.reads, expected.reads);
        assert_eq!(memory_accesses.writes, expected.writes);
    }
}
