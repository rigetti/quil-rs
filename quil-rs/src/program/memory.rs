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

use itertools::Itertools as _;
#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::gen_stub_pyclass;

use crate::instruction::Waveform;
pub(crate) use crate::instruction::{
    Arithmetic, BinaryLogic, CalibrationDefinition, CalibrationIdentifier, CallResolutionError,
    Capture, CircuitDefinition, ClassicalOperand, Comparison, Convert, Delay, Exchange,
    ExternSignatureMap, Gate, GateDefinition, GateSpecification, Instruction, JumpUnless, JumpWhen,
    Load, MeasureCalibrationDefinition, Measurement, MemoryReference, Move, Pulse, RawCapture,
    SetFrequency, SetPhase, SetScale, Sharing, ShiftFrequency, ShiftPhase, Store, UnaryLogic,
    Vector, WaveformInvocation,
};
use crate::pickleable_new;
use crate::{
    expression::{Expression, FunctionCallExpression, InfixExpression, PrefixExpression},
    instruction::WaveformDefinition,
};

#[derive(Clone, Debug, Hash, PartialEq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.program", eq, frozen, hash, get_all, subclass)
)]
pub struct MemoryRegion {
    pub size: Vector,
    pub sharing: Option<Sharing>,
}

pickleable_new! {
    impl MemoryRegion {
        pub fn new(size: Vector, sharing: Option<Sharing>);
    }
}

/// How an instruction or sequence of instructions can access memory.
///
/// Each access is stored as the name of the region that was accessed.  We do not store the
/// individual indices that were accessed.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct MemoryAccesses {
    /// All ways these instructions can read from memory.
    pub reads: HashSet<String>,

    /// All ways these instructions can write to memory from within the processor.
    ///
    /// The "within the processor" clause indicates that this covers the write to the destination of
    /// a [`MOVE`][Instruction::Move], but not the write to the target of a
    /// [`MEASURE`][Instruction::Measurement].
    pub writes: HashSet<String>,

    /// All ways these instructions can write to memory from outside the processor.
    ///
    /// The "outside the processor" clause indicates that this covers the write to the target of a
    /// [`MEASURE`][Instruction::Measurement], but not the write to the destination of a
    /// [`MOVE`][Instruction::Move].
    pub captures: HashSet<String>,
}

impl MemoryAccesses {
    /// Returns a new empty set of memory accesses
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// A more semantically-meaningful alias for [`Self::new`].
    #[inline]
    pub fn none() -> Self {
        Self::new()
    }

    pub fn union(mut self, rhs: MemoryAccesses) -> Self {
        let MemoryAccesses {
            captures,
            reads,
            writes,
        } = rhs;
        self.captures.extend(captures);
        self.reads.extend(reads);
        self.writes.extend(writes);
        self
    }
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
    pub fn memory_accesses(
        &self,
        extern_signature_map: &ExternSignatureMap,
    ) -> MemoryAccessesResult {
        // Building individual access sets

        #[inline]
        fn none() -> HashSet<String> {
            HashSet::new()
        }

        #[inline]
        fn access(reference: &MemoryReference) -> HashSet<String> {
            [reference.name.clone()].into()
        }

        #[inline]
        fn access_dynamic(region: &str) -> HashSet<String> {
            [region.to_owned()].into()
        }

        #[inline]
        fn accesses(reference1: &MemoryReference, reference2: &MemoryReference) -> HashSet<String> {
            [reference1.name.clone(), reference2.name.clone()].into()
        }

        #[inline]
        fn accesses_dynamic_index(region: &str, index: &MemoryReference) -> HashSet<String> {
            [region.to_owned(), index.name.clone()].into()
        }

        #[inline]
        fn access_opt(opt_reference: Option<&MemoryReference>) -> HashSet<String> {
            opt_reference.map_or_else(HashSet::new, access)
        }

        #[inline]
        fn access_operand(operand: &impl ClassicalOperand) -> HashSet<String> {
            access_opt(operand.memory_reference())
        }

        #[inline]
        fn accesses_with_operand(
            reference: &MemoryReference,
            operand: &impl ClassicalOperand,
        ) -> HashSet<String> {
            if let Some(other) = operand.memory_reference() {
                accesses(reference, other)
            } else {
                access(reference)
            }
        }

        // Building complete access patterns

        // Move-like operations: those that read from at most one place and write to another
        fn like_move(
            destination: &MemoryReference,
            source_accesses: HashSet<String>,
        ) -> MemoryAccesses {
            MemoryAccesses {
                reads: source_accesses,
                writes: access(destination),
                captures: none(),
            }
        }

        // Updating binary operators: read from a possible source, read and write to the
        // destination.
        fn binary(destination: &MemoryReference, source: &impl ClassicalOperand) -> MemoryAccesses {
            MemoryAccesses {
                reads: accesses_with_operand(destination, source),
                writes: access(destination),
                captures: none(),
            }
        }

        // Read-write operations, whose inputs are the same as their outputs.
        fn read_write(places: HashSet<String>) -> MemoryAccesses {
            MemoryAccesses {
                reads: places.clone(),
                writes: places,
                captures: none(),
            }
        }

        // Classical instructions that read a single memory reference.
        fn read_one(place: &MemoryReference) -> MemoryAccesses {
            MemoryAccesses {
                reads: access(place),
                writes: none(),
                captures: none(),
            }
        }

        // Instructions that read from many memory references; for instance, those that take an
        // expression as an argument.
        fn read_all<'a>(places: impl IntoIterator<Item = &'a MemoryReference>) -> MemoryAccesses {
            MemoryAccesses {
                reads: places.into_iter().map(|r| r.name.clone()).collect(),
                writes: none(),
                captures: none(),
            }
        }

        // The match

        Ok(match self {
            // Operations with simple memory access patterns as captured (heh) above
            Instruction::Convert(Convert {
                destination,
                source,
            }) => like_move(destination, access(source)),
            Instruction::Move(Move {
                destination,
                source,
            }) => like_move(destination, access_operand(source)),
            Instruction::BinaryLogic(BinaryLogic {
                destination,
                source,
                operator: _,
            }) => binary(destination, source),
            Instruction::Arithmetic(Arithmetic {
                destination,
                source,
                ..
            }) => binary(destination, source),
            Instruction::UnaryLogic(UnaryLogic { operand, .. }) => read_write(access(operand)),
            Instruction::Exchange(Exchange { left, right }) => read_write(accesses(left, right)),
            Instruction::JumpWhen(JumpWhen {
                target: _,
                condition,
            })
            | Instruction::JumpUnless(JumpUnless {
                target: _,
                condition,
            }) => read_one(condition),

            // Our sole ternary operator: read from the operands, write to the destination.
            Instruction::Comparison(Comparison {
                destination,
                lhs,
                rhs,
                operator: _,
            }) => MemoryAccesses {
                reads: accesses_with_operand(lhs, rhs),
                writes: access(destination),
                captures: none(),
            },

            // Quil-T instructions that read from a single expression.
            Instruction::Delay(Delay { duration: expr, .. })
            | Instruction::SetPhase(SetPhase { phase: expr, .. })
            | Instruction::SetScale(SetScale { scale: expr, .. })
            | Instruction::ShiftPhase(ShiftPhase { phase: expr, .. })
            | Instruction::SetFrequency(SetFrequency {
                frequency: expr, ..
            })
            | Instruction::ShiftFrequency(ShiftFrequency {
                frequency: expr, ..
            }) => read_all(expr.memory_references()),

            // Operations that read from memory and nothing else because they interact with the
            // quantum components of the system.
            Instruction::Pulse(Pulse {
                waveform,
                blocking: _,
                frame: _,
            }) => read_all(waveform.memory_references()),
            Instruction::Gate(Gate { parameters, .. }) => {
                read_all(parameters.iter().flat_map(Expression::memory_references))
            }

            // Capturing operations; the Quil-T variants may also read from memory.
            Instruction::Capture(Capture {
                memory_reference,
                waveform,
                blocking: _,
                frame: _,
            }) => MemoryAccesses {
                reads: waveform
                    .memory_references()
                    .map(|r| r.name.clone())
                    .collect(),
                captures: access(memory_reference),
                writes: none(),
            },
            Instruction::Measurement(Measurement { target, .. }) => MemoryAccesses {
                captures: access_opt(target.as_ref()),
                reads: none(),
                writes: none(),
            },
            Instruction::RawCapture(RawCapture {
                duration,
                memory_reference,
                blocking: _,
                frame: _,
            }) => MemoryAccesses {
                reads: duration
                    .memory_references()
                    .map(|r| r.name.clone())
                    .collect(),
                captures: access(memory_reference),
                writes: none(),
            },

            // Calls to external functions, which handle their own logic by looking at their
            // signature.
            Instruction::Call(call) => call.memory_accesses(extern_signature_map)?,

            // Parameterized definitions whose parameters can also themselves reference memory
            Instruction::CalibrationDefinition(CalibrationDefinition {
                identifier:
                    CalibrationIdentifier {
                        parameters,
                        modifiers: _,
                        name: _,
                        qubits: _,
                    },
                instructions,
            }) => {
                let parameter_reads = MemoryAccesses {
                    reads: parameters
                        .iter()
                        .flat_map(Expression::memory_references)
                        .map(|r| r.name.clone())
                        .collect(),
                    writes: none(),
                    captures: none(),
                };
                instructions
                    .iter()
                    .map(|instr| instr.memory_accesses(extern_signature_map))
                    .fold_ok(parameter_reads, MemoryAccesses::union)?
            }

            // Parameterized definitions whose parameters cannot themselves reference memory.  Note
            // that their memory accesses may refer to parameter names instead of global
            // declarations.
            Instruction::GateDefinition(GateDefinition {
                specification,
                name: _,
                parameters: _,
            }) => match specification {
                GateSpecification::Matrix(matrix) => read_all(
                    matrix
                        .iter()
                        .flat_map(|row| row.iter().flat_map(Expression::memory_references)),
                ),
                GateSpecification::Permutation(_) | GateSpecification::PauliSum(_) => {
                    MemoryAccesses::none()
                }
            },
            Instruction::CircuitDefinition(CircuitDefinition {
                instructions,
                name: _,
                parameters: _,
                qubit_variables: _,
            })
            | Instruction::MeasureCalibrationDefinition(MeasureCalibrationDefinition {
                instructions,
                identifier: _,
            }) => instructions
                .iter()
                .map(|instr| instr.memory_accesses(extern_signature_map))
                .fold_ok(MemoryAccesses::new(), MemoryAccesses::union)?,
            Instruction::WaveformDefinition(WaveformDefinition {
                definition:
                    Waveform {
                        matrix,
                        parameters: _,
                    },
                name: _,
            }) => read_all(matrix.iter().flat_map(Expression::memory_references)),

            // Dynamic memory accesses.  If we ever track region indices precisely, these will
            // require conservatively marking accesses (read for load, write for store) as blocking
            // the whole region.
            Instruction::Load(Load {
                destination,
                source,
                offset,
            }) => MemoryAccesses {
                reads: accesses_dynamic_index(source, offset),
                writes: access(destination),
                captures: none(),
            },
            Instruction::Store(Store {
                destination,
                offset,
                source,
            }) => MemoryAccesses {
                reads: accesses_with_operand(offset, source),
                writes: access_dynamic(destination),
                captures: none(),
            },

            // Instructions that can't contain any memory references.  Conservatively includes
            // `INCLUDE`, which we don't handle here, and `PRAGMA`, which we can't.
            Instruction::Declaration(_)
            | Instruction::Fence(_)
            | Instruction::FrameDefinition(_)
            | Instruction::Halt()
            | Instruction::Wait()
            | Instruction::Include(_)
            | Instruction::Jump(_)
            | Instruction::Label(_)
            | Instruction::Nop()
            | Instruction::Pragma(_)
            | Instruction::Reset(_)
            | Instruction::SwapPhases(_) => MemoryAccesses::none(),
        })
    }
}

pub mod expression {
    // Contains an implementation of an iterator over all [`MemoryReference`]s read from by an
    // expression.  We construct a separate iterator here so we can avoid reifying a vector of
    // [`MemoryReference`]s that we will then immediately consume.

    use super::*;

    /// An iterator over all the memory references contained in an expression.
    #[derive(Clone, Debug)]
    pub struct MemoryReferences<'a> {
        pub(super) stack: Vec<&'a Expression>,
    }

    impl<'a> Iterator for MemoryReferences<'a> {
        type Item = &'a MemoryReference;

        fn next(&mut self) -> Option<Self::Item> {
            // If we imagine collecting into a vector, this function is roughly
            //
            // ```
            // fn collect_into(expr: &Expression, output: &mut Vec<&MemoryReference>) {
            //     match expr {
            //         Expression::NoReferences(_) => (),
            //         Expression::Address(reference) => output.push(reference),
            //         Expression::OneSubexpression(expression) => collect_into(expression, output),
            //         Expression::TwoSubexpressions(left, right) => {
            //             collect_into(left, output);
            //             collect_into(right, output);
            //         }
            //     }
            // }
            // ```
            //
            // In order to implement an iterator without allocating the whole vector, we still have
            // to reify the stack for the two-subexpression case; that's what `self.stack` is.
            //
            // We then implement this function with two loops.  The outer loop, `'stack_search`, is
            // our stack traversal; it finds the nearest stack frame.  The inner loop is effectively
            // our *tail calls*, where we don't need to allocate another stack frame; instead, we
            // assign to the current stack frame and keep going.  We don't have to write back to the
            // vector in the tail call case because no leaf node has more than one reference, so
            // once we've popped a stack frame off it'll fully bottom out.
            //
            // Note also that in the two-subexpression case we actually swap the order from the
            // `collect_into` example.  This is because `collect_into` reuses the same `output`, so
            // the state is preserved and `collect_into(right, output)` appends after `left`.
            // However, when we are emitting our results immediately, the tail call will happen
            // *before* the delayed stack frame.
            //
            // And yes, this would all be simpler with `gen` blocks.

            let Self { stack } = self;

            // Search through all parent expressions
            'stack_search: while let Some(mut expr) = stack.pop() {
                // An optimization for when there's only one child expression
                loop {
                    match expr {
                        // We're done with this expression and didn't find anything; time to walk down
                        // another tree branch, if there are any left.
                        Expression::Number(_)
                        | Expression::PiConstant()
                        | Expression::Variable(_) => continue 'stack_search,

                        // We're done with this expression and it was successful; stop iterating here,
                        // and when we return, walk down another tree branch if there are any left.
                        Expression::Address(reference) => return Some(reference),

                        // This expression only has one subexpression; we can avoid pushing a new
                        // "stack frame" and immediately popping it by overwriting the current stack
                        // frame.
                        Expression::FunctionCall(FunctionCallExpression {
                            expression,
                            function: _,
                        })
                        | Expression::Prefix(PrefixExpression {
                            expression,
                            operator: _,
                        }) => expr = expression,

                        // This expression has two subexpression; we delay searching through the
                        // right child by pushing it on the stack, and "tail call" to search through
                        // the left child immediately as we did with the single-subexpression case
                        // above.
                        Expression::Infix(InfixExpression {
                            left,
                            right,
                            operator: _,
                        }) => {
                            stack.push(right);
                            expr = left
                        }
                    }
                }
            }

            // We've finished our traversal; there are no subexpressions left.
            None
        }
    }

    impl std::iter::FusedIterator for MemoryReferences<'_> {}
}

impl Expression {
    /// Return an iterator over all the memory references contained within this expression.
    pub fn memory_references(&self) -> expression::MemoryReferences<'_> {
        expression::MemoryReferences { stack: vec![self] }
    }
}

impl WaveformInvocation {
    /// Return, if any, the memory references contained within this WaveformInvocation.
    pub fn memory_references(&self) -> impl std::iter::FusedIterator<Item = &MemoryReference> {
        self.parameters
            .values()
            .flat_map(Expression::memory_references)
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
            .memory_accesses(&ExternSignatureMap::default())
            .expect("must be able to get memory accesses");
        assert_eq!(memory_accesses.captures, expected.captures);
        assert_eq!(memory_accesses.reads, expected.reads);
        assert_eq!(memory_accesses.writes, expected.writes);
    }
}
