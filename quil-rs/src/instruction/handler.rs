use std::collections::HashSet;

use crate::{
    instruction::{ExternSignatureMap, Qubit},
    program::{FrameSet, MatchedFrames, MemoryAccesses, MemoryAccessesError},
};

use super::InstructionRole;

pub trait InstructionHandler<I = super::Instruction> {
    /// Whether this instruction's timing within the pulse program must be precisely controlled so
    /// as to begin exactly on the end of the latest preceding timed instruction.
    ///
    /// See [the Quil-T portion of the Quil specification (Annex T)][Quil-T] for more information.
    ///
    /// [Quil-T]: https://quil-lang.github.io/#12Annex-T--Pulse-Level-Control
    fn is_scheduled(&self, instruction: &I) -> bool;

    /// Return this instruction's [role][InstructionRole].
    fn role(&self, instruction: &I) -> InstructionRole;

    /// Return the [frames][FrameIdentifier] which are either *used* or *blocked* by the given
    /// instruction.
    ///
    /// - An instruction `I` *uses* a frame `F` if the execution of `I` plays on `F`.
    ///
    /// - An instruction `I` *blocks* a frame `F` if `I` does not play on `F` but, even so, other
    ///   instructions may not play on `F` while `I` is executing.
    ///
    /// Only one instruction may play on a given frame at a time, so using a frame is a stronger
    /// condition than blocking a frame.
    ///
    /// `None` is returned if the instruction does not execute in the context of a frame; this is
    /// the case for purely classical instructions such as [`ADD`][Instruction::Add], for instance.
    ///
    /// See [the Quil-T portion of the Quil specification (Annex T)][Quil-T] for more information.
    ///
    /// [Quil-T]: https://quil-lang.github.io/#12Annex-T--Pulse-Level-Control
    fn matching_frames<'f>(
        &self,
        frames: &'f FrameSet,
        qubits_available: &'f HashSet<Qubit>,
        instruction: &I,
    ) -> Option<MatchedFrames<'f>>;

    /// Return all memory accesses by the instruction.
    ///
    /// Memory accesses may be performed by pure memory manipulation instructions (such as
    /// [`MOVE`][Instruction::Move]), by instructions that perform memory accesses as part of their
    /// semantics (such as [`CAPTURE`][Instruction::Capture]), by variable reads in expressions –
    /// anywhere that memory is read.
    ///
    /// # Errors
    ///
    /// This function is always permitted to fail if the program contains
    /// [`CALL`][Instruction::Call] instructions that cannot be resolved against a signature in the
    /// provided [`ExternSignatureMap`], either because they attempt to call unknown functions or
    /// because they call known functions with incorrect types.  Specific implementations may impose
    /// other failure conditions, and are encouraged to call them out if so.
    fn memory_accesses(
        &self,
        extern_signature_map: &ExternSignatureMap,
        instruction: &I,
    ) -> Result<MemoryAccesses, MemoryAccessesError>;
}

pub(super) mod default {
    use super::InstructionHandler;
    use crate::{instruction::*, program::FrameSet};
    use std::fmt;

    /// The default instruction-handling behavior.
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
    pub struct DefaultHandler;

    impl fmt::Display for DefaultHandler {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "default instruction handler")
        }
    }

    impl InstructionHandler for DefaultHandler {
        fn is_scheduled(&self, instruction: &Instruction) -> bool {
            match instruction {
                Instruction::Reset(_) => false,
                Instruction::Wait() => true,
                _ => self.role(instruction) == InstructionRole::RFControl,
            }
        }

        fn role(&self, instruction: &Instruction) -> InstructionRole {
            match instruction {
                Instruction::CalibrationDefinition(_)
                | Instruction::CircuitDefinition(_)
                | Instruction::Declaration(_)
                | Instruction::FrameDefinition(_)
                | Instruction::Gate(_)
                | Instruction::GateDefinition(_)
                | Instruction::Include(_)
                | Instruction::Label(_)
                | Instruction::MeasureCalibrationDefinition(_)
                | Instruction::Measurement(_)
                | Instruction::WaveformDefinition(_) => InstructionRole::ProgramComposition,

                Instruction::Reset(_)
                | Instruction::Capture(_)
                | Instruction::Delay(_)
                | Instruction::Fence(_)
                | Instruction::Pulse(_)
                | Instruction::RawCapture(_)
                | Instruction::SetFrequency(_)
                | Instruction::SetPhase(_)
                | Instruction::SetScale(_)
                | Instruction::ShiftFrequency(_)
                | Instruction::ShiftPhase(_)
                | Instruction::SwapPhases(_) => InstructionRole::RFControl,

                Instruction::Arithmetic(_)
                | Instruction::Call(_)
                | Instruction::Comparison(_)
                | Instruction::Convert(_)
                | Instruction::BinaryLogic(_)
                | Instruction::UnaryLogic(_)
                | Instruction::Move(_)
                | Instruction::Exchange(_)
                | Instruction::Load(_)
                | Instruction::Nop()
                | Instruction::Pragma(_)
                | Instruction::Store(_) => InstructionRole::ClassicalCompute,

                Instruction::Halt()
                | Instruction::Jump(_)
                | Instruction::JumpWhen(_)
                | Instruction::JumpUnless(_)
                | Instruction::Wait() => InstructionRole::ControlFlow,
            }
        }

        fn matching_frames<'f>(
            &self,
            frames: &'f FrameSet,
            qubits_available: &'f HashSet<Qubit>,
            instruction: &Instruction,
        ) -> Option<MatchedFrames<'f>> {
            instruction
                .default_frame_match_condition(qubits_available)
                .map(|condition| frames.filter(condition))
        }

        fn memory_accesses(
            &self,
            extern_signature_map: &ExternSignatureMap,
            instruction: &Instruction,
        ) -> Result<MemoryAccesses, MemoryAccessesError> {
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
            fn accesses(
                reference1: &MemoryReference,
                reference2: &MemoryReference,
            ) -> HashSet<String> {
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
            fn binary(
                destination: &MemoryReference,
                source: &impl ClassicalOperand,
            ) -> MemoryAccesses {
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
            fn read_all<'a>(
                places: impl IntoIterator<Item = &'a MemoryReference>,
            ) -> MemoryAccesses {
                MemoryAccesses {
                    reads: places.into_iter().map(|r| r.name.clone()).collect(),
                    writes: none(),
                    captures: none(),
                }
            }

            // Memory accesses done by gate applications
            fn gate_application(Gate { parameters, .. }: &Gate) -> MemoryAccesses {
                read_all(parameters.iter().flat_map(Expression::memory_references))
            }

            // The match

            Ok(match instruction {
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
                Instruction::Exchange(Exchange { left, right }) => {
                    read_write(accesses(left, right))
                }
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
                Instruction::Gate(gate) => gate_application(gate),

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
                Instruction::Call(call) => call.default_memory_accesses(extern_signature_map)?,

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
                        .map(|instr| self.memory_accesses(extern_signature_map, instr))
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
                    GateSpecification::Sequence(DefGateSequence { gates, qubits: _ }) => gates
                        .iter()
                        .map(gate_application)
                        .fold(MemoryAccesses::none(), MemoryAccesses::union),
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
                    .map(|instr| self.memory_accesses(extern_signature_map, instr))
                    .fold_ok(MemoryAccesses::none(), MemoryAccesses::union)?,
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
}
