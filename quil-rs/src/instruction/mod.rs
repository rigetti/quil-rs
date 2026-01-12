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

use std::{collections::HashSet, fmt, iter, str::FromStr};

use itertools::Itertools as _;
use nom_locate::LocatedSpan;

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::{gen_stub_pyclass_complex_enum, gen_stub_pymethods};

use crate::{
    expression::Expression,
    parser::{lex, parse_instructions},
    program::{
        frame::{FrameMatchCondition, FrameMatchConditions},
        MatchedFrames, MemoryAccesses, MemoryAccessesError,
    },
    quil::{write_join_quil, Quil, ToQuilResult},
    Program,
};

#[cfg(feature = "python")]
pub(crate) mod quilpy;

mod calibration;
mod circuit;
mod classical;
mod control_flow;
mod declaration;
mod extern_call;
mod frame;
mod gate;
mod gate_sequence;
mod measurement;
mod pragma;
mod qubit;
mod reset;
mod timing;
mod waveform;

pub use self::{
    calibration::{
        CalibrationDefinition, CalibrationIdentifier, CalibrationSignature,
        MeasureCalibrationDefinition, MeasureCalibrationIdentifier,
    },
    circuit::CircuitDefinition,
    classical::{
        Arithmetic, ArithmeticOperand, ArithmeticOperator, BinaryLogic, BinaryOperand,
        BinaryOperator, ClassicalOperand, Comparison, ComparisonOperand, ComparisonOperator,
        Convert, Exchange, Move, UnaryLogic, UnaryOperator,
    },
    control_flow::{Jump, JumpUnless, JumpWhen, Label, Target, TargetPlaceholder},
    declaration::{Declaration, Load, MemoryReference, Offset, ScalarType, Sharing, Store, Vector},
    extern_call::*,
    frame::{
        AttributeValue, Capture, FrameAttributes, FrameDefinition, FrameIdentifier, Pulse,
        RawCapture, SetFrequency, SetPhase, SetScale, ShiftFrequency, ShiftPhase, SwapPhases,
    },
    gate::{
        Gate, GateDefinition, GateError, GateModifier, GateSpecification, GateType, Matrix,
        PauliGate, PauliSum, PauliTerm,
    },
    gate_sequence::{DefGateSequence, DefGateSequenceError, DefGateSequenceExpansionError},
    measurement::Measurement,
    pragma::{Include, Pragma, PragmaArgument, RESERVED_PRAGMA_EXTERN},
    qubit::{Qubit, QubitPlaceholder},
    reset::Reset,
    timing::{Delay, Fence},
    waveform::{Waveform, WaveformDefinition, WaveformInvocation, WaveformParameters},
};

pub(crate) use self::gate::GateSignature;

#[derive(Clone, Debug, thiserror::Error, PartialEq, Eq)]
pub enum ValidationError {
    #[error(transparent)]
    GateError(#[from] GateError),
    #[error(transparent)]
    DefGateSequenceError(#[from] DefGateSequenceError),
}

/// A Quil instruction.
///
/// Each variant (for Python users, each nested subclass)
/// corresponds to a possible type of Quil instruction,
/// which is accessible as a member within the variant.
///
/// # Python Users
///
/// The subclasses of this class are class attributes defined on it,
/// and can be used to "wrap" instructions when they should be stored together.
/// In particular, they are *NOT* the instruction classes you'd typically create,
/// and instances of instruction classes are *NOT* subclasses of this class:
///
/// ```python
/// >>> from quil.instructions import Instruction, Gate, Qubit
/// >>> issubclass(Instruction.Gate, Instruction)
/// True
/// >>> issubclass(Gate, Instruction)
/// False
/// >>> g = Gate("X", (), (Qubit.Fixed(0),), ())
/// >>> isinstance(g, Gate)
/// True
/// >>> isinstance(g, Instruction.Gate)
/// False
/// >>> g_instr = Instruction.Gate(g)
/// >>> isinstance(g_instr, Gate)
/// False
/// >>> isinstance(g_instr, Instruction.Gate)
/// True
/// >>> isinstance(g_instr._0, Gate)
/// True
/// >>> g_instr._0 == g
/// True
/// ```
///
/// The point of this class is to wrap different kinds of instructions
/// when stored together in a collection, all of which are of type `Instruction`.
/// You can check for different instruction variants and destructure them using `match`:
///
/// ```python
/// match g_instr:
///     case Instruction.Gate(gate):
///         assert isinstance(gate, Gate)
///     case Instruction.Wait() | Instruction.Nop():
///         # note the `()` -- these aren't like Python's enumerations!
/// ```
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass_complex_enum)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.instructions", eq, frozen)
)]
pub enum Instruction {
    Arithmetic(Arithmetic),
    BinaryLogic(BinaryLogic),
    CalibrationDefinition(CalibrationDefinition),
    Call(Call),
    Capture(Capture),
    CircuitDefinition(CircuitDefinition),
    Convert(Convert),
    Comparison(Comparison),
    Declaration(Declaration),
    Delay(Delay),
    Exchange(Exchange),
    Fence(Fence),
    FrameDefinition(FrameDefinition),
    Gate(Gate),
    GateDefinition(GateDefinition),
    // Developer note: In Rust, this could be just `Halt`,
    // but to be compatible with PyO3's "complex enums",
    // it has to be an empty tuple variant.
    // The same restriction applies `Nop` and `Wait`,
    // as well as those in the `Expression` enumeration.
    Halt(),
    Include(Include),
    Jump(Jump),
    JumpUnless(JumpUnless),
    JumpWhen(JumpWhen),
    Label(Label),
    Load(Load),
    MeasureCalibrationDefinition(MeasureCalibrationDefinition),
    Measurement(Measurement),
    Move(Move),
    Nop(),
    Pragma(Pragma),
    Pulse(Pulse),
    RawCapture(RawCapture),
    Reset(Reset),
    SetFrequency(SetFrequency),
    SetPhase(SetPhase),
    SetScale(SetScale),
    ShiftFrequency(ShiftFrequency),
    ShiftPhase(ShiftPhase),
    Store(Store),
    SwapPhases(SwapPhases),
    UnaryLogic(UnaryLogic),
    WaveformDefinition(WaveformDefinition),
    Wait(),
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[cfg_attr(feature = "python", pyo3::pymethods)]
impl Instruction {
    /// Returns true if the instruction is a Quil-T instruction.
    pub fn is_quil_t(&self) -> bool {
        match self {
            Instruction::Capture(_)
            | Instruction::CalibrationDefinition(_)
            | Instruction::Delay(_)
            | Instruction::Fence(_)
            | Instruction::FrameDefinition(_)
            | Instruction::MeasureCalibrationDefinition(_)
            | Instruction::Pulse(_)
            | Instruction::RawCapture(_)
            | Instruction::SetFrequency(_)
            | Instruction::SetPhase(_)
            | Instruction::SetScale(_)
            | Instruction::ShiftFrequency(_)
            | Instruction::ShiftPhase(_)
            | Instruction::SwapPhases(_)
            | Instruction::WaveformDefinition(_) => true,

            Instruction::Arithmetic(_)
            | Instruction::BinaryLogic(_)
            | Instruction::Call(_)
            | Instruction::CircuitDefinition(_)
            | Instruction::Convert(_)
            | Instruction::Comparison(_)
            | Instruction::Declaration(_)
            | Instruction::Exchange(_)
            | Instruction::Gate(_)
            | Instruction::GateDefinition(_)
            | Instruction::Halt()
            | Instruction::Include(_)
            | Instruction::Jump(_)
            | Instruction::JumpUnless(_)
            | Instruction::JumpWhen(_)
            | Instruction::Label(_)
            | Instruction::Load(_)
            | Instruction::Measurement(_)
            | Instruction::Move(_)
            | Instruction::Nop()
            | Instruction::Pragma(_)
            | Instruction::Reset(_)
            | Instruction::Store(_)
            | Instruction::Wait()
            | Instruction::UnaryLogic(_) => false,
        }
    }
}

/// What purpose an instruction serves in the program from a [Quil-T] perspective.
///
/// [Quil-T]: https://quil-lang.github.io/#12Annex-T--Pulse-Level-Control
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum InstructionRole {
    /// An instruction that is relevant to the superstructure of the program but not to
    /// [Quil-T][]–level execution; for example, [`DECLARE`][Instruction::Declare].
    ///
    /// Note the callout of Quil-T above: the most surprising entries in this category, [by
    /// default][DefaultHandler], are *[gate application][Instruction::Gate] and
    /// [`MEASURE`][Instruction::Measurement]*.  This is because Quil-T expects all
    /// gates/measurements to be expanded through
    /// [`DEFCAL`][Instruction::CalibrationDefinition]/[`DEFCAL
    /// MEASURE`][Instruction::MeasureCalibrationDefinition] (or, for gates, through
    /// [`DEFGATE`][Instruction::GateDefinition`] or [`DEFCIRCUIT`][Instruction::CircuitDefinition]
    /// until they can be calibrated).
    ///
    /// [Quil-T]: https://quil-lang.github.io/#12Annex-T--Pulse-Level-Control
    ProgramComposition,

    /// An instruction affecting only classical state, such as [`ADD`][Instruction:ADD`].
    ClassicalCompute,

    /// An instruction affecting the pulse level portion of the program, such as
    /// [`PULSE`][Instruction::Pulse].  The RF stands for Radio Frequency.
    ///
    /// Unlike for [`MEASURE`][Instruction::Measurement], [`RESET`] is, [by
    /// default][DefaultHandler], considered an RF control instruction, as it is not realized
    /// through calibration into a lower-level instruction.
    RFControl,

    /// An instruction that can perform control flow, such as [`JUMP-WHEN`][Instruction::JumpWhen].
    ControlFlow,
}

pub fn write_instruction_block<'i, I, Q>(
    f: &mut impl fmt::Write,
    fall_back_to_debug: bool,
    values: I,
) -> crate::quil::ToQuilResult<()>
where
    I: IntoIterator<Item = &'i Q>,
    Q: Quil + 'i,
{
    write_join_quil(f, fall_back_to_debug, values, "\n", "\t")
}

pub(crate) fn write_join(
    f: &mut impl fmt::Write,
    values: &[impl fmt::Display],
    separator: &str,
    prefix: &str,
) -> fmt::Result {
    let mut iter = values.iter();
    if let Some(first) = iter.next() {
        write!(f, "{prefix}{first}")?;

        for value in iter {
            write!(f, "{separator}{prefix}{value}")?;
        }
    }
    Ok(())
}

pub fn format_integer_vector(values: &[u64]) -> String {
    values
        .iter()
        .map(|q| format!("{q}"))
        .collect::<Vec<String>>()
        .join(" ")
}

/// Write a list of qubits, with each prefixed by a space (including the first)
fn write_qubits(
    f: &mut impl fmt::Write,
    fall_back_to_debug: bool,
    qubits: &[Qubit],
) -> crate::quil::ToQuilResult<()> {
    for qubit in qubits {
        write!(f, " ")?;
        qubit.write(f, fall_back_to_debug)?;
    }
    Ok(())
}

/// Write qubits as a Quil parameter list, where all are prefixed with ` `.
fn write_qubit_parameters(
    f: &mut impl fmt::Write,
    fall_back_to_debug: bool,
    qubits: &[Qubit],
) -> ToQuilResult<()> {
    for qubit in qubits.iter() {
        write!(f, " ")?;
        qubit.write(f, fall_back_to_debug)?;
    }
    Ok(())
}

fn write_expression_parameter_string(
    f: &mut impl fmt::Write,
    fall_back_to_debug: bool,
    parameters: &[Expression],
) -> crate::quil::ToQuilResult<()> {
    if parameters.is_empty() {
        return Ok(());
    }

    write!(f, "(")?;
    write_join_quil(f, fall_back_to_debug, parameters, ", ", "")?;
    write!(f, ")")?;
    Ok(())
}

fn write_parameter_string<T: AsRef<str>>(f: &mut impl fmt::Write, parameters: &[T]) -> fmt::Result {
    if parameters.is_empty() {
        return Ok(());
    }

    write!(f, "(")?;
    write_join(
        f,
        parameters
            .iter()
            .map(AsRef::as_ref)
            .collect::<Vec<_>>()
            .as_slice(),
        ", ",
        "%",
    )?;
    write!(f, ")")
}

impl Quil for Instruction {
    fn write(
        &self,
        f: &mut impl fmt::Write,
        fall_back_to_debug: bool,
    ) -> Result<(), crate::quil::ToQuilError> {
        match self {
            Instruction::Arithmetic(arithmetic) => arithmetic.write(f, fall_back_to_debug),
            Instruction::CalibrationDefinition(calibration) => {
                calibration.write(f, fall_back_to_debug)
            }
            Instruction::Call(call) => call.write(f, fall_back_to_debug),
            Instruction::Capture(capture) => capture.write(f, fall_back_to_debug),
            Instruction::CircuitDefinition(circuit) => circuit.write(f, fall_back_to_debug),
            Instruction::Convert(convert) => convert.write(f, fall_back_to_debug),
            Instruction::Declaration(declaration) => declaration.write(f, fall_back_to_debug),
            Instruction::Delay(delay) => delay.write(f, fall_back_to_debug),
            Instruction::Fence(fence) => fence.write(f, fall_back_to_debug),
            Instruction::FrameDefinition(frame_definition) => {
                frame_definition.write(f, fall_back_to_debug)
            }
            Instruction::Gate(gate) => gate.write(f, fall_back_to_debug),
            Instruction::GateDefinition(gate_definition) => {
                gate_definition.write(f, fall_back_to_debug)
            }
            Instruction::Include(include) => include.write(f, fall_back_to_debug),
            Instruction::MeasureCalibrationDefinition(measure_calibration) => {
                measure_calibration.write(f, fall_back_to_debug)
            }
            Instruction::Measurement(measurement) => measurement.write(f, fall_back_to_debug),
            Instruction::Move(r#move) => r#move.write(f, fall_back_to_debug),
            Instruction::Exchange(exchange) => exchange.write(f, fall_back_to_debug),
            Instruction::Load(load) => load.write(f, fall_back_to_debug),
            Instruction::Store(store) => store.write(f, fall_back_to_debug),
            Instruction::Pulse(pulse) => pulse.write(f, fall_back_to_debug),
            Instruction::Pragma(pragma) => pragma.write(f, fall_back_to_debug),
            Instruction::RawCapture(raw_capture) => raw_capture.write(f, fall_back_to_debug),
            Instruction::Reset(reset) => reset.write(f, fall_back_to_debug),
            Instruction::SetFrequency(set_frequency) => set_frequency.write(f, fall_back_to_debug),
            Instruction::SetPhase(set_phase) => set_phase.write(f, fall_back_to_debug),
            Instruction::SetScale(set_scale) => set_scale.write(f, fall_back_to_debug),
            Instruction::ShiftFrequency(shift_frequency) => {
                shift_frequency.write(f, fall_back_to_debug)
            }
            Instruction::ShiftPhase(shift_phase) => shift_phase.write(f, fall_back_to_debug),
            Instruction::SwapPhases(swap_phases) => swap_phases.write(f, fall_back_to_debug),
            Instruction::WaveformDefinition(waveform_definition) => {
                waveform_definition.write(f, fall_back_to_debug)
            }
            Instruction::Halt() => write!(f, "HALT").map_err(Into::into),
            Instruction::Nop() => write!(f, "NOP").map_err(Into::into),
            Instruction::Wait() => write!(f, "WAIT").map_err(Into::into),
            Instruction::Jump(jump) => jump.write(f, fall_back_to_debug),
            Instruction::JumpUnless(jump) => jump.write(f, fall_back_to_debug),
            Instruction::JumpWhen(jump) => jump.write(f, fall_back_to_debug),
            Instruction::Label(label) => label.write(f, fall_back_to_debug),
            Instruction::Comparison(comparison) => comparison.write(f, fall_back_to_debug),
            Instruction::BinaryLogic(binary_logic) => binary_logic.write(f, fall_back_to_debug),
            Instruction::UnaryLogic(unary_logic) => unary_logic.write(f, fall_back_to_debug),
        }
    }
}

pub(crate) struct QuotedString<S>(pub(crate) S);

impl<S> fmt::Display for QuotedString<S>
where
    S: AsRef<str>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"")?;
        for c in self.0.as_ref().chars() {
            match c {
                '"' => write!(f, "\\\"")?,
                '\\' => write!(f, "\\\\")?,
                c => write!(f, "{c}")?,
            }
        }
        write!(f, "\"")
    }
}

#[cfg(test)]
mod test_instruction_display {
    use crate::{instruction::PragmaArgument, quil::Quil};

    use super::{Instruction, Pragma};

    #[test]
    fn pragma() {
        assert_eq!(
            Instruction::Pragma(Pragma {
                name: String::from("INITIAL_REWIRING"),
                arguments: vec![],
                data: Some(String::from("PARTIAL")),
            })
            .to_quil()
            .unwrap(),
            "PRAGMA INITIAL_REWIRING \"PARTIAL\""
        );
        assert_eq!(
            Instruction::Pragma(Pragma {
                name: String::from("LOAD-MEMORY"),
                arguments: vec![PragmaArgument::Identifier("q0".to_string())],
                data: Some(String::from("addr")),
            })
            .to_quil()
            .unwrap(),
            "PRAGMA LOAD-MEMORY q0 \"addr\""
        );
        assert_eq!(
            Instruction::Pragma(Pragma {
                name: String::from("PRESERVE_BLOCK"),
                arguments: vec![],
                data: None,
            })
            .to_quil()
            .unwrap(),
            "PRAGMA PRESERVE_BLOCK"
        );
    }
}

impl Instruction {
    /// Apply the provided closure to this instruction, mutating any `Expression`s within.
    /// Does not affect instructions without `Expression`s within.
    /// Does not traverse or mutate instructions nested within blocks (such as
    /// within `DEFCAL`).
    ///
    /// # Example
    ///
    /// ```rust
    /// use std::mem::replace;
    /// use std::str::FromStr;
    /// use quil_rs::{expression::Expression, Program, quil::Quil};
    ///
    ///
    /// let program = Program::from_str("SHIFT-PHASE 0 \"rf\" 2*2").unwrap();
    /// let mut instructions = program.to_instructions();
    /// instructions.iter_mut().for_each(|inst| inst.apply_to_expressions(Expression::simplify));
    ///
    /// assert_eq!(instructions[0].to_quil().unwrap(), String::from("SHIFT-PHASE 0 \"rf\" 4"))
    ///
    /// ```
    pub fn apply_to_expressions(&mut self, mut closure: impl FnMut(&mut Expression)) {
        match self {
            Instruction::CalibrationDefinition(CalibrationDefinition {
                identifier: CalibrationIdentifier { parameters, .. },
                ..
            })
            | Instruction::Gate(Gate { parameters, .. }) => {
                parameters.iter_mut().for_each(closure);
            }
            Instruction::Capture(Capture { waveform, .. })
            | Instruction::Pulse(Pulse { waveform, .. }) => {
                waveform.parameters.values_mut().for_each(closure);
            }
            Instruction::Delay(Delay { duration, .. })
            | Instruction::RawCapture(RawCapture { duration, .. }) => {
                closure(duration);
            }
            Instruction::FrameDefinition(FrameDefinition { attributes, .. }) => {
                for value in attributes.values_mut() {
                    if let AttributeValue::Expression(expression) = value {
                        closure(expression);
                    }
                }
            }
            Instruction::SetFrequency(SetFrequency {
                frequency: expression,
                ..
            })
            | Instruction::SetPhase(SetPhase {
                phase: expression, ..
            })
            | Instruction::SetScale(SetScale {
                scale: expression, ..
            })
            | Instruction::ShiftFrequency(ShiftFrequency {
                frequency: expression,
                ..
            })
            | Instruction::ShiftPhase(ShiftPhase {
                phase: expression, ..
            }) => {
                closure(expression);
            }
            Instruction::WaveformDefinition(WaveformDefinition { definition, .. }) => {
                definition.matrix.iter_mut().for_each(closure);
            }
            Instruction::GateDefinition(GateDefinition {
                specification: GateSpecification::Matrix(matrix),
                ..
            }) => {
                for row in matrix {
                    for cell in row {
                        closure(cell);
                    }
                }
            }
            _ => {}
        }
    }

    pub(crate) fn default_frame_match_condition<'a>(
        &'a self,
        qubits_available: &'a HashSet<Qubit>,
    ) -> Option<FrameMatchConditions<'a>> {
        match self {
            Instruction::Pulse(Pulse {
                blocking, frame, ..
            })
            | Instruction::Capture(Capture {
                blocking, frame, ..
            })
            | Instruction::RawCapture(RawCapture {
                blocking, frame, ..
            }) => Some(FrameMatchConditions {
                blocked: blocking
                    .then(|| FrameMatchCondition::AnyOfQubits(frame.qubits.iter().collect())),
                used: Some(FrameMatchCondition::Specific(frame)),
            }),
            Instruction::Delay(Delay {
                frame_names,
                qubits,
                ..
            }) => Some(FrameMatchConditions {
                used: Some(if frame_names.is_empty() {
                    FrameMatchCondition::ExactQubits(qubits.iter().collect())
                } else {
                    FrameMatchCondition::And(vec![
                        FrameMatchCondition::ExactQubits(qubits.iter().collect()),
                        FrameMatchCondition::AnyOfNames(
                            frame_names.iter().map(String::as_str).collect(),
                        ),
                    ])
                }),
                blocked: None,
            }),
            Instruction::Fence(Fence { qubits }) => Some(FrameMatchConditions {
                used: None,
                blocked: Some(if qubits.is_empty() {
                    FrameMatchCondition::All
                } else {
                    FrameMatchCondition::AnyOfQubits(qubits.iter().collect())
                }),
            }),
            Instruction::Reset(Reset { qubit }) => {
                let qubits = match qubit {
                    Some(qubit) => {
                        let mut set = HashSet::new();
                        set.insert(qubit);
                        set
                    }
                    None => qubits_available.iter().collect(),
                };

                Some(FrameMatchConditions {
                    used: Some(FrameMatchCondition::ExactQubits(qubits.clone())),
                    blocked: Some(FrameMatchCondition::AnyOfQubits(qubits)),
                })
            }
            Instruction::SetFrequency(SetFrequency { frame, .. })
            | Instruction::SetPhase(SetPhase { frame, .. })
            | Instruction::SetScale(SetScale { frame, .. })
            | Instruction::ShiftFrequency(ShiftFrequency { frame, .. })
            | Instruction::ShiftPhase(ShiftPhase { frame, .. }) => Some(FrameMatchConditions {
                used: Some(FrameMatchCondition::Specific(frame)),
                blocked: None,
            }),
            Instruction::SwapPhases(SwapPhases { frame_1, frame_2 }) => {
                Some(FrameMatchConditions {
                    used: Some(FrameMatchCondition::Or(vec![
                        FrameMatchCondition::Specific(frame_1),
                        FrameMatchCondition::Specific(frame_2),
                    ])),
                    blocked: None,
                })
            }
            Instruction::Arithmetic(_)
            | Instruction::BinaryLogic(_)
            | Instruction::CalibrationDefinition(_)
            | Instruction::Call(_)
            | Instruction::CircuitDefinition(_)
            | Instruction::Comparison(_)
            | Instruction::Convert(_)
            | Instruction::Declaration(_)
            | Instruction::Exchange(_)
            | Instruction::FrameDefinition(_)
            | Instruction::Gate(_)
            | Instruction::GateDefinition(_)
            | Instruction::Halt()
            | Instruction::Include(_)
            | Instruction::Jump(_)
            | Instruction::JumpUnless(_)
            | Instruction::JumpWhen(_)
            | Instruction::Label(_)
            | Instruction::Load(_)
            | Instruction::MeasureCalibrationDefinition(_)
            | Instruction::Measurement(_)
            | Instruction::Move(_)
            | Instruction::Nop()
            | Instruction::Pragma(_)
            | Instruction::Store(_)
            | Instruction::UnaryLogic(_)
            | Instruction::WaveformDefinition(_)
            | Instruction::Wait() => None,
        }
    }

    /// Return immutable references to the [`Qubit`]s contained within an instruction
    #[allow(dead_code)]
    pub fn get_qubits(&self) -> Vec<&Qubit> {
        match self {
            Instruction::Gate(gate) => gate.qubits.iter().collect(),
            Instruction::CalibrationDefinition(calibration) => calibration
                .identifier
                .qubits
                .iter()
                .chain(
                    calibration
                        .instructions
                        .iter()
                        .flat_map(|inst| inst.get_qubits()),
                )
                .collect(),
            Instruction::MeasureCalibrationDefinition(measurement) => {
                iter::once(&measurement.identifier.qubit)
                    .chain(
                        measurement
                            .instructions
                            .iter()
                            .flat_map(|inst| inst.get_qubits()),
                    )
                    .collect()
            }
            Instruction::Measurement(measurement) => vec![&measurement.qubit],
            Instruction::Reset(reset) => match &reset.qubit {
                Some(qubit) => vec![qubit],
                None => vec![],
            },
            Instruction::Delay(delay) => delay.qubits.iter().collect(),
            Instruction::Fence(fence) => fence.qubits.iter().collect(),
            Instruction::Capture(capture) => capture.frame.qubits.iter().collect(),
            Instruction::Pulse(pulse) => pulse.frame.qubits.iter().collect(),
            Instruction::RawCapture(raw_capture) => raw_capture.frame.qubits.iter().collect(),
            _ => vec![],
        }
    }

    /// Return mutable references to the [`Qubit`]s contained within an instruction
    pub fn get_qubits_mut(&mut self) -> Vec<&mut Qubit> {
        match self {
            Instruction::Gate(gate) => gate.qubits.iter_mut().collect(),
            Instruction::CalibrationDefinition(calibration) => calibration
                .identifier
                .qubits
                .iter_mut()
                .chain(
                    calibration
                        .instructions
                        .iter_mut()
                        .flat_map(|inst| inst.get_qubits_mut()),
                )
                .collect(),
            Instruction::MeasureCalibrationDefinition(measurement) => {
                iter::once(&mut measurement.identifier.qubit)
                    .chain(
                        measurement
                            .instructions
                            .iter_mut()
                            .flat_map(|inst| inst.get_qubits_mut()),
                    )
                    .collect()
            }
            Instruction::Measurement(measurement) => vec![&mut measurement.qubit],
            Instruction::Reset(reset) => match &mut reset.qubit {
                Some(qubit) => vec![qubit],
                None => vec![],
            },
            Instruction::Delay(delay) => delay.qubits.iter_mut().collect(),
            Instruction::Fence(fence) => fence.qubits.iter_mut().collect(),
            Instruction::Capture(capture) => capture.frame.qubits.iter_mut().collect(),
            Instruction::Pulse(pulse) => pulse.frame.qubits.iter_mut().collect(),
            Instruction::RawCapture(raw_capture) => raw_capture.frame.qubits.iter_mut().collect(),
            _ => vec![],
        }
    }

    /// Return the waveform _directly_ invoked by the instruction, if any.
    ///
    /// Note: this does not expand calibrations or other instructions which may
    /// indirectly cause a waveform to be invoked.
    pub(crate) fn get_waveform_invocation(&self) -> Option<&WaveformInvocation> {
        match self {
            Instruction::Capture(Capture { waveform, .. }) => Some(waveform),
            Instruction::Pulse(Pulse { waveform, .. }) => Some(waveform),
            _ => None,
        }
    }

    /// Parse a single instruction from an input string. Returns an error if the input fails to parse,
    /// or if there is input left over after parsing.
    #[cfg(test)]
    pub(crate) fn parse_in_test(input: &str) -> Result<Self, String> {
        use crate::parser::instruction::parse_instruction;

        let input = LocatedSpan::new(input);
        let lexed = lex(input).map_err(|err| err.to_string())?;
        let (_, instruction) =
            nom::combinator::all_consuming(parse_instruction)(&lexed).map_err(|e| e.to_string())?;
        Ok(instruction)
    }

    pub(crate) fn resolve_placeholders<TR, QR>(&mut self, target_resolver: TR, qubit_resolver: QR)
    where
        TR: Fn(&TargetPlaceholder) -> Option<String>,
        QR: Fn(&QubitPlaceholder) -> Option<u64>,
    {
        match self {
            Instruction::Label(label) => {
                label.target.resolve_placeholder(target_resolver);
            }
            Instruction::Jump(jump) => {
                jump.target.resolve_placeholder(target_resolver);
            }
            Instruction::JumpWhen(jump_when) => {
                jump_when.target.resolve_placeholder(target_resolver);
            }
            Instruction::JumpUnless(jump_unless) => {
                jump_unless.target.resolve_placeholder(target_resolver);
            }
            other => {
                for qubit in other.get_qubits_mut() {
                    qubit.resolve_placeholder(&qubit_resolver);
                }
            }
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ParseInstructionError {
    #[error("Failed to parse instruction: {0}")]
    Parse(String),
    #[error("Expected to parse exactly one instruction but got {0}")]
    ZeroOrMany(usize),
}

impl FromStr for Instruction {
    type Err = ParseInstructionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let input = LocatedSpan::new(s);
        let lexed = lex(input).map_err(|e| ParseInstructionError::Parse(e.to_string()))?;
        let instructions =
            parse_instructions(&lexed).map_err(|e| ParseInstructionError::Parse(e.to_string()))?;
        if instructions.1.len() != 1 {
            return Err(ParseInstructionError::ZeroOrMany(instructions.1.len()));
        }
        Ok(instructions.1[0].to_owned())
    }
}

pub trait InstructionHandler {
    /// Whether this instruction's timing within the pulse program must be precisely controlled so
    /// as to begin exactly on the end of the latest preceding timed instruction.
    ///
    /// See [the Quil-T portion of the Quil specification (Annex T)][Quil-T] for more information.
    ///
    /// [Quil-T]: https://quil-lang.github.io/#12Annex-T--Pulse-Level-Control
    #[inline]
    fn is_scheduled(&self, instruction: &Instruction) -> bool {
        DefaultHandler.is_scheduled(instruction)
    }

    /// Return this instruction's [role][InstructionRole].
    #[inline]
    fn role(&self, instruction: &Instruction) -> InstructionRole {
        DefaultHandler.role(instruction)
    }

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
    #[inline]
    fn matching_frames<'p>(
        &self,
        program: &'p Program,
        instruction: &Instruction,
    ) -> Option<MatchedFrames<'p>> {
        DefaultHandler.matching_frames(program, instruction)
    }

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
    #[inline]
    fn memory_accesses(
        &self,
        extern_signature_map: &ExternSignatureMap,
        instruction: &Instruction,
    ) -> Result<MemoryAccesses, MemoryAccessesError> {
        DefaultHandler.memory_accesses(extern_signature_map, instruction)
    }
}

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

    fn matching_frames<'p>(
        &self,
        program: &'p Program,
        instruction: &Instruction,
    ) -> Option<MatchedFrames<'p>> {
        instruction
            .default_frame_match_condition(program.get_used_qubits())
            .map(|condition| program.frames.filter(condition))
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

#[cfg(test)]
mod tests {
    use rstest::*;
    use std::str::FromStr as _;

    use crate::{expression::Expression, Program};

    use super::MemoryReference;

    #[test]
    fn apply_to_expressions() {
        let mut program = Program::from_str(
            "DECLARE ro BIT
SET-PHASE 0 \"rf\" pi/2
RX(2) 0",
        )
        .unwrap();
        let closure = |expr: &mut Expression| *expr = Expression::Variable(String::from("a"));
        program.for_each_body_instruction(|instruction| {
            instruction.apply_to_expressions(closure);
        });

        let expected_program = Program::from_str(
            "DECLARE ro BIT
SET-PHASE 0 \"rf\" %a
RX(%a) 0",
        )
        .unwrap();

        assert_eq!(expected_program, program);
    }

    #[rstest(input, expected,
        case("_", MemoryReference { name: "_".to_string(), index: 0 }),
        case("a", MemoryReference { name: "a".to_string(), index: 0 }),
        case("a---b", MemoryReference { name: "a---b".to_string(), index: 0 }),
        case("_a_b_", MemoryReference { name: "_a_b_".to_string(), index: 0 }),
        case("a-2_b-2", MemoryReference { name: "a-2_b-2".to_string(), index: 0 }),
        case("_[0]", MemoryReference { name: "_".to_string(), index: 0 }),
        case("a[1]", MemoryReference { name: "a".to_string(), index: 1 }),
        case("a---b[2]", MemoryReference { name: "a---b".to_string(), index: 2 }),
        case("_a_b_[3]", MemoryReference { name: "_a_b_".to_string(), index: 3 }),
        case("a-2_b-2[4]", MemoryReference { name: "a-2_b-2".to_string(), index: 4 }),
    )]
    fn it_parses_memory_reference_from_str(input: &str, expected: MemoryReference) {
        assert_eq!(MemoryReference::from_str(input), Ok(expected));
    }

    #[rstest(
        input,
        case(""),
        case("[0]"),
        case("a[-1]"),
        case("2a[2]"),
        case("-a"),
        case("NOT[3]"),
        case("a a"),
        case("a[5] a[5]"),
        case("DECLARE a[6]")
    )]
    fn it_fails_to_parse_memory_reference_from_str(input: &str) {
        assert!(MemoryReference::from_str(input).is_err());
    }

    mod placeholders {
        use std::collections::HashMap;

        use crate::instruction::{Label, Qubit, QubitPlaceholder, Target, TargetPlaceholder};

        #[allow(clippy::redundant_clone)]
        #[test]
        fn target() {
            let placeholder_1 = TargetPlaceholder::new(String::from("label"));
            let placeholder_2 = TargetPlaceholder::new(String::from("label"));
            let placeholder_3 = TargetPlaceholder::new(String::from("other"));

            assert_eq!(placeholder_1, placeholder_1);
            assert_eq!(placeholder_1, placeholder_1.clone());
            assert_eq!(placeholder_1.clone(), placeholder_1.clone());
            assert_ne!(placeholder_1, placeholder_2);
            assert_ne!(placeholder_2, placeholder_3);
            assert_ne!(placeholder_1, placeholder_3);
        }

        #[test]
        fn target_resolution() {
            let placeholder_1 = TargetPlaceholder::new(String::from("label"));
            let placeholder_2 = TargetPlaceholder::new(String::from("label"));

            let resolver = HashMap::from([(placeholder_1.clone(), String::from("label_1"))]);

            let mut label_1 = Label {
                target: Target::Placeholder(placeholder_1),
            };
            label_1
                .target
                .resolve_placeholder(|k| resolver.get(k).cloned());
            assert_eq!(label_1.target, Target::Fixed(String::from("label_1")));

            let mut label_2 = Label {
                target: Target::Placeholder(placeholder_2.clone()),
            };
            label_2
                .target
                .resolve_placeholder(|k| resolver.get(k).cloned());
            assert_eq!(label_2.target, Target::Placeholder(placeholder_2));
        }

        #[allow(clippy::redundant_clone)]
        #[test]
        fn qubit() {
            let placeholder_1 = QubitPlaceholder::default();
            let placeholder_2 = QubitPlaceholder::default();

            assert_eq!(placeholder_1, placeholder_1);
            assert_eq!(placeholder_1, placeholder_1.clone());
            assert_eq!(placeholder_1.clone(), placeholder_1.clone());
            assert_ne!(placeholder_1, placeholder_2);
        }

        #[test]
        fn qubit_resolution() {
            let placeholder_1 = QubitPlaceholder::default();
            let placeholder_2 = QubitPlaceholder::default();

            let resolver = HashMap::from([(placeholder_1.clone(), 1)]);

            let mut qubit_1 = Qubit::Placeholder(placeholder_1);
            qubit_1.resolve_placeholder(|k| resolver.get(k).copied());
            assert_eq!(qubit_1, Qubit::Fixed(1));

            let mut qubit_2 = Qubit::Placeholder(placeholder_2.clone());
            qubit_2.resolve_placeholder(|k| resolver.get(k).copied());
            assert_eq!(qubit_2, Qubit::Placeholder(placeholder_2));
        }
    }

    mod instruction_handler {
        use super::super::*;

        struct CustomFrameHandler;

        impl InstructionHandler for CustomFrameHandler {
            fn matching_frames<'p>(
                &self,
                program: &'p Program,
                instruction: &Instruction,
            ) -> Option<MatchedFrames<'p>> {
                if let Instruction::Pragma(_) = instruction {
                    Some(MatchedFrames {
                        used: program.frames.get_keys().into_iter().collect(),
                        blocked: HashSet::new(),
                    })
                } else {
                    DefaultHandler.matching_frames(program, instruction)
                }
            }
        }

        #[test]
        fn it_considers_custom_instruction_frames() {
            let program = r#"DEFFRAME 0 "rf":
    CENTER-FREQUENCY: 3e9

PRAGMA USES-ALL-FRAMES
"#
            .parse::<Program>()
            .unwrap();

            // This test assumes that the default simplification behavior will not assign frames to
            // `PRAGMA` instructions. This is verified below.
            assert!(program.simplify(&DefaultHandler).unwrap().frames.is_empty());

            assert_eq!(
                program.simplify(&CustomFrameHandler).unwrap().frames.len(),
                1
            );
        }
    }
}
