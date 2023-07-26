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
use std::fmt;

use crate::expression::Expression;
use crate::impl_quil_for_ref;
#[cfg(test)]
use crate::parser::lex;
use crate::program::frame::{FrameMatchCondition, FrameMatchConditions};
use crate::quil::{write_join_quil, Quil, ToQuilResult};

#[cfg(test)]
use nom_locate::LocatedSpan;

mod calibration;
mod circuit;
mod classical;
mod declaration;
mod frame;
mod gate;
mod label;
mod measurement;
mod pragma;
mod qubit;
mod reset;
mod timing;
mod waveform;

pub use self::calibration::{Calibration, MeasureCalibrationDefinition};
pub use self::circuit::CircuitDefinition;
pub use self::classical::{
    Arithmetic, ArithmeticOperand, ArithmeticOperator, BinaryLogic, BinaryOperand, BinaryOperands,
    BinaryOperator, Comparison, ComparisonOperand, ComparisonOperator, Convert, Exchange, Move,
    UnaryLogic, UnaryOperator,
};
pub use self::declaration::{
    Declaration, Load, MemoryReference, Offset, ScalarType, Sharing, Store, Vector,
};
pub use self::frame::{
    AttributeValue, Capture, FrameAttributes, FrameDefinition, FrameIdentifier, Pulse, RawCapture,
    SetFrequency, SetPhase, SetScale, ShiftFrequency, ShiftPhase, SwapPhases,
};
pub use self::gate::{
    Gate, GateDefinition, GateError, GateModifier, GateSpecification, GateType, Matrix, PauliGate,
    PauliSum, PauliTerm,
};
pub use self::label::{Label, LabelPlaceholder};
pub use self::measurement::Measurement;
pub use self::pragma::{Include, Pragma, PragmaArgument};
pub use self::qubit::{Qubit, QubitPlaceholder};
pub use self::reset::Reset;
pub use self::timing::{Delay, Fence};
pub use self::waveform::{Waveform, WaveformDefinition, WaveformInvocation};

#[derive(Clone, Debug, thiserror::Error, PartialEq, Eq)]
pub enum ValidationError {
    #[error(transparent)]
    GateError(#[from] GateError),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Jump {
    pub target: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JumpWhen {
    pub target: String,
    pub condition: MemoryReference,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JumpUnless {
    pub target: String,
    pub condition: MemoryReference,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    Arithmetic(Arithmetic),
    BinaryLogic(BinaryLogic),
    CalibrationDefinition(Calibration),
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
    Halt,
    Include(Include),
    Jump(Jump),
    JumpUnless(JumpUnless),
    JumpWhen(JumpWhen),
    Label(Label),
    Load(Load),
    MeasureCalibrationDefinition(MeasureCalibrationDefinition),
    Measurement(Measurement),
    Move(Move),
    Nop,
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
    Wait,
}

#[derive(Clone, Debug)]
pub enum InstructionRole {
    ClassicalCompute,
    ControlFlow,
    ProgramComposition,
    RFControl,
}

impl From<&Instruction> for InstructionRole {
    fn from(instruction: &Instruction) -> Self {
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
            | Instruction::Comparison(_)
            | Instruction::Convert(_)
            | Instruction::BinaryLogic(_)
            | Instruction::UnaryLogic(_)
            | Instruction::Move(_)
            | Instruction::Exchange(_)
            | Instruction::Load(_)
            | Instruction::Nop
            | Instruction::Pragma(_)
            | Instruction::Store(_) => InstructionRole::ClassicalCompute,
            Instruction::Halt
            | Instruction::Jump(_)
            | Instruction::JumpWhen(_)
            | Instruction::JumpUnless(_)
            | Instruction::Wait => InstructionRole::ControlFlow,
        }
    }
}

pub fn write_instruction_block<I, Q>(
    f: &mut impl std::fmt::Write,
    values: I,
) -> crate::quil::ToQuilResult<()>
where
    I: IntoIterator<Item = Q>,
    Q: Quil,
{
    write_join_quil(f, values, "\n", "\t")
}

pub(crate) fn write_join(
    f: &mut impl std::fmt::Write,
    values: &[impl std::fmt::Display],
    separator: &str,
    prefix: &str,
) -> std::fmt::Result {
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

pub fn format_matrix(matrix: &[Vec<Expression>]) -> String {
    matrix
        .iter()
        .map(|row| {
            row.iter()
                .map(|cell| format!("{cell}"))
                .collect::<Vec<String>>()
                .join(", ")
        })
        .collect::<Vec<String>>()
        .join("\n\t")
}

fn write_qubits(f: &mut impl std::fmt::Write, qubits: &[Qubit]) -> crate::quil::ToQuilResult<()> {
    for qubit in qubits {
        write!(f, " ")?;
        qubit.write(f)?;
    }
    Ok(())
}

/// Write qubits as a Quil parameter list, where each variable qubit must be prefixed with a `%`
/// and all are prefixed with ` `. Return an error if any is a qubit placeholder.
fn write_qubit_parameters(f: &mut impl std::fmt::Write, qubits: &[Qubit]) -> ToQuilResult<()> {
    for qubit in qubits.iter() {
        match qubit {
            Qubit::Variable(var) => write!(f, " %{var}")?,
            other => {
                write!(f, " ")?;
                other.write(f)?;
            }
        }
    }

    Ok(())
}

fn write_expression_parameter_string(
    f: &mut impl std::fmt::Write,
    parameters: &[Expression],
) -> crate::quil::ToQuilResult<()> {
    if parameters.is_empty() {
        return Ok(());
    }

    write!(f, "(")?;
    write_join_quil(f, parameters, ", ", "")?;
    write!(f, ")")?;
    Ok(())
}

fn write_parameter_string(f: &mut fmt::Formatter, parameters: &[String]) -> fmt::Result {
    if parameters.is_empty() {
        return Ok(());
    }

    write!(f, "(")?;
    write_join(f, parameters, ",", "%")?;
    write!(f, ")")
}

impl Quil for Instruction {
    fn write(&self, f: &mut impl std::fmt::Write) -> Result<(), crate::quil::ToQuilError> {
        match self {
            Instruction::Arithmetic(arithmetic) => arithmetic.write(f),
            Instruction::CalibrationDefinition(calibration) => calibration.write(f),
            Instruction::Capture(capture) => capture.write(f),
            Instruction::CircuitDefinition(circuit) => circuit.write(f),
            Instruction::Convert(convert) => convert.write(f),
            Instruction::Declaration(declaration) => declaration.write(f),
            Instruction::Delay(delay) => delay.write(f),
            Instruction::Fence(fence) => fence.write(f),
            Instruction::FrameDefinition(frame_definition) => frame_definition.write(f),
            Instruction::Gate(gate) => gate.write(f),
            Instruction::GateDefinition(gate_definition) => gate_definition.write(f),
            Instruction::Include(include) => include.write(f),
            Instruction::MeasureCalibrationDefinition(measure_calibration) => {
                measure_calibration.write(f)
            }
            Instruction::Measurement(measurement) => measurement.write(f),
            Instruction::Move(r#move) => r#move.write(f),
            Instruction::Exchange(exchange) => exchange.write(f),
            Instruction::Load(load) => load.write(f),
            Instruction::Store(store) => store.write(f),
            Instruction::Pulse(pulse) => pulse.write(f),
            Instruction::Pragma(pragma) => pragma.write(f),
            Instruction::RawCapture(raw_capture) => raw_capture.write(f),
            Instruction::Reset(reset) => reset.write(f),
            Instruction::SetFrequency(set_frequency) => set_frequency.write(f),
            Instruction::SetPhase(set_phase) => set_phase.write(f),
            Instruction::SetScale(set_scale) => set_scale.write(f),
            Instruction::ShiftFrequency(shift_frequency) => shift_frequency.write(f),
            Instruction::ShiftPhase(shift_phase) => shift_phase.write(f),
            Instruction::SwapPhases(swap_phases) => swap_phases.write(f),
            Instruction::WaveformDefinition(waveform_definition) => waveform_definition.write(f),
            Instruction::Halt => write!(f, "HALT").map_err(Into::into),
            Instruction::Nop => write!(f, "NOP").map_err(Into::into),
            Instruction::Wait => write!(f, "WAIT").map_err(Into::into),
            Instruction::Jump(Jump { target }) => write!(f, "JUMP @{target}").map_err(Into::into),
            Instruction::JumpUnless(JumpUnless { condition, target }) => {
                write!(f, "JUMP-UNLESS @{target} {condition}").map_err(Into::into)
            }
            Instruction::JumpWhen(JumpWhen { condition, target }) => {
                write!(f, "JUMP-WHEN @{target} {condition}").map_err(Into::into)
            }
            Instruction::Label(label) => label.write(f),
            Instruction::Comparison(comparison) => comparison.write(f),
            Instruction::BinaryLogic(binary_logic) => binary_logic.write(f),
            Instruction::UnaryLogic(unary_logic) => unary_logic.write(f),
        }
    }
}

impl_quil_for_ref!(Instruction);

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
            Instruction::CalibrationDefinition(Calibration { parameters, .. })
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

    pub(crate) fn get_frame_match_condition<'a>(
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
            | Instruction::CircuitDefinition(_)
            | Instruction::Comparison(_)
            | Instruction::Convert(_)
            | Instruction::Declaration(_)
            | Instruction::Exchange(_)
            | Instruction::FrameDefinition(_)
            | Instruction::Gate(_)
            | Instruction::GateDefinition(_)
            | Instruction::Halt
            | Instruction::Include(_)
            | Instruction::Jump(_)
            | Instruction::JumpUnless(_)
            | Instruction::JumpWhen(_)
            | Instruction::Label(_)
            | Instruction::Load(_)
            | Instruction::MeasureCalibrationDefinition(_)
            | Instruction::Measurement(_)
            | Instruction::Move(_)
            | Instruction::Nop
            | Instruction::Pragma(_)
            | Instruction::Store(_)
            | Instruction::UnaryLogic(_)
            | Instruction::WaveformDefinition(_)
            | Instruction::Wait => None,
        }
    }

    /// Return immutable references to the [`Qubit`]s contained within an instruction
    /// TODO: replace the logic in Program::get_used_qubits with this
    #[allow(dead_code)]
    pub(crate) fn get_qubits(&self) -> Vec<&Qubit> {
        match self {
            Instruction::Gate(gate) => gate.qubits.iter().collect(),
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
    pub(crate) fn get_qubits_mut(&mut self) -> Vec<&mut Qubit> {
        match self {
            Instruction::Gate(gate) => gate.qubits.iter_mut().collect(),
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

    #[cfg(test)]
    /// Parse a single instruction from an input string. Returns an error if the input fails to parse,
    /// or if there is input left over after parsing.
    pub(crate) fn parse(input: &str) -> Result<Self, String> {
        use crate::parser::instruction::parse_instruction;

        let input = LocatedSpan::new(input);
        let lexed = lex(input).map_err(|err| err.to_string())?;
        let (_, instruction) =
            nom::combinator::all_consuming(parse_instruction)(&lexed).map_err(|e| e.to_string())?;
        Ok(instruction)
    }

    /// Per the Quil-T spec, whether this instruction's timing within the pulse
    /// program must be precisely controlled so as to begin exactly on the end of
    /// the latest preceding timed instruction
    pub fn is_scheduled(&self) -> bool {
        match self {
            Instruction::Capture(_)
            | Instruction::Delay(_)
            | Instruction::Fence(_)
            | Instruction::Pulse(_)
            | Instruction::RawCapture(_)
            | Instruction::SetFrequency(_)
            | Instruction::SetPhase(_)
            | Instruction::SetScale(_)
            | Instruction::ShiftFrequency(_)
            | Instruction::ShiftPhase(_)
            | Instruction::SwapPhases(_)
            | Instruction::Wait => true,
            Instruction::Arithmetic(_)
            | Instruction::BinaryLogic(_)
            | Instruction::CalibrationDefinition(_)
            | Instruction::CircuitDefinition(_)
            | Instruction::Convert(_)
            | Instruction::Comparison(_)
            | Instruction::Declaration(_)
            | Instruction::Exchange(_)
            | Instruction::FrameDefinition(_)
            | Instruction::Gate(_)
            | Instruction::GateDefinition(_)
            | Instruction::Halt
            | Instruction::Include(_)
            | Instruction::Jump(_)
            | Instruction::JumpUnless(_)
            | Instruction::JumpWhen(_)
            | Instruction::Label(_)
            | Instruction::Load(_)
            | Instruction::MeasureCalibrationDefinition(_)
            | Instruction::Measurement(_)
            | Instruction::Move(_)
            | Instruction::Nop
            | Instruction::Pragma(_)
            | Instruction::Reset(_)
            | Instruction::Store(_)
            | Instruction::UnaryLogic(_)
            | Instruction::WaveformDefinition(_) => false,
        }
    }

    pub(crate) fn resolve_placeholders<LR, QR>(&mut self, label_resolver: LR, qubit_resolver: QR)
    where
        LR: Fn(&LabelPlaceholder) -> Option<String>,
        QR: Fn(&QubitPlaceholder) -> Option<u64>,
    {
        match self {
            Instruction::Label(label) => {
                label.resolve_placeholder(label_resolver);
            }
            other => {
                for qubit in other.get_qubits_mut() {
                    qubit.resolve_placeholder(&qubit_resolver);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use rstest::*;
    use std::str::FromStr;

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

        use crate::instruction::{Label, LabelPlaceholder, Qubit, QubitPlaceholder};

        #[allow(clippy::redundant_clone)]
        #[test]
        fn label() {
            let placeholder_1 = LabelPlaceholder::new(String::from("label"));
            let placeholder_2 = LabelPlaceholder::new(String::from("label"));
            let placeholder_3 = LabelPlaceholder::new(String::from("other"));

            assert_eq!(placeholder_1, placeholder_1);
            assert_eq!(placeholder_1, placeholder_1.clone());
            assert_eq!(placeholder_1.clone(), placeholder_1.clone());
            assert_ne!(placeholder_1, placeholder_2);
            assert_ne!(placeholder_2, placeholder_3);
            assert_ne!(placeholder_1, placeholder_3);
        }

        #[test]
        fn label_resolution() {
            let placeholder_1 = LabelPlaceholder::new(String::from("label"));
            let placeholder_2 = LabelPlaceholder::new(String::from("label"));

            let resolver = HashMap::from([(placeholder_1.clone(), String::from("label_1"))]);

            let mut label_1 = Label::Placeholder(placeholder_1);
            label_1.resolve_placeholder(|k| resolver.get(k).cloned());
            assert_eq!(label_1, Label::Fixed(String::from("label_1")));

            let mut label_2 = Label::Placeholder(placeholder_2.clone());
            label_2.resolve_placeholder(|k| resolver.get(k).cloned());
            assert_eq!(label_2, Label::Placeholder(placeholder_2));
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
}
