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
#[cfg(test)]
use crate::parser::lex;
use crate::program::frame::FrameMatchCondition;

#[cfg(test)]
use nom_locate::LocatedSpan;

mod calibration;
mod circuit;
mod classical;
mod declaration;
mod frame;
mod gate;
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
    Gate, GateDefinition, GateError, GateModifier, GateSpecification, GateType, PauliGate,
    PauliSum, PauliTerm,
};
pub use self::measurement::Measurement;
pub use self::pragma::{Include, Pragma, PragmaArgument};
pub use self::qubit::Qubit;
pub use self::reset::Reset;
pub use self::timing::{Delay, Fence};
pub use self::waveform::{Waveform, WaveformDefinition, WaveformInvocation};

#[derive(Clone, Debug, thiserror::Error, PartialEq, Eq)]
pub enum ValidationError {
    #[error(transparent)]
    GateError(#[from] GateError),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Label(pub String);

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

pub fn format_instructions(values: &[Instruction]) -> String {
    values
        .iter()
        .map(|i| format!("{i}"))
        .collect::<Vec<String>>()
        .join("\n\t")
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

pub fn format_qubits(qubits: &[Qubit]) -> String {
    qubits
        .iter()
        .map(|q| format!("{q}"))
        .collect::<Vec<String>>()
        .join(" ")
}

fn write_qubits(f: &mut fmt::Formatter, qubits: &[Qubit]) -> fmt::Result {
    for qubit in qubits {
        write!(f, " {qubit}")?;
    }
    Ok(())
}

/// Write qubits as a Quil parameter list, where each variable qubit must be prefixed with a `%`.
fn write_qubit_parameters(f: &mut fmt::Formatter, qubits: &[Qubit]) -> fmt::Result {
    for qubit in qubits.iter() {
        match qubit {
            Qubit::Fixed(index) => write!(f, " {index}")?,
            Qubit::Variable(var) => write!(f, " %{var}")?,
        }
    }

    Ok(())
}

/// Write the values as a comma separated list, with an optional prefix before each value.
fn write_comma_separated_list(
    f: &mut fmt::Formatter,
    values: &[impl fmt::Display],
    prefix: Option<&str>,
) -> fmt::Result {
    let prefix = prefix.unwrap_or_default();
    let mut iter = values.iter();

    if let Some(value) = iter.next() {
        write!(f, "{prefix}{value}")?;
    }

    for value in iter {
        write!(f, ", {prefix}{value}")?;
    }

    Ok(())
}

fn write_expression_parameter_string(
    f: &mut fmt::Formatter,
    parameters: &[Expression],
) -> fmt::Result {
    if parameters.is_empty() {
        return Ok(());
    }

    write!(f, "(")?;
    write_comma_separated_list(f, parameters, None)?;
    write!(f, ")")
}

fn write_parameter_string(f: &mut fmt::Formatter, parameters: &[String]) -> fmt::Result {
    if parameters.is_empty() {
        return Ok(());
    }

    write!(f, "(")?;
    write_comma_separated_list(f, parameters, Some("%"))?;
    write!(f, ")")
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Arithmetic(arithmetic) => write!(f, "{arithmetic}"),
            Instruction::CalibrationDefinition(calibration) => write!(f, "{calibration}"),
            Instruction::Capture(capture) => write!(f, "{capture}"),
            Instruction::CircuitDefinition(circuit) => write!(f, "{circuit}"),
            Instruction::Convert(convert) => write!(f, "{convert}"),
            Instruction::Declaration(declaration) => write!(f, "{declaration}"),
            Instruction::Delay(delay) => write!(f, "{delay}"),
            Instruction::Fence(fence) => write!(f, "{fence}"),
            Instruction::FrameDefinition(frame_defintion) => write!(f, "{frame_defintion}"),
            Instruction::Gate(gate) => write!(f, "{gate}"),
            Instruction::GateDefinition(gate_definition) => write!(f, "{gate_definition}"),
            Instruction::Include(include) => write!(f, "{include}"),
            Instruction::MeasureCalibrationDefinition(measure_calibration) => {
                write!(f, "{measure_calibration}")
            }
            Instruction::Measurement(measurement) => write!(f, "{measurement}"),
            Instruction::Move(r#move) => write!(f, "{move}"),
            Instruction::Exchange(exchange) => write!(f, "{exchange}"),
            Instruction::Load(load) => write!(f, "{load}"),
            Instruction::Store(store) => write!(f, "{store}"),
            Instruction::Pulse(pulse) => write!(f, "{pulse}"),
            Instruction::Pragma(pragma) => write!(f, "{pragma}"),
            Instruction::RawCapture(raw_capture) => write!(f, "{raw_capture}"),
            Instruction::Reset(reset) => write!(f, "{reset}"),
            Instruction::SetFrequency(set_frequency) => write!(f, "{set_frequency}"),
            Instruction::SetPhase(set_phase) => write!(f, "{set_phase}"),
            Instruction::SetScale(set_scale) => write!(f, "{set_scale}"),
            Instruction::ShiftFrequency(shift_frequency) => write!(f, "{shift_frequency}"),
            Instruction::ShiftPhase(shift_phase) => write!(f, "{shift_phase}"),
            Instruction::SwapPhases(swap_phases) => write!(f, "{swap_phases}"),
            Instruction::WaveformDefinition(waveform_definition) => {
                write!(f, "{waveform_definition}")
            }
            Instruction::Halt => write!(f, "HALT"),
            Instruction::Nop => write!(f, "NOP"),
            Instruction::Wait => write!(f, "WAIT"),
            Instruction::Jump(Jump { target }) => write!(f, "JUMP @{target}"),
            Instruction::JumpUnless(JumpUnless { condition, target }) => {
                write!(f, "JUMP-UNLESS @{target} {condition}")
            }
            Instruction::JumpWhen(JumpWhen { condition, target }) => {
                write!(f, "JUMP-WHEN @{target} {condition}")
            }
            Instruction::Label(Label(label)) => write!(f, "LABEL @{label}"),
            Instruction::Comparison(comparison) => write!(f, "{comparison}"),
            Instruction::BinaryLogic(binary_logic) => write!(f, "{binary_logic}"),
            Instruction::UnaryLogic(unary_logic) => write!(f, "{unary_logic}"),
        }
    }
}

#[cfg(test)]
mod test_instruction_display {
    use crate::instruction::PragmaArgument;

    use super::{Instruction, Pragma};

    #[test]
    fn pragma() {
        assert_eq!(
            Instruction::Pragma(Pragma {
                name: String::from("INITIAL_REWIRING"),
                arguments: vec![],
                data: Some(String::from("PARTIAL")),
            })
            .to_string(),
            "PRAGMA INITIAL_REWIRING \"PARTIAL\""
        );
        assert_eq!(
            Instruction::Pragma(Pragma {
                name: String::from("LOAD-MEMORY"),
                arguments: vec![PragmaArgument::Identifier("q0".to_string())],
                data: Some(String::from("addr")),
            })
            .to_string(),
            "PRAGMA LOAD-MEMORY q0 \"addr\""
        );
        assert_eq!(
            Instruction::Pragma(Pragma {
                name: String::from("PRESERVE_BLOCK"),
                arguments: vec![],
                data: None,
            })
            .to_string(),
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
    /// use quil_rs::{expression::Expression, Program};
    ///
    ///
    /// let program = Program::from_str("SHIFT-PHASE 0 \"rf\" 2*2").unwrap();
    /// let mut instructions = program.to_instructions();
    /// instructions.iter_mut().for_each(|inst| inst.apply_to_expressions(Expression::simplify));
    ///
    /// assert_eq!(instructions[0].to_string(), String::from("SHIFT-PHASE 0 \"rf\" 4"))
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
        include_blocked: bool,
        qubits_available: HashSet<&'a Qubit>,
    ) -> Option<FrameMatchCondition<'a>> {
        match self {
            Instruction::Pulse(Pulse {
                blocking, frame, ..
            })
            | Instruction::Capture(Capture {
                blocking, frame, ..
            })
            | Instruction::RawCapture(RawCapture {
                blocking, frame, ..
            }) => Some(if *blocking && include_blocked {
                FrameMatchCondition::AnyOfQubits(frame.qubits.iter().collect())
            } else {
                FrameMatchCondition::Specific(frame)
            }),
            Instruction::Delay(Delay {
                frame_names,
                qubits,
                ..
            }) => Some(if frame_names.is_empty() {
                FrameMatchCondition::ExactQubits(qubits.iter().collect())
            } else {
                FrameMatchCondition::And(vec![
                    FrameMatchCondition::ExactQubits(qubits.iter().collect()),
                    FrameMatchCondition::AnyOfNames(
                        frame_names.iter().map(String::as_str).collect(),
                    ),
                ])
            }),
            Instruction::Fence(Fence { qubits }) => {
                if include_blocked {
                    Some(if qubits.is_empty() {
                        FrameMatchCondition::All
                    } else {
                        FrameMatchCondition::AnyOfQubits(qubits.iter().collect())
                    })
                } else {
                    None
                }
            }
            Instruction::Reset(Reset { qubit }) => {
                let qubits = match qubit {
                    Some(qubit) => {
                        let mut set = HashSet::new();
                        set.insert(qubit);
                        set
                    }
                    None => qubits_available,
                };

                if include_blocked {
                    Some(FrameMatchCondition::AnyOfQubits(qubits))
                } else {
                    Some(FrameMatchCondition::ExactQubits(qubits))
                }
            }
            Instruction::SetFrequency(SetFrequency { frame, .. })
            | Instruction::SetPhase(SetPhase { frame, .. })
            | Instruction::SetScale(SetScale { frame, .. })
            | Instruction::ShiftFrequency(ShiftFrequency { frame, .. })
            | Instruction::ShiftPhase(ShiftPhase { frame, .. }) => {
                Some(FrameMatchCondition::Specific(frame))
            }
            Instruction::SwapPhases(SwapPhases { frame_1, frame_2 }) => {
                Some(FrameMatchCondition::Or(vec![
                    FrameMatchCondition::Specific(frame_1),
                    FrameMatchCondition::Specific(frame_2),
                ]))
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
        for instruction in program.body_instructions_mut() {
            instruction.apply_to_expressions(closure);
        }

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
}
