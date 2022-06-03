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

use serde::{Deserialize, Serialize};
use std::{collections::HashMap, fmt};

use crate::expression::Expression;
use crate::program::frame::FrameMatchCondition;

#[cfg(test)]
use proptest_derive::Arbitrary;

#[derive(Clone, Debug, PartialEq)]
pub enum ArithmeticOperand {
    LiteralInteger(i64),
    LiteralReal(f64),
    MemoryReference(MemoryReference),
}

impl fmt::Display for ArithmeticOperand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ArithmeticOperand::LiteralInteger(value) => write!(f, "{}", value),
            ArithmeticOperand::LiteralReal(value) => write!(f, "{}", value),
            ArithmeticOperand::MemoryReference(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ArithmeticOperator {
    Add,
    Subtract,
    Divide,
    Multiply,
}

impl fmt::Display for ArithmeticOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ArithmeticOperator::Add => write!(f, "ADD"),
            ArithmeticOperator::Divide => write!(f, "DIV"),
            ArithmeticOperator::Multiply => write!(f, "MUL"),
            ArithmeticOperator::Subtract => write!(f, "SUB"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOperand {
    LiteralInteger(i64),
    MemoryReference(MemoryReference),
}

impl fmt::Display for BinaryOperand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            BinaryOperand::LiteralInteger(value) => write!(f, "{}", value),
            BinaryOperand::MemoryReference(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOperator {
    And,
    Ior,
    Xor,
}
impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            BinaryOperator::And => write!(f, "AND"),
            BinaryOperator::Ior => write!(f, "IOR"),
            BinaryOperator::Xor => write!(f, "XOR"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOperator {
    Neg,
    Not,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            UnaryOperator::Neg => write!(f, "NEG"),
            UnaryOperator::Not => write!(f, "NOT"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TernaryOperand {
    LiteralInteger(i64),
    LiteralReal(f64),
    MemoryReference(MemoryReference),
}

impl fmt::Display for TernaryOperand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            TernaryOperand::LiteralInteger(value) => write!(f, "{}", value),
            TernaryOperand::LiteralReal(value) => write!(f, "{}", value),
            TernaryOperand::MemoryReference(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ComparisonOperator {
    Equal,
    GreaterThanOrEqual,
    GreaterThan,
    LessThanOrEqual,
    LessThan,
}

impl fmt::Display for ComparisonOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ComparisonOperator::Equal => write!(f, "EQ"),
            ComparisonOperator::GreaterThanOrEqual => write!(f, "GE"),
            ComparisonOperator::GreaterThan => write!(f, "GT"),
            ComparisonOperator::LessThanOrEqual => write!(f, "LE"),
            ComparisonOperator::LessThan => write!(f, "LT"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum AttributeValue {
    String(String),
    Expression(Expression),
}

impl fmt::Display for AttributeValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use AttributeValue::*;
        match self {
            String(value) => write!(f, "\"{}\"", value),
            Expression(value) => write!(f, "{}", value),
        }
    }
}

pub type FrameAttributes = HashMap<String, AttributeValue>;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Calibration {
    pub instructions: Vec<Instruction>,
    pub modifiers: Vec<GateModifier>,
    pub name: String,
    pub parameters: Vec<Expression>,
    pub qubits: Vec<Qubit>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct FrameIdentifier {
    pub name: String,
    pub qubits: Vec<Qubit>,
}

impl fmt::Display for FrameIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} \"{}\"", format_qubits(&self.qubits), self.name)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum GateModifier {
    Controlled,
    Dagger,
    Forked,
}

impl fmt::Display for GateModifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use GateModifier::*;
        write!(
            f,
            "{}",
            match self {
                Controlled => "CONTROLLED",
                Dagger => "DAGGER",
                Forked => "FORKED",
            }
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum GateType {
    Matrix,
    Permutation,
}

impl fmt::Display for GateType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use GateType::*;
        write!(
            f,
            "{}",
            match self {
                Matrix => "MATRIX",
                Permutation => "PERMUTATION",
            }
        )
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum ScalarType {
    Bit,
    Integer,
    Octet,
    Real,
}

impl fmt::Display for ScalarType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ScalarType::*;
        write!(
            f,
            "{}",
            match self {
                Bit => "BIT",
                Integer => "INTEGER",
                Octet => "OCTET",
                Real => "REAL",
            }
        )
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct Vector {
    pub data_type: ScalarType,
    pub length: u64,
}

impl fmt::Display for Vector {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}[{}]", self.data_type, self.length)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct WaveformInvocation {
    pub name: String,
    pub parameters: HashMap<String, Expression>,
}

impl fmt::Display for WaveformInvocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut key_value_pairs = self
            .parameters
            .iter()
            .collect::<Vec<(&String, &Expression)>>();

        key_value_pairs.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));

        write!(
            f,
            "{}({})",
            self.name,
            key_value_pairs
                .iter()
                .map(|(k, v)| format!("{}: {}", k, v))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct MemoryReference {
    pub name: String,
    pub index: u64,
}

impl Eq for MemoryReference {}

impl fmt::Display for MemoryReference {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}[{}]", self.name, self.index)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Gate {
    pub name: String,
    pub parameters: Vec<Expression>,
    pub qubits: Vec<Qubit>,
    pub modifiers: Vec<GateModifier>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CircuitDefinition {
    pub name: String,
    pub parameters: Vec<String>,
    // These cannot be fixed qubits and thus are not typed as `Qubit`
    pub qubit_variables: Vec<String>,
    pub instructions: Vec<Instruction>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct GateDefinition {
    pub name: String,
    pub parameters: Vec<String>,
    pub matrix: Vec<Vec<Expression>>,
    pub r#type: GateType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Declaration {
    pub name: String,
    pub size: Vector,
    pub sharing: Option<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Measurement {
    pub qubit: Qubit,
    pub target: Option<MemoryReference>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Reset {
    pub qubit: Option<Qubit>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Capture {
    pub blocking: bool,
    pub frame: FrameIdentifier,
    pub memory_reference: MemoryReference,
    pub waveform: WaveformInvocation,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Delay {
    pub duration: Expression,
    pub frame_names: Vec<String>,
    pub qubits: Vec<Qubit>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Fence {
    pub qubits: Vec<Qubit>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FrameDefinition {
    pub identifier: FrameIdentifier,
    pub attributes: HashMap<String, AttributeValue>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MeasureCalibrationDefinition {
    pub qubit: Option<Qubit>,
    pub parameter: String,
    pub instructions: Vec<Instruction>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Pragma {
    pub name: String,
    pub arguments: Vec<String>,
    pub data: Option<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Pulse {
    pub blocking: bool,
    pub frame: FrameIdentifier,
    pub waveform: WaveformInvocation,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RawCapture {
    pub blocking: bool,
    pub frame: FrameIdentifier,
    pub duration: Expression,
    pub memory_reference: MemoryReference,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SetFrequency {
    pub frame: FrameIdentifier,
    pub frequency: Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SetPhase {
    pub frame: FrameIdentifier,
    pub phase: Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SetScale {
    pub frame: FrameIdentifier,
    pub scale: Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ShiftFrequency {
    pub frame: FrameIdentifier,
    pub frequency: Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ShiftPhase {
    pub frame: FrameIdentifier,
    pub phase: Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SwapPhases {
    pub frame_1: FrameIdentifier,
    pub frame_2: FrameIdentifier,
}

#[derive(Clone, Debug, PartialEq)]
pub struct WaveformDefinition {
    pub name: String,
    pub definition: Waveform,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Arithmetic {
    pub operator: ArithmeticOperator,
    pub destination: ArithmeticOperand,
    pub source: ArithmeticOperand,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Comparison {
    pub operator: ComparisonOperator,
    pub operands: (MemoryReference, MemoryReference, TernaryOperand),
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinaryLogic {
    pub operator: BinaryOperator,
    pub operands: (MemoryReference, BinaryOperand),
}

#[derive(Clone, Debug, PartialEq)]
pub struct UnaryLogic {
    pub operator: UnaryOperator,
    pub operand: MemoryReference,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Label(pub String);

#[derive(Clone, Debug, PartialEq)]
pub struct Move {
    pub destination: ArithmeticOperand,
    pub source: ArithmeticOperand,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Exchange {
    pub left: ArithmeticOperand,
    pub right: ArithmeticOperand,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Load {
    pub destination: MemoryReference,
    pub source: String,
    pub offset: MemoryReference,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Store {
    pub destination: String,
    pub offset: MemoryReference,
    pub source: ArithmeticOperand,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Jump {
    pub target: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct JumpWhen {
    pub target: String,
    pub condition: MemoryReference,
}

#[derive(Clone, Debug, PartialEq)]
pub struct JumpUnless {
    pub target: String,
    pub condition: MemoryReference,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    Gate(Gate),
    CircuitDefinition(CircuitDefinition),
    GateDefinition(GateDefinition),
    Declaration(Declaration),
    Measurement(Measurement),
    Reset(Reset),
    CalibrationDefinition(Calibration),
    Capture(Capture),
    Delay(Delay),
    Fence(Fence),
    FrameDefinition(FrameDefinition),
    MeasureCalibrationDefinition(MeasureCalibrationDefinition),
    Pragma(Pragma),
    Pulse(Pulse),
    RawCapture(RawCapture),
    SetFrequency(SetFrequency),
    SetPhase(SetPhase),
    SetScale(SetScale),
    ShiftFrequency(ShiftFrequency),
    ShiftPhase(ShiftPhase),
    SwapPhases(SwapPhases),
    WaveformDefinition(WaveformDefinition),
    Arithmetic(Arithmetic),
    Comparison(Comparison),
    BinaryLogic(BinaryLogic),
    UnaryLogic(UnaryLogic),
    Halt,
    Label(Label),
    Move(Move),
    Exchange(Exchange),
    Load(Load),
    Store(Store),
    Jump(Jump),
    JumpWhen(JumpWhen),
    JumpUnless(JumpUnless),
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
            | Instruction::BinaryLogic(_)
            | Instruction::UnaryLogic(_)
            | Instruction::Move(_)
            | Instruction::Exchange(_)
            | Instruction::Load(_)
            | Instruction::Pragma(_)
            | Instruction::Store(_) => InstructionRole::ClassicalCompute,
            Instruction::Halt
            | Instruction::Jump(_)
            | Instruction::JumpWhen(_)
            | Instruction::JumpUnless(_) => InstructionRole::ControlFlow,
        }
    }
}

pub fn format_instructions(values: &[Instruction]) -> String {
    values
        .iter()
        .map(|i| format!("{}", i))
        .collect::<Vec<String>>()
        .join("\n\t")
}

pub fn format_integer_vector(values: &[u64]) -> String {
    values
        .iter()
        .map(|q| format!("{}", q))
        .collect::<Vec<String>>()
        .join(" ")
}

pub fn format_matrix(matrix: &[Vec<Expression>]) -> String {
    matrix
        .iter()
        .map(|row| {
            row.iter()
                .map(|cell| format!("{}", cell))
                .collect::<Vec<String>>()
                .join(", ")
        })
        .collect::<Vec<String>>()
        .join("\n\t")
}

pub fn format_qubits(qubits: &[Qubit]) -> String {
    qubits
        .iter()
        .map(|q| format!("{}", q))
        .collect::<Vec<String>>()
        .join(" ")
}

pub fn get_expression_parameter_string(parameters: &[Expression]) -> String {
    if parameters.is_empty() {
        return String::from("");
    }

    let parameter_str: String = parameters.iter().map(|e| format!("{}", e)).collect();
    format!("({})", parameter_str)
}

pub fn get_string_parameter_string(parameters: &[String]) -> String {
    if parameters.is_empty() {
        return String::from("");
    }

    let parameter_str: String = parameters.join(",");
    format!("({})", parameter_str)
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Arithmetic(Arithmetic {
                operator,
                destination,
                source,
            }) => write!(f, "{} {} {}", operator, destination, source),
            Instruction::CalibrationDefinition(calibration) => {
                let parameter_str = get_expression_parameter_string(&calibration.parameters);
                write!(
                    f,
                    "DEFCAL {}{} {}:",
                    calibration.name,
                    parameter_str,
                    format_qubits(&calibration.qubits)
                )?;
                for instruction in &calibration.instructions {
                    write!(f, "\n\t{}", instruction)?;
                }
                Ok(())
            }
            Instruction::Capture(Capture {
                blocking,
                frame,
                waveform,
                memory_reference,
            }) => {
                if !blocking {
                    write!(f, "NONBLOCKING ")?;
                }
                write!(f, "CAPTURE {} {} {}", frame, waveform, memory_reference)
            }
            Instruction::CircuitDefinition(CircuitDefinition {
                name,
                parameters,
                qubit_variables,
                instructions,
            }) => {
                let mut parameter_str: String = parameters
                    .iter()
                    .map(|p| format!("%{}", p))
                    .collect::<Vec<String>>()
                    .join(", ");
                if !parameter_str.is_empty() {
                    parameter_str = format!("({})", parameter_str);
                }
                write!(f, "DEFCIRCUIT {}{}", name, parameter_str)?;
                for qubit_variable in qubit_variables {
                    write!(f, " {}", qubit_variable)?;
                }
                writeln!(f, ":")?;
                for instruction in &**instructions {
                    writeln!(f, "\t{}", instruction)?;
                }
                Ok(())
            }
            Instruction::Declaration(Declaration {
                name,
                size,
                sharing,
            }) => {
                write!(f, "DECLARE {} {}", name, size)?;
                match sharing {
                    Some(shared) => write!(f, "SHARING {}", shared)?,
                    None => {}
                }
                Ok(())
            }
            Instruction::Delay(Delay {
                qubits,
                frame_names,
                duration,
            }) => {
                write!(f, "DELAY {}", format_qubits(qubits))?;
                for frame_name in frame_names {
                    write!(f, " \"{}\"", frame_name)?;
                }
                write!(f, " {}", duration)
            }
            Instruction::Fence(Fence { qubits }) => {
                if qubits.is_empty() {
                    write!(f, "FENCE")
                } else {
                    write!(f, "FENCE {}", format_qubits(qubits))
                }
            }
            Instruction::FrameDefinition(FrameDefinition {
                identifier,
                attributes,
            }) => write!(
                f,
                "DEFFRAME {}:{}",
                identifier,
                attributes
                    .iter()
                    .map(|(k, v)| format!("\n\t{}: {}", k, v))
                    .collect::<String>()
            ),
            Instruction::Gate(Gate {
                name,
                parameters,
                qubits,
                modifiers,
            }) => {
                let parameter_str = get_expression_parameter_string(parameters);

                let qubit_str = format_qubits(qubits);
                let modifier_str = modifiers
                    .iter()
                    .map(|m| format!("{} ", m))
                    .collect::<Vec<String>>()
                    .join("");
                write!(f, "{}{}{} {}", modifier_str, name, parameter_str, qubit_str)
            }
            Instruction::GateDefinition(GateDefinition {
                name,
                parameters,
                matrix,
                r#type,
            }) => {
                let parameter_str: String = parameters.iter().map(|p| p.to_string()).collect();
                writeln!(f, "DEFGATE {}{} AS {}:", name, parameter_str, r#type)?;
                for row in matrix {
                    writeln!(
                        f,
                        "\t{}",
                        row.iter()
                            .map(|cell| format!("{}", cell))
                            .collect::<Vec<String>>()
                            .join(",")
                    )?;
                }
                Ok(())
            }
            Instruction::MeasureCalibrationDefinition(MeasureCalibrationDefinition {
                qubit,
                parameter,
                instructions,
            }) => {
                write!(f, "DEFCAL MEASURE")?;
                match qubit {
                    Some(qubit) => {
                        write!(f, " {}", qubit)?;
                    }
                    None => {}
                }

                writeln!(
                    f,
                    " %{}:\n\t{}",
                    parameter,
                    format_instructions(instructions)
                )
            }
            Instruction::Measurement(Measurement { qubit, target }) => match target {
                Some(reference) => write!(f, "MEASURE {} {}", qubit, reference),
                None => write!(f, "MEASURE {}", qubit),
            },
            Instruction::Move(Move {
                destination,
                source,
            }) => write!(f, "MOVE {} {}", destination, source),
            Instruction::Exchange(Exchange { left, right }) => {
                write!(f, "EXCHANGE {} {}", left, right)
            }
            Instruction::Load(Load {
                destination,
                source,
                offset,
            }) => {
                write!(f, "LOAD {} {} {}", destination, source, offset)
            }
            Instruction::Store(Store {
                destination,
                offset,
                source,
            }) => {
                write!(f, "STORE {} {} {}", destination, offset, source)
            }
            Instruction::Pulse(Pulse {
                blocking,
                frame,
                waveform,
            }) => {
                if !blocking {
                    write!(f, "NONBLOCKING ")?
                }
                write!(f, "PULSE {} {}", frame, waveform)
            }
            Instruction::Pragma(Pragma {
                name,
                arguments,
                data,
            }) => {
                write!(f, "PRAGMA {}", name)?;
                if !arguments.is_empty() {
                    write!(f, " {}", arguments.join(" "))?;
                }
                if let Some(data) = data {
                    write!(f, " \"{}\"", data)?;
                }
                Ok(())
            }
            Instruction::RawCapture(RawCapture {
                blocking,
                frame,
                duration,
                memory_reference,
            }) => {
                if !blocking {
                    write!(f, "NONBLOCKING ")?
                }
                write!(f, "RAW-CAPTURE {} {} {}", frame, duration, memory_reference)
            }
            Instruction::Reset(Reset { qubit }) => match qubit {
                Some(qubit) => write!(f, "RESET {}", qubit),
                None => write!(f, "RESET"),
            },
            Instruction::SetFrequency(SetFrequency { frame, frequency }) => {
                write!(f, "SET-FREQUENCY {} {}", frame, frequency)
            }
            Instruction::SetPhase(SetPhase { frame, phase }) => {
                write!(f, "SET-PHASE {} {}", frame, phase)
            }
            Instruction::SetScale(SetScale { frame, scale }) => {
                write!(f, "SET-SCALE {} {}", frame, scale)
            }
            Instruction::ShiftFrequency(ShiftFrequency { frame, frequency }) => {
                write!(f, "SHIFT-FREQUENCY {} {}", frame, frequency)
            }
            Instruction::ShiftPhase(ShiftPhase { frame, phase }) => {
                write!(f, "SHIFT-PHASE {} {}", frame, phase)
            }
            Instruction::SwapPhases(SwapPhases { frame_1, frame_2 }) => {
                write!(f, "SWAP-PHASES {} {}", frame_1, frame_2)
            }
            Instruction::WaveformDefinition(WaveformDefinition { name, definition }) => write!(
                f,
                "DEFWAVEFORM {}{}:\n\t{}",
                name,
                get_string_parameter_string(&definition.parameters),
                definition
                    .matrix
                    .iter()
                    .map(|e| format!("{}", e))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Instruction::Halt => write!(f, "HALT"),
            Instruction::Jump(Jump { target }) => write!(f, "JUMP @{}", target),
            Instruction::JumpUnless(JumpUnless { condition, target }) => {
                write!(f, "JUMP-UNLESS @{} {}", target, condition)
            }
            Instruction::JumpWhen(JumpWhen { condition, target }) => {
                write!(f, "JUMP-WHEN @{} {}", target, condition)
            }
            Instruction::Label(Label(label)) => write!(f, "LABEL @{}", label),
            Instruction::Comparison(Comparison { operator, operands }) => {
                write!(
                    f,
                    "{} {} {} {}",
                    operator, operands.0, operands.1, operands.2
                )
            }
            Instruction::BinaryLogic(BinaryLogic { operator, operands }) => {
                write!(f, "{} {} {}", operator, operands.0, operands.1)
            }
            Instruction::UnaryLogic(UnaryLogic { operator, operand }) => {
                write!(f, "{} {}", operator, operand)
            }
        }
    }
}

#[cfg(test)]
mod test_instruction_display {
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
                arguments: vec![String::from("q0")],
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

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Qubit {
    Fixed(u64),
    Variable(String),
}

impl fmt::Display for Qubit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Qubit::*;
        match self {
            Fixed(value) => write!(f, "{}", value),
            Variable(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Waveform {
    pub matrix: Vec<Expression>,
    pub parameters: Vec<String>,
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
    /// let mut instructions = program.to_instructions(true);
    /// instructions.iter_mut().for_each(|inst| inst.apply_to_expressions(Expression::simplify));
    ///
    /// assert_eq!(instructions[0].to_string(), String::from("SHIFT-PHASE 0 \"rf\" 4.0"))
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
            Instruction::GateDefinition(GateDefinition { matrix, .. }) => {
                for row in matrix {
                    for cell in row {
                        closure(cell);
                    }
                }
            }
            _ => {}
        }
    }

    pub(crate) fn get_frame_match_condition(
        &self,
        include_blocked: bool,
    ) -> Option<FrameMatchCondition> {
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
                FrameMatchCondition::AnyOfQubits(&frame.qubits)
            } else {
                FrameMatchCondition::Specific(frame)
            }),
            Instruction::Delay(Delay {
                frame_names,
                qubits,
                ..
            }) => Some(if frame_names.is_empty() {
                FrameMatchCondition::ExactQubits(qubits)
            } else {
                FrameMatchCondition::And(vec![
                    FrameMatchCondition::ExactQubits(qubits),
                    FrameMatchCondition::AnyOfNames(frame_names),
                ])
            }),
            Instruction::Fence(Fence { qubits }) => Some(if qubits.is_empty() {
                FrameMatchCondition::All
            } else {
                FrameMatchCondition::AnyOfQubits(qubits)
            }),
            Instruction::SetFrequency(SetFrequency { frame, .. })
            | Instruction::SetPhase(SetPhase { frame, .. })
            | Instruction::SetScale(SetScale { frame, .. })
            | Instruction::ShiftFrequency(ShiftFrequency { frame, .. })
            | Instruction::ShiftPhase(ShiftPhase { frame, .. }) => {
                Some(FrameMatchCondition::Specific(frame))
            }
            Instruction::SwapPhases(SwapPhases { frame_1, frame_2 }) => {
                Some(FrameMatchCondition::And(vec![
                    FrameMatchCondition::Specific(frame_1),
                    FrameMatchCondition::Specific(frame_2),
                ]))
            }
            Instruction::Gate(_)
            | Instruction::CircuitDefinition(_)
            | Instruction::GateDefinition(_)
            | Instruction::Declaration(_)
            | Instruction::Measurement(_)
            | Instruction::Reset(_)
            | Instruction::CalibrationDefinition(_)
            | Instruction::FrameDefinition(_)
            | Instruction::MeasureCalibrationDefinition(_)
            | Instruction::Pragma(_)
            | Instruction::WaveformDefinition(_)
            | Instruction::Arithmetic(_)
            | Instruction::BinaryLogic(_)
            | Instruction::UnaryLogic(_)
            | Instruction::Comparison(_)
            | Instruction::Halt
            | Instruction::Label(_)
            | Instruction::Move(_)
            | Instruction::Exchange(_)
            | Instruction::Load(_)
            | Instruction::Store(_)
            | Instruction::Jump(_)
            | Instruction::JumpWhen(_)
            | Instruction::JumpUnless(_) => None,
        }
    }

    #[cfg(test)]
    /// Parse a single instruction from an input string. Returns an error if the input fails to parse,
    /// or if there is input left over after parsing.
    pub(crate) fn parse(input: &str) -> Result<Self, String> {
        use crate::parser::{instruction::parse_instruction, lex};

        let lexed = lex(input)?;
        let (_, instruction) =
            nom::combinator::all_consuming(parse_instruction)(&lexed).map_err(|e| e.to_string())?;
        Ok(instruction)
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use crate::{expression::Expression, Program};

    #[test]
    fn apply_to_expressions() {
        let mut program = Program::from_str(
            "DECLARE ro BIT
SET-PHASE 0 \"rf\" pi/2
RX(2) 0",
        )
        .unwrap();
        let closure = |expr: &mut Expression| *expr = Expression::Variable(String::from("a"));
        for instruction in program.instructions.iter_mut() {
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
}
