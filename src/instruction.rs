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

use nom_locate::LocatedSpan;
use std::borrow::Cow;
use std::collections::HashMap;
use std::collections::HashSet;
use std::str::FromStr;
use std::sync::Arc;

use crate::expression::Expression;
use crate::parser::{common::parse_memory_reference, lex, ParseError};
use crate::program::{disallow_leftover, frame::FrameMatchCondition, SyntaxError};
use crate::quil::{Quil, ToQuilError};

#[cfg(test)]
use proptest_derive::Arbitrary;

#[derive(Clone, Debug, PartialEq)]
pub enum ArithmeticOperand {
    LiteralInteger(i64),
    LiteralReal(f64),
    MemoryReference(MemoryReference),
}

impl Quil for ArithmeticOperand {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        match &self {
            ArithmeticOperand::LiteralInteger(value) => {
                write!(writer, "{}", value).map_err(Into::into)
            }
            ArithmeticOperand::LiteralReal(value) => {
                write!(writer, "{}", value).map_err(Into::into)
            }
            ArithmeticOperand::MemoryReference(value) => value.write(writer),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ArithmeticOperator {
    Add,
    Subtract,
    Divide,
    Multiply,
}

impl Quil for ArithmeticOperator {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        match &self {
            ArithmeticOperator::Add => write!(writer, "ADD"),
            ArithmeticOperator::Divide => write!(writer, "DIV"),
            ArithmeticOperator::Multiply => write!(writer, "MUL"),
            ArithmeticOperator::Subtract => write!(writer, "SUB"),
        }
        .map_err(Into::into)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BinaryOperand {
    LiteralInteger(i64),
    MemoryReference(MemoryReference),
}

impl Quil for BinaryOperand {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        match &self {
            BinaryOperand::LiteralInteger(value) => write!(writer, "{value}").map_err(Into::into),
            BinaryOperand::MemoryReference(value) => value.write(writer),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BinaryOperator {
    And,
    Ior,
    Xor,
}
impl Quil for BinaryOperator {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        match &self {
            BinaryOperator::And => write!(writer, "AND"),
            BinaryOperator::Ior => write!(writer, "IOR"),
            BinaryOperator::Xor => write!(writer, "XOR"),
        }
        .map_err(Into::into)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryOperator {
    Neg,
    Not,
}

impl Quil for UnaryOperator {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        match &self {
            UnaryOperator::Neg => write!(writer, "NEG"),
            UnaryOperator::Not => write!(writer, "NOT"),
        }
        .map_err(Into::into)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ComparisonOperand {
    LiteralInteger(i64),
    LiteralReal(f64),
    MemoryReference(MemoryReference),
}

impl Quil for ComparisonOperand {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        match &self {
            ComparisonOperand::LiteralInteger(value) => {
                write!(writer, "{value}").map_err(Into::into)
            }
            ComparisonOperand::LiteralReal(value) => write!(writer, "{value}").map_err(Into::into),
            ComparisonOperand::MemoryReference(value) => value.write(writer),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ComparisonOperator {
    Equal,
    GreaterThanOrEqual,
    GreaterThan,
    LessThanOrEqual,
    LessThan,
}

impl Quil for ComparisonOperator {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        match &self {
            ComparisonOperator::Equal => write!(writer, "EQ"),
            ComparisonOperator::GreaterThanOrEqual => write!(writer, "GE"),
            ComparisonOperator::GreaterThan => write!(writer, "GT"),
            ComparisonOperator::LessThanOrEqual => write!(writer, "LE"),
            ComparisonOperator::LessThan => write!(writer, "LT"),
        }
        .map_err(Into::into)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AttributeValue {
    String(String),
    Expression(Expression),
}

impl Quil for AttributeValue {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        use AttributeValue::*;
        match self {
            String(value) => write!(writer, "{value:?}"),
            Expression(value) => write!(writer, "{value}"),
        }
        .map_err(Into::into)
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Convert {
    pub from: MemoryReference,
    pub to: MemoryReference,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct FrameIdentifier {
    pub name: String,
    pub qubits: Vec<Qubit>,
}

impl Quil for FrameIdentifier {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        for qubit in &self.qubits {
            qubit.write(writer)?;
            write!(writer, " ")?;
        }
        write!(writer, "{:?}", self.name).map_err(Into::into)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Include {
    pub filename: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GateModifier {
    Controlled,
    Dagger,
    Forked,
}

impl Quil for GateModifier {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        match self {
            Self::Controlled => write!(writer, "CONTROLLED"),
            Self::Dagger => write!(writer, "DAGGER"),
            Self::Forked => write!(writer, "FORKED"),
        }
        .map_err(Into::into)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GateType {
    Matrix,
    Permutation,
}

impl Quil for GateType {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        match self {
            Self::Matrix => write!(writer, "MATRIX"),
            Self::Permutation => write!(writer, "PERMUTATION"),
        }
        .map_err(Into::into)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ScalarType {
    Bit,
    Integer,
    Octet,
    Real,
}

impl Quil for ScalarType {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        match self {
            Self::Bit => write!(writer, "BIT"),
            Self::Integer => write!(writer, "INTEGER"),
            Self::Octet => write!(writer, "OCTET"),
            Self::Real => write!(writer, "REAL"),
        }
        .map_err(Into::into)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Vector {
    pub data_type: ScalarType,
    pub length: u64,
}

impl Quil for Vector {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        self.data_type.write(writer)?;
        write!(writer, "[{}]", self.length).map_err(Into::into)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WaveformInvocation {
    pub name: String,
    pub parameters: HashMap<String, Expression>,
}

impl Quil for WaveformInvocation {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        let mut key_value_pairs = self
            .parameters
            .iter()
            .collect::<Vec<(&String, &Expression)>>();

        key_value_pairs.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));

        if key_value_pairs.is_empty() {
            write!(writer, "{}", self.name,)
        } else {
            write!(
                writer,
                "{}({})",
                self.name,
                key_value_pairs
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        }
        .map_err(Into::into)
    }
}

#[cfg(test)]
mod waveform_invocation_tests {
    use std::collections::HashMap;

    use crate::{instruction::WaveformInvocation, quil::Quil};

    #[test]
    fn format_no_parameters() {
        let wfi = WaveformInvocation {
            name: "CZ".into(),
            parameters: HashMap::new(),
        };
        assert_eq!(wfi.to_quil().unwrap(), "CZ".to_string());
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct MemoryReference {
    pub name: String,
    pub index: u64,
}

impl Eq for MemoryReference {}

impl Quil for MemoryReference {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        write!(writer, "{}[{}]", self.name, self.index).map_err(Into::into)
    }
}

impl std::fmt::Display for MemoryReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}[{}]", self.name, self.index)
    }
}

impl FromStr for MemoryReference {
    type Err = SyntaxError<Self>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let input = LocatedSpan::new(s);
        let tokens = lex(input)?;
        disallow_leftover(
            parse_memory_reference(&tokens).map_err(ParseError::from_nom_internal_err),
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GateSpecification {
    Matrix(Vec<Vec<Expression>>),
    Permutation(Vec<u64>),
}

impl Quil for GateSpecification {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        match self {
            GateSpecification::Matrix(matrix) => matrix.iter().try_for_each(|row| {
                write!(
                    writer,
                    "\n\t{}",
                    row.iter()
                        .map(|cell| format!("{}", cell))
                        .collect::<Vec<String>>()
                        .join(",")
                )
            }),
            GateSpecification::Permutation(permutation) => {
                write!(
                    writer,
                    "\t{}",
                    permutation
                        .iter()
                        .map(|i| format!("{}", i))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        }
        .map_err(Into::into)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GateDefinition {
    pub name: String,
    pub parameters: Vec<String>,
    pub specification: GateSpecification,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Declaration {
    pub name: String,
    pub size: Vector,
    pub sharing: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Measurement {
    pub qubit: Qubit,
    pub target: Option<MemoryReference>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Reset {
    pub qubit: Option<Qubit>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Capture {
    pub blocking: bool,
    pub frame: FrameIdentifier,
    pub memory_reference: MemoryReference,
    pub waveform: WaveformInvocation,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Delay {
    pub duration: Expression,
    pub frame_names: Vec<String>,
    pub qubits: Vec<Qubit>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Fence {
    pub qubits: Vec<Qubit>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Pragma {
    pub name: String,
    pub arguments: Vec<PragmaArgument>,
    pub data: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PragmaArgument {
    Identifier(String),
    Integer(u64),
}
impl Quil for PragmaArgument {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        match self {
            PragmaArgument::Identifier(i) => write!(writer, "{i}"),
            PragmaArgument::Integer(i) => write!(writer, "{i}"),
        }
        .map_err(Into::into)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Pulse {
    pub blocking: bool,
    pub frame: FrameIdentifier,
    pub waveform: WaveformInvocation,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RawCapture {
    pub blocking: bool,
    pub frame: FrameIdentifier,
    pub duration: Expression,
    pub memory_reference: MemoryReference,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SetFrequency {
    pub frame: FrameIdentifier,
    pub frequency: Expression,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SetPhase {
    pub frame: FrameIdentifier,
    pub phase: Expression,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SetScale {
    pub frame: FrameIdentifier,
    pub scale: Expression,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ShiftFrequency {
    pub frame: FrameIdentifier,
    pub frequency: Expression,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ShiftPhase {
    pub frame: FrameIdentifier,
    pub phase: Expression,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SwapPhases {
    pub frame_1: FrameIdentifier,
    pub frame_2: FrameIdentifier,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
    pub operands: (MemoryReference, MemoryReference, ComparisonOperand),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BinaryLogic {
    pub operator: BinaryOperator,
    pub operands: (MemoryReference, BinaryOperand),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UnaryLogic {
    pub operator: UnaryOperator,
    pub operand: MemoryReference,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Label {
    Fixed(String),
    Placeholder(LabelPlaceholder),
}

impl Label {
    fn resolve_placeholder<R>(&mut self, resolver: R)
    where
        R: Fn(&LabelPlaceholder) -> Option<String>,
    {
        if let Label::Placeholder(placeholder) = self {
            if let Some(resolved) = resolver(placeholder) {
                *self = Label::Fixed(resolved);
            }
        }
    }
}

type LabelPlaceholderInner = Arc<String>;

/// An opaque placeholder for a qubit whose index may be assigned
/// at a later time.
#[derive(Clone, Debug, Eq)]
pub struct LabelPlaceholder(LabelPlaceholderInner);

impl LabelPlaceholder {
    pub fn new(base_label: String) -> Self {
        Self(Arc::new(base_label))
    }

    pub fn as_inner(&self) -> &String {
        &self.0
    }

    fn address(&self) -> usize {
        &*self.0 as *const _ as usize
    }
}

impl std::hash::Hash for LabelPlaceholder {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.address().hash(state);
    }
}

impl PartialOrd for LabelPlaceholder {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.address().partial_cmp(&other.address())
    }
}

impl Ord for LabelPlaceholder {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.address().cmp(&other.address())
    }
}

impl PartialEq for LabelPlaceholder {
    #[allow(clippy::ptr_eq)]
    fn eq(&self, other: &Self) -> bool {
        &*self.0 as *const _ == &*other.0 as *const _
    }
}

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

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Jump {
    pub target: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct JumpWhen {
    pub target: String,
    pub condition: MemoryReference,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
            | Instruction::JumpUnless(_) => InstructionRole::ControlFlow,
        }
    }
}

pub fn write_join_quil<I, T>(
    writer: &mut impl std::fmt::Write,
    values: I,
    joiner: &str,
) -> Result<(), ToQuilError>
where
    I: IntoIterator<Item = T>,
    T: Quil,
{
    let mut iter = values.into_iter();
    if let Some(first) = iter.next() {
        first.write(writer)?;

        for value in iter {
            write!(writer, "{}", joiner)?;
            value.write(writer)?;
        }
    }
    Ok(())
}

pub fn write_expression_parameter_string(
    writer: &mut impl std::fmt::Write,
    parameters: &[Expression],
) -> Result<(), ToQuilError> {
    if !parameters.is_empty() {
        let parameter_str: String = parameters
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        write!(writer, "({})", parameter_str)?;
    }
    Ok(())
}

pub fn get_string_parameter_string(parameters: &[String]) -> String {
    if parameters.is_empty() {
        return String::from("");
    }

    let parameter_str: String = parameters.join(",");
    format!("({})", parameter_str)
}

impl Quil for Instruction {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        match self {
            Instruction::Arithmetic(Arithmetic {
                operator,
                destination,
                source,
            }) => {
                write!(
                    writer,
                    "{} {} {}",
                    operator.to_quil()?,
                    destination.to_quil()?,
                    source.to_quil()?
                )?;
            }
            Instruction::CalibrationDefinition(calibration) => {
                write!(writer, "DEFCAL {}", calibration.name)?;
                write_expression_parameter_string(writer, &calibration.parameters)?;

                for qubit in &calibration.qubits {
                    write!(writer, " ")?;
                    qubit.write(writer)?;
                }

                write!(writer, ":")?;

                for instruction in &calibration.instructions {
                    write!(writer, "\n\t")?;
                    instruction.write(writer)?;
                }
            }
            Instruction::Capture(Capture {
                blocking,
                frame,
                waveform,
                memory_reference,
            }) => {
                if *blocking {
                    write!(writer, "CAPTURE ")?;
                } else {
                    write!(writer, "NONBLOCKING CAPTURE ")?;
                }

                frame.write(writer)?;
                write!(writer, " ")?;
                waveform.write(writer)?;
                write!(writer, " ")?;
                memory_reference.write(writer)?;
            }
            Instruction::CircuitDefinition(CircuitDefinition {
                name,
                parameters,
                qubit_variables,
                instructions,
            }) => {
                let parameter_str: String = parameters
                    .iter()
                    .map(|p| format!("%{}", p))
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(writer, "DEFCIRCUIT {}", name)?;
                if !parameter_str.is_empty() {
                    write!(writer, "({})", parameter_str)?;
                }
                for qubit_variable in qubit_variables {
                    write!(writer, " {}", qubit_variable)?;
                }
                write!(writer, ":")?;
                for instruction in &**instructions {
                    write!(writer, "\t")?;
                    instruction.write(writer)?;
                }
            }
            Instruction::Convert(Convert { from, to }) => {
                write!(writer, "CONVERT ")?;
                to.write(writer)?;
                write!(writer, " ")?;
                from.write(writer)?;
            }
            Instruction::Declaration(Declaration {
                name,
                size,
                sharing,
            }) => {
                write!(writer, "DECLARE {} ", name)?;
                size.write(writer)?;
                match sharing {
                    Some(shared) => {
                        write!(writer, "SHARING {}", shared)?;
                    }
                    None => {}
                }
            }
            Instruction::Delay(Delay {
                qubits,
                frame_names,
                duration,
            }) => {
                write!(writer, "DELAY")?;
                for qubit in qubits {
                    write!(writer, " ")?;
                    qubit.write(writer)?;
                }
                for frame_name in frame_names {
                    write!(writer, " {frame_name:?}")?;
                }
                write!(writer, " {}", duration)?;
            }
            Instruction::Fence(Fence { qubits }) => {
                write!(writer, "FENCE")?;

                for qubit in qubits {
                    write!(writer, " ")?;
                    qubit.write(writer)?;
                }
            }
            Instruction::FrameDefinition(FrameDefinition {
                identifier,
                attributes,
            }) => {
                write!(writer, "DEFFRAME ")?;
                identifier.write(writer)?;
                write!(writer, ":")?;
                for (key, value) in attributes {
                    write!(writer, "\n\t{}: ", key)?;
                    value.write(writer)?;
                }
            }
            Instruction::Gate(Gate {
                name,
                parameters,
                qubits,
                modifiers,
            }) => {
                for modifier in modifiers {
                    modifier.write(writer)?;
                    write!(writer, " ")?;
                }

                write!(writer, "{name}")?;
                write_expression_parameter_string(writer, parameters)?;

                for qubit in qubits {
                    write!(writer, " ")?;
                    qubit.write(writer)?;
                }
            }
            Instruction::GateDefinition(GateDefinition {
                name,
                parameters,
                specification,
            }) => {
                let parameter_str: String = parameters
                    .iter()
                    .map(|p| format!("%{}", p))
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(writer, "DEFGATE {}", name,)?;

                if !parameter_str.is_empty() {
                    write!(writer, "({})", parameter_str)?;
                }

                write!(
                    writer,
                    " AS {}",
                    match specification {
                        GateSpecification::Matrix(_) => "MATRIX",
                        GateSpecification::Permutation(_) => "PERMUTATION",
                    }
                )?;

                writeln!(writer, ":")?;
                specification.write(writer)?;
            }
            Instruction::Include(Include { filename }) => {
                write!(writer, r#"INCLUDE {:?}"#, filename)?;
            }
            Instruction::MeasureCalibrationDefinition(MeasureCalibrationDefinition {
                qubit,
                parameter,
                instructions,
            }) => {
                write!(writer, "DEFCAL MEASURE")?;
                match qubit {
                    Some(qubit) => {
                        write!(writer, " ")?;
                        qubit.write(writer)?;
                    }
                    None => {}
                }

                write!(writer, " {}:", parameter,)?;
                for instruction in instructions {
                    write!(writer, "\n\t")?;
                    instruction.write(writer)?;
                }
            }
            Instruction::Measurement(Measurement { qubit, target }) => match target {
                Some(reference) => {
                    write!(writer, "MEASURE ",)?;
                    qubit.write(writer)?;
                    write!(writer, " ")?;
                    reference.write(writer)?;
                }
                None => {
                    write!(writer, "MEASURE ")?;
                    qubit.write(writer)?;
                }
            },
            Instruction::Move(Move {
                destination,
                source,
            }) => {
                write!(writer, "MOVE ",)?;
                destination.write(writer)?;
                write!(writer, " ")?;
                source.write(writer)?;
            }
            Instruction::Exchange(Exchange { left, right }) => {
                write!(writer, "EXCHANGE ")?;
                left.write(writer)?;
                write!(writer, " ")?;
                right.write(writer)?;
            }
            Instruction::Load(Load {
                destination,
                source,
                offset,
            }) => {
                write!(writer, "LOAD ",)?;
                destination.write(writer)?;
                write!(writer, " {source} ")?;
                offset.write(writer)?;
            }
            Instruction::Store(Store {
                destination,
                offset,
                source,
            }) => {
                write!(writer, "STORE {} ", destination,)?;
                offset.write(writer)?;
                write!(writer, " ")?;
                source.write(writer)?;
            }
            Instruction::Pulse(Pulse {
                blocking,
                frame,
                waveform,
            }) => {
                if *blocking {
                    write!(writer, "PULSE ")?;
                } else {
                    write!(writer, "NONBLOCKING PULSE ")?;
                }
                frame.write(writer)?;
                write!(writer, " ")?;
                waveform.write(writer)?;
            }
            Instruction::Pragma(Pragma {
                name,
                arguments,
                data,
            }) => {
                write!(writer, "PRAGMA {}", name)?;
                if !arguments.is_empty() {
                    for arg in arguments {
                        write!(writer, " ")?;
                        arg.write(writer)?;
                    }
                }
                if let Some(data) = data {
                    write!(writer, " {:?}", data)?;
                }
            }
            Instruction::RawCapture(RawCapture {
                blocking,
                frame,
                duration,
                memory_reference,
            }) => {
                if *blocking {
                    write!(writer, "RAW-CAPTURE ")?;
                } else {
                    write!(writer, "NONBLOCKING RAW-CAPTURE ")?;
                }

                frame.write(writer)?;
                write!(writer, " {duration} ")?;
                memory_reference.write(writer)?;
            }
            Instruction::Reset(Reset { qubit }) => match qubit {
                Some(qubit) => {
                    write!(writer, "RESET ")?;
                    qubit.write(writer)?;
                }
                None => {
                    write!(writer, "RESET")?;
                }
            },
            Instruction::SetFrequency(SetFrequency { frame, frequency }) => {
                write!(writer, "SET-FREQUENCY ")?;
                frame.write(writer)?;
                write!(writer, " {frequency}")?;
            }
            Instruction::SetPhase(SetPhase { frame, phase }) => {
                write!(writer, "SET-PHASE ")?;
                frame.write(writer)?;
                write!(writer, " {phase}")?;
            }
            Instruction::SetScale(SetScale { frame, scale }) => {
                write!(writer, "SET-SCALE ")?;
                frame.write(writer)?;
                write!(writer, " {scale}")?;
            }
            Instruction::ShiftFrequency(ShiftFrequency { frame, frequency }) => {
                write!(writer, "SHIFT-FREQUENCY ")?;
                frame.write(writer)?;
                write!(writer, " {frequency}")?;
            }
            Instruction::ShiftPhase(ShiftPhase { frame, phase }) => {
                write!(writer, "SHIFT-PHASE ")?;
                frame.write(writer)?;
                write!(writer, " {phase}")?;
            }
            Instruction::SwapPhases(SwapPhases { frame_1, frame_2 }) => {
                write!(writer, "SWAP-PHASES ")?;
                frame_1.write(writer)?;
                write!(writer, " ")?;
                frame_2.write(writer)?;
            }
            Instruction::WaveformDefinition(WaveformDefinition { name, definition }) => {
                write!(writer, "DEFWAVEFORM {}", name)?;
                write!(
                    writer,
                    "{}:\n\t{}",
                    get_string_parameter_string(&definition.parameters),
                    definition
                        .matrix
                        .iter()
                        .map(|e| format!("{}", e))
                        .collect::<Vec<_>>()
                        .join(", ")
                )?;
            }
            Instruction::Halt => {
                write!(writer, "HALT")?;
            }
            Instruction::Nop => {
                write!(writer, "NOP")?;
            }
            Instruction::Jump(Jump { target }) => {
                write!(writer, "JUMP @{}", target)?;
            }
            Instruction::JumpUnless(JumpUnless { condition, target }) => {
                write!(writer, "JUMP-UNLESS @{} ", target)?;
                condition.write(writer)?;
            }
            Instruction::JumpWhen(JumpWhen { condition, target }) => {
                write!(writer, "JUMP-WHEN @{} ", target)?;
                condition.write(writer)?;
            }
            Instruction::Label(label) => match label {
                Label::Fixed(label) => {
                    write!(writer, "LABEL @{}", label)?;
                }
                Label::Placeholder(_) => {
                    return Err(ToQuilError::UnresolvedLabelPlaceholder);
                }
            },
            Instruction::Comparison(Comparison { operator, operands }) => {
                operator.write(writer)?;
                write!(writer, " ")?;
                operands.0.write(writer)?;
                write!(writer, " ")?;
                operands.1.write(writer)?;
                write!(writer, " ")?;
                operands.2.write(writer)?;
            }
            Instruction::BinaryLogic(BinaryLogic { operator, operands }) => {
                operator.write(writer)?;
                write!(writer, " ")?;
                operands.0.write(writer)?;
                write!(writer, " ")?;
                operands.1.write(writer)?;
            }
            Instruction::UnaryLogic(UnaryLogic { operator, operand }) => {
                operator.write(writer)?;
                write!(writer, " ")?;
                operand.write(writer)?;
            }
        }
        Ok(())
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

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Qubit {
    Fixed(u64),
    Placeholder(QubitPlaceholder),
    Variable(String),
}

impl Qubit {
    fn resolve_placeholder<R>(&mut self, resolver: R)
    where
        R: Fn(&QubitPlaceholder) -> Option<u64>,
    {
        if let Qubit::Placeholder(placeholder) = self {
            if let Some(resolved) = resolver(placeholder) {
                *self = Qubit::Fixed(resolved);
            }
        }
    }
}

impl Quil for Qubit {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        use Qubit::*;
        match self {
            Fixed(value) => write!(writer, "{value}").map_err(Into::into),
            Placeholder(_) => Err(ToQuilError::UnresolvedQubitPlaceholder),
            Variable(value) => write!(writer, "{value}s").map_err(Into::into),
        }
    }
}

type QubitPlaceholderInner = Arc<()>;

/// An opaque placeholder for a qubit whose index may be assigned
/// at a later time.
#[derive(Clone, Debug, Eq)]
pub struct QubitPlaceholder(QubitPlaceholderInner);

impl QubitPlaceholder {
    fn address(&self) -> usize {
        &*self.0 as *const _ as usize
    }
}

impl Default for QubitPlaceholder {
    fn default() -> Self {
        Self(Arc::new(()))
    }
}

impl std::hash::Hash for QubitPlaceholder {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.address().hash(state);
    }
}

impl PartialEq for QubitPlaceholder {
    #[allow(clippy::ptr_eq)]
    fn eq(&self, other: &Self) -> bool {
        self.address().eq(&other.address())
    }
}

impl PartialOrd for QubitPlaceholder {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.address().partial_cmp(&other.address())
    }
}

impl Ord for QubitPlaceholder {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.address().cmp(&other.address())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
    /// use quil_rs::{expression::Expression, Program, quil::Quil};
    ///
    ///
    /// let program = Program::from_str("SHIFT-PHASE 0 \"rf\" 2*2").unwrap();
    /// let mut instructions = program.to_instructions(true);
    /// instructions.iter_mut().for_each(|inst| inst.apply_to_expressions(Expression::simplify));
    ///
    /// assert_eq!(instructions[0].to_quil_or_debug(), String::from("SHIFT-PHASE 0 \"rf\" 4"))
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

    pub(crate) fn get_frame_match_condition(
        &self,
        include_blocked: bool,
        qubits_available: HashSet<Qubit>,
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
                FrameMatchCondition::AnyOfQubits(Cow::Borrowed(&frame.qubits))
            } else {
                FrameMatchCondition::Specific(frame)
            }),
            Instruction::Delay(Delay {
                frame_names,
                qubits,
                ..
            }) => Some(if frame_names.is_empty() {
                FrameMatchCondition::ExactQubits(Cow::Borrowed(qubits))
            } else {
                FrameMatchCondition::And(vec![
                    FrameMatchCondition::ExactQubits(Cow::Borrowed(qubits)),
                    FrameMatchCondition::AnyOfNames(frame_names),
                ])
            }),
            Instruction::Fence(Fence { qubits }) => {
                if include_blocked {
                    Some(if qubits.is_empty() {
                        FrameMatchCondition::All
                    } else {
                        FrameMatchCondition::AnyOfQubits(Cow::Borrowed(qubits))
                    })
                } else {
                    None
                }
            }
            Instruction::Reset(Reset { qubit }) => {
                let qubits = match qubit {
                    Some(qubit) => {
                        let mut set = HashSet::new();
                        set.insert(qubit.clone());
                        set
                    }
                    None => qubits_available,
                };
                let qubits = qubits.into_iter().collect();

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
            | Instruction::WaveformDefinition(_) => None,
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

    /// Return immutable references to the [`Qubit`]s contained within an instruction
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

    pub(crate) fn resolve_placeholders<QR>(
        &mut self,
        label_resolver: &dyn Fn(&LabelPlaceholder) -> Option<String>,
        qubit_resolver: QR,
    ) where
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

    /// Per the Quil-T spec, whether this instruction's timing within the pulse
    /// program must be precisely controlled so as to begin exactly on the end of
    /// the latest preceding timed instruction
    pub(crate) fn is_scheduled(&self) -> bool {
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
            | Instruction::SwapPhases(_) => true,
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
