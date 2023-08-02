use std::fmt;

use crate::{hash::hash_f64, impl_quil_from_display};

use super::MemoryReference;

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct Arithmetic {
    pub operator: ArithmeticOperator,
    pub destination: ArithmeticOperand,
    pub source: ArithmeticOperand,
}

impl Arithmetic {
    pub fn new(
        operator: ArithmeticOperator,
        destination: ArithmeticOperand,
        source: ArithmeticOperand,
    ) -> Self {
        Self {
            operator,
            destination,
            source,
        }
    }
}

impl fmt::Display for Arithmetic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.operator, self.destination, self.source)
    }
}

impl_quil_from_display!(Arithmetic);

#[derive(Clone, Debug, PartialEq)]
pub enum ArithmeticOperand {
    LiteralInteger(i64),
    LiteralReal(f64),
    MemoryReference(MemoryReference),
}

impl std::hash::Hash for ArithmeticOperand {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::LiteralInteger(operand) => operand.hash(state),
            Self::LiteralReal(operand) => hash_f64(*operand, state),
            Self::MemoryReference(operand) => operand.hash(state),
        }
    }
}

impl fmt::Display for ArithmeticOperand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ArithmeticOperand::LiteralInteger(value) => write!(f, "{value}"),
            ArithmeticOperand::LiteralReal(value) => write!(f, "{value}"),
            ArithmeticOperand::MemoryReference(value) => write!(f, "{value}"),
        }
    }
}

impl_quil_from_display!(ArithmeticOperand);

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, strum::Display)]
pub enum ArithmeticOperator {
    #[strum(to_string = "ADD")]
    Add,
    #[strum(to_string = "SUB")]
    Subtract,
    #[strum(to_string = "DIV")]
    Divide,
    #[strum(to_string = "MUL")]
    Multiply,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BinaryOperand {
    LiteralInteger(i64),
    MemoryReference(MemoryReference),
}

impl fmt::Display for BinaryOperand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            BinaryOperand::LiteralInteger(value) => write!(f, "{value}"),
            BinaryOperand::MemoryReference(value) => write!(f, "{value}"),
        }
    }
}

impl_quil_from_display!(BinaryOperand);

pub type BinaryOperands = (MemoryReference, BinaryOperand);

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, strum::Display)]
#[strum(serialize_all = "UPPERCASE")]
pub enum BinaryOperator {
    And,
    Ior,
    Xor,
}

impl_quil_from_display!(BinaryOperator);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct BinaryLogic {
    pub operator: BinaryOperator,
    pub operands: BinaryOperands,
}

impl fmt::Display for BinaryLogic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.operator, self.operands.0, self.operands.1
        )
    }
}

impl_quil_from_display!(BinaryLogic);

impl BinaryLogic {
    pub fn new(operator: BinaryOperator, operands: BinaryOperands) -> Self {
        Self { operator, operands }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Convert {
    pub destination: MemoryReference,
    pub source: MemoryReference,
}

impl Convert {
    pub fn new(destination: MemoryReference, source: MemoryReference) -> Self {
        Self {
            destination,
            source,
        }
    }
}

impl fmt::Display for Convert {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "CONVERT {} {}", self.destination, self.source)
    }
}

impl_quil_from_display!(Convert);

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct Move {
    pub destination: MemoryReference,
    pub source: ArithmeticOperand,
}

impl Move {
    pub fn new(destination: MemoryReference, source: ArithmeticOperand) -> Self {
        Self {
            destination,
            source,
        }
    }
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "MOVE {} {}", self.destination, self.source)
    }
}

impl_quil_from_display!(Move);

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct Exchange {
    pub left: MemoryReference,
    pub right: MemoryReference,
}

impl fmt::Display for Exchange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "EXCHANGE {} {}", self.left, self.right)
    }
}

impl_quil_from_display!(Exchange);

impl Exchange {
    pub fn new(left: MemoryReference, right: MemoryReference) -> Self {
        Self { left, right }
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct Comparison {
    pub operator: ComparisonOperator,
    pub operands: (MemoryReference, MemoryReference, ComparisonOperand),
}

impl Comparison {
    pub fn new(
        operator: ComparisonOperator,
        operands: (MemoryReference, MemoryReference, ComparisonOperand),
    ) -> Self {
        Self { operator, operands }
    }
}

impl fmt::Display for Comparison {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {} {} {}",
            self.operator, self.operands.0, self.operands.1, self.operands.2
        )
    }
}

impl_quil_from_display!(Comparison);

#[derive(Clone, Debug, PartialEq)]
pub enum ComparisonOperand {
    LiteralInteger(i64),
    LiteralReal(f64),
    MemoryReference(MemoryReference),
}

impl fmt::Display for ComparisonOperand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ComparisonOperand::LiteralInteger(value) => write!(f, "{value}"),
            ComparisonOperand::LiteralReal(value) => write!(f, "{value}"),
            ComparisonOperand::MemoryReference(value) => write!(f, "{value}"),
        }
    }
}

impl_quil_from_display!(ComparisonOperand);

impl std::hash::Hash for ComparisonOperand {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            ComparisonOperand::LiteralInteger(operand) => operand.hash(state),
            ComparisonOperand::LiteralReal(operand) => hash_f64(*operand, state),
            ComparisonOperand::MemoryReference(operand) => operand.hash(state),
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
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

impl_quil_from_display!(ComparisonOperator);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct UnaryLogic {
    pub operator: UnaryOperator,
    pub operand: MemoryReference,
}

impl UnaryLogic {
    pub fn new(operator: UnaryOperator, operand: MemoryReference) -> Self {
        Self { operator, operand }
    }
}

impl fmt::Display for UnaryLogic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.operator, self.operand)
    }
}

impl_quil_from_display!(UnaryLogic);

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, strum::Display)]
#[strum(serialize_all = "UPPERCASE")]
pub enum UnaryOperator {
    Neg,
    Not,
}

impl_quil_from_display!(UnaryOperator);
