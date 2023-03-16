use std::fmt;

use super::MemoryReference;

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
pub enum ArithmeticOperand {
    LiteralInteger(i64),
    LiteralReal(f64),
    MemoryReference(MemoryReference),
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
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

pub type BinaryOperands = (MemoryReference, BinaryOperand);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BinaryLogic {
    pub operator: BinaryOperator,
    pub operands: BinaryOperands,
}

impl BinaryLogic {
    pub fn new(operator: BinaryOperator, operands: BinaryOperands) -> Self {
        Self { operator, operands }
    }
}
