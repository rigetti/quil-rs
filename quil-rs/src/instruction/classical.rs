#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::{
    gen_stub_pyclass, gen_stub_pyclass_complex_enum, gen_stub_pyclass_enum,
};

use super::MemoryReference;

use crate::{floating_point_eq, pickleable_new, quil::Quil};

#[derive(Clone, Debug, Hash, PartialEq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.instructions", eq, frozen, hash, get_all, subclass)
)]
pub struct Arithmetic {
    pub operator: ArithmeticOperator,
    pub destination: MemoryReference,
    pub source: ArithmeticOperand,
}

pickleable_new! {
    impl Arithmetic {
        pub fn new(
            operator: ArithmeticOperator,
            destination: MemoryReference,
            source: ArithmeticOperand,
        );
    }
}

impl Quil for Arithmetic {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        self.operator.write(f, fall_back_to_debug)?;
        write!(f, " ")?;
        self.destination.write(f, fall_back_to_debug)?;
        write!(f, " ")?;
        self.source.write(f, fall_back_to_debug)
    }
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass_complex_enum)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.instructions", eq, frozen, hash, get_all)
)]
pub enum ArithmeticOperand {
    LiteralInteger(i64),
    LiteralReal(f64),
    MemoryReference(MemoryReference),
}

impl PartialEq for ArithmeticOperand {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::LiteralInteger(this), Self::LiteralInteger(that)) => this == that,
            (Self::LiteralReal(this), Self::LiteralReal(that)) => {
                floating_point_eq::f64::eq(*this, *that)
            }
            (Self::MemoryReference(this), Self::MemoryReference(that)) => this == that,
            // This explicit or-pattern ensures that we'll get a compilation error if
            // `ArithmeticOperand` grows another constructor.
            (Self::LiteralInteger(_) | Self::LiteralReal(_) | Self::MemoryReference(_), _) => false,
        }
    }
}

impl Eq for ArithmeticOperand {}

impl std::hash::Hash for ArithmeticOperand {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::LiteralInteger(operand) => operand.hash(state),
            Self::LiteralReal(operand) => floating_point_eq::f64::hash(*operand, state),
            Self::MemoryReference(operand) => operand.hash(state),
        }
    }
}

impl Quil for ArithmeticOperand {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        match &self {
            ArithmeticOperand::LiteralInteger(value) => write!(f, "{value}").map_err(Into::into),
            ArithmeticOperand::LiteralReal(value) => write!(f, "{value}").map_err(Into::into),
            ArithmeticOperand::MemoryReference(value) => value.write(f, fall_back_to_debug),
        }
    }
}

impl From<MemoryReference> for ArithmeticOperand {
    fn from(memory_reference: MemoryReference) -> Self {
        ArithmeticOperand::MemoryReference(memory_reference)
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass_enum)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(
        module = "quil.instructions",
        eq,
        frozen,
        hash,
        rename_all = "SCREAMING_SNAKE_CASE"
    )
)]
pub enum ArithmeticOperator {
    Add,
    Subtract,
    Divide,
    Multiply,
}

impl Quil for ArithmeticOperator {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        _fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        match &self {
            ArithmeticOperator::Add => write!(f, "ADD"),
            ArithmeticOperator::Subtract => write!(f, "SUB"),
            ArithmeticOperator::Divide => write!(f, "DIV"),
            ArithmeticOperator::Multiply => write!(f, "MUL"),
        }
        .map_err(Into::into)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass_complex_enum)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.instructions", eq, frozen, hash, get_all)
)]
pub enum BinaryOperand {
    LiteralInteger(i64),
    MemoryReference(MemoryReference),
}

impl Quil for BinaryOperand {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        match &self {
            BinaryOperand::LiteralInteger(value) => write!(f, "{value}").map_err(Into::into),
            BinaryOperand::MemoryReference(value) => value.write(f, fall_back_to_debug),
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass_enum)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(
        module = "quil.instructions",
        eq,
        frozen,
        hash,
        rename_all = "SCREAMING_SNAKE_CASE"
    )
)]
pub enum BinaryOperator {
    And,
    Ior,
    Xor,
}

impl Quil for BinaryOperator {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        _fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        match &self {
            BinaryOperator::And => write!(f, "AND"),
            BinaryOperator::Ior => write!(f, "IOR"),
            BinaryOperator::Xor => write!(f, "XOR"),
        }
        .map_err(Into::into)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.instructions", eq, frozen, hash, get_all, subclass)
)]
pub struct BinaryLogic {
    pub operator: BinaryOperator,
    pub destination: MemoryReference,
    pub source: BinaryOperand,
}

impl Quil for BinaryLogic {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        self.operator.write(f, fall_back_to_debug)?;
        write!(f, " ")?;
        self.destination.write(f, fall_back_to_debug)?;
        write!(f, " ")?;
        self.source.write(f, fall_back_to_debug)?;
        Ok(())
    }
}

pickleable_new! {
    impl BinaryLogic {
        pub fn new(
            operator: BinaryOperator,
            destination: MemoryReference,
            source: BinaryOperand,
        );
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.instructions", eq, frozen, hash, get_all, subclass)
)]
pub struct Convert {
    pub destination: MemoryReference,
    pub source: MemoryReference,
}

pickleable_new! {
    impl Convert {
        pub fn new(destination: MemoryReference, source: MemoryReference);
    }
}

impl Quil for Convert {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(f, "CONVERT ")?;
        self.destination.write(f, fall_back_to_debug)?;
        write!(f, " ")?;
        self.source.write(f, fall_back_to_debug)?;
        Ok(())
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.instructions", eq, frozen, hash, get_all, subclass)
)]
pub struct Move {
    pub destination: MemoryReference,
    pub source: ArithmeticOperand,
}

pickleable_new! {
    impl Move {
        pub fn new(destination: MemoryReference, source: ArithmeticOperand);
    }
}

impl Quil for Move {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(f, "MOVE ")?;
        self.destination.write(f, fall_back_to_debug)?;
        write!(f, " ")?;
        self.source.write(f, fall_back_to_debug)?;
        Ok(())
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.instructions", eq, frozen, hash, get_all, subclass)
)]
pub struct Exchange {
    pub left: MemoryReference,
    pub right: MemoryReference,
}

impl Quil for Exchange {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(f, "EXCHANGE ")?;
        self.left.write(f, fall_back_to_debug)?;
        write!(f, " ")?;
        self.right.write(f, fall_back_to_debug)?;
        Ok(())
    }
}

pickleable_new! {
    impl Exchange {
        pub fn new(left: MemoryReference, right: MemoryReference);
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.instructions", eq, frozen, hash, get_all, subclass)
)]
pub struct Comparison {
    pub operator: ComparisonOperator,
    pub destination: MemoryReference,
    pub lhs: MemoryReference,
    pub rhs: ComparisonOperand,
}

pickleable_new! {
    impl Comparison {
        pub fn new(
            operator: ComparisonOperator,
            destination: MemoryReference,
            lhs: MemoryReference,
            rhs: ComparisonOperand,
        );
    }
}

impl Quil for Comparison {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        self.operator.write(f, fall_back_to_debug)?;
        write!(f, " ")?;
        self.destination.write(f, fall_back_to_debug)?;
        write!(f, " ")?;
        self.lhs.write(f, fall_back_to_debug)?;
        write!(f, " ")?;
        self.rhs.write(f, fall_back_to_debug)?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass_complex_enum)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.instructions", eq, frozen, hash, get_all)
)]
pub enum ComparisonOperand {
    LiteralInteger(i64),
    LiteralReal(f64),
    MemoryReference(MemoryReference),
}

impl Quil for ComparisonOperand {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        match &self {
            ComparisonOperand::LiteralInteger(value) => write!(f, "{value}").map_err(Into::into),
            ComparisonOperand::LiteralReal(value) => write!(f, "{value}").map_err(Into::into),
            ComparisonOperand::MemoryReference(value) => value.write(f, fall_back_to_debug),
        }
    }
}

impl PartialEq for ComparisonOperand {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::LiteralInteger(this), Self::LiteralInteger(that)) => this == that,
            (Self::LiteralReal(this), Self::LiteralReal(that)) => {
                floating_point_eq::f64::eq(*this, *that)
            }
            (Self::MemoryReference(this), Self::MemoryReference(that)) => this == that,
            // This explicit or-pattern ensures that we'll get a compilation error if
            // `ComparisonOperand` grows another constructor.
            (Self::LiteralInteger(_) | Self::LiteralReal(_) | Self::MemoryReference(_), _) => false,
        }
    }
}

impl Eq for ComparisonOperand {}

impl std::hash::Hash for ComparisonOperand {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            ComparisonOperand::LiteralInteger(operand) => operand.hash(state),
            ComparisonOperand::LiteralReal(operand) => {
                floating_point_eq::f64::hash(*operand, state)
            }
            ComparisonOperand::MemoryReference(operand) => operand.hash(state),
        }
    }
}

pub trait ClassicalOperand: Clone + std::fmt::Debug {
    fn memory_reference(&self) -> Option<&MemoryReference>;
}

macro_rules! classical_operands {
    ($($operand:ident),* $(,)?) => {
        $(
            impl ClassicalOperand for $operand {
                fn memory_reference(&self) -> Option<&MemoryReference> {
                    if let Self::MemoryReference(memory_reference) = self {
                        Some(memory_reference)
                    } else {
                        None
                    }
                }
            }
        )*
    }
}

classical_operands! { ArithmeticOperand, BinaryOperand, ComparisonOperand }

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass_enum)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(
        module = "quil.instructions",
        eq,
        frozen,
        hash,
        rename_all = "SCREAMING_SNAKE_CASE"
    )
)]
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
        f: &mut impl std::fmt::Write,
        _fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        match &self {
            ComparisonOperator::Equal => write!(f, "EQ"),
            ComparisonOperator::GreaterThanOrEqual => write!(f, "GE"),
            ComparisonOperator::GreaterThan => write!(f, "GT"),
            ComparisonOperator::LessThanOrEqual => write!(f, "LE"),
            ComparisonOperator::LessThan => write!(f, "LT"),
        }
        .map_err(Into::into)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.instructions", eq, frozen, hash, get_all, subclass)
)]
pub struct UnaryLogic {
    pub operator: UnaryOperator,
    pub operand: MemoryReference,
}

pickleable_new! {
    impl UnaryLogic {
        pub fn new(operator: UnaryOperator, operand: MemoryReference);
    }
}

impl Quil for UnaryLogic {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        self.operator.write(f, fall_back_to_debug)?;
        write!(f, " ")?;
        self.operand.write(f, fall_back_to_debug)?;
        Ok(())
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass_enum)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(
        module = "quil.instructions",
        eq,
        frozen,
        hash,
        rename_all = "SCREAMING_SNAKE_CASE"
    )
)]
pub enum UnaryOperator {
    Neg,
    Not,
}

impl Quil for UnaryOperator {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        _fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        match &self {
            UnaryOperator::Neg => write!(f, "NEG"),
            UnaryOperator::Not => write!(f, "NOT"),
        }
        .map_err(Into::into)
    }
}
