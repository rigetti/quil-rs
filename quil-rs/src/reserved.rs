//! This module contains enums for reserved tokens in [quil](https://quil-lang.github.io)

use std::{fmt::Display, str::FromStr};

use strum;

/// An enum that can represent any reserved token in quil.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ReservedToken {
    Keyword(ReservedKeyword),
    Gate(ReservedGate),
    Constant(ReservedConstant),
}

#[derive(Clone, Debug)]
pub struct NotReservedToken(String);

impl FromStr for ReservedToken {
    type Err = NotReservedToken;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(keyword) = ReservedKeyword::from_str(s) {
            Ok(Self::Keyword(keyword))
        } else if let Ok(gate) = ReservedGate::from_str(s) {
            Ok(Self::Gate(gate))
        } else if let Ok(constant) = ReservedConstant::from_str(s) {
            Ok(Self::Constant(constant))
        } else {
            Err(NotReservedToken(format!("{s} is not a reserved token")))
        }
    }
}

impl Display for ReservedToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Keyword(keyword) => write!(f, "{keyword}"),
            Self::Gate(gate) => write!(f, "{gate}"),
            Self::Constant(constant) => write!(f, "{constant}"),
        }
    }
}

/// Any reserved keyword that isn't specifically a gate identifier or constant
#[derive(Clone, Debug, PartialEq, Eq, strum::Display, strum::EnumString)]
#[strum(serialize_all = "UPPERCASE")]
pub enum ReservedKeyword {
    Add,
    And,
    As,
    Controlled,
    Convert,
    Dagger,
    Declare,
    DefCircuit,
    DefGate,
    Div,
    Eq,
    Exchange,
    Forked,
    Ge,
    Gt,
    Halt,
    Include,
    Ior,
    Jump,
    #[strum(serialize = "JUMP-UNLESS")]
    JumpUnless,
    #[strum(serialize = "JUMP-WHEN")]
    JumpWhen,
    Label,
    Le,
    Load,
    Lt,
    Matrix,
    Measure,
    Move,
    Mul,
    Neg,
    Nop,
    Not,
    Offset,
    #[strum(serialize = "PAULI-SUM")]
    PauliSum,
    Permutation,
    Pragma,
    Reset,
    Sharing,
    Store,
    Sub,
    Wait,
    Xor,
}

/// Every reserved Gate identifier
#[derive(Clone, Debug, PartialEq, Eq, strum::Display, strum::EnumString)]
#[strum(serialize_all = "UPPERCASE")]
pub enum ReservedGate {
    CAN,
    CCNOT,
    CNOT,
    CPHASE,
    CPHASE00,
    CPHASE01,
    CPHASE10,
    CSWAP,
    CZ,
    H,
    I,
    ISWAP,
    PHASE,
    PISWAP,
    PSWAP,
    RX,
    RY,
    RZ,
    S,
    SWAP,
    T,
    X,
    XY,
    Y,
    Z,
}

/// Every reserved constant
#[derive(Clone, Debug, PartialEq, Eq, strum::Display, strum::EnumString)]
#[strum(serialize_all = "lowercase")]
pub enum ReservedConstant {
    #[strum(serialize = "i")]
    Imaginary,
    Pi,
}
