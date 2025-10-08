//! This module contains enums for reserved tokens in [quil](https://quil-lang.github.io)

use std::{fmt::Display, str::FromStr};

use strum;

pub use crate::parser::{Command, DataType, KeywordToken, Modifier};

/// An enum that can represent any reserved token in quil.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ReservedToken {
    Command(Command),
    DataType(DataType),
    Modifier(Modifier),
    OtherKeyword(KeywordToken),
    Gate(ReservedGate),
    Constant(ReservedConstant),
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("{0} is not a reserved token")]
pub struct NotReservedToken(String);

impl FromStr for ReservedToken {
    type Err = NotReservedToken;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        fn parse<T: FromStr>(
            reserved: impl Fn(T) -> ReservedToken,
            s: &str,
        ) -> Result<ReservedToken, T::Err> {
            T::from_str(s).map(reserved)
        }

        parse(Self::Command, s)
            .or_else(|_| parse(Self::DataType, s))
            .or_else(|_| parse(Self::Modifier, s))
            .or_else(|_| parse(Self::OtherKeyword, s))
            .or_else(|_| parse(Self::Gate, s))
            .or_else(|_| parse(Self::Constant, s))
            .map_err(|_| NotReservedToken(s.to_string()))
    }
}

impl Display for ReservedToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Command(command) => write!(f, "{command}"),
            Self::DataType(data_type) => write!(f, "{data_type}"),
            Self::Modifier(modifier) => write!(f, "{modifier}"),
            Self::OtherKeyword(keyword_token) => write!(f, "{keyword_token}"),
            Self::Gate(gate) => write!(f, "{gate}"),
            Self::Constant(constant) => write!(f, "{constant}"),
        }
    }
}

/// Every reserved Gate identifier
#[derive(Clone, Copy, Debug, PartialEq, Eq, strum::Display, strum::EnumString)]
#[strum(serialize_all = "UPPERCASE")]
#[allow(clippy::upper_case_acronyms)]
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
#[derive(Clone, Copy, Debug, PartialEq, Eq, strum::Display, strum::EnumString)]
#[strum(serialize_all = "lowercase")]
pub enum ReservedConstant {
    #[strum(serialize = "i")]
    Imaginary,
    Pi,
}
