use std::{fmt, str::FromStr};

use nom_locate::LocatedSpan;

#[cfg(test)]
use proptest_derive::Arbitrary;

use crate::{
    parser::{common::parse_memory_reference, lex, ParseError},
    program::{disallow_leftover, SyntaxError},
};

use super::ArithmeticOperand;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
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

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Vector {
    pub data_type: ScalarType,
    pub length: u64,
}

impl Vector {
    pub fn new(data_type: ScalarType, length: u64) -> Self {
        Self { data_type, length }
    }
}

impl fmt::Display for Vector {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}[{}]", self.data_type, self.length)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Sharing {
    pub name: String,
    pub offsets: Vec<Offset>,
}

impl Sharing {
    pub fn new(name: String, offsets: Vec<Offset>) -> Self {
        Self { name, offsets }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Offset {
    pub offset: u64,
    pub data_type: ScalarType,
}

impl Offset {
    pub fn new(offset: u64, data_type: ScalarType) -> Self {
        Self { offset, data_type }
    }
}

impl fmt::Display for Offset {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.offset, self.data_type)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Declaration {
    pub name: String,
    pub size: Vector,
    pub sharing: Option<Sharing>,
}

impl Declaration {
    pub fn new(name: String, size: Vector, sharing: Option<Sharing>) -> Self {
        Self {
            name,
            size,
            sharing,
        }
    }
}

impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "DECLARE {} {}", self.name, self.size)?;
        if let Some(shared) = &self.sharing {
            write!(f, " SHARING {}", shared.name)?;
            if !shared.offsets.is_empty() {
                write!(f, " OFFSET")?;
                for offset in shared.offsets.iter() {
                    write!(f, " {offset}")?
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod test_declaration {
    use super::{Declaration, Offset, ScalarType, Sharing, Vector};
    use insta::assert_snapshot;
    use rstest::rstest;

    #[rstest]
    #[case(
        "Basic Declaration",
        Declaration{
            name: "ro".to_string(),
            size: Vector{data_type: ScalarType::Bit, length: 1},
            sharing: None,

        }
    )]
    #[case(
        "Shared Declaration",
        Declaration{
            name: "ro".to_string(),
            size: Vector{data_type: ScalarType::Integer, length: 2},
            sharing: Some(Sharing{name: "foo".to_string(), offsets: vec![]})
        }
    )]
    #[case(
        "Shared Declaration with Offsets",
        Declaration{
            name: "ro".to_string(),
            size: Vector{data_type: ScalarType::Real, length: 3},
            sharing: Some(Sharing{
                name: "bar".to_string(),
                offsets: vec![
                    Offset{offset: 4, data_type: ScalarType::Bit},
                    Offset{offset: 5, data_type: ScalarType::Bit}
                ]})
        }
    )]
    fn test_display(#[case] description: &str, #[case] declaration: Declaration) {
        insta::with_settings!({
            snapshot_suffix => description,
        }, {
            assert_snapshot!(declaration.to_string())
        })
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct MemoryReference {
    pub name: String,
    pub index: u64,
}

impl MemoryReference {
    pub fn new(name: String, index: u64) -> Self {
        Self { name, index }
    }
}

impl Eq for MemoryReference {}

impl fmt::Display for MemoryReference {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
pub struct Load {
    pub destination: MemoryReference,
    pub source: String,
    pub offset: MemoryReference,
}

impl Load {
    pub fn new(destination: MemoryReference, source: String, offset: MemoryReference) -> Self {
        Self {
            destination,
            source,
            offset,
        }
    }
}

impl fmt::Display for Load {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "LOAD {} {} {}",
            self.destination, self.source, self.offset
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Store {
    pub destination: String,
    pub offset: MemoryReference,
    pub source: ArithmeticOperand,
}

impl Store {
    pub fn new(destination: String, offset: MemoryReference, source: ArithmeticOperand) -> Self {
        Self {
            destination,
            offset,
            source,
        }
    }
}

impl fmt::Display for Store {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "STORE {} {} {}",
            self.destination, self.offset, self.source
        )
    }
}
