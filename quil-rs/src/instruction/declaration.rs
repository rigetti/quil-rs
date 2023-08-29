use std::str::FromStr;

use nom_locate::LocatedSpan;

use crate::{
    parser::{common::parse_memory_reference, lex, ParseError},
    program::{disallow_leftover, SyntaxError},
    quil::Quil,
};

use super::ArithmeticOperand;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ScalarType {
    Bit,
    Integer,
    Octet,
    Real,
}

impl Quil for ScalarType {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        _fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
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
        .map_err(Into::into)
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

impl Quil for Vector {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        self.data_type.write(f, fall_back_to_debug)?;
        write!(f, "[{}]", self.length).map_err(Into::into)
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

impl Quil for Offset {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(f, "{} ", self.offset)?;
        self.data_type.write(f, fall_back_to_debug)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

impl Quil for Declaration {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(f, "DECLARE {} ", self.name)?;
        self.size.write(f, fall_back_to_debug)?;
        if let Some(shared) = &self.sharing {
            write!(f, " SHARING {}", shared.name)?;
            if !shared.offsets.is_empty() {
                write!(f, " OFFSET")?;
                for offset in shared.offsets.iter() {
                    write!(f, " ")?;
                    offset.write(f, fall_back_to_debug)?;
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod test_declaration {
    use super::{Declaration, Offset, ScalarType, Sharing, Vector};
    use crate::quil::Quil;
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
            assert_snapshot!(declaration.to_quil_or_debug())
        })
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct MemoryReference {
    pub name: String,
    pub index: u64,
}

impl MemoryReference {
    pub fn new(name: String, index: u64) -> Self {
        Self { name, index }
    }
}

impl Quil for MemoryReference {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        _fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(f, "{}[{}]", self.name, self.index).map_err(Into::into)
    }
}

impl std::fmt::Display for MemoryReference {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

impl Quil for Load {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(f, "LOAD ")?;
        self.destination.write(f, fall_back_to_debug)?;
        write!(f, " {} ", self.source)?;
        self.offset.write(f, fall_back_to_debug)?;
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Hash)]
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

impl Quil for Store {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(f, "STORE {} ", self.destination)?;
        self.offset.write(f, fall_back_to_debug)?;
        write!(f, " ")?;
        self.source.write(f, fall_back_to_debug)?;
        Ok(())
    }
}
