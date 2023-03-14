use std::fmt;

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Declaration {
    pub name: String,
    pub size: Vector,
    pub sharing: Option<String>,
}

impl Declaration {
    pub fn new(name: String, size: Vector, sharing: Option<String>) -> Self {
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
            write!(f, "SHARING {shared}")?
        }
        Ok(())
    }
}
