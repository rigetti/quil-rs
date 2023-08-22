use crate::quil::{Quil, ToQuilError};

use super::QubitPlaceholder;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Pragma {
    pub name: String,
    pub arguments: Vec<PragmaArgument>,
    pub data: Option<String>,
}

impl Pragma {
    pub fn new(name: String, arguments: Vec<PragmaArgument>, data: Option<String>) -> Self {
        Self {
            name,
            arguments,
            data,
        }
    }
}

impl Pragma {
    pub(crate) fn resolve_placeholders<R>(&mut self, resolver: R)
    where
        R: Fn(&QubitPlaceholder) -> Option<u64>,
    {
        self.arguments.iter_mut().for_each(|argument| {
            if let PragmaArgument::Placeholder(placeholder) = argument {
                if let Some(resolved) = resolver(placeholder) {
                    *argument = PragmaArgument::Integer(resolved)
                }
            }
        })
    }
}

impl Quil for Pragma {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(f, "PRAGMA {}", self.name)?;
        for arg in &self.arguments {
            write!(f, " ")?;
            arg.write(f, fall_back_to_debug)?;
        }
        if let Some(data) = &self.data {
            write!(f, " {data:?}")?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PragmaArgument {
    Identifier(String),
    Integer(u64),
    Placeholder(QubitPlaceholder),
}

impl Quil for PragmaArgument {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        match self {
            PragmaArgument::Identifier(i) => write!(f, "{i}"),
            PragmaArgument::Integer(i) => write!(f, "{i}"),
            PragmaArgument::Placeholder(placeholder) => {
                if fall_back_to_debug {
                    write!(f, "{:?}", placeholder)
                } else {
                    Err(ToQuilError::UnresolvedQubitPlaceholder)?
                }
            }
        }
        .map_err(Into::into)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Include {
    pub filename: String,
}

impl Quil for Include {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        _fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(f, r#"INCLUDE {:?}"#, self.filename).map_err(Into::into)
    }
}

impl Include {
    pub fn new(filename: String) -> Self {
        Self { filename }
    }
}
