use std::fmt;

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

impl fmt::Display for Pragma {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "PRAGMA {}", self.name)?;
        for arg in &self.arguments {
            write!(f, " {arg}")?;
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
}

impl fmt::Display for PragmaArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PragmaArgument::Identifier(i) => write!(f, "{i}"),
            PragmaArgument::Integer(i) => write!(f, "{i}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Include {
    pub filename: String,
}

impl fmt::Display for Include {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, r#"INCLUDE {:?}"#, self.filename)
    }
}

impl Include {
    pub fn new(filename: String) -> Self {
        Self { filename }
    }
}
