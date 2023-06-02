use std::fmt;

use super::Qubit;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Reset {
    pub qubit: Option<Qubit>,
}

impl Reset {
    pub fn new(qubit: Option<Qubit>) -> Self {
        Self { qubit }
    }
}

impl fmt::Display for Reset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.qubit {
            Some(qubit) => write!(f, "RESET {qubit}"),
            None => write!(f, "RESET"),
        }
    }
}
