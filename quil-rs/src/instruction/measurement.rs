use std::fmt;

use super::{MemoryReference, Qubit};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Measurement {
    pub qubit: Qubit,
    pub target: Option<MemoryReference>,
}

impl Measurement {
    pub fn new(qubit: Qubit, target: Option<MemoryReference>) -> Self {
        Self { qubit, target }
    }
}

impl fmt::Display for Measurement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.target {
            Some(reference) => write!(f, "MEASURE {} {reference}", self.qubit),
            None => write!(f, "MEASURE {}", self.qubit),
        }
    }
}
