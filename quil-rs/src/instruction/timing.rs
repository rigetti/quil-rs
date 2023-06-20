use std::fmt;

use super::Qubit;
use crate::expression::Expression;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Delay {
    pub duration: Expression,
    pub frame_names: Vec<String>,
    pub qubits: Vec<Qubit>,
}

impl Delay {
    pub fn new(duration: Expression, frame_names: Vec<String>, qubits: Vec<Qubit>) -> Self {
        Self {
            duration,
            frame_names,
            qubits,
        }
    }
}

impl fmt::Display for Delay {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "DELAY")?;
        for qubit in &self.qubits {
            write!(f, " {qubit}")?
        }
        for frame_name in &self.frame_names {
            write!(f, " \"{frame_name}\"")?;
        }
        write!(f, " {}", self.duration)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Fence {
    pub qubits: Vec<Qubit>,
}

impl fmt::Display for Fence {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "FENCE")?;
        for qubit in &self.qubits {
            write!(f, " {qubit}")?
        }
        Ok(())
    }
}

impl Fence {
    pub fn new(qubits: Vec<Qubit>) -> Self {
        Self { qubits }
    }
}
