use super::Qubit;
use crate::{expression::Expression, quil::Quil};

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

impl Quil for Delay {
    fn write(&self, writer: &mut impl std::fmt::Write) -> crate::quil::ToQuilResult<()> {
        write!(writer, "DELAY")?;
        for qubit in &self.qubits {
            write!(writer, " ")?;
            qubit.write(writer)?;
        }
        for frame_name in &self.frame_names {
            write!(writer, " \"{}\"", frame_name)?;
        }
        write!(writer, " {}", self.duration).map_err(Into::into)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Fence {
    pub qubits: Vec<Qubit>,
}

impl Quil for Fence {
    fn write(&self, writer: &mut impl std::fmt::Write) -> Result<(), crate::quil::ToQuilError> {
        write!(writer, "FENCE")?;
        for qubit in &self.qubits {
            write!(writer, " ")?;
            qubit.write(writer)?;
        }
        Ok(())
    }
}

impl Fence {
    pub fn new(qubits: Vec<Qubit>) -> Self {
        Self { qubits }
    }
}
