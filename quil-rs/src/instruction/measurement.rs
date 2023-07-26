use crate::quil::Quil;

use super::{MemoryReference, Qubit};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Measurement {
    pub qubit: Qubit,
    pub target: Option<MemoryReference>,
}

impl Measurement {
    pub fn new(qubit: Qubit, target: Option<MemoryReference>) -> Self {
        Self { qubit, target }
    }
}

impl Quil for Measurement {
    fn write(&self, writer: &mut impl std::fmt::Write) -> Result<(), crate::quil::ToQuilError> {
        match &self.target {
            Some(reference) => {
                write!(writer, "MEASURE ",)?;
                self.qubit.write(writer)?;
                write!(writer, " ")?;
                reference.write(writer)
            }
            None => {
                write!(writer, "MEASURE ")?;
                self.qubit.write(writer)
            }
        }
    }
}
