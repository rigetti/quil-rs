use crate::quil::Quil;

use super::Qubit;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Reset {
    pub qubit: Option<Qubit>,
}

impl Reset {
    pub fn new(qubit: Option<Qubit>) -> Self {
        Self { qubit }
    }
}

impl Quil for Reset {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        match &self.qubit {
            Some(qubit) => {
                write!(writer, "RESET ")?;
                qubit.write(writer, fall_back_to_debug)
            }
            None => write!(writer, "RESET").map_err(Into::into),
        }
    }
}
