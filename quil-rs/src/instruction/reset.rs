use pyo3::prelude::*;

use crate::{pickleable_new, quil::Quil};

use super::Qubit;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[pyclass(module = "quil.instructions", eq, frozen, hash, get_all, subclass)]
pub struct Reset {
    pub qubit: Option<Qubit>,
}

pickleable_new! {
    impl Reset {
        pub fn new(qubit: Option<Qubit>);
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
