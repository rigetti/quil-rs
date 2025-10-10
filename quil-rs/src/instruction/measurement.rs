#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::gen_stub_pyclass;

use crate::quil::Quil;

use super::{MemoryReference, Qubit};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.instructions", eq, frozen, hash, get_all, subclass)
)]
pub struct Measurement {
    pub name: Option<String>,
    pub qubit: Qubit,
    pub target: Option<MemoryReference>,
}

impl Measurement {
    pub const fn new(name: Option<String>, qubit: Qubit, target: Option<MemoryReference>) -> Self {
        Self {
            name,
            qubit,
            target,
        }
    }
}

impl Quil for Measurement {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> Result<(), crate::quil::ToQuilError> {
        let Self {
            name,
            qubit,
            target,
        } = self;

        write!(writer, "MEASURE")?;
        if let Some(name) = name {
            write!(writer, "!{name}")?;
        }
        write!(writer, " ")?;
        qubit.write(writer, fall_back_to_debug)?;
        if let Some(target) = target {
            write!(writer, " ")?;
            target.write(writer, fall_back_to_debug)?;
        }

        Ok(())
    }
}
