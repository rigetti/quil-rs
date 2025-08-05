#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::gen_stub_pyclass;

use super::Qubit;
use crate::{expression::Expression, pickleable_new, quil::Quil};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.instructions", eq, frozen, hash, get_all, subclass)
)]
pub struct Delay {
    pub duration: Expression,
    pub frame_names: Vec<String>,
    pub qubits: Vec<Qubit>,
}

pickleable_new! {
    impl Delay {
        pub fn new(duration: Expression, frame_names: Vec<String>, qubits: Vec<Qubit>);
    }
}

impl Quil for Delay {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(writer, "DELAY")?;
        for qubit in &self.qubits {
            write!(writer, " ")?;
            qubit.write(writer, fall_back_to_debug)?;
        }
        for frame_name in &self.frame_names {
            write!(writer, " \"{frame_name}\"")?;
        }
        write!(writer, " ",)?;
        self.duration.write(writer, fall_back_to_debug)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.instructions", eq, frozen, hash, get_all, subclass)
)]
pub struct Fence {
    pub qubits: Vec<Qubit>,
}

impl Quil for Fence {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> Result<(), crate::quil::ToQuilError> {
        write!(writer, "FENCE")?;
        for qubit in &self.qubits {
            write!(writer, " ")?;
            qubit.write(writer, fall_back_to_debug)?;
        }
        Ok(())
    }
}

pickleable_new! {
    impl Fence {
        pub fn new(qubits: Vec<Qubit>);
    }
}
