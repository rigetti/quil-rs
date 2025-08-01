use crate::{pickleable_new, quil::Quil};

use super::{MemoryReference, Qubit};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "python", pyo3::pyclass(module = "quil.instructions", eq, frozen, hash, get_all, subclass))]
pub struct Measurement {
    pub qubit: Qubit,
    pub target: Option<MemoryReference>,
}

pickleable_new! {
    impl Measurement {
        pub fn new(qubit: Qubit, target: Option<MemoryReference>);
    }
}

impl Quil for Measurement {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> Result<(), crate::quil::ToQuilError> {
        write!(writer, "MEASURE ")?;
        self.qubit.write(writer, fall_back_to_debug)?;
        if let Some(target) = &self.target {
            write!(writer, " ")?;
            target.write(writer, fall_back_to_debug)?;
        }

        Ok(())
    }
}
