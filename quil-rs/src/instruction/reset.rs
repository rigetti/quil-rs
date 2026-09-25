#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::gen_stub_pyclass;

use crate::{pickleable_new, quil::Quil};

use super::Qubit;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(
        module = "quil._quil.instructions",
        eq,
        frozen,
        hash,
        get_all,
        subclass,
        from_py_object
    )
)]
pub struct Reset {
    pub name: Option<String>,
    pub qubit: Option<Qubit>,
}

pickleable_new! {
    impl Reset {
        pub fn new(name: Option<String>, qubit: Option<Qubit>);
    }
}

impl Quil for Reset {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        let Self { name, qubit } = self;

        write!(writer, "RESET")?;

        if let Some(name) = name {
            write!(writer, "!{name}")?;
        }

        if let Some(qubit) = qubit {
            write!(writer, " ")?;
            qubit.write(writer, fall_back_to_debug)?;
        }

        Ok(())
    }
}
