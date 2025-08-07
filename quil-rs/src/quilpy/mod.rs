use pyo3::{prelude::*, types::PyDict, wrap_pymodule};

#[cfg(feature = "stubs")]
use pyo3_stub_gen::define_stub_info_gatherer;

use crate::expression;
use crate::instruction;
use crate::program;
use crate::validation;
use crate::waveform;

pub(crate) mod errors;

#[pymodule]
#[pyo3(name = "_quil")]
fn init_module(m: &Bound<'_, PyModule>) -> PyResult<()> {
    use crate::quilpy::errors;

    let py = m.py();

    m.add_wrapped(wrap_pymodule!(expression::quilpy::init_submodule))?;
    m.add_wrapped(wrap_pymodule!(instruction::quilpy::init_submodule))?;
    m.add_wrapped(wrap_pymodule!(program::quilpy::init_submodule))?;
    m.add_wrapped(wrap_pymodule!(validation::quilpy::init_submodule))?;
    m.add_wrapped(wrap_pymodule!(waveform::quilpy::init_submodule))?;

    m.add("QuilError", py.get_type::<errors::QuilError>())?;
    m.add("ToQuilError", py.get_type::<errors::ToQuilError>())?;

    let sys = PyModule::import(py, "sys")?;
    let sys_modules: Bound<'_, PyDict> = sys.getattr("modules")?.downcast_into()?;
    sys_modules.set_item("quil.expression", m.getattr("expression")?)?;
    sys_modules.set_item("quil.instructions", m.getattr("instructions")?)?;
    sys_modules.set_item("quil.program", m.getattr("program")?)?;
    // validation is added below
    sys_modules.set_item("quil.waveforms", m.getattr("waveforms")?)?;

    let validation_module = m.getattr("validation")?;
    sys_modules.set_item(
        "quil.validation.identifier",
        validation_module.getattr("identifier")?,
    )?;
    sys_modules.set_item("quil.validation", validation_module)?;
    Ok(())
}

/// Add Python `to_quil` and `to_quil_or_debug` methods
/// for types that implements [`Quil`](quil_rs::quil::Quil).
macro_rules! impl_to_quil {
    ($name: ident) => {
        #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
        #[pyo3::pymethods]
        impl $name {
            #[pyo3(name = "to_quil")]
            fn py_to_quil(&self) -> pyo3::PyResult<String> {
                Ok(self.to_quil()?)
            }

            #[pyo3(name = "to_quil_or_debug")]
            fn py_to_quil_or_debug(&self) -> String {
                self.to_quil_or_debug()
            }
        }
    };
}

/// Add a `__repr__` method that returns the Rust type's `Debug` string.
macro_rules! impl_repr {
    ($name: ident) => {
        #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
        #[pyo3::pymethods]
        impl $name {
            fn __repr__(&self) -> String {
                format!("{self:?}")
            }
        }
    };
}

pub(crate) use impl_repr;
pub(crate) use impl_to_quil;

#[cfg(feature = "stubs")]
define_stub_info_gatherer!(stub_info);
