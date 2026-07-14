use pyo3::{prelude::*, PyClass};
use rigetti_pyo3::create_init_submodule;

#[cfg(feature = "stubs")]
use pyo3_stub_gen::define_stub_info_gatherer;

use crate::expression;
use crate::instruction;
use crate::program;
use crate::validation;
use crate::waveform;

pub(crate) mod errors;

create_init_submodule! {
    errors: [
        errors::QuilError,
        errors::ValueError,
        errors::ToQuilStringError,
        errors::PickleError
    ],
    submodules: [
        "expression": expression::quilpy::init_submodule,
        "instructions": instruction::quilpy::init_submodule,
        "program": program::quilpy::init_submodule,
        "validation": validation::quilpy::init_submodule,
        "waveform": waveform::quilpy::init_submodule
    ],
}

#[pymodule]
#[pyo3(name = "_quil")]
fn init_module(m: &Bound<'_, PyModule>) -> PyResult<()> {
    let py = m.py();
    init_submodule("quil", py, m)?;
    waveform::sampling::quilpy::register_abcs(py)?;
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

pub(crate) use impl_to_quil;

#[cfg(feature = "stubs")]
define_stub_info_gatherer!(stub_info);

/// An alternative to [`PyAnyMethods::extract`] that only fails if there are multithreaded mutable
/// borrows; if casting would fail, it returns [`None`] instead.  This allows avoiding [the
/// performance hit due to allocation on `extract`
/// failing](https://pyo3.rs/v0.29.0/performance.html#extract-versus-cast).  It is for use in
/// situations where you want to handle failure internally to the Rust function; if you want a good
/// Python-facing error message, just use `extract`.
pub(crate) fn py_cast_and_borrow<'a, 'py, T: PyClass + FromPyObject<'a, 'py>>(
    obj: &'a Bound<'py, PyAny>,
) -> PyResult<Option<PyRef<'py, T>>> {
    obj.cast::<T>()
        .ok()
        .map(Bound::try_borrow)
        .transpose()
        .map_err(Into::into)
}

/// Like [`py_cast_and_borrow`], but clones the pointed-to value to get an owned version.
pub(crate) fn py_cast_and_clone<'a, 'py, T: PyClass + FromPyObject<'a, 'py> + Clone>(
    obj: &'a Bound<'py, PyAny>,
) -> PyResult<Option<T>> {
    py_cast_and_borrow(obj).map(|obj| obj.as_deref().cloned())
}
