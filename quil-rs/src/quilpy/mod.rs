use pyo3::prelude::*;
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
        "waveforms": waveform::quilpy::init_submodule
    ],
}

#[pymodule]
#[pyo3(name = "_quil")]
fn init_module(m: &Bound<'_, PyModule>) -> PyResult<()> {
    init_submodule("quil", m.py(), m)
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
