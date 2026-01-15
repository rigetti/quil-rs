use pyo3::prelude::*;
use rigetti_pyo3::create_init_submodule;

use crate::expression;
use crate::program;

pub(crate) mod errors;

create_init_submodule! {
    errors: [
        errors::QuilError,
        errors::ValueError,
    ],
    submodules: [
        "expression": expression::quilpy::init_submodule,
        "program": program::quilpy::init_submodule,
    ],
}

#[pymodule]
#[pyo3(name = "_quil")]
fn init_module(m: &Bound<'_, PyModule>) -> PyResult<()> {
    init_submodule("quil", m.py(), m)
}
