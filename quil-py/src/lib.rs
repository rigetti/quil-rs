use pyo3::prelude::*;
use rigetti_pyo3::create_init_submodule;

pub mod expression;
pub mod instruction;
pub mod program;
pub mod validation;

create_init_submodule! {
    submodules: [
        "expression": expression::init_submodule,
        "instructions": instruction::init_submodule,
        "program": program::init_submodule,
        "validation": validation::init_submodule
    ],
}

#[pymodule]
fn quil(py: Python<'_>, m: &PyModule) -> PyResult<()> {
    init_submodule("quil", py, m)?;
    Ok(())
}

pub fn init_quil_submodule(name: &str, py: Python<'_>, m: &PyModule) -> PyResult<()> {
    init_submodule(name, py, m)?;
    Ok(())
}

/// Implement `to_quil` and `to_quil_or_debug` methods for wrapper types whose inner type
/// implements [`Quil`](quil_rs::quil::Quil).
#[macro_export]
macro_rules! impl_to_quil {
    ($name: ident) => {
        #[pyo3::pymethods]
        impl $name {
            pub fn to_quil(&self) -> pyo3::PyResult<String> {
                quil_rs::quil::Quil::to_quil(rigetti_pyo3::PyWrapper::as_inner(self))
                    .map_err(|e| pyo3::exceptions::PyValueError::new_err(e.to_string()))
            }

            pub fn to_quil_or_debug(&self) -> String {
                quil_rs::quil::Quil::to_quil_or_debug(rigetti_pyo3::PyWrapper::as_inner(self))
            }
        }
    };
}

#[macro_export]
macro_rules! impl_eq {
    ($name: ident) => {
        #[pyo3::pymethods]
        impl $name {
            pub fn __richcmp__(
                &self,
                py: pyo3::Python<'_>,
                other: &Self,
                op: pyo3::pyclass::CompareOp,
            ) -> pyo3::PyObject {
                use pyo3::IntoPy;
                match op {
                    pyo3::pyclass::CompareOp::Eq => (self == other).into_py(py),
                    pyo3::pyclass::CompareOp::Ne => (self != other).into_py(py),
                    _ => py.NotImplemented(),
                }
            }
        }
    };
}
