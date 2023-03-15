use quil_rs::{instruction::Vector, program::MemoryRegion};
use rigetti_pyo3::{
    impl_hash, impl_repr, py_wrap_data_struct,
    pyo3::{pymethods, types::PyString, Py, PyResult, Python},
    PyTryFrom,
};

use crate::instruction::PyVector;

py_wrap_data_struct! {
    #[derive(Debug, Eq, PartialEq, Hash)]
    #[pyo3(subclass)]
    PyMemoryRegion(MemoryRegion) as "MemoryRegion" {
        size: Vector => PyVector,
        sharing: Option<String> => Option<Py<PyString>>
    }
}
impl_repr!(PyMemoryRegion);
impl_hash!(PyMemoryRegion);

#[pymethods]
impl PyMemoryRegion {
    #[new]
    pub fn new(py: Python<'_>, size: PyVector, sharing: Option<String>) -> PyResult<Self> {
        Ok(Self(MemoryRegion::new(
            Vector::py_try_from(py, &size)?,
            sharing,
        )))
    }
}
