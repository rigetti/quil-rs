use quil_rs::{
    instruction::{Sharing, Vector},
    program::MemoryRegion,
};
use rigetti_pyo3::{
    impl_hash, impl_repr, py_wrap_data_struct,
    pyo3::{pyclass::CompareOp, pymethods, IntoPy, PyObject, PyResult, Python},
    PyTryFrom, PyWrapper,
};

use crate::instruction::{PySharing, PyVector};

py_wrap_data_struct! {
    #[derive(Debug, Eq, PartialEq, Hash)]
    #[pyo3(subclass)]
    PyMemoryRegion(MemoryRegion) as "MemoryRegion" {
        size: Vector => PyVector,
        sharing: Option<Sharing> => Option<PySharing>
    }
}
impl_repr!(PyMemoryRegion);
impl_hash!(PyMemoryRegion);

#[pymethods]
impl PyMemoryRegion {
    #[new]
    pub fn new(py: Python<'_>, size: PyVector, sharing: Option<PySharing>) -> PyResult<Self> {
        Ok(Self(MemoryRegion::new(
            Vector::py_try_from(py, &size)?,
            Option::<Sharing>::py_try_from(py, &sharing)?,
        )))
    }

    pub fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self.as_inner() == other.as_inner()).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}
