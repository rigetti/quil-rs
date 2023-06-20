use quil_rs::instruction::{Measurement, MemoryReference, Qubit};
use rigetti_pyo3::{
    impl_hash, impl_repr, impl_str, py_wrap_data_struct,
    pyo3::{pyclass::CompareOp, pymethods, IntoPy, PyObject, PyResult, Python},
    PyTryFrom, PyWrapper,
};

use crate::instruction::{PyMemoryReference, PyQubit};

py_wrap_data_struct! {
    #[pyo3(subclass)]
    #[derive(Debug, PartialEq, Eq)]
    PyMeasurement(Measurement) as "Measurement" {
        qubit: Qubit => PyQubit,
        target: Option<MemoryReference> => Option<PyMemoryReference>
    }
}
impl_str!(PyMeasurement);
impl_hash!(PyMeasurement);
impl_repr!(PyMeasurement);

#[pymethods]
impl PyMeasurement {
    #[new]
    pub fn new(
        py: Python<'_>,
        qubit: PyQubit,
        target: Option<PyMemoryReference>,
    ) -> PyResult<PyMeasurement> {
        Ok(Self(Measurement::new(
            Qubit::py_try_from(py, &qubit)?,
            Option::<MemoryReference>::py_try_from(py, &target)?,
        )))
    }

    pub fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self.as_inner() == other.as_inner()).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}
