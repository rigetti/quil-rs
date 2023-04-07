use quil_rs::instruction::{Qubit, Reset};

use rigetti_pyo3::{
    impl_repr, impl_str, py_wrap_data_struct,
    pyo3::{pyclass::CompareOp, pymethods, IntoPy, PyObject, PyResult, Python},
    PyTryFrom, PyWrapper,
};

use crate::instruction::PyQubit;

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyReset(Reset) as "Reset" {
        qubit: Option<Qubit> => Option<PyQubit>
    }
}
impl_repr!(PyReset);
impl_str!(PyReset);

#[pymethods]
impl PyReset {
    #[new]
    fn new(py: Python<'_>, qubit: Option<PyQubit>) -> PyResult<Self> {
        Ok(Self(Reset::new(Option::<Qubit>::py_try_from(py, &qubit)?)))
    }

    pub fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self.as_inner() == other.as_inner()).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}
