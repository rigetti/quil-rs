use quil_rs::expression::Expression;
use quil_rs::instruction::{Delay, Fence, Qubit};

use rigetti_pyo3::{
    impl_hash, impl_repr, py_wrap_data_struct,
    pyo3::{
        pyclass::CompareOp, pymethods, types::PyString, IntoPy, Py, PyObject, PyResult, Python,
    },
    PyTryFrom, PyWrapper,
};

use super::PyQubit;
use crate::{expression::PyExpression, impl_quil};

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyDelay(Delay) as "Delay" {
        duration: Expression => PyExpression,
        frame_names: Vec<String> => Vec<Py<PyString>>,
        qubits: Vec<Qubit> => Vec<PyQubit>
    }
}
impl_repr!(PyDelay);
impl_quil!(PyDelay);
impl_hash!(PyDelay);

#[pymethods]
impl PyDelay {
    #[new]
    pub fn new(
        py: Python<'_>,
        duration: PyExpression,
        frame_names: Vec<String>,
        qubits: Vec<PyQubit>,
    ) -> PyResult<Self> {
        Ok(Self(Delay::new(
            Expression::py_try_from(py, &duration)?,
            frame_names,
            Vec::<Qubit>::py_try_from(py, &qubits)?,
        )))
    }

    pub fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self.as_inner() == other.as_inner()).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyFence(Fence) as "Fence" {
        qubits: Vec<Qubit> => Vec<PyQubit>
    }
}
impl_repr!(PyFence);
impl_quil!(PyFence);
impl_hash!(PyFence);

#[pymethods]
impl PyFence {
    #[new]
    pub fn new(py: Python<'_>, qubits: Vec<PyQubit>) -> PyResult<Self> {
        Ok(Self(Fence::new(Vec::<Qubit>::py_try_from(py, &qubits)?)))
    }

    pub fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self.as_inner() == other.as_inner()).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}
