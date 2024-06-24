use quil_rs::expression::Expression;
use quil_rs::instruction::{Delay, Fence, Qubit};

use rigetti_pyo3::{
    impl_hash, impl_repr, py_wrap_data_struct,
    pyo3::{pymethods, types::PyString, Py, PyResult, Python},
    PyTryFrom,
};

use super::PyQubit;
use crate::{expression::PyExpression, impl_copy_for_instruction, impl_eq, impl_to_quil};

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
impl_to_quil!(PyDelay);
impl_copy_for_instruction!(PyDelay);
impl_hash!(PyDelay);
impl_eq!(PyDelay);

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
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyFence(Fence) as "Fence" {
        qubits: Vec<Qubit> => Vec<PyQubit>
    }
}
impl_repr!(PyFence);
impl_to_quil!(PyFence);
impl_copy_for_instruction!(PyFence);
impl_hash!(PyFence);
impl_eq!(PyFence);

#[pymethods]
impl PyFence {
    #[new]
    pub fn new(py: Python<'_>, qubits: Vec<PyQubit>) -> PyResult<Self> {
        Ok(Self(Fence::new(Vec::<Qubit>::py_try_from(py, &qubits)?)))
    }
}
