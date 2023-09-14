use quil_rs::instruction::{CircuitDefinition, Instruction};

use rigetti_pyo3::{
    impl_repr, py_wrap_data_struct,
    pyo3::{
        pyclass::CompareOp, pymethods, types::PyString, IntoPy, Py, PyObject, PyResult, Python,
    },
    PyTryFrom, PyWrapper,
};

use super::PyInstruction;
use crate::{impl_copy_for_instruction, impl_to_quil};

py_wrap_data_struct! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass)]
    PyCircuitDefinition(CircuitDefinition) as "CircuitDefinition" {
        name: String => Py<PyString>,
        parameters: Vec<String> => Vec<Py<PyString>>,
        qubit_variables: Vec<String> => Vec<Py<PyString>>,
        instructions: Vec<Instruction> => Vec<PyInstruction>
    }
}
impl_repr!(PyCircuitDefinition);
impl_to_quil!(PyCircuitDefinition);
impl_copy_for_instruction!(PyCircuitDefinition);

#[pymethods]
impl PyCircuitDefinition {
    #[new]
    pub fn new(
        py: Python<'_>,
        name: String,
        parameters: Vec<String>,
        qubit_variables: Vec<String>,
        instructions: Vec<PyInstruction>,
    ) -> PyResult<Self> {
        Ok(Self(CircuitDefinition::new(
            name,
            parameters,
            qubit_variables,
            Vec::<Instruction>::py_try_from(py, &instructions)?,
        )))
    }

    pub fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self.as_inner() == other.as_inner()).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}
