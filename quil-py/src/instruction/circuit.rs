use quil_rs::instruction::{CircuitDefinition, Instruction};

use rigetti_pyo3::{
    impl_hash, impl_repr, impl_str, py_wrap_data_struct,
    pyo3::{
        pyclass::CompareOp, pymethods, types::PyString, IntoPy, Py, PyObject, PyResult, Python,
    },
    PyTryFrom, PyWrapper,
};

use super::PyInstruction;

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
impl_str!(PyCircuitDefinition);
impl_hash!(PyCircuitDefinition);

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
