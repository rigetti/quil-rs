use quil_rs::{
    expression::Expression,
    instruction::{Calibration, GateModifier, Instruction, MeasureCalibrationDefinition, Qubit},
};

use rigetti_pyo3::{
    impl_repr, impl_str, py_wrap_data_struct,
    pyo3::{
        pyclass::CompareOp, pymethods, types::PyString, IntoPy, Py, PyObject, PyResult, Python,
    },
    PyTryFrom, PyWrapper, ToPythonError,
};

use crate::{
    expression::PyExpression,
    instruction::{PyGateModifier, PyInstruction, PyQubit},
    validation::identifier::RustIdentifierValidationError,
};

py_wrap_data_struct! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass)]
    PyCalibration(Calibration) as "Calibration" {
        instructions: Vec<Instruction> => Vec<PyInstruction>,
        modifiers: Vec<GateModifier> => Vec<PyGateModifier>,
        name: String => Py<PyString>,
        parameters: Vec<Expression> => Vec<PyExpression>,
        qubits: Vec<Qubit> => Vec<PyQubit>
    }
}
impl_repr!(PyCalibration);
impl_str!(PyCalibration);

#[pymethods]
impl PyCalibration {
    #[new]
    pub fn new(
        py: Python<'_>,
        name: &str,
        parameters: Vec<PyExpression>,
        qubits: Vec<PyQubit>,
        instructions: Vec<PyInstruction>,
        modifiers: Vec<PyGateModifier>,
    ) -> PyResult<Self> {
        Ok(Self(
            Calibration::new(
                name,
                Vec::<Expression>::py_try_from(py, &parameters)?,
                Vec::<Qubit>::py_try_from(py, &qubits)?,
                Vec::<Instruction>::py_try_from(py, &instructions)?,
                Vec::<GateModifier>::py_try_from(py, &modifiers)?,
            )
            .map_err(RustIdentifierValidationError::from)
            .map_err(RustIdentifierValidationError::to_py_err)?,
        ))
    }

    pub fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self.as_inner() == other.as_inner()).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass)]
    PyMeasureCalibrationDefinition(MeasureCalibrationDefinition) as "MeasureCalibrationDefinition" {
        qubit: Option<Qubit> => Option<PyQubit>,
        parameter: String => Py<PyString>,
        instructions: Vec<Instruction> => Vec<PyInstruction>
    }
}
impl_repr!(PyMeasureCalibrationDefinition);
impl_str!(PyMeasureCalibrationDefinition);

#[pymethods]
impl PyMeasureCalibrationDefinition {
    #[new]
    pub fn new(
        py: Python<'_>,
        qubit: Option<PyQubit>,
        parameter: String,
        instructions: Vec<PyInstruction>,
    ) -> PyResult<Self> {
        Ok(Self(MeasureCalibrationDefinition::new(
            Option::<Qubit>::py_try_from(py, &qubit)?,
            parameter,
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
