use quil_rs::{
    expression::Expression,
    instruction::{
        Calibration, CalibrationIdentifier, GateModifier, Instruction,
        MeasureCalibrationDefinition, MeasureCalibrationIdentifier, Qubit,
    },
};

use rigetti_pyo3::{
    impl_repr, py_wrap_data_struct,
    pyo3::{pymethods, types::PyString, Py, PyResult, Python},
    PyTryFrom, PyWrapper, ToPythonError,
};

use crate::{
    expression::PyExpression,
    impl_copy_for_instruction, impl_eq, impl_pickle_for_instruction, impl_to_quil,
    instruction::{PyGateModifier, PyInstruction, PyQubit},
    validation::identifier::RustIdentifierValidationError,
};

py_wrap_data_struct! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass, module = "quil.instructions")]
    PyCalibration(Calibration) as "Calibration" {
        identifier: CalibrationIdentifier => PyCalibrationIdentifier,
        instructions: Vec<Instruction> => Vec<PyInstruction>
    }
}
impl_repr!(PyCalibration);
impl_to_quil!(PyCalibration);
impl_copy_for_instruction!(PyCalibration);
impl_eq!(PyCalibration);
impl_pickle_for_instruction!(PyCalibration);

#[pymethods]
impl PyCalibration {
    #[new]
    pub fn new(
        py: Python<'_>,
        identifier: PyCalibrationIdentifier,
        instructions: Vec<PyInstruction>,
    ) -> PyResult<Self> {
        Ok(Self(
            Calibration::new(
                CalibrationIdentifier::py_try_from(py, &identifier)?,
                Vec::<Instruction>::py_try_from(py, &instructions)?,
            )
            .map_err(RustIdentifierValidationError::from)
            .map_err(RustIdentifierValidationError::to_py_err)?,
        ))
    }

    pub fn name(&self) -> &str {
        &self.as_inner().identifier.name
    }

    pub fn parameters(&self) -> Vec<PyExpression> {
        self.as_inner()
            .identifier
            .parameters
            .clone()
            .into_iter()
            .map(Into::into)
            .collect()
    }

    pub fn qubits(&self) -> Vec<PyQubit> {
        self.as_inner()
            .identifier
            .qubits
            .clone()
            .into_iter()
            .map(Into::into)
            .collect()
    }

    pub fn modifiers(&self) -> Vec<PyGateModifier> {
        self.as_inner()
            .identifier
            .modifiers
            .clone()
            .into_iter()
            .map(Into::into)
            .collect()
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass)]
    PyCalibrationIdentifier(CalibrationIdentifier) as "CalibrationIdentifier" {
        modifiers: Vec<GateModifier> => Vec<PyGateModifier>,
        name: String => Py<PyString>,
        parameters: Vec<Expression> => Vec<PyExpression>,
        qubits: Vec<Qubit> => Vec<PyQubit>
    }
}
impl_repr!(PyCalibrationIdentifier);
impl_to_quil!(PyCalibrationIdentifier);
impl_copy_for_instruction!(PyCalibrationIdentifier);
impl_eq!(PyCalibrationIdentifier);

#[pymethods]
impl PyCalibrationIdentifier {
    #[new]
    pub fn new(
        py: Python<'_>,
        name: &str,
        parameters: Vec<PyExpression>,
        qubits: Vec<PyQubit>,
        modifiers: Vec<PyGateModifier>,
    ) -> PyResult<Self> {
        Ok(Self(
            CalibrationIdentifier::new(
                name.to_string(),
                Vec::<GateModifier>::py_try_from(py, &modifiers)?,
                Vec::<Expression>::py_try_from(py, &parameters)?,
                Vec::<Qubit>::py_try_from(py, &qubits)?,
            )
            .map_err(RustIdentifierValidationError::from)
            .map_err(RustIdentifierValidationError::to_py_err)?,
        ))
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass, module = "quil.instructions")]
    PyMeasureCalibrationDefinition(MeasureCalibrationDefinition) as "MeasureCalibrationDefinition" {
        identifier: MeasureCalibrationIdentifier => PyMeasureCalibrationIdentifier,
        instructions: Vec<Instruction> => Vec<PyInstruction>
    }
}
impl_repr!(PyMeasureCalibrationDefinition);
impl_to_quil!(PyMeasureCalibrationDefinition);
impl_copy_for_instruction!(PyMeasureCalibrationDefinition);
impl_eq!(PyMeasureCalibrationDefinition);
impl_pickle_for_instruction!(PyMeasureCalibrationDefinition);

#[pymethods]
impl PyMeasureCalibrationDefinition {
    #[new]
    #[pyo3(signature = (identifier, instructions))]
    pub fn new(
        py: Python<'_>,
        identifier: PyMeasureCalibrationIdentifier,
        instructions: Vec<PyInstruction>,
    ) -> PyResult<Self> {
        Ok(Self(MeasureCalibrationDefinition::new(
            MeasureCalibrationIdentifier::py_try_from(py, &identifier)?,
            Vec::<Instruction>::py_try_from(py, &instructions)?,
        )))
    }

    pub fn qubit(&self) -> Option<PyQubit> {
        self.as_inner().identifier.qubit.clone().map(Into::into)
    }

    pub fn parameter(&self) -> String {
        self.as_inner().identifier.parameter.clone()
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass)]
    PyMeasureCalibrationIdentifier(MeasureCalibrationIdentifier) as "MeasureCalibrationIdentifier" {
        qubit: Option<Qubit> => Option<PyQubit>,
        parameter: String => Py<PyString>
    }
}
impl_repr!(PyMeasureCalibrationIdentifier);
impl_to_quil!(PyMeasureCalibrationIdentifier);
impl_copy_for_instruction!(PyMeasureCalibrationIdentifier);
impl_eq!(PyMeasureCalibrationIdentifier);

#[pymethods]
impl PyMeasureCalibrationIdentifier {
    #[new]
    #[pyo3(signature = (qubit, parameter))]
    pub fn new(py: Python<'_>, qubit: Option<PyQubit>, parameter: String) -> PyResult<Self> {
        Ok(Self(MeasureCalibrationIdentifier::new(
            Option::<Qubit>::py_try_from(py, &qubit)?,
            parameter,
        )))
    }
}
