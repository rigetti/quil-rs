use pyo3::prelude::*;
use quil_rs::{
    expression::Expression,
    instruction::{GateModifier, Qubit},
    program::CalibrationSet,
};
use rigetti_pyo3::{impl_repr, py_wrap_type, PyTryFrom, PyWrapper};

use crate::instruction::{
    PyCalibration, PyExpression, PyGateModifier, PyMeasureCalibrationDefinition, PyQubit,
};

py_wrap_type! {
    PyCalibrationSet(CalibrationSet) as "CalibrationSet"
}

#[pymethods]
impl PyCalibrationSet {
    #[new]
    pub fn new(
        py: Python<'_>,
        calibrations: Vec<PyCalibration>,
        measure_calibrations: Vec<PyMeasureCalibrationDefinition>,
    ) -> PyResult<Self> {
        Ok(Self(CalibrationSet {
            calibrations: Vec::<_>::py_try_from(py, &calibrations)?,
            measure_calibrations: Vec::<_>::py_try_from(py, &measure_calibrations)?,
        }))
    }

    pub fn get_match_for_gate(
        &self,
        py: Python<'_>,
        gate_modifiers: Vec<PyGateModifier>,
        gate_name: &str,
        gate_parameters: Vec<PyExpression>,
        gate_qubits: Vec<PyQubit>,
    ) -> PyResult<Option<PyCalibration>> {
        Ok(self
            .as_inner()
            .get_match_for_gate(
                &Vec::<GateModifier>::py_try_from(py, &gate_modifiers)?,
                gate_name,
                &Vec::<Expression>::py_try_from(py, &gate_parameters)?,
                &Vec::<Qubit>::py_try_from(py, &gate_qubits)?,
            )
            .map(PyCalibration::from))
    }

    pub fn __len__(&self) -> usize {
        self.as_inner().len()
    }

    pub fn is_empty(&self) -> bool {
        self.as_inner().is_empty()
    }
}

impl_repr!(PyCalibrationSet);
