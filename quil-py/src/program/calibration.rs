use pyo3::prelude::*;
use quil_rs::{
    expression::Expression,
    instruction::{Calibration, GateModifier, Instruction, MeasureCalibrationDefinition, Qubit},
    program::CalibrationSet,
};
use rigetti_pyo3::{
    impl_as_mut_for_wrapper, impl_repr, py_wrap_type, PyTryFrom, PyWrapper, PyWrapperMut, ToPython,
    ToPythonError,
};

use crate::instruction::{
    PyCalibration, PyExpression, PyGateModifier, PyInstruction, PyMeasureCalibrationDefinition,
    PyQubit,
};

use super::ProgramError;

py_wrap_type! {
    PyCalibrationSet(CalibrationSet) as "CalibrationSet"
}
impl_as_mut_for_wrapper!(PyCalibrationSet);
impl_repr!(PyCalibrationSet);

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

    pub fn expand(
        &self,
        py: Python<'_>,
        instruction: PyInstruction,
        previous_calibrations: Vec<PyInstruction>,
    ) -> PyResult<Option<Vec<PyInstruction>>> {
        self.as_inner()
            .expand(
                &Instruction::py_try_from(py, &instruction)?,
                &Vec::<Instruction>::py_try_from(py, &previous_calibrations)?,
            )
            .map_err(ProgramError::from)
            .map_err(ProgramError::to_py_err)?
            .to_python(py)
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

    pub fn push_calibration(&mut self, py: Python<'_>, calibration: PyCalibration) -> PyResult<()> {
        self.as_inner_mut()
            .push_calibration(Calibration::py_try_from(py, &calibration)?);
        Ok(())
    }

    pub fn push_measurement_calibration(
        &mut self,
        py: Python<'_>,
        calibration: PyMeasureCalibrationDefinition,
    ) -> PyResult<()> {
        self.as_inner_mut().push_measurement_calibration(
            MeasureCalibrationDefinition::py_try_from(py, &calibration)?,
        );
        Ok(())
    }

    pub fn extend(&mut self, py: Python<'_>, other: PyCalibrationSet) -> PyResult<()> {
        self.as_inner_mut()
            .extend(CalibrationSet::py_try_from(py, &other)?);
        Ok(())
    }

    pub fn to_instructions(&self, py: Python<'_>) -> PyResult<Vec<PyInstruction>> {
        self.as_inner().to_instructions().to_python(py)
    }
}
