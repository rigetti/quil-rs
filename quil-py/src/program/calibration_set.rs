use pyo3::prelude::*;
use quil_rs::{
    instruction::{Calibration, MeasureCalibrationDefinition},
    program::CalibrationSet,
};
use rigetti_pyo3::{impl_repr, py_wrap_data_struct, PyTryFrom};

use crate::instruction::calibration::{PyCalibration, PyMeasureCalibrationDefinition};

py_wrap_data_struct! {
    PyCalibrationSet(CalibrationSet) as "CalibrationSet" {
        calibrations: Vec<Calibration> => Vec<PyCalibration>,
        measure_calibrations: Vec<MeasureCalibrationDefinition> => Vec<PyMeasureCalibrationDefinition>
    }
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
}

impl_repr!(PyCalibrationSet);
