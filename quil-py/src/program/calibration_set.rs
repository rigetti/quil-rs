use pyo3::prelude::*;
use quil_rs::program::CalibrationSet;
use rigetti_pyo3::{impl_repr, py_wrap_type, PyTryFrom};

use crate::instruction::{PyCalibration, PyMeasureCalibrationDefinition};

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
}

impl_repr!(PyCalibrationSet);
