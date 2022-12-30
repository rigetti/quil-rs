use quil_rs::{instruction::Calibration, program::CalibrationSet};
use rigetti_pyo3::py_wrap_data_struct;

use crate::instruction::calibration::PyCalibrations;

py_wrap_data_struct! {
    PyCalibrationSet(CalibrationSet) as "CalibrationSet" {
        calibrations: Vec::<Calibration> => PyCalibrations
        // measure_calibrations: Vec::<MeasureCalibrationDefinition> =>
        // PyMeasureCalibrationDefinitions
    }
}
