use pyo3::prelude::*;
use rigetti_pyo3::create_init_submodule;

use program::{calibration_set::PyCalibrationSet, PyProgram};

pub mod instruction;
pub mod program;

create_init_submodule! {
    classes: [ PyProgram, PyCalibrationSet ],
    submodules: [ "instructions": instruction::init_submodule ],
}

#[pymodule]
fn quil(py: Python<'_>, m: &PyModule) -> PyResult<()> {
    init_submodule("quil", py, m)?;
    Ok(())
}

pub fn init_quil_submodule(name: &str, py: Python<'_>, m: &PyModule) -> PyResult<()> {
    init_submodule(name, py, m)?;
    Ok(())
}
