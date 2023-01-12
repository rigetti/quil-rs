use pyo3::prelude::*;
use rigetti_pyo3::create_init_submodule;

use instruction::{
    declaration::{PyDeclaration, PyScalarType, PyVector},
    expression::{PyExpressionFunction, PyFunctionCallExpression},
    PyInstruction,
};
use program::{calibration_set::PyCalibrationSet, PyProgram};

pub mod instruction;
pub mod program;

create_init_submodule! {
    classes: [ PyProgram, PyCalibrationSet, PyFunctionCallExpression, PyExpressionFunction, PyVector, PyScalarType, PyDeclaration, PyInstruction ],
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
