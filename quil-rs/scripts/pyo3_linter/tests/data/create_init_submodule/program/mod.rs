#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::gen_stub_pyclass;

#[cfg(feature = "python")]
pub(crate) mod quilpy;

#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(feature = "python", pyo3::pyclass(module = "quil.program"))]
pub struct Program {
    pub instructions: Vec<String>,
}

#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(feature = "python", pyo3::pyclass(module = "quil.program"))]
pub struct CalibrationSet {
    pub calibrations: Vec<String>,
}
