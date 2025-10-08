#[derive(Clone, Copy, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "python", derive(pyo3::FromPyObject, pyo3::IntoPyObject))]
#[cfg_attr(feature = "python", pyo3(transparent))]
pub struct Cycles<T>(pub T);

#[derive(Clone, Copy, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "python", derive(pyo3::FromPyObject, pyo3::IntoPyObject))]
#[cfg_attr(feature = "python", pyo3(transparent))]
pub struct Radians<T>(pub T);

impl From<Cycles<f64>> for Radians<f64> {
    fn from(cycles: Cycles<f64>) -> Self {
        Radians(cycles.0 * 2.0 * std::f64::consts::PI)
    }
}

impl From<Radians<f64>> for Cycles<f64> {
    fn from(radians: Radians<f64>) -> Self {
        Cycles(radians.0 / (2.0 * std::f64::consts::PI))
    }
}

#[cfg(feature = "stubs")]
impl pyo3_stub_gen::PyStubType for Cycles<f64> {
    fn type_output() -> pyo3_stub_gen::TypeInfo {
        pyo3_stub_gen::TypeInfo::builtin("float")
    }
}

#[cfg(feature = "stubs")]
impl pyo3_stub_gen::PyStubType for Radians<f64> {
    fn type_output() -> pyo3_stub_gen::TypeInfo {
        pyo3_stub_gen::TypeInfo::builtin("float")
    }
}
