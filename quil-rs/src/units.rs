#[derive(
    Clone,
    Copy,
    Debug,
    PartialEq,
    serde::Serialize,
    serde::Deserialize,
    derive_more::Add,
    derive_more::AddAssign,
    derive_more::Sub,
    derive_more::SubAssign,
    derive_more::Neg,
)]
#[cfg_attr(feature = "python", derive(pyo3::FromPyObject, pyo3::IntoPyObject))]
#[cfg_attr(feature = "python", pyo3(transparent))]
pub struct Cycles<T>(pub T);

impl<S> Cycles<S> {
    pub fn map<T>(self, f: impl FnOnce(S) -> T) -> Cycles<T> {
        Cycles(f(self.0))
    }

    pub fn try_map<T, E>(self, f: impl FnOnce(S) -> Result<T, E>) -> Result<Cycles<T>, E> {
        Ok(Cycles(f(self.0)?))
    }
}

#[derive(
    Clone,
    Copy,
    Debug,
    PartialEq,
    serde::Serialize,
    serde::Deserialize,
    derive_more::Add,
    derive_more::AddAssign,
    derive_more::Sub,
    derive_more::SubAssign,
    derive_more::Neg,
)]
#[cfg_attr(feature = "python", derive(pyo3::FromPyObject, pyo3::IntoPyObject))]
#[cfg_attr(feature = "python", pyo3(transparent))]
pub struct Radians<T>(pub T);

impl<S> Radians<S> {
    pub fn map<T>(self, f: impl FnOnce(S) -> T) -> Radians<T> {
        Radians(f(self.0))
    }

    pub fn try_map<T, E>(self, f: impl FnOnce(S) -> Result<T, E>) -> Result<Radians<T>, E> {
        Ok(Radians(f(self.0)?))
    }
}

impl From<Cycles<f64>> for Radians<f64> {
    #[inline]
    fn from(cycles: Cycles<f64>) -> Self {
        Radians(cycles.0 * 2.0 * std::f64::consts::PI)
    }
}

impl From<Radians<f64>> for Cycles<f64> {
    #[inline]
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
