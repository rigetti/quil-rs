use pyo3::prelude::*;

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pymethods};

use crate::{
    units::Cycles,
    waveform::quilpy::{PyAnyRust, Pythonic},
};

use super::{BuiltinWaveform, CommonBuiltinParameters};

pub use super::quilpy_waveforms::*;

#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(module = "quil.waveform", name = "BuiltinWaveform", subclass, eq)]
pub struct PyBuiltinWaveform(pub BuiltinWaveform<Pythonic>);

#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(
    module = "quil.waveform",
    name = "CommonBuiltinParameters",
    subclass,
    eq
)]
pub struct PyCommonBuiltinParameters(pub CommonBuiltinParameters<Pythonic>);

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PyCommonBuiltinParameters {
    #[getter(duration)]
    fn py_get_duration(&self) -> f64 {
        self.0.duration
    }

    #[setter(duration)]
    fn py_set_duration(&mut self, duration: f64) {
        self.0.duration = duration;
    }

    #[getter(scale)]
    #[gen_stub(override_return_type(type_repr = "typing.Optional[Real]", imports = ("typing")))]
    fn py_get_scale<'py>(&self, py: Python<'py>) -> Option<&Bound<'py, PyAny>> {
        self.0.scale.as_ref().map(|scale| scale.0.bind(py))
    }

    #[setter(scale)]
    fn py_set_scale(
        &mut self,
        #[gen_stub(override_type(type_repr = "typing.Optional[Real]", imports = ("typing")))]
        scale: Option<Py<PyAny>>,
    ) {
        self.0.scale = scale.map(PyAnyRust);
    }

    #[getter(phase)]
    #[gen_stub(override_return_type(type_repr = "typing.Optional[Real]", imports = ("typing")))]
    fn py_get_phase<'py>(&self, py: Python<'py>) -> Option<&Bound<'py, PyAny>> {
        self.0.phase.as_ref().map(|Cycles(phase)| phase.0.bind(py))
    }

    #[setter(phase)]
    fn py_set_phase(
        &mut self,
        #[gen_stub(override_type(type_repr = "typing.Optional[Real]", imports = ("typing")))]
        phase: Option<Py<PyAny>>,
    ) {
        self.0.phase = phase.map(|phase| Cycles(PyAnyRust(phase)));
    }

    #[getter(detuning)]
    #[gen_stub(override_return_type(type_repr = "typing.Optional[Real]", imports = ("typing")))]
    fn py_get_detuning<'py>(&self, py: Python<'py>) -> Option<&Bound<'py, PyAny>> {
        self.0.detuning.as_ref().map(|detuning| detuning.0.bind(py))
    }

    #[setter(detuning)]
    fn py_set_detuning(
        &mut self,
        #[gen_stub(override_type(type_repr = "typing.Optional[Real]", imports = ("typing")))]
        detuning: Option<Py<PyAny>>,
    ) {
        self.0.detuning = detuning.map(PyAnyRust);
    }
}
