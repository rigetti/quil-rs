#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pymethods};

#[cfg(feature = "python")]
use crate::expression::Expression;

use crate::{
    units::Cycles,
    waveform::{Concrete, Syntactic},
};

use super::{BuiltinWaveform, CommonBuiltinParameters};

pub use super::quilpy_waveforms::*;

#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.waveform", subclass, eq)
)]
pub struct SyntacticBuiltinWaveform(pub BuiltinWaveform<Syntactic>);

#[derive(Clone, Copy, PartialEq, Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.waveform", subclass, eq)
)]
pub struct ConcreteBuiltinWaveform(pub BuiltinWaveform<Concrete>);

#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.waveform", subclass, eq)
)]
pub struct SyntacticCommonBuiltinParameters(pub CommonBuiltinParameters<Syntactic>);

#[derive(Clone, Copy, PartialEq, Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.waveform", subclass, eq)
)]
pub struct ConcreteCommonBuiltinParameters(pub CommonBuiltinParameters<Concrete>);

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[cfg(feature = "python")]
#[pyo3::pymethods]
impl SyntacticCommonBuiltinParameters {
    #[getter(duration)]
    fn py_get_duration(&self) -> f64 {
        self.0.duration
    }

    #[setter(duration)]
    fn py_set_duration(&mut self, duration: f64) {
        self.0.duration = duration;
    }

    #[getter(scale)]
    fn py_get_scale(&self) -> Option<Expression> {
        self.0.scale.clone()
    }

    #[setter(scale)]
    fn py_set_scale(&mut self, scale: Option<Expression>) {
        self.0.scale = scale;
    }

    #[getter(phase)]
    fn py_get_phase(&self) -> Option<Expression> {
        self.0.phase.clone().map(|Cycles(phase)| phase)
    }

    #[setter(phase)]
    fn py_set_phase(&mut self, phase: Option<Expression>) {
        self.0.phase = phase.map(Cycles);
    }

    #[getter(detuning)]
    fn py_get_detuning(&self) -> Option<Expression> {
        self.0.detuning.clone()
    }

    #[setter(detuning)]
    fn py_set_detuning(&mut self, detuning: Option<Expression>) {
        self.0.detuning = detuning;
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[cfg(feature = "python")]
#[pyo3::pymethods]
impl ConcreteCommonBuiltinParameters {
    #[getter(duration)]
    fn py_get_duration(&self) -> f64 {
        self.0.duration
    }

    #[setter(duration)]
    fn py_set_duration(&mut self, duration: f64) {
        self.0.duration = duration;
    }

    #[getter(scale)]
    fn py_get_scale(&self) -> Option<f64> {
        self.0.scale
    }

    #[setter(scale)]
    fn py_set_scale(&mut self, scale: Option<f64>) {
        self.0.scale = scale;
    }

    #[getter(phase)]
    fn py_get_phase(&self) -> Option<f64> {
        self.0.phase.map(|Cycles(phase)| phase)
    }

    #[setter(phase)]
    fn py_set_phase(&mut self, phase: Option<f64>) {
        self.0.phase = phase.map(Cycles);
    }

    #[getter(detuning)]
    fn py_get_detuning(&self) -> Option<f64> {
        self.0.detuning
    }

    #[setter(detuning)]
    fn py_set_detuning(&mut self, detuning: Option<f64>) {
        self.0.detuning = detuning;
    }
}
