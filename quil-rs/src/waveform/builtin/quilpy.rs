use num_complex::Complex64;
use numpy::{PyArray1, PyArrayMethods as _};
use pyo3::{
    exceptions::PyTypeError,
    prelude::*,
    types::{IntoPyDict as _, PyTuple},
    IntoPyObjectExt as _,
};

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pyfunction, gen_stub_pymethods};

use crate::{
    units::Cycles,
    waveform::quilpy::{PyAnyRust, Pythonic},
    waveform::sampling::quilpy::PyIqSamples,
};

use super::{
    apply_phase_and_detuning_at_index, BoxcarKernel, BuiltinWaveform,
    BuiltinWaveformParameters as _, CommonBuiltinParameters, ExplicitCommonBuiltinParameters,
};

pub use super::quilpy_waveforms::*;

#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(module = "quil.waveform", name = "BuiltinWaveform", subclass, eq)]
pub struct PyBuiltinWaveform(pub BuiltinWaveform<Pythonic>);

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PyBuiltinWaveform {
    #[new]
    #[gen_stub(override_return_type(type_repr = "BuiltinWaveform[Real, Complex]"))]
    fn __new__<'py>(
        #[gen_stub(override_type(type_repr = "\
            Flat[Real, Complex] | \
            Gaussian[Real, Complex] | \
            DragGaussian[Real, Complex] | \
            ErfSquare[Real, Complex] | \
            HermiteGaussian[Real, Complex] | \
            BoxcarKernel\
        ",))]
        waveform: Bound<'py, PyAny>,
    ) -> PyResult<Self> {
        if let Ok(PyFlat(flat)) = waveform.extract() {
            Ok(Self(BuiltinWaveform::Flat(flat)))
        } else if let Ok(PyGaussian(gaussian)) = waveform.extract() {
            Ok(Self(BuiltinWaveform::Gaussian(gaussian)))
        } else if let Ok(PyDragGaussian(drag_gaussian)) = waveform.extract() {
            Ok(Self(BuiltinWaveform::DragGaussian(drag_gaussian)))
        } else if let Ok(PyErfSquare(erf_square)) = waveform.extract() {
            Ok(Self(BuiltinWaveform::ErfSquare(erf_square)))
        } else if let Ok(PyHermiteGaussian(hermite_gaussian)) = waveform.extract() {
            Ok(Self(BuiltinWaveform::HermiteGaussian(hermite_gaussian)))
        } else if let Ok(BoxcarKernel) = waveform.extract() {
            Ok(Self(BuiltinWaveform::BoxcarKernel(BoxcarKernel)))
        } else {
            Err(PyTypeError::new_err((waveform.unbind(),)))
        }
    }

    #[gen_stub(override_return_type(type_repr = "\
            Flat[Real, Complex] | \
            Gaussian[Real, Complex] | \
            DragGaussian[Real, Complex] | \
            ErfSquare[Real, Complex] | \
            HermiteGaussian[Real, Complex] | \
            BoxcarKernel\
        ",))]
    fn as_inner<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        Ok(
            match self
                .0
                .as_ref()
                .try_evaluate(|r| r.clone_ref_ok(py), |c| c.clone_ref_ok(py))
                .unwrap_or_else(|never| match never {})
            {
                BuiltinWaveform::Flat(flat) => PyFlat(flat).into_bound_py_any(py)?,
                BuiltinWaveform::Gaussian(gaussian) => {
                    PyGaussian(gaussian).into_bound_py_any(py)?
                }
                BuiltinWaveform::DragGaussian(drag_gaussian) => {
                    PyDragGaussian(drag_gaussian).into_bound_py_any(py)?
                }
                BuiltinWaveform::ErfSquare(erf_square) => {
                    PyErfSquare(erf_square).into_bound_py_any(py)?
                }
                BuiltinWaveform::HermiteGaussian(hermite_gaussian) => {
                    PyHermiteGaussian(hermite_gaussian).into_bound_py_any(py)?
                }
                BuiltinWaveform::BoxcarKernel(boxcar_kernel) => {
                    boxcar_kernel.into_bound_py_any(py)?
                }
            },
        )
    }

    fn __repr__<'py>(&self, py: Python<'py>) -> PyResult<String> {
        self.0.py_repr(py)
    }
}

impl BuiltinWaveform<Pythonic> {
    pub(crate) fn py_repr<'py>(&self, py: Python<'py>) -> PyResult<String> {
        match self {
            Self::Flat(flat) => flat.py_repr(py),
            Self::Gaussian(gaussian) => gaussian.py_repr(py),
            Self::DragGaussian(drag_gaussian) => drag_gaussian.py_repr(py),
            Self::ErfSquare(erf_square) => erf_square.py_repr(py),
            Self::HermiteGaussian(hermite_gaussian) => hermite_gaussian.py_repr(py),
            Self::BoxcarKernel(boxcar_kernel) => Ok(boxcar_kernel.__repr__().to_owned()),
        }
    }
}

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
    #[pyo3(signature = (*, duration, scale = None, phase = None, detuning = None))]
    #[new]
    fn __new__(
        duration: f64,
        #[gen_stub(override_type(type_repr = "typing.Optional[Real]", imports = ("typing")))]
        scale: Option<PyAnyRust>,
        #[gen_stub(override_type(type_repr = "typing.Optional[Real]", imports = ("typing")))]
        phase: Option<PyAnyRust>,
        #[gen_stub(override_type(type_repr = "typing.Optional[Real]", imports = ("typing")))]
        detuning: Option<PyAnyRust>,
    ) -> Self {
        Self(CommonBuiltinParameters {
            duration,
            scale,
            phase: phase.map(Cycles),
            detuning,
        })
    }

    fn __getnewargs_ex__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        let Self(CommonBuiltinParameters {
            duration,
            scale,
            phase,
            detuning,
        }) = self;
        let arguments = [
            ("duration", duration.into_bound_py_any(py)?),
            (
                "scale",
                scale
                    .as_ref()
                    .map(|scale| scale.clone_ref(py).into_bound_py_any(py))
                    .transpose()?
                    .into_bound_py_any(py)?,
            ),
            (
                "phase",
                phase
                    .as_ref()
                    .map(|Cycles(phase)| phase.clone_ref(py).into_bound_py_any(py))
                    .transpose()?
                    .into_bound_py_any(py)?,
            ),
            (
                "detuning",
                detuning
                    .as_ref()
                    .map(|detuning| detuning.clone_ref(py).into_bound_py_any(py))
                    .transpose()?
                    .into_bound_py_any(py)?,
            ),
        ]
        .into_py_dict(py)?;
        (PyTuple::empty(py), arguments).into_pyobject(py)
    }

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

    #[pyo3(name = "resolve_with_sample_rate")]
    fn py_resolve_with_sample_rate<'py>(
        // ASZ: expose a way to do this in the stub_gen binary
        // #[gen_stub(override_type(
        //     type_repr = "CommonBuiltinParameters[builtins.float, Complex]",
        //     imports = ("builtins"))
        // )]
        &self,
        py: Python<'py>,
        sample_rate: f64,
    ) -> PyResult<ExplicitCommonBuiltinParameters> {
        Ok(self
            .0
            .as_ref()
            .try_evaluate(|PyAnyRust(r)| r.extract(py), |PyAnyRust(c)| c.extract(py))?
            .resolve_with_sample_rate(sample_rate)?)
    }

    fn __repr__<'py>(&self, py: Python<'py>) -> PyResult<String> {
        self.0.py_repr(py)
    }
}

impl CommonBuiltinParameters<Pythonic> {
    pub(crate) fn py_repr<'py>(&self, py: Python<'py>) -> PyResult<String> {
        let Self {
            duration,
            scale,
            phase,
            detuning,
        } = self;

        let mut output = format!("CommonBuiltinParameters(duration={duration}");
        if let Some(PyAnyRust(scale)) = scale {
            output.push_str(", scale=");
            output.push_str(scale.bind(py).repr()?.to_str()?);
        }
        if let Some(Cycles(PyAnyRust(phase))) = phase {
            output.push_str(", phase=");
            output.push_str(phase.bind(py).repr()?.to_str()?);
        }
        if let Some(PyAnyRust(detuning)) = detuning {
            output.push_str(", detuning=");
            output.push_str(detuning.bind(py).repr()?.to_str()?);
        }
        output.push(')');
        Ok(output)
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl ExplicitCommonBuiltinParameters {
    #[pyo3(signature = (*, sample_count, scale, phase, detuning))]
    #[new]
    fn __new__(sample_count: u32, scale: f64, phase: f64, detuning: f64) -> Self {
        Self {
            sample_count,
            scale,
            phase: Cycles(phase),
            detuning,
        }
    }

    fn __getnewargs_ex__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        let Self {
            sample_count,
            scale,
            phase,
            detuning,
        } = *self;
        let arguments = [
            ("sample_count", sample_count.into_bound_py_any(py)?),
            ("scale", scale.into_bound_py_any(py)?),
            ("phase", phase.0.into_bound_py_any(py)?),
            ("detuning", detuning.into_bound_py_any(py)?),
        ]
        .into_py_dict(py)?;
        (PyTuple::empty(py), arguments).into_pyobject(py)
    }

    fn __repr__(&self) -> String {
        let Self {
            sample_count,
            scale,
            phase: Cycles(phase),
            detuning,
        } = self;

        format!(
            "ExplicitBuiltinParameters(\
                 sample_count={sample_count}, \
                 scale={scale}, \
                 phase={phase}, \
                 detuning={detuning}
             )"
        )
    }
}

/// Modulate and phase shift waveform IQ data in place.
#[cfg_attr(feature = "stubs", gen_stub_pyfunction(module = "quil.waveform"))]
#[pyfunction(name = "apply_phase_and_detuning")]
pub fn py_apply_phase_and_detuning(
    iq_values: &Bound<'_, PyArray1<Complex64>>,
    phase: f64,
    detuning: f64,
    sample_rate: f64,
) -> PyResult<()> {
    // Though we could call the Rust version of this function and then modify the Python list,
    // this version allows us to iterate only once and avoid allocating a new `Vec<Complex64>`.
    for (index, value) in iq_values
        .try_readwrite()?
        .as_array_mut()
        .into_iter()
        .enumerate()
    {
        *value =
            apply_phase_and_detuning_at_index(*value, Cycles(phase), detuning, sample_rate, index);
    }

    Ok(())
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PyBuiltinWaveform {
    #[pyo3(name = "iq_values_at_sample_rate")]
    fn py_iq_values_at_sample_rate<'py>(
        // ASZ: expose a way to do this in the stub_gen binary
        // #[gen_stub(override_type(
        //     type_repr = "BuiltinWaveform[builtins.float, builtins.complex]",
        //     imports = ("builtins"))
        // )]
        &self,
        py: Python<'py>,
        #[gen_stub(override_type(
            type_repr = "CommonBuiltinParameters[builtins.float, Complex]",
            imports = ("builtins"))
        )]
        common: PyCommonBuiltinParameters,
        sample_rate: f64,
    ) -> PyResult<PyIqSamples> {
        Ok(self
            .0
            .as_ref()
            .try_evaluate(|PyAnyRust(r)| r.extract(py), |PyAnyRust(c)| c.extract(py))?
            .iq_values_at_sample_rate(
                common
                    .0
                    .as_ref()
                    .try_evaluate(|PyAnyRust(r)| r.extract(py), |PyAnyRust(c)| c.extract(py))?,
                sample_rate,
            )?
            .into())
    }
}
