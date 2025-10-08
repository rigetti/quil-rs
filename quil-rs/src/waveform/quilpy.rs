use num_complex::Complex64;
use pyo3::{prelude::*, types::PyList};

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::{gen_stub_pyfunction, gen_stub_pymethods};

use super::templates::*;
use crate::quilpy::{errors::ValueError, impl_repr};

#[pymodule]
#[pyo3(name = "waveforms", module = "quil", submodule)]
pub(crate) fn init_submodule(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<BoxcarKernel>()?;
    m.add_class::<ErfSquare>()?;
    m.add_class::<Gaussian>()?;
    m.add_class::<DragGaussian>()?;
    m.add_class::<HermiteGaussian>()?;
    m.add_function(wrap_pyfunction!(py_apply_phase_and_detuning, m)?)?;
    Ok(())
}

impl_repr!(BoxcarKernel);
impl_repr!(DragGaussian);
impl_repr!(ErfSquare);
impl_repr!(Gaussian);
impl_repr!(HermiteGaussian);

/// Modulate and phase shift waveform IQ data in place.
#[cfg_attr(feature = "stubs", gen_stub_pyfunction(module = "quil.waveforms"))]
#[pyfunction(name = "apply_phase_and_detuning")]
fn py_apply_phase_and_detuning(
    iq_values: &Bound<'_, PyList>,
    phase: f64,
    detuning: f64,
    sample_rate: f64,
) -> PyResult<()> {
    // Though we could call the Rust version of this function and then modify the Python list,
    // this version allows us to iterate only once and avoid allocating a new `Vec<Complex64>`.
    for (index, value) in iq_values.iter().enumerate() {
        let value = value.extract::<Complex64>()?;
        let value = apply_phase_and_detuning_impl(value, phase, detuning, sample_rate, index);
        iq_values.set_item(index, value)?;
    }

    Ok(())
}

/// A simple wrapper around [`std::num::NonZeroU64`] with [`pyo3_stub_gen::PyStubType`] information.
#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Hash, pyo3::IntoPyObject)]
struct NonZeroU64(std::num::NonZeroU64);

// PyO3 has a conversion we could derive from,
// but it raises a TypeError that says "failed to extract field NonZeroU64.0".
// By implementing it manually, an invalid value instead reads:
// "quil.QuilValueError: expected a positive value".
impl<'py> pyo3::FromPyObject<'py> for NonZeroU64 {
    fn extract_bound(ob: &Bound<'py, PyAny>) -> PyResult<Self> {
        ob.extract::<u64>().and_then(|value| {
            std::num::NonZeroU64::try_from(value)
                .map_err(|_err| ValueError::new_err("expected a positive value"))
                .map(Self)
        })
    }
}

#[cfg(feature = "stubs")]
impl pyo3_stub_gen::PyStubType for NonZeroU64 {
    fn type_output() -> pyo3_stub_gen::TypeInfo {
        pyo3_stub_gen::TypeInfo::builtin("int")
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl BoxcarKernel {
    /// Create a new `BoxcarKernel`.
    ///
    /// This raises an error if `sample_count` is zero.
    #[new]
    fn __new__(phase: f64, scale: f64, sample_count: NonZeroU64) -> Self {
        Self {
            phase: crate::units::Cycles(phase),
            scale,
            sample_count: sample_count.0.into(),
        }
    }

    /// Get the `complex` value this `BoxcarKernel` represents.
    #[pyo3(name = "into_iq_value")]
    fn py_into_iq_value(&self) -> Complex64 {
        self.into_iq_value()
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl ErfSquare {
    #[new]
    #[allow(clippy::too_many_arguments)]
    fn __new__(
        duration: f64,
        risetime: f64,
        sample_rate: f64,
        pad_left: f64,
        pad_right: f64,
        positive_polarity: bool,
        scale: f64,
        phase: f64,
        detuning: f64,
    ) -> Self {
        Self {
            duration,
            risetime,
            sample_rate,
            pad_left,
            pad_right,
            positive_polarity,
            scale,
            phase,
            detuning,
        }
    }

    /// Get the a list of `complex` values from this waveform.
    #[pyo3(name = "into_iq_value")]
    fn py_into_iq_values(&self) -> Vec<Complex64> {
        self.into_iq_values()
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Gaussian {
    #[new]
    fn __new__(
        duration: f64,
        fwhm: f64,
        t0: f64,
        sample_rate: f64,
        scale: f64,
        phase: f64,
        detuning: f64,
    ) -> Self {
        Self {
            duration,
            fwhm,
            t0,
            sample_rate,
            scale,
            phase,
            detuning,
        }
    }

    /// Get the a list of `complex` values from this waveform.
    #[pyo3(name = "into_iq_value")]
    fn py_into_iq_values(&self) -> Vec<Complex64> {
        self.into_iq_values()
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl DragGaussian {
    #[new]
    #[allow(clippy::too_many_arguments)]
    fn __new__(
        duration: f64,
        fwhm: f64,
        t0: f64,
        anh: f64,
        alpha: f64,
        sample_rate: f64,
        scale: f64,
        phase: f64,
        detuning: f64,
    ) -> Self {
        Self {
            duration,
            fwhm,
            t0,
            anh,
            alpha,
            sample_rate,
            scale,
            phase,
            detuning,
        }
    }

    /// Get the a list of `complex` values from this waveform.
    #[pyo3(name = "into_iq_value")]
    fn py_into_iq_values(&self) -> Vec<Complex64> {
        self.into_iq_values()
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl HermiteGaussian {
    #[allow(clippy::too_many_arguments)]
    #[new]
    fn __new__(
        duration: f64,
        fwhm: f64,
        t0: f64,
        anh: f64,
        alpha: f64,
        sample_rate: f64,
        second_order_hrm_coeff: f64,
        scale: f64,
        phase: f64,
        detuning: f64,
    ) -> Self {
        Self {
            duration,
            fwhm,
            t0,
            anh,
            alpha,
            sample_rate,
            second_order_hrm_coeff,
            scale,
            phase,
            detuning,
        }
    }

    /// Get the a list of `complex` values from this waveform.
    #[pyo3(name = "into_iq_value")]
    fn py_into_iq_values(&self) -> Vec<Complex64> {
        self.into_iq_values()
    }
}
