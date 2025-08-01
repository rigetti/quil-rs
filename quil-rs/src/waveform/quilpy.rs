use pyo3::{prelude::*, types::PyList};
use pyo3::exceptions::PyValueError;
use num_complex::Complex64;

use crate::impl_repr;
use super::templates::*;

/// Modulate and phase shift waveform IQ data in place.
#[pyfunction(name = "apply_phase_and_detuning")]
pub(crate) fn py_apply_phase_and_detuning(
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

#[pymodule]
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

#[pymethods]
impl BoxcarKernel {
    /// Create a new `BoxcarKernel`.
    ///
    /// This raises a `ValueError` if `sample_count` is zero.
    #[new]
    pub fn __new__(phase: f64, scale: f64, sample_count: u64) -> PyResult<Self> {
        if sample_count == 0 {
            return Err(PyValueError::new_err("sample_count must be positive"));
        }

        Ok(Self {
            phase: crate::units::Cycles(phase),
            scale,
            sample_count,
        })
    }

    /// Get the `complex` value this `BoxcarKernel` represents.
    #[pyo3(name = "into_iq_value")]
    fn py_into_iq_value(&self) -> Complex64 {
        self.into_iq_value()
    }
}

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

    /// Convert `ErfSquare` into a list of `complex` values.
    #[pyo3(name = "into_iq_value")]
    fn py_into_iq_values(&self) -> Vec<Complex64> {
        self.into_iq_values()
    }
}

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

    #[pyo3(name = "into_iq_value")]
    fn py_into_iq_values(&self) -> Vec<Complex64> {
        self.into_iq_values()
    }
}

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

    #[pyo3(name = "into_iq_value")]
    fn py_into_iq_values(&self) -> Vec<Complex64> {
        self.into_iq_values()
    }
}

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

    #[pyo3(name = "into_iq_value")]
    fn py_into_iq_values(&self) -> Vec<Complex64> {
        self.into_iq_values()
    }
}
