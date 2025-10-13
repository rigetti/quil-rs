use num_complex::Complex64;
use pyo3::{
    prelude::*,
    types::{IntoPyDict as _, PyList, PyTuple},
};

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::{gen_stub_pyfunction, gen_stub_pymethods};

use crate::{quilpy::impl_repr, units::Cycles, waveform::builtin::*};

#[pymodule]
#[pyo3(name = "waveforms", module = "quil", submodule)]
pub(crate) fn init_submodule(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<CommonBuiltinParameters>()?;
    m.add_class::<ExplicitCommonBuiltinParameters>()?;
    m.add_class::<Flat>()?;
    m.add_class::<Gaussian>()?;
    m.add_class::<DragGaussian>()?;
    m.add_class::<ErfSquare>()?;
    m.add_class::<HermiteGaussian>()?;
    m.add_class::<BoxcarKernel>()?;
    m.add_function(wrap_pyfunction!(py_apply_phase_and_detuning, m)?)?;
    Ok(())
}

impl_repr! {
    CommonBuiltinParameters,
    ExplicitCommonBuiltinParameters,
    Flat,
    Gaussian,
    DragGaussian,
    ErfSquare,
    HermiteGaussian,
    BoxcarKernel,
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl CommonBuiltinParameters {
    #[pyo3(signature = (*, duration, scale = None, phase = None, detuning = None))]
    #[new]
    fn __new__(
        duration: f64,
        scale: Option<f64>,
        phase: Option<f64>,
        detuning: Option<f64>,
    ) -> Self {
        Self {
            duration,
            scale,
            phase: phase.map(Cycles),
            detuning,
        }
    }

    #[pyo3(name = "explicit")]
    fn py_explicit(&self) -> ExplicitCommonBuiltinParameters {
        (*self).into()
    }

    fn __getnewargs_ex__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        let Self {
            duration,
            scale,
            phase,
            detuning,
        } = *self;
        let arguments = [
            ("duration", Some(duration)),
            ("scale", scale),
            ("phase", phase.map(|p| p.0)),
            ("detuning", detuning),
        ]
        .into_py_dict(py)?;
        (PyTuple::empty(py), arguments).into_pyobject(py)
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl ExplicitCommonBuiltinParameters {
    #[pyo3(signature = (*, duration, scale, phase, detuning))]
    #[new]
    fn __new__(duration: f64, scale: f64, phase: f64, detuning: f64) -> Self {
        Self {
            duration,
            scale,
            phase: Cycles(phase),
            detuning,
        }
    }

    fn __getnewargs_ex__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        let Self {
            duration,
            scale,
            phase,
            detuning,
        } = *self;
        let arguments = [
            ("duration", duration),
            ("scale", scale),
            ("phase", phase.0),
            ("detuning", detuning),
        ]
        .into_py_dict(py)?;
        (PyTuple::empty(py), arguments).into_pyobject(py)
    }
}

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
        let value =
            apply_phase_and_detuning_at_index(value, Cycles(phase), detuning, sample_rate, index);
        iq_values.set_item(index, value)?;
    }

    Ok(())
}

macro_rules! first_ty {
    ($first:ty $(, $rest:ty)*) => {
        $first
    };
}

macro_rules! python_waveforms {
    ($($waveform:ident { $($field:ident: $ty:ty),* $(,)? })*) => {
        $(
            #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
            #[pymethods]
            impl $waveform {
                #[pyo3(signature = (* $(, $field)*))]
                #[new]
                fn __new__($($field: $ty),*) -> Self {
                    Self { $($field),* }
                }

                fn __getnewargs_ex__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
                    let Self { $($field),* } = *self;
                    let arguments: [(&'static str, first_ty!($($ty,)* ())); _] =
                        [$((stringify!($field), $field)),*];
                    (PyTuple::empty(py), arguments.into_py_dict(py)?).into_pyobject(py)
                }
            }
        )*
    }
}

python_waveforms! {
    Flat {
        iq: Complex64,
    }

    Gaussian {
        fwhm: f64,
        t0: f64,
    }

    DragGaussian {
        fwhm: f64,
        t0: f64,
        anh: f64,
        alpha: f64,
    }

    ErfSquare {
        risetime: f64,
        pad_left: f64,
        pad_right: f64,
    }

    HermiteGaussian {
        fwhm: f64,
        t0: f64,
        anh: f64,
        alpha: f64,
        second_order_hrm_coeff: f64,
    }

    BoxcarKernel {}
}
