use num_complex::Complex64;
use pyo3::{prelude::*, types::PyList};
use rigetti_pyo3::{create_init_submodule, impl_repr, py_wrap_data_struct, PyWrapper};

use quil_rs::{
    units::Cycles,
    waveform::{
        apply_phase_and_detuning as rs_apply_phase_and_detuning, BoxcarKernel, DragGaussian,
        ErfSquare, Gaussian, HermiteGaussian,
    },
};

use crate::{impl_eq, units::PyCycles};

create_init_submodule! {
    classes: [PyBoxcarKernel, PyErfSquare, PyGaussian, PyDragGaussian, PyHermiteGaussian],
    funcs: [apply_phase_and_detuning],
}

#[pyfunction]
pub fn apply_phase_and_detuning(
    iq_values: &PyList,
    phase: f64,
    detuning: f64,
    sample_rate: f64,
) -> PyResult<()> {
    // There isn't a way to provide a mutable Vec<Complex64> as an argument to a pyfunction so we
    // create a new Vec<Complex64> from the PyList, and use the updated Vec to update the PyList.
    let mut rust_iq_values: Vec<Complex64> = iq_values
        .iter()
        .map(|item| {
            let num: Complex64 = item.extract().unwrap();
            num
        })
        .collect();

    rs_apply_phase_and_detuning(&mut rust_iq_values, phase, detuning, sample_rate);

    for (i, num) in rust_iq_values.iter().enumerate() {
        iq_values.set_item(i, num)?;
    }

    Ok(())
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass)]
    PyBoxcarKernel(BoxcarKernel) as "BoxcarKernel" {
        phase: Cycles<f64> => PyCycles,
        scale: f64 => f64,
        sample_count: u64 => u64
    }
}
impl_repr!(PyBoxcarKernel);
impl_eq!(PyBoxcarKernel);

#[pymethods]
impl PyBoxcarKernel {
    #[new]
    pub fn new(phase: f64, scale: f64, sample_count: u64) -> Self {
        PyBoxcarKernel(BoxcarKernel {
            phase: Cycles(phase),
            scale,
            sample_count,
        })
    }

    #[pyo3(name = "into_iq_value")]
    pub fn py_into_iq_value(&self) -> Complex64 {
        self.as_inner().into_iq_value()
    }

    /// Override the implementation from `py_wrap_data_struct` so that f64 is returned directly.
    #[getter]
    pub fn phase(&self) -> f64 {
        self.as_inner().phase.0
    }
}

macro_rules! impl_into_iq_values {
    ($name: ident) => {
        #[pyo3::pymethods]
        impl $name {
            #[pyo3(name = "into_iq_values")]
            pub fn py_into_iq_values(&self) -> Vec<Complex64> {
                quil_rs::waveform::WaveformTemplate::into_iq_values(self.clone().into_inner())
            }
        }
    };
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass)]
    PyErfSquare(ErfSquare) as "ErfSquare" {
        duration: f64 => f64,
        risetime: f64 => f64,
        sample_rate: f64 => f64,
        pad_left: f64 => f64,
        pad_right: f64 => f64,
        positive_polarity: bool => bool,
        scale: f64 => f64,
        phase: f64 => f64,
        detuning: f64 => f64
    }
}
impl_into_iq_values!(PyErfSquare);
impl_repr!(PyErfSquare);
impl_eq!(PyErfSquare);

py_wrap_data_struct! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass)]
    PyGaussian(Gaussian) as "Gaussian" {
        duration: f64 => f64,
        fwhm: f64 => f64,
        t0: f64 => f64,
        sample_rate: f64 => f64,
        scale: f64 => f64,
        phase: f64 => f64,
        detuning: f64 => f64
    }
}
impl_into_iq_values!(PyGaussian);
impl_repr!(PyGaussian);
impl_eq!(PyGaussian);

py_wrap_data_struct! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass)]
    PyDragGaussian(DragGaussian) as "DragGaussian" {
        duration: f64 => f64,
        fwhm: f64 => f64,
        t0: f64 => f64,
        anh: f64 => f64,
        alpha: f64 => f64,
        sample_rate: f64 => f64,
        scale: f64 => f64,
        phase: f64 => f64,
        detuning: f64 => f64
    }
}
impl_into_iq_values!(PyDragGaussian);
impl_repr!(PyDragGaussian);
impl_eq!(PyDragGaussian);

py_wrap_data_struct! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass)]
    PyHermiteGaussian(HermiteGaussian) as "HermiteGaussian" {
        duration: f64 => f64,
        fwhm: f64 => f64,
        t0: f64 => f64,
        anh: f64 => f64,
        alpha: f64 => f64,
        sample_rate: f64 => f64,
        second_order_hrm_coeff: f64 => f64,
        scale: f64 => f64,
        phase: f64 => f64,
        detuning: f64 => f64
    }
}
impl_into_iq_values!(PyHermiteGaussian);
impl_repr!(PyHermiteGaussian);
impl_eq!(PyHermiteGaussian);
