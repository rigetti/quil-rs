//! Templates for generating discrete IQ value sequences representing Quil's defined set of waveforms.

use std::f64::consts::PI;

use crate::{imag, real};
use ndarray::Array;
use num_complex::Complex64;
use statrs::function::erf::erf;

use serde::{Deserialize, Serialize};

const J: Complex64 = Complex64::new(0f64, 1f64);

/// Modulate and phase shift waveform IQ data in place.
pub fn apply_phase_and_detuning(
    iq_values: &mut [Complex64],
    phase: f64,
    detuning: f64,
    sample_rate: f64,
) {
    for (index, value) in iq_values.iter_mut().enumerate() {
        *value *= (2.0 * PI * J * (detuning * (index as f64) / sample_rate + phase)).exp();
    }
}

/// A custom `ceil()` method that includes a machine-epsilon sized region above each integer in the
/// set of values that are mapped to that integer. In other words:
/// ceil_eps(x) = n, for all x and integers n s.t. n <= x < (n + n*epsilon)
///
/// To handle accumulated floating point errors in sweeps above typical floating point imprecision
/// we make epsilon 10x larger than floating point epsilon.
pub(super) fn ceiling_with_epsilon(value: f64) -> f64 {
    let truncated = value - (value * 10.0 * std::f64::EPSILON);
    truncated.ceil()
}

pub fn polar_to_rectangular(magnitude: f64, angle: crate::units::Radians<f64>) -> Complex64 {
    magnitude * imag!(angle.0).exp()
}

#[derive(Clone, Debug)]
pub struct BoxcarKernelParameters {
    pub phase: crate::units::Cycles<f64>,
    pub scale: f64,
    pub sample_count: u64,
}

/// Return the IQ value used for a boxcar kernel
pub fn boxcar_kernel(parameters: BoxcarKernelParameters) -> Complex64 {
    polar_to_rectangular(
        parameters.scale / parameters.sample_count as f64,
        parameters.phase.into(),
    )
}

#[derive(Serialize, Deserialize)]
pub struct ErfSquareParameters {
    /// Full duration of the pulse (s)
    pub duration: f64,
    /// Slope of erf shoulders (2x FWHM of erf in s)
    pub risetime: f64,
    /// Generate wavform samples at this rate (Hz)
    pub sample_rate: f64,
    /// Length of zero padding to add to beginning of pulse (s)
    pub pad_left: f64,
    /// Length of zero padding to add to end of pulse (s)
    pub pad_right: f64,
    /// Toggle for positive/negative polarity
    pub positive_polarity: bool,
    /// Scale to apply to waveform envelope
    pub scale: f64,
    /// Phase shift for entire waveform
    pub phase: f64,
    /// Explicit detuning to bake into iq values
    pub detuning: f64,
}

/// Creates a waveform with a flat top and edges that are error functions (erfs).
pub fn erf_square(parameters: ErfSquareParameters) -> Vec<Complex64> {
    let length = ceiling_with_epsilon(parameters.duration * parameters.sample_rate);
    let mut time_steps = Array::<f64, _>::range(0f64, length, 1f64) / parameters.sample_rate;

    let fwhm = 0.5 * parameters.risetime;
    let t1 = fwhm;
    let t2 = parameters.duration - fwhm;
    let sigma = 0.5 * fwhm / (2f64 * 2f64.ln()).sqrt();
    time_steps.mapv_inplace(|el| 0.5 * (erf((el - t1) / sigma) - erf((el - t2) / sigma)));
    if !parameters.positive_polarity {
        time_steps *= -1f64;
    }

    let left_pad_length = (parameters.pad_left * parameters.sample_rate).ceil() as usize;
    let mut waveform = vec![real!(0f64); left_pad_length];
    waveform.extend(
        time_steps
            .into_iter()
            .map(|el| parameters.scale * real!(el)),
    );

    let right_pad_length = (parameters.pad_right * parameters.sample_rate).ceil() as usize;
    waveform.extend_from_slice(&vec![real!(0f64); right_pad_length]);
    apply_phase_and_detuning(
        &mut waveform,
        parameters.phase,
        parameters.detuning,
        parameters.sample_rate,
    );
    waveform
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GaussianParameters {
    /// Full duration of the pulse (s)
    pub duration: f64,
    /// Full width half maximum of the pulse (s)
    pub fwhm: f64,
    /// Center/offset for pulse centroid (s)
    pub t0: f64,
    /// Generate waveform samples at this rate (Hz)
    pub sample_rate: f64,
    /// Scale to apply to waveform envelope
    pub scale: f64,
    /// Phase shift for entire waveform
    pub phase: f64,
    /// Explicit detuning to bake into IQ values
    pub detuning: f64,
}

/// Creates a waveform with a Gaussian shape.
pub fn gaussian(parameters: GaussianParameters) -> Vec<Complex64> {
    let length = ceiling_with_epsilon(parameters.duration * parameters.sample_rate);
    let mut time_steps = Array::<f64, _>::range(0f64, length, 1f64) / parameters.sample_rate;

    let sigma = 0.5 * parameters.fwhm / (2f64 * 2f64.ln()).sqrt();
    time_steps.mapv_inplace(|el| (-0.5 * (el - parameters.t0).powf(2f64) / sigma.powf(2f64)).exp());

    let mut waveform = time_steps
        .into_iter()
        .map(|el| parameters.scale * real!(el))
        .collect::<Vec<_>>();
    apply_phase_and_detuning(
        &mut waveform,
        parameters.phase,
        parameters.detuning,
        parameters.sample_rate,
    );
    waveform
}

/// Struct holding parameters that completely specify a DragGaussian Pulse.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DragGaussianParameters {
    /// Full duration of the pulse (s)
    pub duration: f64,
    /// Full width half maximum of the pulse (s)
    pub fwhm: f64,
    /// Center/offset for pulse centroid (s)
    pub t0: f64,
    /// Qubit anharmonicity - sets rate of evolution for the imaginary term (Hz)
    pub anh: f64,
    /// DRAG parameter - controls strength of the imaginary term
    pub alpha: f64,
    /// Generate samples at this rate (Hz)
    pub sample_rate: f64,
    /// Scale to apply to waveform envelope
    pub scale: f64,
    /// Phase shift for entire waveform
    pub phase: f64,
    /// Explicit detuning to bake into iq values
    pub detuning: f64,
}

/// Creates a waveform with a DRAG-corrected Gaussian shape.
///
/// This is a Gaussian shape with an additional component proportional to the time derivative of the main Gaussian pulse.
///
/// See Motzoi F. et al., Phys. Rev. Lett., 103 (2009) 110501. for details.
pub fn drag_gaussian(parameters: DragGaussianParameters) -> Vec<Complex64> {
    let length = ceiling_with_epsilon(parameters.duration * parameters.sample_rate);
    let time_steps = Array::<f64, _>::range(0f64, length, 1f64) / parameters.sample_rate;

    let sigma = 0.5 * parameters.fwhm / (2f64 * 2f64.ln()).sqrt();

    let mut waveform = vec![Complex64::new(0f64, 0f64,); length as usize];
    for i in 0..length as usize {
        // Generate envelope sample
        let env = (-0.5 * (time_steps[i] - parameters.t0).powf(2f64) / sigma.powf(2f64)).exp();
        // Generate modified envelope sample
        let env_mod = (parameters.alpha * (1f64 / (2f64 * PI * parameters.anh * sigma.powf(2f64))))
            * (time_steps[i] - parameters.t0)
            * env;
        waveform[i] = parameters.scale * Complex64::new(env, env_mod);
    }
    apply_phase_and_detuning(
        &mut waveform,
        parameters.phase,
        parameters.detuning,
        parameters.sample_rate,
    );

    waveform
}

/// Struct holding parameters that specify a Hermite Gaussian Pulse.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct HermiteGaussianParameters {
    /// Full duration of the pulse
    pub duration: f64,
    /// Full width half maximum of the pulse
    pub fwhm: f64,
    /// Center/offset for pulse centroid
    pub t0: f64,
    /// Qubit anharmonicity - sets rate of evolution for the imaginary term
    pub anh: f64,
    /// DRAG parameter - controls strength of the imaginary term
    pub alpha: f64,
    /// Generate samples at this rate
    pub sample_rate: f64,
    /// Coefficient of the second order Hermite polynomial term.
    pub second_order_hrm_coeff: f64,
    /// Scale to apply to waveform envelope
    pub scale: f64,
    /// Phase shift for entire waveform
    pub phase: f64,
    /// Explicit detuning to bake into iq values
    pub detuning: f64,
}

/// Creates a Hermite Gaussian waveform.
///
/// This extends the basic DRAG pulse by adding an additional imaginary term to the pulse envelope consisting of a
/// Gaussian pulse modified by the second order Hermite polynomial. Refer to
/// "Effects of arbitrary laser or NMR pulse shapes on population inversion and coherence"
/// Warren S. Warren. 81, (1984); doi: 10.1063/1.447644
/// for details.
pub fn hermite_gaussian(parameters: HermiteGaussianParameters) -> Vec<Complex64> {
    let length = ceiling_with_epsilon(parameters.duration * parameters.sample_rate);
    let time_steps = Array::<f64, _>::range(0f64, length, 1f64) / parameters.sample_rate;

    let sigma = 0.5 * parameters.fwhm / (2f64 * 2f64.ln()).sqrt();
    let deriv_prefactor = -parameters.alpha / (2f64 * PI * parameters.anh);

    let mut waveform = vec![Complex64::new(0f64, 0f64,); length as usize];
    for i in 0..length as usize {
        let exp_t = 0.5 * (time_steps[i] - parameters.t0).powf(2f64) / sigma.powf(2f64);
        let g = (-exp_t).exp();
        let env = (1f64 - parameters.second_order_hrm_coeff * exp_t) * g;
        let env_derived = deriv_prefactor * (time_steps[i] - parameters.t0) / sigma.powf(2f64)
            * g
            * (parameters.second_order_hrm_coeff * (exp_t - 1f64) - 1f64);
        waveform[i] = parameters.scale * Complex64::new(env, env_derived);
    }
    apply_phase_and_detuning(
        &mut waveform,
        parameters.phase,
        parameters.detuning,
        parameters.sample_rate,
    );

    waveform
}

#[cfg(test)]
mod tests {}
