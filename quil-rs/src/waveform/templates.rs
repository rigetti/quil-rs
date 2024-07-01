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
fn ceiling_with_epsilon(value: f64) -> f64 {
    let truncated = value - (value * 10.0 * f64::EPSILON);
    truncated.ceil()
}

/// Convert polar coordinates to rectangular coordinates.
fn polar_to_rectangular(magnitude: f64, angle: crate::units::Radians<f64>) -> Complex64 {
    magnitude * imag!(angle.0).exp()
}

pub trait WaveformTemplate {
    fn into_iq_values(self) -> Vec<Complex64>;
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub struct BoxcarKernel {
    pub phase: crate::units::Cycles<f64>,
    pub scale: f64,
    pub sample_count: u64,
}

impl BoxcarKernel {
    pub fn into_iq_value(self) -> Complex64 {
        polar_to_rectangular(self.scale / self.sample_count as f64, self.phase.into())
    }
}

/// Creates a waveform with a flat top and edges that are error functions (erfs).
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub struct ErfSquare {
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

impl WaveformTemplate for ErfSquare {
    fn into_iq_values(self) -> Vec<Complex64> {
        let length = ceiling_with_epsilon(self.duration * self.sample_rate);
        let mut time_steps = Array::<f64, _>::range(0f64, length, 1f64) / self.sample_rate;

        let fwhm = 0.5 * self.risetime;
        let t1 = fwhm;
        let t2 = self.duration - fwhm;
        let sigma = 0.5 * fwhm / (2f64 * 2f64.ln()).sqrt();
        time_steps.mapv_inplace(|el| 0.5 * (erf((el - t1) / sigma) - erf((el - t2) / sigma)));
        if !self.positive_polarity {
            time_steps *= -1f64;
        }

        let left_pad_length = (self.pad_left * self.sample_rate).ceil() as usize;
        let mut waveform = vec![real!(0f64); left_pad_length];
        waveform.extend(time_steps.into_iter().map(|el| self.scale * real!(el)));

        let right_pad_length = (self.pad_right * self.sample_rate).ceil() as usize;
        waveform.extend_from_slice(&vec![real!(0f64); right_pad_length]);
        apply_phase_and_detuning(&mut waveform, self.phase, self.detuning, self.sample_rate);
        waveform
    }
}

/// Creates a waveform with a Gaussian shape.
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub struct Gaussian {
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

impl WaveformTemplate for Gaussian {
    fn into_iq_values(self) -> Vec<Complex64> {
        let length = ceiling_with_epsilon(self.duration * self.sample_rate);
        let mut time_steps = Array::<f64, _>::range(0f64, length, 1f64) / self.sample_rate;

        let sigma = 0.5 * self.fwhm / (2f64 * 2f64.ln()).sqrt();
        time_steps.mapv_inplace(|el| (-0.5 * (el - self.t0).powf(2f64) / sigma.powf(2f64)).exp());

        let mut waveform = time_steps
            .into_iter()
            .map(|el| self.scale * real!(el))
            .collect::<Vec<_>>();
        apply_phase_and_detuning(&mut waveform, self.phase, self.detuning, self.sample_rate);
        waveform
    }
}

/// Creates a waveform with a DRAG-corrected Gaussian shape.
///
/// This is a Gaussian shape with an additional component proportional to the time derivative of the main Gaussian pulse.
///
/// See Motzoi F. et al., Phys. Rev. Lett., 103 (2009) 110501. for details.
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub struct DragGaussian {
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

impl WaveformTemplate for DragGaussian {
    fn into_iq_values(self) -> Vec<Complex64> {
        let length = ceiling_with_epsilon(self.duration * self.sample_rate);
        let time_steps = Array::<f64, _>::range(0f64, length, 1f64) / self.sample_rate;

        let sigma = 0.5 * self.fwhm / (2f64 * 2f64.ln()).sqrt();

        let mut waveform = vec![Complex64::new(0f64, 0f64,); length as usize];
        for i in 0..length as usize {
            // Generate envelope sample
            let env = (-0.5 * (time_steps[i] - self.t0).powf(2f64) / sigma.powf(2f64)).exp();
            // Generate modified envelope sample
            let env_mod = (self.alpha * (1f64 / (2f64 * PI * self.anh * sigma.powf(2f64))))
                * (time_steps[i] - self.t0)
                * env;
            waveform[i] = self.scale * Complex64::new(env, env_mod);
        }
        apply_phase_and_detuning(&mut waveform, self.phase, self.detuning, self.sample_rate);

        waveform
    }
}

/// Creates a Hermite Gaussian waveform.
///
/// This extends the basic DRAG pulse by adding an additional imaginary term to the pulse envelope consisting of a
/// Gaussian pulse modified by the second order Hermite polynomial.
///
/// Refer to
/// "Effects of arbitrary laser or NMR pulse shapes on population inversion and coherence"
/// Warren S. Warren. 81, (1984); doi: 10.1063/1.447644
/// for details.
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub struct HermiteGaussian {
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

impl WaveformTemplate for HermiteGaussian {
    fn into_iq_values(self) -> Vec<Complex64> {
        let length = ceiling_with_epsilon(self.duration * self.sample_rate);
        let time_steps = Array::<f64, _>::range(0f64, length, 1f64) / self.sample_rate;

        let sigma = 0.5 * self.fwhm / (2f64 * 2f64.ln()).sqrt();
        let deriv_prefactor = -self.alpha / (2f64 * PI * self.anh);

        let mut waveform = vec![Complex64::new(0f64, 0f64,); length as usize];
        for i in 0..length as usize {
            let exp_t = 0.5 * (time_steps[i] - self.t0).powf(2f64) / sigma.powf(2f64);
            let g = (-exp_t).exp();
            let env = (1f64 - self.second_order_hrm_coeff * exp_t) * g;
            let env_derived = deriv_prefactor * (time_steps[i] - self.t0) / sigma.powf(2f64)
                * g
                * (self.second_order_hrm_coeff * (exp_t - 1f64) - 1f64);
            waveform[i] = self.scale * Complex64::new(env, env_derived);
        }
        apply_phase_and_detuning(&mut waveform, self.phase, self.detuning, self.sample_rate);

        waveform
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Clean up a Debug representation to something more apt for a filename.
    /// It's important that this remain stable or there'll be a mess of updating snapshot files.
    fn format_snapshot_name(item: impl std::fmt::Debug) -> String {
        format!("{item:?}")
            .replace(['{', '}', ':', ','], "")
            .replace([' ', '.'], "_")
    }

    fn assert_almost_eq(left: Complex64, right: Complex64, epsilon: f64) {
        assert!(
            (left - right).norm() < epsilon,
            "Expected {} to be almost equal to {} with epsilon {}",
            left,
            right,
            epsilon
        );
    }

    #[rstest::rstest]
    #[case(BoxcarKernel {
        phase: crate::units::Cycles(0.0),
        scale: 1.0,
        sample_count: 10,
    }, Complex64::new(0.1, 0.0))]
    #[case(BoxcarKernel {
        phase: crate::units::Cycles(0.5),
        scale: 1.0,
        sample_count: 10,
    }, Complex64::new(-0.1, 0.0))]
    #[case(BoxcarKernel {
        phase: crate::units::Cycles(0.0),
        scale: -1.0,
        sample_count: 10,
    }, Complex64::new(-0.1, 0.0))]
    #[case(BoxcarKernel {
        phase: crate::units::Cycles(0.0),
        scale: 0.0,
        sample_count: 10,
    }, Complex64::new(0.0, 0.0))]
    fn boxcar_kernel(#[case] kernel: BoxcarKernel, #[case] expected: Complex64) {
        let iq_value = kernel.into_iq_value();
        assert_almost_eq(iq_value, expected, 1e-10);
    }

    #[rstest::rstest]
    #[case(0.0, 0.0)]
    #[case(0.0-f64::EPSILON, 0.0)]
    #[case(0.0+f64::EPSILON, 1.0)]
    // Based on a past edge case
    #[case(8.800_000_000_000_001e-8 * 1.0e9, 88.0)]
    fn ceiling_with_epsilon(#[case] value: f64, #[case] expected: f64) {
        let result = super::ceiling_with_epsilon(value);
        assert_eq!(result, expected);
    }

    /// Assert that for some exemplar waveform templates, the right IQ values are generated.
    /// This is done by comparing the generated IQ values to two snapshots:
    ///
    /// * one, a rendered IQ plot in ascii art format. This is mostly for the benefit of the reviewer.
    /// * two, the raw IQ values. At the end of the day this is all that matters.
    ///
    /// The IQ values may not need to be inspected carefully, but the benefit of the snapshot approach is that
    /// we'll be alerted when they change. The plot is only generated for shorter lists of IQ values.
    ///
    /// The plot snapshot is asserted first (before IQ values) so that the user can get a visual impression of the problem.
    ///
    /// Snapshot filenames are based on the debug representation of the waveform template, so if template fields
    /// are added, removed, or renamed, then this test will fail for those cases. Additionally, if the string printing
    /// of `Complex64` changes, then the IQ values snapshot will also change.
    #[rstest::rstest]
    #[case(ErfSquare { duration: 1e-4, risetime: 1e-5, sample_rate: 1e6, pad_left: 0.0, pad_right: 0.0, positive_polarity: true, scale: 1.0, phase: 0.0, detuning: 0.0 })]
    #[case(ErfSquare { duration: 1e-4, risetime: 1e-5, sample_rate: 1e6, pad_left: 1e-5, pad_right: 2e-5, positive_polarity: false, scale: 1.0, phase: 0.0, detuning: 0.0 })]
    #[case(ErfSquare { duration: 1e-4, risetime: 1e-4, sample_rate: 1e6, pad_left: 0.0, pad_right: 0.0, positive_polarity: false, scale: 1.0, phase: 0.0, detuning: 1e6 })]
    #[case(ErfSquare { duration: 1e-4, risetime: 1e-5, sample_rate: 1e6, pad_left: 0.0, pad_right: 0.0, positive_polarity: true, scale: 1.0, phase: 0.5, detuning: 0.0 })]
    #[case(ErfSquare { duration: 1e-4, risetime: 1e-5, sample_rate: 1e6, pad_left: 0.0, pad_right: 0.0, positive_polarity: true, scale: -1.0, phase: 0.0, detuning: 0.0 })]
    #[case(ErfSquare { duration: 1e-4, risetime: 1e-5, sample_rate: 1e6, pad_left: 0.0, pad_right: 0.0, positive_polarity: true, scale: 0.0, phase: 0.0, detuning: 0.0 })]
    #[case(ErfSquare { duration: 1e-4, risetime: 1e-5, sample_rate: 1e6, pad_left: 0.0, pad_right: 0.0, positive_polarity: false, scale: 0.5, phase: 0.4, detuning: -1e6 })]
    #[case(Gaussian { duration: 1e-4, fwhm: 1e-5, t0: 0.0, sample_rate: 1e6, scale: 1.0, phase: 0.0, detuning: 0.0 })]
    #[case(Gaussian { duration: 1e-4, fwhm: 1e-5, t0: 5e-5, sample_rate: 1e6, scale: 1.0, phase: 0.0, detuning: 1e6 })]
    #[case(Gaussian { duration: 1e-4, fwhm: 2e-5, t0: 5e-5, sample_rate: 1e6, scale: 0.5, phase: 0.0, detuning: 0.0 })]
    #[case(Gaussian { duration: 1e-4, fwhm: 4e-5, t0: 5e-5, sample_rate: 1e6, scale: 0.5, phase: 0.5, detuning: 0.0 })]
    #[case(Gaussian { duration: 1e-4, fwhm: 4e-5, t0: 5e-5, sample_rate: 1e6, scale: -1.0, phase: 0.0, detuning: 0.0 })]
    #[case(DragGaussian { duration: 1e-4, fwhm: 1e-5, t0: 0.0, anh: 1e6, alpha: 1.0, sample_rate: 1e6, scale: 1.0, phase: 0.0, detuning: 0.0 })]
    #[case(DragGaussian { duration: 1e-4, fwhm: 1e-5, t0: 0.0, anh: 1e6, alpha: 1.0, sample_rate: 1e6, scale: 1.0, phase: 0.0, detuning: 1e6 })]
    #[case(HermiteGaussian { duration: 1e-4, fwhm: 1e-5, t0: 0.0, anh: 1e6, alpha: 1.0, sample_rate: 1e6, second_order_hrm_coeff: 0.1, scale: 1.0, phase: 0.0, detuning: 0.0 })]
    #[case(HermiteGaussian { duration: 1e-4, fwhm: 1e-5, t0: 0.0, anh: 1e6, alpha: 1.0, sample_rate: 1e6, second_order_hrm_coeff: 0.1, scale: 1.0, phase: 0.0, detuning: 1e6 })]
    fn into_iq_values(#[case] parameters: impl WaveformTemplate + Copy + std::fmt::Debug) {
        let iq_values = parameters.into_iq_values();
        let count = iq_values.len();

        let all_values_zero = iq_values.iter().all(|el| el == &Complex64::new(0.0, 0.0));

        // count <= 200 prevents huge runaway plots if we test a long waveform
        // !all_values_zero prevents a useless plot that appears to render differently on different platforms, making the snapshot a poor comparison
        if count <= 200 && !all_values_zero {
            let split = iq_values.clone().into_iter().fold(
                (vec![], vec![]),
                |(mut reals, mut imags), el| {
                    reals.push(el.re);
                    imags.push(el.im);
                    (reals, imags)
                },
            );
            let split = vec![split.0, split.1];

            let res = rasciigraph::plot_many(
                split,
                rasciigraph::Config::default()
                    .with_width(count as u32 + 10)
                    .with_height(20),
            );

            // This snapshot is taken so that the developer has a visual impression of the waveform in a way that's committed to source control.
            // however, the test should only be considered a true failure if the IQ data in the next snapshot is not equal to what's expected.
            insta::assert_snapshot!(format!("{}__plot", format_snapshot_name(parameters)), res);
        }

        let neat_iq_values = iq_values
            .iter()
            .map(|el| format!("{:+.5e}, {:+.5e}", el.re, el.im))
            .collect::<Vec<_>>()
            .join("\n");

        insta::assert_snapshot!(
            format!("{}__data", format_snapshot_name(parameters)),
            neat_iq_values
        )
    }
}
