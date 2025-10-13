//! Built-in waveforms.

use std::{
    f64::consts::{LN_2, PI},
    iter::repeat_n,
};

use ndarray::{Array, Array1};
use num_complex::{c64, Complex64};
use serde::{Deserialize, Serialize};
use statrs::function::erf::erf;

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pyclass_complex_enum};

use crate::{
    real,
    units::{Cycles, Radians},
};

use super::sampling::IqSamples;

////////////////////////////////////////////////////////////////////////////////
// General built-in waveform types
////////////////////////////////////////////////////////////////////////////////

/// One of the waveforms that is always available to Quil programs.
#[derive(Clone, Copy, PartialEq, Debug, derive_more::From)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass_complex_enum)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.waveforms", eq, frozen)
)]
pub enum BuiltinWaveform {
    // Quil-T spec
    Flat(Flat),
    Gaussian(Gaussian),
    DragGaussian(DragGaussian),
    ErfSquare(ErfSquare),
    // Rigetti extensions
    HermiteGaussian(HermiteGaussian),
    BoxcarKernel(BoxcarKernel),
}

/// Parameters that can be applied to all built-in waveforms.
#[derive(Clone, Copy, PartialEq, Debug, Serialize, Deserialize)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.waveforms", subclass, get_all, set_all, eq)
)]
pub struct CommonBuiltinParameters {
    /// Full duration of the pulse (s)
    pub duration: f64,
    /// Scale to apply to waveform envelope (default: `1.0`)
    pub scale: Option<f64>,
    /// Phase shift for the entire waveform (default: `0.0`)
    pub phase: Option<Cycles<f64>>,
    /// Explicit detuning to bake into IQ values (default: `0.0`)
    pub detuning: Option<f64>,
}

/// Like [`CommonBuiltinParameters`], but with the defaults resolved.
#[derive(Clone, Copy, PartialEq, Debug, Serialize, Deserialize)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.waveforms", subclass, get_all, set_all, eq)
)]
pub struct ExplicitCommonBuiltinParameters {
    /// Full duration of the pulse (s)
    pub duration: f64,
    /// Scale to apply to waveform envelope
    pub scale: f64,
    /// Phase shift for the entire waveform
    pub phase: Cycles<f64>,
    /// Explicit detuning to bake into IQ values
    pub detuning: f64,
}

impl From<CommonBuiltinParameters> for ExplicitCommonBuiltinParameters {
    fn from(parameters: CommonBuiltinParameters) -> Self {
        let CommonBuiltinParameters {
            duration,
            scale,
            phase,
            detuning,
        } = parameters;
        Self {
            duration,
            scale: scale.unwrap_or(1.0),
            phase: phase.unwrap_or(Cycles(0.0)),
            detuning: detuning.unwrap_or(0.0),
        }
    }
}

/// A trait for all built-in waveform types.
///
/// This is a closed trait, corresponding precisely to the constructors of [`BuiltinWaveform`] as
/// well as [`BuiltinWaveform`] itself.
pub trait BuiltinWaveformParameters:
    Into<BuiltinWaveform> + Copy + PartialEq + std::fmt::Debug + private::Sealed
{
    /// Convert this waveform into a sequence of sampled IQ values.
    fn iq_values_at_sample_rate(
        self,
        common: CommonBuiltinParameters,
        sample_rate: f64,
    ) -> IqSamples;
}

mod private {
    pub trait Sealed {}
    impl Sealed for super::BuiltinWaveform {}
    impl Sealed for super::Flat {}
    impl Sealed for super::Gaussian {}
    impl Sealed for super::DragGaussian {}
    impl Sealed for super::ErfSquare {}
    impl Sealed for super::HermiteGaussian {}
    impl Sealed for super::BoxcarKernel {}
}

////////////////////////////////////////////////////////////////////////////////
// Built-in waveform types (parameters only)
////////////////////////////////////////////////////////////////////////////////

/// A flat waveform, repeating a given IQ value for the given duration.
///
/// This waveform is part of the [Quil-T][] spec ([§12.2, Waveforms][]).
///
/// [Quil-T]: https://quil-lang.github.io/#12Annex-T--Pulse-Level-Control
/// [§12.2, Waveforms]: https://quil-lang.github.io/#12-2Waveforms
#[derive(Clone, Copy, PartialEq, Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.waveforms", subclass, get_all, set_all, eq)
)]
pub struct Flat {
    /// The IQ value to play
    pub iq: Complex64,
}

/// A waveform with a Gaussian shape.
///
/// This waveform is part of the [Quil-T][] spec ([§12.2, Waveforms][]).
///
/// [Quil-T]: https://quil-lang.github.io/#12Annex-T--Pulse-Level-Control
/// [§12.2, Waveforms]: https://quil-lang.github.io/#12-2Waveforms
#[derive(Clone, Copy, PartialEq, Debug, Serialize, Deserialize)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.waveforms", subclass, get_all, set_all, eq)
)]
pub struct Gaussian {
    /// Full width half maximum of the pulse (s)
    pub fwhm: f64,
    /// Center/offset for pulse centroid (s)
    pub t0: f64,
}

/// Creates a waveform with a DRAG-corrected Gaussian shape.
///
/// This is a Gaussian shape with an additional component proportional to the time derivative of the
/// main Gaussian pulse.
///
/// For details, see "Simple Pulses for Elimination of Leakage in Weakly Nonlinear Qubits",
/// F. Motzoi, J. M. Gambetta, J. M., P. Rebentrost, and F. K. Wilhelm, Physical Review Letters 103,
/// 110501 (September 8, 2009).  DOI: 10.1103/PhysRevLett.103.110501; publication URL:
/// <https://journals.aps.org/prl/abstract/10.1103/PhysRevLett.103.110501>, preprint URL:
/// <https://arxiv.org/abs/0901.0534>.
///
/// This waveform is part of the [Quil-T][] spec ([§12.2, Waveforms][]).
///
/// [Quil-T]: https://quil-lang.github.io/#12Annex-T--Pulse-Level-Control
/// [§12.2, Waveforms]: https://quil-lang.github.io/#12-2Waveforms
#[derive(Clone, Copy, PartialEq, Debug, Serialize, Deserialize)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.waveforms", subclass, get_all, set_all, eq)
)]
pub struct DragGaussian {
    /// Full width half maximum of the pulse (s)
    pub fwhm: f64,
    /// Center/offset for pulse centroid (s)
    pub t0: f64,
    /// Qubit anharmonicity - sets rate of evolution for the imaginary term (Hz)
    pub anh: f64,
    /// DRAG parameter - controls strength of the imaginary term
    pub alpha: f64,
}

/// A waveform with a flat top and edges that are error functions (erfs).
///
/// This waveform is part of the [Quil-T][] spec ([§12.2, Waveforms][]).
///
/// [Quil-T]: https://quil-lang.github.io/#12Annex-T--Pulse-Level-Control
/// [§12.2, Waveforms]: https://quil-lang.github.io/#12-2Waveforms
#[derive(Clone, Copy, PartialEq, Debug, Serialize, Deserialize)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.waveforms", subclass, get_all, set_all, eq)
)]
pub struct ErfSquare {
    /// Slope of erf shoulders (2x FWHM of erf in s)
    pub risetime: f64,
    /// Length of zero padding to add to beginning of pulse (s)
    pub pad_left: f64,
    /// Length of zero padding to add to end of pulse (s)
    pub pad_right: f64,
}

/// Creates a Hermite Gaussian waveform.
///
/// This extends the basic DRAG pulse by adding an additional imaginary term to the pulse envelope
/// consisting of a Gaussian pulse modified by the second order Hermite polynomial.
///
/// For details, see "Effects of arbitrary laser or NMR pulse shapes on population inversion and
/// coherence", Warren S. Warren, The Journal of Chemical Physics 81(12) (December 20, 1984).  DOI:
/// 10.1063/1.447644; publication URL:
/// <https://pubs.aip.org/aip/jcp/article-abstract/81/12/5437/90781/Effects-of-arbitrary-laser-or-NMR-pulse-shapes-on>.
///
/// This waveform is a Rigetti extension to Quil-T.
#[derive(Clone, Copy, PartialEq, Debug, Serialize, Deserialize)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.waveforms", subclass, get_all, set_all, eq)
)]
pub struct HermiteGaussian {
    /// Full width half maximum of the pulse (s)
    pub fwhm: f64,
    /// Center/offset for pulse centroid (s)
    pub t0: f64,
    /// Qubit anharmonicity - sets rate of evolution for the imaginary term (Hz)
    pub anh: f64,
    /// DRAG parameter - controls strength of the imaginary term
    pub alpha: f64,
    /// Coefficient of the second order Hermite polynomial term.
    pub second_order_hrm_coeff: f64,
}

/// A boxcar waveform.
///
/// This waveform is a Rigetti extension to Quil-T.
#[derive(Clone, Copy, PartialEq, Debug, Serialize, Deserialize)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.waveforms", subclass, eq)
)]
pub struct BoxcarKernel;

////////////////////////////////////////////////////////////////////////////////
// IQ sample computation
////////////////////////////////////////////////////////////////////////////////

impl BuiltinWaveformParameters for BuiltinWaveform {
    /// Sample the given waveform (with the additional common parameters) at the given sample rate
    /// (in Hz).
    fn iq_values_at_sample_rate(
        self,
        common: CommonBuiltinParameters,
        sample_rate: f64,
    ) -> IqSamples {
        match self {
            BuiltinWaveform::Flat(flat) => flat.iq_values_at_sample_rate(common, sample_rate),

            BuiltinWaveform::Gaussian(gaussian) => {
                gaussian.iq_values_at_sample_rate(common, sample_rate)
            }

            BuiltinWaveform::DragGaussian(drag_gaussian) => {
                drag_gaussian.iq_values_at_sample_rate(common, sample_rate)
            }

            BuiltinWaveform::ErfSquare(erf_square) => {
                erf_square.iq_values_at_sample_rate(common, sample_rate)
            }

            BuiltinWaveform::HermiteGaussian(hermite_gaussian) => {
                hermite_gaussian.iq_values_at_sample_rate(common, sample_rate)
            }

            BuiltinWaveform::BoxcarKernel(boxcar_kernel) => {
                boxcar_kernel.iq_values_at_sample_rate(common, sample_rate)
            }
        }
    }
}

impl BuiltinWaveformParameters for Flat {
    fn iq_values_at_sample_rate(
        self,
        common: CommonBuiltinParameters,
        sample_rate: f64,
    ) -> IqSamples {
        let ExplicitCommonBuiltinParameters {
            duration,
            scale,
            phase,
            detuning,
        } = common.into();
        let Self { iq } = self;

        // TODO: no verification of integer sample count
        let sample_count = ceiling_with_epsilon(duration * sample_rate) as usize;
        let scaled_iq = scale * iq;

        if detuning == 0.0 {
            IqSamples::Flat {
                iq: apply_phase(scaled_iq, phase),
                sample_count,
            }
        } else {
            let mut samples = vec![scaled_iq; sample_count];
            apply_phase_and_detuning(&mut samples, phase, detuning, sample_rate);
            IqSamples::Samples(samples)
        }
    }
}

impl BuiltinWaveformParameters for Gaussian {
    fn iq_values_at_sample_rate(
        self,
        common: CommonBuiltinParameters,
        sample_rate: f64,
    ) -> IqSamples {
        let Self { fwhm, t0 } = self;

        build_samples_and_adjust_for_common_parameters(
            SamplingParameters { sample_rate, fwhm },
            common,
            |SamplingInfo { time_steps, sigma }| {
                time_steps
                    .into_iter()
                    .map(move |el| real!((-0.5 * (el - t0).powf(2.0) / sigma.powf(2.0)).exp()))
            },
        )
    }
}

impl BuiltinWaveformParameters for DragGaussian {
    fn iq_values_at_sample_rate(
        self,
        common: CommonBuiltinParameters,
        sample_rate: f64,
    ) -> IqSamples {
        let Self {
            fwhm,
            t0,
            anh,
            alpha,
        } = self;

        build_samples_and_adjust_for_common_parameters(
            SamplingParameters { sample_rate, fwhm },
            common,
            |SamplingInfo { time_steps, sigma }| {
                time_steps.into_iter().map(move |el| {
                    // Generate envelope sample
                    let env = (-0.5 * (el - t0).powf(2.0) / sigma.powf(2.0)).exp();
                    // Generate modified envelope sample
                    let env_mod =
                        (alpha * (1.0 / (2.0 * PI * anh * sigma.powf(2.0)))) * (el - t0) * env;
                    c64(env, env_mod)
                })
            },
        )
    }
}

impl BuiltinWaveformParameters for ErfSquare {
    fn iq_values_at_sample_rate(
        self,
        common: CommonBuiltinParameters,
        sample_rate: f64,
    ) -> IqSamples {
        let CommonBuiltinParameters { duration, .. } = common;
        let Self {
            risetime,
            pad_left,
            pad_right,
        } = self;

        let fwhm = 0.5 * risetime;
        let t1 = fwhm;
        let t2 = duration - fwhm;

        build_samples_and_adjust_for_common_parameters(
            SamplingParameters { sample_rate, fwhm },
            common,
            |SamplingInfo { time_steps, sigma }| {
                let waveform = time_steps
                    .into_iter()
                    .map(move |el| real!(0.5 * (erf((el - t1) / sigma) - erf((el - t2) / sigma))));

                let left_padding = repeat_n(real!(0.0), (pad_left * sample_rate).ceil() as usize);
                let right_padding = repeat_n(real!(0.0), (pad_right * sample_rate).ceil() as usize);

                left_padding.chain(waveform).chain(right_padding)
            },
        )
    }
}

impl BuiltinWaveformParameters for HermiteGaussian {
    fn iq_values_at_sample_rate(
        self,
        common: CommonBuiltinParameters,
        sample_rate: f64,
    ) -> IqSamples {
        let Self {
            fwhm,
            t0,
            anh,
            alpha,
            second_order_hrm_coeff,
        } = self;

        build_samples_and_adjust_for_common_parameters(
            SamplingParameters { sample_rate, fwhm },
            common,
            |SamplingInfo { time_steps, sigma }| {
                let deriv_prefactor = -alpha / (2f64 * PI * anh);

                time_steps.into_iter().map(move |el| {
                    let exp_t = 0.5 * (el - t0).powf(2.0) / sigma.powf(2.0);
                    let g = (-exp_t).exp();
                    let env = (1.0 - second_order_hrm_coeff * exp_t) * g;
                    let env_derived = deriv_prefactor * (el - t0) / sigma.powf(2.0)
                        * g
                        * (second_order_hrm_coeff * (exp_t - 1.0) - 1.0);
                    c64(env, env_derived)
                })
            },
        )
    }
}

impl BuiltinWaveformParameters for BoxcarKernel {
    fn iq_values_at_sample_rate(
        self,
        common: CommonBuiltinParameters,
        sample_rate: f64,
    ) -> IqSamples {
        let ExplicitCommonBuiltinParameters {
            duration,
            scale,
            phase,
            detuning,
        } = common.into();
        let Self = self; // Get errors if the definition changes

        // TODO: no verification of integer sample count
        let sample_count = ceiling_with_epsilon(duration * sample_rate) as usize;

        if detuning == 0.0 {
            let iq = polar_to_rectangular(scale / sample_count as f64, phase);
            IqSamples::Flat { iq, sample_count }
        } else {
            let samples = (0..sample_count).map(|index| {
                polar_to_rectangular(
                    scale / sample_count as f64,
                    Cycles(detuning * (index as f64) / sample_rate) + phase,
                )
            });
            IqSamples::Samples(samples.collect())
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
// Utility functions
////////////////////////////////////////////////////////////////////////////////

/// Parameters used uniformly in every waveform generator that produces a vector of samples.
#[derive(Clone, Copy, Debug)]
struct SamplingParameters {
    sample_rate: f64,
    fwhm: f64,
}

/// Derived information used in every waveform generator that produces a vector of samples.
#[derive(Clone, Debug)]
struct SamplingInfo {
    time_steps: Array1<f64>,
    sigma: f64,
}

/// Encapsulates the common pattern for generating a sequence of samples for a waveform:
/// 1. Generate information from some generally-used parameters.
/// 2. Use that information to generate the samples.
/// 3. Rescale those samples.
/// 4. Apply the phase adjustment and detuning to those samples.
fn build_samples_and_adjust_for_common_parameters<I: IntoIterator<Item = Complex64>>(
    parameters: SamplingParameters,
    common: CommonBuiltinParameters,
    build: impl FnOnce(SamplingInfo) -> I,
) -> IqSamples {
    let SamplingParameters { sample_rate, fwhm } = parameters;
    let ExplicitCommonBuiltinParameters {
        duration,
        scale,
        phase,
        detuning,
    } = common.into();

    // It would be nice to be able to optimize `scale: 0.0` to `IqSamples::Flat`, but we'd have to
    // figure out how to take care of the `erf_squared` padding durations.

    let length = ceiling_with_epsilon(duration * sample_rate);
    let time_steps = Array::range(0.0, length, 1.0) / sample_rate;
    let sigma = 0.5 * fwhm / (2.0 * LN_2).sqrt();

    let mut samples: Vec<_> = build(SamplingInfo { time_steps, sigma })
        .into_iter()
        .collect();

    for (index, sample) in samples.iter_mut().enumerate() {
        *sample =
            apply_phase_and_detuning_at_index(scale * *sample, phase, detuning, sample_rate, index);
    }

    IqSamples::Samples(samples)
}

/// Modulate and phase shift waveform IQ data in place.
pub fn apply_phase_and_detuning(
    iq_values: &mut [Complex64],
    phase: Cycles<f64>,
    detuning: f64,
    sample_rate: f64,
) {
    for (index, value) in iq_values.iter_mut().enumerate() {
        *value = apply_phase_and_detuning_at_index(*value, phase, detuning, sample_rate, index);
    }
}

/// Apply phase offset and detuning to a single sample.
#[inline]
pub(super) fn apply_phase_and_detuning_at_index(
    iq_value: Complex64,
    phase: Cycles<f64>,
    detuning: f64,
    sample_rate: f64,
    index: usize,
) -> Complex64 {
    apply_phase(
        iq_value,
        Cycles(detuning * (index as f64) / sample_rate + phase.0),
    )
}

/// Apply a phase offset to a single sample
#[inline]
fn apply_phase(iq_value: Complex64, phase: Cycles<f64>) -> Complex64 {
    iq_value * Complex64::cis(Radians::from(phase).0)
}

/// A custom `ceil()` method that includes a machine-epsilon sized region above each integer in the
/// set of values that are mapped to that integer. In other words:
/// ceil_eps(x) = n, for all x and integers n s.t. n <= x < (n + n*epsilon)
///
/// To handle accumulated floating point errors in sweeps above typical floating point imprecision
/// we make epsilon 10x larger than floating point epsilon.
fn ceiling_with_epsilon(value: f64) -> f64 {
    // This isn't really what EPSILON is for:
    // <https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/>.
    // However, changing this now might break behavior, so we leave it be.
    let truncated = value - (value * 10.0 * f64::EPSILON);
    truncated.ceil()
}

/// Convert polar coordinates to rectangular coordinates.
fn polar_to_rectangular(magnitude: f64, angle: Cycles<f64>) -> Complex64 {
    Complex64::from_polar(magnitude, Radians::from(angle).0)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Clean up the debug representation of waveform data to something more apt for a filename.
    ///
    /// It's important that this remain stable or there'll be a mess of updating snapshot files.
    fn format_snapshot_name(
        waveform: impl BuiltinWaveformParameters,
        common: CommonBuiltinParameters,
        tag: &str,
    ) -> String {
        format!("{waveform:?}__{common:?}__{tag}")
            .replace(['{', '}', ':', ','], "")
            .replace([' ', '.'], "_")
    }

    fn assert_almost_eq(left: Complex64, right: Complex64, epsilon: f64) {
        assert!(
            (left - right).norm() < epsilon,
            "Expected {left} to be almost equal to {right} with epsilon {epsilon}"
        );
    }

    #[rstest::rstest]
    #[case(1.0, Cycles(0.0), Complex64::new(0.1, 0.0))]
    #[case(1.0, Cycles(0.5), Complex64::new(-0.1, 0.0))]
    #[case(-1.0, Cycles(0.0), Complex64::new(-0.1, 0.0))]
    #[case(0.0, Cycles(0.0), Complex64::new(0.0, 0.0))]
    fn boxcar_kernel(#[case] scale: f64, #[case] phase: Cycles<f64>, #[case] expected: Complex64) {
        match BoxcarKernel.iq_values_at_sample_rate(
            CommonBuiltinParameters {
                duration: 0.1,
                scale: Some(scale),
                phase: Some(phase),
                detuning: None,
            },
            100.0,
        ) {
            IqSamples::Flat { iq, sample_count } => {
                assert_eq!(sample_count, 10);
                assert_almost_eq(iq, expected, 1e-10);
            }
            IqSamples::Samples(samples) => {
                panic!(
                    "Boxcar kernel must report a flat result, but got samples: {samples:?}",
                    samples = samples
                );
            }
        };
    }

    #[rstest::rstest]
    #[case(0.0, 0.0)]
    #[case(-f64::EPSILON, 0.0)]
    #[case(f64::EPSILON, 1.0)]
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
    #[case(
        ErfSquare { risetime: 1e-5, pad_left: 0.0, pad_right: 0.0 },
        CommonBuiltinParameters { duration: 1e-4, scale: Some(1.0), phase: Some(Cycles(0.0)), detuning: Some(0.0)},
    )]
    #[case(
        ErfSquare { risetime: 1e-5, pad_left: 0.0, pad_right: 0.0 },
        CommonBuiltinParameters { duration: 1e-4, scale: Some(1.0), phase: Some(Cycles(0.5)), detuning: Some(0.0)},
    )]
    #[case(
        ErfSquare { risetime: 1e-5, pad_left: 0.0, pad_right: 0.0 },
        CommonBuiltinParameters { duration: 1e-4, scale: Some(-1.0), phase: Some(Cycles(0.0)), detuning: Some(0.0)},
    )]
    #[case(
        ErfSquare { risetime: 1e-5, pad_left: 0.0, pad_right: 0.0 },
        CommonBuiltinParameters { duration: 1e-4, scale: Some(0.0), phase: Some(Cycles(0.0)), detuning: Some(0.0)},
    )]
    #[case(
        Gaussian { fwhm: 1e-5, t0: 0.0 },
        CommonBuiltinParameters { duration: 1e-4, scale: Some(1.0), phase: Some(Cycles(0.0)), detuning: Some(0.0)},
    )]
    #[case(
        Gaussian { fwhm: 1e-5, t0: 5e-5 },
        CommonBuiltinParameters { duration: 1e-4, scale: Some(1.0), phase: Some(Cycles(0.0)), detuning: Some(1e6)},
    )]
    #[case(
        Gaussian { fwhm: 2e-5, t0: 5e-5 },
        CommonBuiltinParameters { duration: 1e-4, scale: Some(0.5), phase: Some(Cycles(0.0)), detuning: Some(0.0)},
    )]
    #[case(
        Gaussian { fwhm: 4e-5, t0: 5e-5 },
        CommonBuiltinParameters { duration: 1e-4, scale: Some(0.5), phase: Some(Cycles(0.5)), detuning: Some(0.0)},
    )]
    #[case(
        Gaussian { fwhm: 4e-5, t0: 5e-5 },
        CommonBuiltinParameters { duration: 1e-4, scale: Some(-1.0), phase: Some(Cycles(0.0)), detuning: Some(0.0)},
    )]
    #[case(
        DragGaussian { fwhm: 1e-5, t0: 0.0, anh: 1e6, alpha: 1.0 },
        CommonBuiltinParameters { duration: 1e-4, scale: Some(1.0), phase: Some(Cycles(0.0)), detuning: Some(0.0)},
    )]
    #[case(
        DragGaussian { fwhm: 1e-5, t0: 0.0, anh: 1e6, alpha: 1.0 },
        CommonBuiltinParameters { duration: 1e-4, scale: Some(1.0), phase: Some(Cycles(0.0)), detuning: Some(1e6)},
    )]
    #[case(
        HermiteGaussian { fwhm: 1e-5, t0: 0.0, anh: 1e6, alpha: 1.0, second_order_hrm_coeff: 0.1 },
        CommonBuiltinParameters { duration: 1e-4, scale: Some(1.0), phase: Some(Cycles(0.0)), detuning: Some(0.0)},
    )]
    #[case(
        HermiteGaussian { fwhm: 1e-5, t0: 0.0, anh: 1e6, alpha: 1.0, second_order_hrm_coeff: 0.1 },
        CommonBuiltinParameters { duration: 1e-4, scale: Some(1.0), phase: Some(Cycles(0.0)), detuning: Some(1e6)},
    )]
    fn into_iq_values(
        #[case] parameters: impl BuiltinWaveformParameters,
        #[case] common: CommonBuiltinParameters,
    ) {
        let iq_values = parameters
            .iq_values_at_sample_rate(common, 1e6)
            .into_iq_values();
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
            insta::assert_snapshot!(format_snapshot_name(parameters, common, "plot"), res);
        }

        let neat_iq_values = iq_values
            .iter()
            .map(|el| format!("{:+.5e}, {:+.5e}", el.re, el.im))
            .collect::<Vec<_>>()
            .join("\n");

        insta::assert_snapshot!(
            format_snapshot_name(parameters, common, "data"),
            neat_iq_values
        )
    }
}
