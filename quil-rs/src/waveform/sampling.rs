//! Representations of IQ value sequences.

use num_complex::Complex64;

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::gen_stub_pyclass_complex_enum;

/// The result of sampling a waveform, representing a sequence of IQ-value samples.
#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass_complex_enum)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.waveforms", eq, frozen)
)]
pub enum IqSamples {
    /// A flat waveform, consisting of a single IQ value repeated some number of times.
    ///
    /// This is a more optimizable special-case representation for [`IqSamples::Samples(vec![iq;
    /// sample_count])`][Self::Samples], but is otherwise equivalent.
    Flat { iq: Complex64, sample_count: usize },

    /// A literal sequence of IQ samples.
    Samples(Vec<Complex64>),
}

impl IqSamples {
    /// The number of samples.
    pub fn sample_count(&self) -> usize {
        match self {
            Self::Flat {
                sample_count,
                iq: _,
            } => *sample_count,
            Self::Samples(samples) => samples.len(),
        }
    }

    /// Get the nth sample.
    pub fn get(&self, index: usize) -> Option<Complex64> {
        match self {
            Self::Flat { iq, sample_count } => (index < *sample_count).then_some(*iq),
            Self::Samples(samples) => samples.get(index).copied(),
        }
    }

    /// Convert this sequence of samples into an explicit vector.
    ///
    /// The length of this vector is [`self.sample_count()`][Self::sample_count], and its values are
    /// given by [`self.get(_)`][Self::get].
    pub fn into_iq_values(self) -> Vec<Complex64> {
        match self {
            Self::Flat { iq, sample_count } => vec![iq; sample_count],
            Self::Samples(samples) => samples,
        }
    }
}

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize, thiserror::Error)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass_complex_enum)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.waveforms", eq, frozen)
)]
pub enum SamplingError {
    #[error(
        "A duration of {duration} s cannot be produced with a sample rate of {sample_rate} Hz.  \
         There is a {misalignment_type} of {abs_misalignment} s that is not accounted for, \
         but the largest allowed gap or surplus is is {max_misalignment} s.",
        misalignment_type = if *misalignment < 0.0 { "gap" } else { "surplus" },
        abs_misalignment = misalignment.abs(),
    )]
    MisalignedDuration {
        duration: f64,
        sample_rate: f64,
        misalignment: f64,
        max_misalignment: f64,
    },
}
