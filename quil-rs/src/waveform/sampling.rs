//! Representations of IQ value sequences.

use num_complex::Complex64;

/// The result of sampling a waveform, representing a sequence of IQ-value samples.
#[derive(Clone, PartialEq, Debug)]
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
    /// The length of this vector will be [`self.sample_count()`][Self::sample_count], and its
    /// values are given by [`self.get(_)`][Self::get].
    pub fn into_iq_values(self) -> Vec<Complex64> {
        match self {
            Self::Flat { iq, sample_count } => vec![iq; sample_count],
            Self::Samples(samples) => samples,
        }
    }
}
