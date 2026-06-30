//! Representations of IQ value sequences.

use std::iter::{repeat_n, RepeatN};

use itertools::Either;

#[cfg(feature = "python")]
pub(crate) mod quilpy;

/// The result of sampling a waveform, representing a sequence of IQ value samples (of type `T`).
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum IqSamples<T> {
    /// A flat waveform, consisting of a single IQ value repeated some number of times.
    ///
    /// This is a more optimizable special-case representation for [`IqSamples::Samples(vec![iq;
    /// sample_count])`][Self::Samples], but is otherwise equivalent.
    Flat { iq: T, sample_count: usize },

    /// A literal sequence of IQ samples.
    Samples(Vec<T>),
}

impl<T> IqSamples<T> {
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

    /// Get a reference to the nth sample.
    pub fn get_ref(&self, index: usize) -> Option<&T> {
        match self {
            Self::Flat { iq, sample_count } => (index < *sample_count).then_some(iq),
            Self::Samples(samples) => samples.get(index),
        }
    }

    /// Get the nth sample.
    pub fn get(&self, index: usize) -> Option<T>
    where
        T: Clone,
    {
        self.get_ref(index).cloned()
    }

    /// Iterate over (references to) every sample in this sequence.
    pub fn iter(&self) -> Iter<'_, T> {
        match self {
            Self::Flat { iq, sample_count } => Either::Left(repeat_n(iq, *sample_count)),
            Self::Samples(samples) => Either::Right(samples.iter()),
        }
    }

    /// Convert this sequence of samples into an explicit vector.
    ///
    /// The length of this vector is [`self.sample_count()`][Self::sample_count], and its values are
    /// given by [`self.get(_)`][Self::get].
    pub fn into_iq_values(self) -> Vec<T>
    where
        T: Clone,
    {
        match self {
            Self::Flat { iq, sample_count } => vec![iq; sample_count],
            Self::Samples(samples) => samples,
        }
    }
}

impl<T: Clone> IntoIterator for IqSamples<T> {
    type Item = T;

    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Self::Flat { iq, sample_count } => Either::Left(repeat_n(iq, sample_count)),
            Self::Samples(samples) => Either::Right(samples.into_iter()),
        }
    }
}

impl<'a, T> IntoIterator for &'a IqSamples<T> {
    type Item = &'a T;

    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// An iterator that moves out of an [`IqSamples`].
///
/// **NOTE:** The *specific type* defining `IntoIter` is *not considered an exposed interface
/// detail*, and *is not stable*.  What is stable are the trait implementations:
///
/// - [`std::clone::Clone`] if `T: std::clone::Clone`
/// - [`std::fmt::Debug`] if `T: std::fmt::Debug`
/// - [`std::iter::Iterator<Item = T>`]
/// - [`std::iter::DoubleEndedIterator`]
/// - [`std::iter::ExactSizeIterator`]
/// - [`std::iter::FusedIterator`]
pub type IntoIter<T> = Either<RepeatN<T>, std::vec::IntoIter<T>>;

/// An iterator that provides a view of an [`IqSamples`].
///
/// **NOTE:** The *specific type* defining `Iter` is *not considered an exposed interface detail*,
/// and *is not stable*.  What is stable are the trait implementations:
///
/// - [`std::clone::Clone`]
/// - [`std::fmt::Debug`] if `T: std::fmt::Debug`
/// - [`std::iter::Iterator<Item = T>`]
/// - [`std::iter::DoubleEndedIterator`]
/// - [`std::iter::ExactSizeIterator`]
/// - [`std::iter::FusedIterator`]
pub type Iter<'a, T> = Either<RepeatN<&'a T>, std::slice::Iter<'a, T>>;

#[derive(Clone, Debug)]
struct _ExampleCloneDebug(String);

#[derive(Debug)]
struct _ExampleDebug(String);

const fn _guarantee_iterator_trait_impls<I, T>()
where
    I: std::clone::Clone
        + std::fmt::Debug
        + std::iter::Iterator<Item = T>
        + std::iter::DoubleEndedIterator
        + std::iter::ExactSizeIterator
        + std::iter::FusedIterator,
{
}

const _GUARANTEE_INTO_ITER_TRAIT_IMPLS: () =
    _guarantee_iterator_trait_impls::<IntoIter<_ExampleCloneDebug>, _ExampleCloneDebug>();

const _GUARANTEE_ITER_TRAIT_IMPLS: () =
    _guarantee_iterator_trait_impls::<Iter<'static, _ExampleDebug>, &'static _ExampleDebug>();

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize, thiserror::Error)]
pub enum SamplingError {
    #[error(
        "A duration of {duration} s cannot be discretized with a sample rate of {sample_rate} Hz, \
         as the resulting number of samples ({sample_count}) \
         is not in the representable range \u{5B}0, 2³²)."
        // U+005B is `[` (i.e., LEFT SQUARE BRACKET), but we have to hide it with an escape so as
        // not to confuse our homegrown PyO3 linter script (`quil-rs/scripts/lint-quil-rs.py`).
    )]
    SampleCountOutOfRange {
        duration: f64,
        sample_rate: f64,
        sample_count: f64,
    },

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
