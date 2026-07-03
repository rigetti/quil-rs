//! Machinery for working with possibly-partial waveforms

use std::convert::Infallible;

use derive_where::derive_where;

use crate::waveform::{sampling::IqSamples, Concrete, Partial, WaveformData};

use super::IqSamplesOrPlaceholder;

/// [`WaveformData`] that can be sampled from.
pub(super) trait Sampleable: WaveformData {
    /// Evidence that this type is or isn't partial: [`()`] if it is partial, [`Infallible`] if it
    /// isn't.  Used to make functions that are partial for partial data and total for total data.
    type IsPartial: Copy + Eq + std::fmt::Debug;

    /// Convert a real number to a concrete [`f64`] if the number is present.
    fn eval_real(real: Self::Real) -> Result<f64, Self::IsPartial>;
}

impl Sampleable for Partial<Concrete> {
    type IsPartial = ();

    #[inline(always)]
    fn eval_real(real: Self::Real) -> Result<f64, Self::IsPartial> {
        real.ok_or(())
    }
}

impl Sampleable for Concrete {
    type IsPartial = Infallible;

    #[inline(always)]
    fn eval_real(real: Self::Real) -> Result<f64, Self::IsPartial> {
        Ok(real)
    }
}

/// A value that depends on whether the input was partial or total.  If this is for total data, then
/// the partial case is impossible.
#[derive_where(Clone, Copy, PartialEq, Eq, Debug; P, T)]
pub(super) enum Value<S: Sampleable, P, T> {
    Partial(S::IsPartial, P),
    Total(T),
}

/// The possibly-partial version of [`IqSamplesOrPlaceholder`].
pub(super) type IqSamplesFor<S: Sampleable> = Value<S, IqSamples<()>, IqSamples<Complex64>>;

impl<S: Sampleable<IsPartial = Infallible>, P, T> Value<S, P, T> {
    pub(super) fn unwrap_total(self) -> T {
        match self {
            Self::Partial(never, _) => match never {},
            Self::Total(total) => total,
        }
    }
}

impl<S: Sampleable<IsPartial = ()>, T> Value<S, (), T> {
    pub(super) fn into_option(self) -> Option<T> {
        match self {
            Self::Partial((), ()) => None,
            Self::Total(total) => Some(total),
        }
    }
}

impl<S: Sampleable<IsPartial = ()>, P, T> Value<S, P, T> {
    pub(super) fn into_result(self) -> Result<T, P> {
        match self {
            Self::Partial((), partial) => Err(partial),
            Self::Total(total) => Ok(total),
        }
    }
}

impl IqSamplesFor<Partial<Concrete>> {
    pub(super) fn sampled_partial(self) -> IqSamplesOrPlaceholder {
        match self {
            Self::Partial((), placeholder) => IqSamplesOrPlaceholder::Placeholder(placeholder),
            Self::Total(samples) => IqSamplesOrPlaceholder::Samples(samples),
        }
    }
}
