//! Machinery for working with possibly-partial waveforms

use std::convert::Infallible;

use derive_where::derive_where;

use crate::waveform::{Concrete, Partial, WaveformData};

/// [`WaveformData`] that can be sampled from.
pub(super) trait Sampleable: WaveformData {
    /// Evidence that this type is or isn't partial: [`()`] if it is partial, [`Infallible`] if it
    /// isn't.  Used to make functions that are partial for partial data and total for total data.
    type Partial: Copy + Eq + std::fmt::Debug;

    /// Convert a real number to a concrete [`f64`] if the number is present.
    fn eval_real(real: Self::Real) -> Result<f64, Self::Partial>;
}

impl Sampleable for Partial<Concrete> {
    type Partial = ();

    #[inline(always)]
    fn eval_real(real: Self::Real) -> Result<f64, Self::Partial> {
        real.ok_or(())
    }
}

impl Sampleable for Concrete {
    type Partial = Infallible;

    #[inline(always)]
    fn eval_real(real: Self::Real) -> Result<f64, Self::Partial> {
        Ok(real)
    }
}

/// A value that depends on whether the input was partial or total.  If this is for total data, then
/// the partial case is impossible.
#[derive_where(Clone, Copy, PartialEq, Eq, Debug; P, T)]
pub(super) enum Value<S: Sampleable, P, T> {
    Partial(S::Partial, P),
    Total(T),
}

impl<S: Sampleable<Partial = Infallible>, P, T> Value<S, P, T> {
    pub(super) fn unwrap_total(self) -> T {
        match self {
            Value::Partial(never, _) => match never {},
            Value::Total(total) => total,
        }
    }
}
