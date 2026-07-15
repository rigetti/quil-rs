//! Traits for interpreting waveform parameter types as higher-kinded type constructors.
//!
//! You probably don't need the machinery in this module unless you're trying to do some abstraction
//! over all generic waveforms, and there isn't much you can usefully do with that.  However, it
//! powers some implementation details of waveform sampling, so we expose the machinery in case it's
//! useful.

use super::{Partial, Reference, WaveformData};

/// A trait representing all builtin waveform parameter types that themselves take a generic
/// `WaveformData` parameter: [`Flat`][super::builtin::Flat],
/// [`Gaussian`][super::builtin::Gaussian`], etc.
///
/// For every waveform type constructor `W`, this trait should be implemented for all `W<T>` at
/// once.
//
// Note for developers: Implementations of this trait are generated for all the generic built-in waveforms
// (e.g., [`super::builtin::Flat`]) by [`crate::waveform::builtin::macros::define_waveforms`].  These
// implementations are trivial wrappers around the inherent methods of the same name.
pub trait WaveformParameters {
    /// The underlying generic parameter for this type.
    type WaveformData: WaveformData;

    /// `Self<T>`, where `Self` is understood as a type *constructor*.
    type WithWaveformData<T: WaveformData>: WaveformParameters<WaveformData = T>;

    /// Convert this owned waveform into an equivalent one whose (non-concrete) parameters are all
    /// references.
    fn as_ref(&self) -> Self::WithWaveformData<Reference<'_, Self::WaveformData>>;

    /// Convert one waveform into another by replacing its associated data.
    ///
    /// Given two forms of waveform data, `S` and `T`, the user specifies how to evaluate `S`'s real
    /// numbers into `T`'s real numbers and how to evaluate `S`'s complex numbers to `T`'s complex
    /// numbers.  For example, to convert parsed ([`Syntactic`][crate::waveform::Syntactic])
    /// parameters into sampleable ([`Concrete`][crate::waveform::Concrete]) parameters, you can
    /// pass [`Expression::evaluate`][crate::expression::Expression::evaluate] to this function.
    ///
    /// For a more detailed example, see the documentation for
    /// [`Waveform::try_evaluate`][crate::waveform::Waveform::try_evaluate], which has the same
    /// structure as this function.
    fn try_evaluate<T: WaveformData, E>(
        self,
        real: impl Fn(<Self::WaveformData as WaveformData>::Real) -> Result<T::Real, E>,
        complex: impl Fn(<Self::WaveformData as WaveformData>::Complex) -> Result<T::Complex, E>,
    ) -> Result<Self::WithWaveformData<T>, E>;
}

/// A trait representing all builtin waveform parameter types that themselves take a generic
/// `WaveformData` parameter, in the specific case where that parameter is [`Partial<_>`].
///
/// For every waveform type constructor `W`, this trait should be implemented for all
/// `W<Partial<T>>` at once.
//
// Note for developers: Implementations of this trait are generated for all the generic built-in waveforms
// (e.g., [`super::builtin::Flat`]) by [`crate::waveform::builtin::macros::define_waveforms`].  These
// implementations are trivial wrappers around the inherent methods of the same name.
pub trait PartialWaveformParameters:
    WaveformParameters<
    WaveformData = Partial<Self::TotalWaveformData>,
    WithWaveformData<Partial<Self::TotalWaveformData>> = Self,
>
{
    /// The underlying generic parameter that has been wrapped in [`Partial`].
    type TotalWaveformData: WaveformData;

    /// Returns `None` if any of thispartial waveform's data is missing, and returns its underlying
    /// total form otherwise.
    fn transpose(self) -> Option<Self::WithWaveformData<Self::TotalWaveformData>>;
}
