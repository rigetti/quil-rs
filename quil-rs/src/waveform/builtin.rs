//! Built-in waveforms.

use std::{
    f64::consts::{LN_2, PI},
    iter::repeat_n,
};

use derive_where::derive_where;
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

use super::{
    sampling::{IqSamples, SamplingError},
    Concrete, Syntactic, WaveformType,
};

////////////////////////////////////////////////////////////////////////////////
// General built-in waveform types
////////////////////////////////////////////////////////////////////////////////

/// One of the waveforms that is always available to Quil programs.
#[derive_where(Clone, PartialEq, Debug)]
#[derive_where(Copy; T::Real, T::Complex)]
#[derive(derive_more::From)]
pub enum BuiltinWaveform<T: WaveformType> {
    // Quil-T spec
    Flat(Flat<T>),
    Gaussian(Gaussian<T>),
    DragGaussian(DragGaussian<T>),
    ErfSquare(ErfSquare<T>),
    // Rigetti extensions
    HermiteGaussian(HermiteGaussian<T>),
    BoxcarKernel(BoxcarKernel),
}

/// Parameters that can be applied to all built-in waveforms.
#[derive_where(Clone, PartialEq, Debug)]
#[derive_where(Copy, Serialize, Deserialize; T::Real)]
pub struct CommonBuiltinParameters<T: WaveformType> {
    /// Full duration of the pulse, in seconds.
    ///
    /// Note that this is *always* a concrete real number, even for [`Syntactic`] parameters!  It
    /// must be possible to know the exact duration of a waveform at all times.
    pub duration: f64,

    /// Scale to apply to waveform envelope (default: `1.0`).
    pub scale: Option<T::Real>,

    /// Phase shift for the entire waveform (default: `0.0`).
    pub phase: Option<Cycles<T::Real>>,

    /// Explicit detuning to bake into IQ values (default: `0.0`).
    pub detuning: Option<T::Real>,
}

/// Like [`CommonBuiltinParameters<Concrete>`], but with the defaults resolved and the duration
/// discretized.
///
/// This does not take a [`WaveformType`] parameter because it only makes sense for [`Concrete`]
/// waveforms.
#[derive(Clone, Copy, PartialEq, Debug, Serialize, Deserialize)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.waveforms", subclass, get_all, set_all, eq)
)]
pub struct ExplicitCommonBuiltinParameters {
    /// Integral number of samples that should be taken of the pulse
    ///
    /// Corresponds to [`CommonBuiltinParameters::duration`].
    pub sample_count: f64,

    /// Scale to apply to waveform envelope.
    pub scale: f64,

    /// Phase shift for the entire waveform.
    pub phase: Cycles<f64>,

    /// Explicit detuning to bake into IQ values.
    pub detuning: f64,
}

impl<S: WaveformType> CommonBuiltinParameters<S> {
    pub fn try_evaluate<T: WaveformType, E>(
        self,
        real: impl Fn(S::Real) -> Result<T::Real, E>,
        complex: impl Fn(S::Complex) -> Result<T::Complex, E>,
    ) -> Result<CommonBuiltinParameters<T>, E> {
        let _ = complex; // We want a uniform API
        let Self {
            duration,
            scale,
            phase,
            detuning,
        } = self;
        Ok(CommonBuiltinParameters {
            duration,
            scale: scale.map(&real).transpose()?,
            phase: phase.map(|phase| phase.try_map(&real)).transpose()?,
            detuning: detuning.map(&real).transpose()?,
        })
    }
}

impl CommonBuiltinParameters<Concrete> {
    /// Given a sample rate, return the corresponding [explicit
    /// parameters][ExplicitCommonBuiltinParameters].
    ///
    /// For the three optional fields ([`scale`][Self::scale], [`phase`][Self::phase], and
    /// [`detuning`][Self::detuning]), the explicit version is either their original value (if
    /// present) or their default value (if missing).
    ///
    /// For the [`duration`][Self::duration], the explicit version is [the integer number of
    /// samples][ExplicitCommonBuiltinParameters::sample_count] required to fill out the
    /// [`Self::duration`].  If the requested duration is misaligned with respect to the sample
    /// rate, returns an error.
    ///
    /// *Misalignment* means that to fill the duration would require a nonintegral number of
    /// samples.  This function will ignore any error less than 1% – that is, if the function would
    /// an integer number of samples plus one fractional sample of size between 99% and 101%, then
    /// that last sample is considered another integral sample.
    #[inline]
    pub fn resolve_with_sample_rate(
        self,
        sample_rate: f64,
    ) -> Result<ExplicitCommonBuiltinParameters, SamplingError> {
        let CommonBuiltinParameters {
            duration,
            scale,
            phase,
            detuning,
        } = self;

        let sample_count_fract = duration * sample_rate;
        let sample_count = sample_count_fract.round();
        let misalignment = sample_count_fract - sample_count;
        let max_misalignment = 1.0 / (sample_rate * 100.0);
        if misalignment.abs() >= max_misalignment {
            return Err(SamplingError::MisalignedDuration {
                duration,
                sample_rate,
                misalignment,
                max_misalignment,
            });
        }

        Ok(ExplicitCommonBuiltinParameters {
            sample_count,
            scale: scale.unwrap_or(1.0),
            phase: phase.unwrap_or(Cycles(0.0)),
            detuning: detuning.unwrap_or(0.0),
        })
    }
}

/// A trait for all built-in waveform types.
///
/// This is a closed trait, corresponding precisely to the constructors of [`BuiltinWaveform`] as
/// well as [`BuiltinWaveform`] itself, all instantiated with [`Concrete`].
pub trait BuiltinWaveformParameters:
    Into<BuiltinWaveform<Concrete>> + Copy + PartialEq + std::fmt::Debug + private::Sealed
{
    /// Convert this waveform into a sequence of sampled IQ values.
    fn iq_values_at_sample_rate(
        self,
        common: CommonBuiltinParameters<Concrete>,
        sample_rate: f64,
    ) -> Result<IqSamples, SamplingError>;
}

////////////////////////////////////////////////////////////////////////////////
// Built-in waveform types (parameters only)
////////////////////////////////////////////////////////////////////////////////

macro_rules! field_parser {
    (ConcreteReal) => {
        super::concrete_real
    };
    (Real) => {
        super::mandatory
    };
    (Complex) => {
        super::mandatory
    };
}

macro_rules! field_type {
    ($waveform_type:ident, $field_type:ident) => {
        field_type!(($waveform_type), $field_type)
    };
    (($($waveform_type:tt)+), ConcreteReal) => {
        f64
    };
    (($($waveform_type:tt)+), Real) => {
        $($waveform_type)+::Real
    };
    (($($waveform_type:tt)+), Complex) => {
        $($waveform_type)+::Complex
    };
}

macro_rules! field_derive_where {
    ($($ty:ident)?; ConcreteReal $(, $($rest:tt)*)?) => {
        field_derive_where!($($ty)?; $($($rest)*)?)
    };
    ($(Real)?; Real $(, $($rest:tt)*)?) => {
        field_derive_where!(Real; $($($rest)*)?)
    };
    ($(Complex)?; Complex $(, $($rest:tt)*)?) => {
        field_derive_where!(Complex; $($($rest)*)?)
    };
    ($ty:ident;) => {
        T::$ty
    };
    ($($rest:tt)*) => {
        compile_error!(concat!(
            "Invalid struct for `field_derive_where!`:",
            stringify!($($rest)*)
        ))
    }
}

macro_rules! field_evaluator {
    (ConcreteReal, real = $real:ident, complex = $complex:ident, $field:expr) => {
        $field
    };
    (Real, real = $real:ident, complex = $complex:ident, $field:expr) => {
        $real($field)?
    };
    (Complex, real = $real:ident, complex = $complex:ident, $field:expr) => {
        $complex($field)?
    };
}

macro_rules! waveform_source {
    (QuilT) => {
        r#"This waveform is part of the [Quil-T][] spec ([§12.2, Waveforms][]).

[Quil-T]: https://quil-lang.github.io/#12Annex-T--Pulse-Level-Control
[§12.2, Waveforms]: https://quil-lang.github.io/#12-2Waveforms"#
    };
    (Rigetti) => {
        r#"This waveform is a Rigetti extension to Quil-T."#
    };
}

macro_rules! concrete_waveform {
    ($name:ident, $($field:ident)+) => {
        super::$name<super::Concrete>
    };
    ($name:ident,) => {
        super::$name
    };
}

macro_rules! define_python_waveform_wrapper {
    (
        $name:ident,
        $waveform_type:ident,
        derive($($derive:ident),*),
        { $($field:ident: $ty:ident),+ }
    ) => {
        paste::paste! {
            #[derive(Clone, PartialEq, Debug $(, $derive)*)]
            #[cfg_attr(feature = "stubs", gen_stub_pyclass)]
            #[cfg_attr(
                feature = "python",
                pyo3::pyclass(module = "quil.waveforms", subclass, eq)
            )]
            pub struct [<$waveform_type $name>](pub $name<$waveform_type>);

            #[cfg(feature = "python")]
            #[pyo3::pymethods]
            impl [<$waveform_type $name>] {
                $(
                    #[getter($field)]
                    fn [<py_get_$field>](
                        &self
                    ) -> field_type!((<$waveform_type as WaveformType>), $ty) {
                        self.0.$field.clone()
                    }

                    #[setter($field)]
                    fn [<py_set_$field>](
                        &mut self,
                        $field: field_type!((<$waveform_type as WaveformType>), $ty),
                    ) {
                        self.0.$field = $field;
                    }
                )+
            }
        }
    }
}

macro_rules! define_python_waveform {
    ($name:ident) => {};

    ($name:ident { $($field:ident: $ty:ident),+ }) => {
        define_python_waveform_wrapper!($name, Syntactic, derive(), { $($field: $ty),+ });
        define_python_waveform_wrapper!($name, Concrete, derive(Copy), { $($field: $ty),+ });

        paste::paste! {
            #[cfg(feature = "python")]
            #[pyo3::pymethods]
            impl [<Syntactic $name>] {
                #[pyo3(name = "try_evaluate")]
                fn py_try_evaluate(
                    &self,
                    variables: std::collections::HashMap<String, Complex64>,
                    memory_references: std::collections::HashMap<String, Vec<f64>>,
                ) -> pyo3::PyResult<[<Concrete $name>]> {
                    let complex = |expr: crate::expression::Expression| {
                        expr.evaluate(&variables, &memory_references)
                    };

                    let real = |expr| {
                        let Complex64 { re, im } = complex(expr)?;
                        if im == 0.0 {
                            Ok(re)
                        } else {
                            Err(crate::expression::EvaluationError::NumberNotReal)
                        }
                    };

                    Ok([<Concrete $name>](self.0.clone().try_evaluate(real, complex)?))
                }
            }
        }
    }
}

macro_rules! define_waveform {
    {
        $(#[$struct_meta:meta])*
        pub struct $name:ident
    } => {
        $(#[$struct_meta])*
        #[derive(Clone, PartialEq, Debug, Copy, Serialize, Deserialize)]
        #[cfg_attr(feature = "stubs", gen_stub_pyclass)]
        #[cfg_attr(feature = "python", pyo3::pyclass(module = "quil.waveforms", subclass, eq))]
        pub struct $name;

        #[automatically_derived]
        impl super::Extractable for $name {
            fn extract_from(
                parameters: &mut super::WaveformParameters
            ) -> Result<Self, super::WaveformParameterError> {
                Ok(Self)
            }
        }
    };

    {
        $(#[$struct_meta:meta])*
        pub struct $name:ident {
            $(
                $(#[$field_meta:meta])*
                pub $field:ident: $ty:ident
            ),+
            $(,)?
        }
    } => {
        $(#[$struct_meta])*
        #[derive_where(Clone, PartialEq, Debug)]
        #[derive_where(Copy, Serialize, Deserialize; field_derive_where!(; $($ty),+))]
        pub struct $name<T: WaveformType> {
            $(
                $(#[$field_meta])*
                pub $field: field_type!(T, $ty)
            ),+
        }

        #[automatically_derived]
        impl super::Extractable for $name<Syntactic> {
            fn extract_from(
                parameters: &mut super::WaveformParameters,
            ) -> Result<Self, super::WaveformParameterError> {
                $(
                    let $field = field_parser!($ty)(parameters, stringify!($field))?;
                )+
                Ok(Self { $($field),+ })
            }
        }

        impl<S: WaveformType> $name<S> {
            #[allow(unused_variables, reason = "macro-generated code")]
            pub fn try_evaluate<T: WaveformType, E>(
                self,
                real: impl Fn(S::Real) -> Result<T::Real, E>,
                complex: impl Fn(S::Complex) -> Result<T::Complex, E>,
            ) -> Result<$name<T>, E> {
                let Self { $($field),+ } = self;
                Ok($name {
                    $($field: field_evaluator!($ty, real = real, complex = complex, $field)),+
                })
            }
        }
    }
}

macro_rules! define_waveforms {
    (
        $(
            $(#[doc = $struct_doc:literal])*
            #[waveform_source($waveform_source:ident)]
            $(#[$struct_meta:meta])*
            pub struct $name:ident $({
                $(
                    $(#[$field_meta:meta])*
                    pub $field:ident: $ty:ident
                ),+
                $(,)?
            })?
            $(;)?
        )+
    ) => {
        $(
            define_waveform! {
                $(#[doc = $struct_doc])*
                ///
                #[doc = waveform_source!($waveform_source)]
                $(#[$struct_meta])*
                pub struct $name $({
                    $(
                        $(#[$field_meta])*
                        pub $field: $ty
                    ),+
                })?
            }
        )+

        mod quilpy_waveforms {
            use super::*;

            $(define_python_waveform!($name $({ $($field: $ty),+ })?);)*
        }

        mod private {
            pub trait Sealed {}
            impl Sealed for super::BuiltinWaveform<super::Concrete> {}
            $(
                #[automatically_derived]
                impl Sealed for concrete_waveform!($name, $($($field)+)?) {}
            )*
        }
    }
}

define_waveforms! {
    /// A flat waveform, repeating a given IQ value for the given duration.
    #[waveform_source(QuilT)]
    pub struct Flat {
        /// The IQ value to play
        pub iq: Complex,
    }

    /// A waveform with a Gaussian shape.
    #[waveform_source(QuilT)]
    pub struct Gaussian {
        /// Full width half maximum of the pulse (s)
        pub fwhm: Real,
        /// Center/offset for pulse centroid (s)
        pub t0: Real,
    }

    /// Creates a waveform with a DRAG-corrected Gaussian shape.
    ///
    /// This is a Gaussian shape with an additional component proportional to the time derivative of
    /// the main Gaussian pulse.
    ///
    /// For details, see "Simple Pulses for Elimination of Leakage in Weakly Nonlinear Qubits",
    /// F. Motzoi, J. M. Gambetta, J. M., P. Rebentrost, and F. K. Wilhelm, Physical Review Letters
    /// 103, 110501 (September 8, 2009).  DOI: 10.1103/PhysRevLett.103.110501; publication URL:
    /// <https://journals.aps.org/prl/abstract/10.1103/PhysRevLett.103.110501>, preprint URL:
    /// <https://arxiv.org/abs/0901.0534>.
    #[waveform_source(QuilT)]
    pub struct DragGaussian {
        /// Full width half maximum of the pulse (s)
        pub fwhm: Real,
        /// Center/offset for pulse centroid (s)
        pub t0: Real,
        /// Qubit anharmonicity - sets rate of evolution for the imaginary term (Hz)
        pub anh: Real,
        /// DRAG parameter - controls strength of the imaginary term
        pub alpha: Real,
    }

    /// A waveform with a flat top and edges that are error functions (erfs).
    #[waveform_source(QuilT)]
    pub struct ErfSquare {
        /// Slope of erf shoulders (2x FWHM of erf in s)
        pub risetime: Real,

        /// Length of zero padding to add to beginning of pulse (s)
        ///
        /// Note that this is *always* a concrete real number, even if the waveform is
        /// [`Syntactic`]!  It must be possible to know the exact duration of a waveform at all
        /// times.
        pub pad_left: ConcreteReal,

        /// Length of zero padding to add to end of pulse (s)
        ///
        /// Note that this is *always* a concrete real number, even if the waveform is
        /// [`Syntactic`]!  It must be possible to know the exact duration of a waveform at all
        /// times.
        pub pad_right: ConcreteReal,
    }

    /// Creates a Hermite Gaussian waveform.
    ///
    /// This extends the basic DRAG pulse by adding an additional imaginary term to the pulse
    /// envelope consisting of a Gaussian pulse modified by the second order Hermite polynomial.
    ///
    /// For details, see "Effects of arbitrary laser or NMR pulse shapes on population inversion and
    /// coherence", Warren S. Warren, The Journal of Chemical Physics 81(12) (December 20, 1984).
    /// DOI: 10.1063/1.447644; publication URL:
    /// <https://pubs.aip.org/aip/jcp/article-abstract/81/12/5437/90781/Effects-of-arbitrary-laser-or-NMR-pulse-shapes-on>.
    #[waveform_source(Rigetti)]
    pub struct HermiteGaussian {
        /// Full width half maximum of the pulse (s)
        pub fwhm: Real,
        /// Center/offset for pulse centroid (s)
        pub t0: Real,
        /// Qubit anharmonicity - sets rate of evolution for the imaginary term (Hz)
        pub anh: Real,
        /// DRAG parameter - controls strength of the imaginary term
        pub alpha: Real,
        /// Coefficient of the second order Hermite polynomial term.
        pub second_order_hrm_coeff: Real,
    }

    /// A boxcar waveform.
    #[waveform_source(Rigetti)]
    pub struct BoxcarKernel;
}

impl<S: WaveformType> BuiltinWaveform<S> {
    pub fn try_evaluate<T: WaveformType, E>(
        self,
        real: impl Fn(S::Real) -> Result<T::Real, E>,
        complex: impl Fn(S::Complex) -> Result<T::Complex, E>,
    ) -> Result<BuiltinWaveform<T>, E> {
        match self {
            Self::Flat(flat) => flat.try_evaluate(real, complex).map(BuiltinWaveform::Flat),
            Self::Gaussian(gaussian) => gaussian
                .try_evaluate(real, complex)
                .map(BuiltinWaveform::Gaussian),
            Self::DragGaussian(drag_gaussian) => drag_gaussian
                .try_evaluate(real, complex)
                .map(BuiltinWaveform::DragGaussian),
            Self::ErfSquare(erf_square) => erf_square
                .try_evaluate(real, complex)
                .map(BuiltinWaveform::ErfSquare),
            Self::HermiteGaussian(hermite_gaussian) => hermite_gaussian
                .try_evaluate(real, complex)
                .map(BuiltinWaveform::HermiteGaussian),
            Self::BoxcarKernel(boxcar_kernel) => Ok(BuiltinWaveform::BoxcarKernel(boxcar_kernel)),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
// IQ sample computation
////////////////////////////////////////////////////////////////////////////////

impl BuiltinWaveformParameters for BuiltinWaveform<Concrete> {
    /// Sample the given waveform (with the additional common parameters) at the given sample rate
    /// (in Hz).
    fn iq_values_at_sample_rate(
        self,
        common: CommonBuiltinParameters<Concrete>,
        sample_rate: f64,
    ) -> Result<IqSamples, SamplingError> {
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

impl BuiltinWaveformParameters for Flat<Concrete> {
    fn iq_values_at_sample_rate(
        self,
        common: CommonBuiltinParameters<Concrete>,
        sample_rate: f64,
    ) -> Result<IqSamples, SamplingError> {
        let ExplicitCommonBuiltinParameters {
            sample_count,
            scale,
            phase,
            detuning,
        } = common.resolve_with_sample_rate(sample_rate)?;
        let Self { iq } = self;

        let sample_count = sample_count as usize;
        let scaled_iq = scale * iq;

        Ok(if detuning == 0.0 {
            IqSamples::Flat {
                iq: apply_phase(scaled_iq, phase),
                sample_count,
            }
        } else {
            let mut samples = vec![scaled_iq; sample_count];
            apply_phase_and_detuning(&mut samples, phase, detuning, sample_rate);
            IqSamples::Samples(samples)
        })
    }
}

impl BuiltinWaveformParameters for Gaussian<Concrete> {
    fn iq_values_at_sample_rate(
        self,
        common: CommonBuiltinParameters<Concrete>,
        sample_rate: f64,
    ) -> Result<IqSamples, SamplingError> {
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

impl BuiltinWaveformParameters for DragGaussian<Concrete> {
    fn iq_values_at_sample_rate(
        self,
        common: CommonBuiltinParameters<Concrete>,
        sample_rate: f64,
    ) -> Result<IqSamples, SamplingError> {
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

impl BuiltinWaveformParameters for ErfSquare<Concrete> {
    fn iq_values_at_sample_rate(
        self,
        common: CommonBuiltinParameters<Concrete>,
        sample_rate: f64,
    ) -> Result<IqSamples, SamplingError> {
        let Self {
            risetime,
            pad_left,
            pad_right,
        } = self;

        let fwhm = 0.5 * risetime;
        let t1 = fwhm;
        let t2 = common.duration - fwhm;

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

impl BuiltinWaveformParameters for HermiteGaussian<Concrete> {
    fn iq_values_at_sample_rate(
        self,
        common: CommonBuiltinParameters<Concrete>,
        sample_rate: f64,
    ) -> Result<IqSamples, SamplingError> {
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
        common: CommonBuiltinParameters<Concrete>,
        sample_rate: f64,
    ) -> Result<IqSamples, SamplingError> {
        let ExplicitCommonBuiltinParameters {
            sample_count,
            scale,
            phase,
            detuning,
        } = common.resolve_with_sample_rate(sample_rate)?;
        let Self = self; // Get errors if the definition changes

        let sample_count = sample_count as usize;

        Ok(if detuning == 0.0 {
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
        })
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
    common: CommonBuiltinParameters<Concrete>,
    build: impl FnOnce(SamplingInfo) -> I,
) -> Result<IqSamples, SamplingError> {
    let SamplingParameters { sample_rate, fwhm } = parameters;
    let ExplicitCommonBuiltinParameters {
        sample_count,
        scale,
        phase,
        detuning,
    } = common.resolve_with_sample_rate(sample_rate)?;

    // It would be nice to be able to optimize `scale: 0.0` to `IqSamples::Flat`, but we'd have to
    // figure out how to take care of the `erf_squared` padding durations.

    let time_steps = Array::range(0.0, sample_count, 1.0) / sample_rate;
    let sigma = 0.5 * fwhm / (2.0 * LN_2).sqrt();

    let mut samples: Vec<_> = build(SamplingInfo { time_steps, sigma })
        .into_iter()
        .collect();

    // Like [`apply_phase_and_detuning`], but also applies the scale
    for (index, sample) in samples.iter_mut().enumerate() {
        *sample =
            apply_phase_and_detuning_at_index(scale * *sample, phase, detuning, sample_rate, index);
    }

    Ok(IqSamples::Samples(samples))
}

/// Modulate and phase shift waveform IQ data in place.
#[inline]
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

/// Convert polar coordinates to rectangular coordinates.
#[inline]
fn polar_to_rectangular(magnitude: f64, angle: Cycles<f64>) -> Complex64 {
    Complex64::from_polar(magnitude, Radians::from(angle).0)
}

pub mod quilpy {
    use super::*;

    use crate::expression::Expression;

    pub use quilpy_waveforms::*;

    #[derive(Clone, PartialEq, Debug)]
    #[cfg_attr(feature = "stubs", gen_stub_pyclass)]
    #[cfg_attr(
        feature = "python",
        pyo3::pyclass(module = "quil.waveforms", subclass, eq)
    )]
    pub struct SyntacticBuiltinWaveform(pub BuiltinWaveform<Syntactic>);

    #[derive(Clone, Copy, PartialEq, Debug)]
    #[cfg_attr(feature = "stubs", gen_stub_pyclass)]
    #[cfg_attr(
        feature = "python",
        pyo3::pyclass(module = "quil.waveforms", subclass, eq)
    )]
    pub struct ConcreteBuiltinWaveform(pub BuiltinWaveform<Concrete>);

    #[derive(Clone, PartialEq, Debug)]
    #[cfg_attr(feature = "stubs", gen_stub_pyclass)]
    #[cfg_attr(
        feature = "python",
        pyo3::pyclass(module = "quil.waveforms", subclass, eq)
    )]
    pub struct SyntacticCommonBuiltinParameters(pub CommonBuiltinParameters<Syntactic>);

    #[derive(Clone, Copy, PartialEq, Debug)]
    #[cfg_attr(feature = "stubs", gen_stub_pyclass)]
    #[cfg_attr(
        feature = "python",
        pyo3::pyclass(module = "quil.waveforms", subclass, eq)
    )]
    pub struct ConcreteCommonBuiltinParameters(pub CommonBuiltinParameters<Concrete>);

    #[cfg(feature = "python")]
    #[pyo3::pymethods]
    impl SyntacticCommonBuiltinParameters {
        #[getter(duration)]
        fn py_get_duration(&self) -> f64 {
            self.0.duration
        }

        #[setter(duration)]
        fn py_set_duration(&mut self, duration: f64) {
            self.0.duration = duration;
        }

        #[getter(scale)]
        fn py_get_scale(&self) -> Option<Expression> {
            self.0.scale.clone()
        }

        #[setter(scale)]
        fn py_set_scale(&mut self, scale: Option<Expression>) {
            self.0.scale = scale;
        }

        #[getter(phase)]
        fn py_get_phase(&self) -> Option<Expression> {
            self.0.phase.clone().map(|Cycles(phase)| phase)
        }

        #[setter(phase)]
        fn py_set_phase(&mut self, phase: Option<Expression>) {
            self.0.phase = phase.map(Cycles);
        }

        #[getter(detuning)]
        fn py_get_detuning(&self) -> Option<Expression> {
            self.0.detuning.clone()
        }

        #[setter(detuning)]
        fn py_set_detuning(&mut self, detuning: Option<Expression>) {
            self.0.detuning = detuning;
        }
    }

    #[cfg(feature = "python")]
    #[pyo3::pymethods]
    impl ConcreteCommonBuiltinParameters {
        #[getter(duration)]
        fn py_get_duration(&self) -> f64 {
            self.0.duration
        }

        #[setter(duration)]
        fn py_set_duration(&mut self, duration: f64) {
            self.0.duration = duration;
        }

        #[getter(scale)]
        fn py_get_scale(&self) -> Option<f64> {
            self.0.scale
        }

        #[setter(scale)]
        fn py_set_scale(&mut self, scale: Option<f64>) {
            self.0.scale = scale;
        }

        #[getter(phase)]
        fn py_get_phase(&self) -> Option<f64> {
            self.0.phase.map(|Cycles(phase)| phase)
        }

        #[setter(phase)]
        fn py_set_phase(&mut self, phase: Option<f64>) {
            self.0.phase = phase.map(Cycles);
        }

        #[getter(detuning)]
        fn py_get_detuning(&self) -> Option<f64> {
            self.0.detuning
        }

        #[setter(detuning)]
        fn py_set_detuning(&mut self, detuning: Option<f64>) {
            self.0.detuning = detuning;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Clean up the debug representation of waveform data to something more apt for a filename.
    ///
    /// It's important that this remain stable or there'll be a mess of updating snapshot files.
    fn format_snapshot_name(
        waveform: impl BuiltinWaveformParameters,
        common: CommonBuiltinParameters<Concrete>,
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
        match BoxcarKernel
            .iq_values_at_sample_rate(
                CommonBuiltinParameters {
                    duration: 0.1,
                    scale: Some(scale),
                    phase: Some(phase),
                    detuning: None,
                },
                100.0,
            )
            .unwrap()
        {
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
    #[case(0.0, 0.0, Some(0.0))]
    #[case(0.0, 1e9, Some(0.0))]
    #[case(1e9, 0.0, Some(0.0))]
    #[case(f64::EPSILON, 1.0, Some(0.0))]
    #[case(-f64::EPSILON, 1.0, Some(0.0))]
    #[case(0.9999999, 101.0, Some(101.0))]
    #[case(1.0000001, 101.0, Some(101.0))]
    #[case(0.99, 101.0, None)]
    #[case(1.01, 101.0, None)]
    #[case(8.800_000_000_000_001e-8, 1.0e9, Some(88.0))] // Based on a past edge case
    #[case(0.5, 3.0, None)]
    fn sample_count(
        #[case] duration: f64,
        #[case] sample_rate: f64,
        #[case] expected: Option<f64>,
    ) {
        let actual = CommonBuiltinParameters {
            duration,
            scale: None,
            phase: None,
            detuning: None,
        }
        .resolve_with_sample_rate(sample_rate);

        match (actual, expected) {
            (
                Ok(ExplicitCommonBuiltinParameters {
                    sample_count: actual,
                    ..
                }),
                Some(expected),
            ) => {
                assert_eq!(
                    expected, actual,
                    "duration = {duration} s,\n\
                     sample_rate = {sample_rate} Hz,\n\
                     expected = {expected} samples,\n\
                     actual = {actual} samples"
                )
            }
            (Err(_), None) => {}
            (
                Ok(ExplicitCommonBuiltinParameters {
                    sample_count: actual,
                    ..
                }),
                None,
            ) => {
                panic!(
                    "duration = {duration} s, sample_rate = {sample_rate} Hz: \
                     expected to be unable to generate a sample count, but generated {actual}",
                    duration = duration,
                    sample_rate = sample_rate,
                    actual = actual,
                )
            }
            (Err(actual), Some(expected)) => {
                panic!(
                    "duration = {duration} s, sample_rate = {sample_rate} Hz: \
                     expected a sample count of {expected} samples, but got the following error:\n\
                     {actual}",
                    duration = duration,
                    sample_rate = sample_rate,
                    expected = expected,
                    actual = actual,
                )
            }
        }
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
        #[case] common: CommonBuiltinParameters<Concrete>,
    ) {
        let iq_values = parameters
            .iq_values_at_sample_rate(common, 1e6)
            .unwrap()
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
