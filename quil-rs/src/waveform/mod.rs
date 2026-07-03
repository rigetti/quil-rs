//! Waveforms that have been parsed from a bare invocation into a structured form, distinguishing
//! [built-in waveforms][builtin].

use std::{convert::Infallible, marker::PhantomData};

use derive_where::derive_where;
use indexmap::IndexMap;
use num_complex::Complex64;

use crate::{
    expression::{EvaluationError, Expression},
    instruction::WaveformInvocation,
};

use self::builtin::{BuiltinWaveform, CommonBuiltinParameters};

pub mod builtin;
pub mod higher_kinded;
pub mod sampling;

#[cfg(feature = "python")]
pub(crate) mod quilpy;

/// Types that implement [`WaveformData`] are type-level markers that are used to control the values
/// contained in [Quil-T][] (both [`Waveform`] and other related types).
///
/// This module provides
///
/// - [`Syntactic`], for source-level waveforms that contain expressions, such as
///   `flat(duration: 1e-6 + 1e-6, iq: sin(pi))`.  Waveforms with this sort of data are generated
///   during parsing, such as from the [`WaveformInvocation`]s contained in
///   [`Instruction`s][crate::instruction::Instruction].
///
/// - [`Concrete`], for waveforms that are fully instantiated with numbers, such as
///   `flat(duration: 2e-6, iq: 0)`.  Waveforms with this sort of data may be generated during
///   compilation or can be constructed manually to do sampling.
///
/// [Quil-T]: https://quil-lang.github.io/#12Annex-T--Pulse-Level-Control
pub trait WaveformData {
    /// The type of real-valued waveform parameters.
    type Real: Clone + PartialEq + std::fmt::Debug;

    /// The type of complex-valued waveform parameters.
    type Complex: Clone + PartialEq + std::fmt::Debug;
}

/// A [`WaveformData`] implementor indicating that this is a source-level waveform containing
/// expressions, such as `flat(duration: 1e-6 + 1e-6, iq: sin(pi))`.  Waveforms with this sort of
/// data are generated during parsing and contained in
/// [`Instruction`s][crate::instruction::Instruction].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Syntactic {}

/// A [`WaveformData`] implementor indicating that this waveform has been fully instantiated with
/// numbers, such as `flat(duration: 2e-6, iq: 0)`.  Waveforms with this sort of data may be
/// generated during compilation or can be constructed manually to do sampling.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Concrete {}

/// A transformer of [`WaveformData`] implementors indicating that this waveform may be instantiated
/// with the wrapped data *or* that data may be absent.  Waveforms with this sort of data may be
/// generated during complilation in order to account for those waveforms that can have their data
/// modified at patch time.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct Partial<T>(pub T);

/// A transformer of [`WaveformData`] implementors indicating that this waveform is like another
/// one, but has its fields taken by reference.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct Reference<'a, T: WaveformData>(pub T, pub PhantomData<(&'a T::Real, &'a T::Complex)>);

impl WaveformData for Syntactic {
    type Real = Expression;
    type Complex = Expression;
}

impl WaveformData for Concrete {
    type Real = f64;
    type Complex = Complex64;
}

impl<T: WaveformData> WaveformData for Partial<T> {
    type Real = Option<T::Real>;
    type Complex = Option<T::Complex>;
}

impl<'a, T: WaveformData> WaveformData for Reference<'a, T> {
    type Real = &'a T::Real;
    type Complex = &'a T::Complex;
}

/// A waveform, as specified in a [Quil-T][] [`Instruction`][crate::instruction::Instruction].
///
/// The [`WaveformData`] type parameter indicates what sort of data is stored by this waveform.  It
/// will typically be either [`Syntactic`], for expression-valued waveforms, or [`Concrete`], for
/// numerically-valued waveforms.
///
/// [Quil-T]: https://quil-lang.github.io/#12Annex-T--Pulse-Level-Control
#[derive_where(Clone, PartialEq, Debug)]
pub enum Waveform<T: WaveformData> {
    /// One of the built-in waveforms `quil-rs` always knows about.
    Builtin {
        waveform: BuiltinWaveform<T>,
        common_parameters: CommonBuiltinParameters<T>,
    },

    /// A reference to a user-defined waveform.
    Custom {
        name: String,
        parameters: IndexMap<String, T::Complex>,
    },
}

impl<T: WaveformData> Waveform<T> {
    /// Create a new waveform from a name and a parameter map.
    ///
    /// If the name corresponds to one of the built-in waveforms, then this will return a
    /// [`Waveform::BuiltinWaveform`] as long as the parameters are correct, and return an error if
    /// they are incorrect; individual parameters are evaluted with one the three evaluator
    /// functions (`concrete_real`, `real`, or `complex`).
    ///
    /// If the name doesn't name one of the built-in waveforms, then this will return a
    /// [`Waveform::Custom`]; the parameter map is placed into its final form by the `custom`
    /// function.
    ///
    /// This is a generalization of [`Waveform<Syntactic>::new`]; if you only need to handle
    /// creating waveforms from Quil [`WaveformInvocation`]s, prefer that function instead.
    pub fn from_parameters<P: GeneralWaveformParameters, EF64, ER, EC>(
        name: String,
        parameters: P,
        concrete_real: impl FnMut(P::Value) -> Result<f64, EF64>,
        real: impl FnMut(P::Value) -> Result<T::Real, ER>,
        complex: impl FnMut(P::Value) -> Result<T::Complex, EC>,
        custom: impl FnOnce(
            P,
        ) -> Result<
            IndexMap<String, T::Complex>,
            GeneralWaveformParameterError<EF64, ER, EC>,
        >,
    ) -> Result<Self, WaveformInvocationError<GeneralWaveformParameterError<EF64, ER, EC>>> {
        // This can't be a closure because the type of [`BuiltinWaveform::$waveform`] is
        // polymorphic, and it can't be a regular function because the whole point is to refer to
        // local parameters.
        macro_rules! parse_builtin {
            ($waveform:ident) => {
                self::parse::builtin(
                    BuiltinWaveform::$waveform,
                    parameters,
                    concrete_real,
                    real,
                    complex,
                )
                .map_err(|error| WaveformInvocationError { name, error })
            };
        }

        match name.as_str() {
            "flat" => parse_builtin!(Flat),
            "gaussian" => parse_builtin!(Gaussian),
            "drag_gaussian" => parse_builtin!(DragGaussian),
            "erf_square" => parse_builtin!(ErfSquare),
            "hrm_gauss" => parse_builtin!(HermiteGaussian),
            "boxcar_kernel" => parse_builtin!(BoxcarKernel),
            _ => match custom(parameters) {
                Ok(parameters) => Ok(Self::Custom { name, parameters }),
                Err(error) => Err(WaveformInvocationError { name, error }),
            },
        }
    }
}

impl Waveform<Syntactic> {
    /// Create a new [`Waveform`] from a parsed [`WaveformInvocation`].
    ///
    /// If the [`WaveformInvocation`] names one of the built-in waveforms, then this will return a
    /// [`Waveform::BuiltinWaveform`] as long as the parameters are correct, and return an error
    /// if they are incorrect.
    ///
    /// If the [`WaveformInvocation`] doesn't name one of the built-in waveforms, then this will
    /// return a [`Waveform::Custom`].
    pub fn new(invocation: WaveformInvocation) -> Result<Self, WaveformInvocationError> {
        let WaveformInvocation { name, parameters } = invocation;
        Self::from_parameters(name, parameters, |expr| expr.to_real(), Ok, Ok, Ok)
            .map_err(|error| error.map(Into::into))
    }
}

impl<S: WaveformData> Waveform<S> {
    /// Convert an owned [`Waveform`] into an equivalent one whose (non-concrete) parameters are all
    /// references.  This will, however, clone the names and the mapping used in
    /// [`Waveform::Custom`].
    pub fn as_ref(&self) -> Waveform<Reference<'_, S>> {
        match self {
            Self::Builtin {
                waveform,
                common_parameters,
            } => Waveform::Builtin {
                waveform: waveform.as_ref(),
                common_parameters: common_parameters.as_ref(),
            },

            Self::Custom { name, parameters } => Waveform::Custom {
                name: name.clone(),
                parameters: parameters
                    .iter()
                    .map(|(name, z)| (name.clone(), z))
                    .collect(),
            },
        }
    }

    /// Convert one [`Waveform`] into another by replacing its associated data.
    ///
    /// Given two forms of waveform data, `S` and `T`, the user specifies how to evaluate `S`'s real
    /// numbers into `T`'s real numbers and how to evaluate `S`'s complex numbers to `T`'s complex
    /// numbers.  For example, to convert parsed ([`Syntactic`]) waveforms into sampleable
    /// ([`Concrete`]) waveforms, you can pass [`Expression::evaluate`] to this function.
    ///
    /// As an example:
    ///
    /// ```
    /// # use std::{borrow::Borrow, collections::HashMap, hash::Hash};
    /// #  
    /// # use num_complex::Complex64;
    /// #  
    /// # use quil_rs::{
    /// #     expression::EvaluationError,
    /// #     waveform::{
    /// #       builtin::{BuiltinWaveform, CommonBuiltinParameters, Gaussian},
    /// #       Concrete, Syntactic, Waveform,
    /// #   },
    /// # };
    ///
    /// fn concretize<K: Borrow<str> + Hash + Eq>(
    ///     waveform: Waveform<Syntactic>,
    ///     variables: &HashMap<K, Complex64>, // Values for Quil `%variable`s
    ///     memory_references: &HashMap<K, Vec<f64>>, // Values for Quil `DECLARE mem TYPE`s
    /// ) -> Result<Waveform<Concrete>, EvaluationError> {
    ///     waveform.try_evaluate(
    ///         |real_expr| match real_expr.evaluate(variables, memory_references)? {
    ///             Complex64 { re, im: 0.0 } => Ok(re),
    ///             _ => Err(EvaluationError::NumberNotReal),
    ///         },
    ///         |complex_expr| complex_expr.evaluate(variables, memory_references),
    ///     )
    /// }
    ///
    /// let waveform = Waveform::Builtin {
    ///     waveform: BuiltinWaveform::Gaussian(Gaussian {
    ///         fwhm: "%fwhm_scale * 0.5e-8".parse().unwrap(),
    ///         t0: "(exp(0) - sin(pi/2)) + t0_offset".parse().unwrap(),
    ///     }),
    ///     common_parameters: CommonBuiltinParameters {
    ///         duration: "1e-6".parse().unwrap(),
    ///         scale: None,
    ///         phase: None,
    ///         detuning: None,
    ///     },
    /// };
    ///
    /// assert_eq!(
    ///     concretize(
    ///         waveform.clone(),
    ///         &[("fwhm_scale", Complex64::new(4.0, 0.0))].into(),
    ///         &[("t0_offset", vec![1e-7])].into(),
    ///     ),
    ///     Ok(Waveform::Builtin {
    ///         waveform: BuiltinWaveform::Gaussian(Gaussian {
    ///             fwhm: 2e-8,
    ///             t0: 1e-7,
    ///         }),
    ///         common_parameters: CommonBuiltinParameters {
    ///             duration: 1e-6,
    ///             scale: None,
    ///             phase: None,
    ///             detuning: None,
    ///         },
    ///     }),
    /// );
    ///
    /// assert_eq!(
    ///     concretize(
    ///         waveform.clone(),
    ///         &[("fwhm_scale", Complex64::new(8.0, 0.0))].into(),
    ///         &[("t0_offset", vec![2e-7])].into(),
    ///     ),
    ///     Ok(Waveform::Builtin {
    ///         waveform: BuiltinWaveform::Gaussian(Gaussian {
    ///             fwhm: 4e-8,
    ///             t0: 2e-7,
    ///         }),
    ///         common_parameters: CommonBuiltinParameters {
    ///             duration: 1e-6,
    ///             scale: None,
    ///             phase: None,
    ///             detuning: None,
    ///         },
    ///     }),
    /// );
    ///
    /// assert!(concretize(
    ///     waveform.clone(),
    ///     &[("fwhm_scale", Complex64::i())].into(),
    ///     &[("t0_offset", vec![2e-7])].into(),
    /// )
    /// .is_err());
    ///
    /// assert!(concretize(
    ///     waveform.clone(),
    ///     &HashMap::new(),
    ///     &[("t0_offset", vec![2e-7])].into(),
    /// )
    /// .is_err());
    ///
    /// assert!(concretize(
    ///     waveform.clone(),
    ///     &[("fwhm_scale", Complex64::new(3.0, 0.0))].into(),
    ///     &HashMap::new(),
    /// )
    /// .is_err());
    /// ```
    pub fn try_evaluate<T: WaveformData, E>(
        self,
        real: impl Fn(S::Real) -> Result<T::Real, E>,
        complex: impl Fn(S::Complex) -> Result<T::Complex, E>,
    ) -> Result<Waveform<T>, E> {
        Ok(match self {
            Self::Builtin {
                waveform,
                common_parameters,
            } => Waveform::Builtin {
                waveform: waveform.try_evaluate(&real, &complex)?,
                common_parameters: common_parameters.try_evaluate(real, complex)?,
            },

            Self::Custom { name, parameters } => Waveform::Custom {
                name,
                parameters: parameters
                    .into_iter()
                    .map(|(name, z)| Ok((name, complex(z)?)))
                    .collect::<Result<_, _>>()?,
            },
        })
    }
}

/// A trait covering types, like [`WaveformParameters]`, that are maps from strings to values
/// that can be used to construct [`Waveform`]s.
pub trait GeneralWaveformParameters {
    /// The type of value contained in the map.
    type Value;

    /// If the named parameter is in the map, remove it and return its value.  Otherwise, return
    /// [`None`].
    fn remove_parameter(&mut self, name: &'static str) -> Option<Self::Value>;

    /// Take the map and return all its parameter names.
    fn into_keys(self) -> Vec<String>;
}

impl<V> GeneralWaveformParameters for IndexMap<String, V> {
    type Value = V;

    #[inline(always)]
    fn remove_parameter(&mut self, name: &'static str) -> Option<Self::Value> {
        self.shift_remove(name)
    }

    #[inline(always)]
    fn into_keys(self) -> Vec<String> {
        self.into_keys().collect()
    }
}

impl<V> GeneralWaveformParameters for std::collections::HashMap<String, V> {
    type Value = V;

    #[inline(always)]
    fn remove_parameter(&mut self, name: &'static str) -> Option<Self::Value> {
        self.remove(name)
    }

    #[inline(always)]
    fn into_keys(self) -> Vec<String> {
        self.into_keys().collect()
    }
}

impl<V> GeneralWaveformParameters for std::collections::BTreeMap<String, V> {
    type Value = V;

    #[inline(always)]
    fn remove_parameter(&mut self, name: &'static str) -> Option<Self::Value> {
        self.remove(name)
    }

    #[inline(always)]
    fn into_keys(self) -> Vec<String> {
        self.into_keys().collect()
    }
}

/// Errors arising when trying to convert a parameter mapping to a [`Waveform::Builtin`].
///
/// For the case where we're specifically dealing with expressions, see [`WaveformParameterError`].
// Note for developers: [`thiserror::Error`] couldn't handle the display implementations for a
// generic type (it didn't insert the bounds), so we had to use [`derive_more::Display`] instead.
#[derive(Clone, PartialEq, Eq, Debug, derive_more::Display, thiserror::Error)]
pub enum GeneralWaveformParameterError<EF64, ER, EC> {
    #[display("missing mandatory parameter {_0}")]
    Missing(&'static str),

    #[display("parameter {_0} must be a real number known at compile time, but {_1}")]
    BadConcreteReal(&'static str, #[source] EF64),

    #[display("parameter {_0} must be a real number, but {_1}")]
    BadReal(&'static str, #[source] ER),

    #[display("parameter {_0} must be a complex number, but {_1}")]
    BadComplex(&'static str, #[source] EC),

    #[display("extra unknown parameters: {}", _0.join(", "))]
    Extra(Vec<String>),
}

/// Errors arising when trying to convert [`WaveformInvocation`]s to [`Waveform::Builtin`]s.
#[derive(Clone, PartialEq, Eq, Debug, thiserror::Error)]
pub enum WaveformParameterError {
    #[error("missing mandatory parameter {_0}")]
    Missing(&'static str),

    #[error("parameter {_0} must be a real number known at compile time, but {_1}")]
    Nonreal(&'static str, EvaluationError),

    #[error("extra unknown parameters: {}", _0.join(", "))]
    Extra(Vec<String>),
}

impl From<GeneralWaveformParameterError<EvaluationError, Infallible, Infallible>>
    for WaveformParameterError
{
    fn from(value: GeneralWaveformParameterError<EvaluationError, Infallible, Infallible>) -> Self {
        match value {
            GeneralWaveformParameterError::Missing(name) => Self::Missing(name),
            GeneralWaveformParameterError::BadConcreteReal(name, error) => {
                Self::Nonreal(name, error)
            }
            GeneralWaveformParameterError::Extra(names) => Self::Extra(names),
            GeneralWaveformParameterError::BadReal(_, never)
            | GeneralWaveformParameterError::BadComplex(_, never) => match never {},
        }
    }
}

/// Errors arising when trying to convert a named [`WaveformInvocation`] or similar set of named
/// parameters to the corresponding [`Waveform::Builtin`]s.
#[derive(Clone, PartialEq, Eq, Debug, thiserror::Error)]
#[error("invalid invocation of waveform {name}: {error}")]
pub struct WaveformInvocationError<E = WaveformParameterError> {
    name: String,
    #[source]
    error: E,
}

impl<E1> WaveformInvocationError<E1> {
    /// Transform the wrapped error
    fn map<E2>(self, f: impl FnOnce(E1) -> E2) -> WaveformInvocationError<E2> {
        let Self { name, error } = self;
        WaveformInvocationError {
            name,
            error: f(error),
        }
    }
}

/// Internal code to convert [`WaveformInvocation`]s to [`Waveform::Builtin`]s.
///
/// Parsing is done by destructively mutating a [`WaveformParameters`] value, removing parameters we
/// have already recognized.
mod parse {
    use super::*;

    /// A trait for types that can be destructively parsed from parameters into a value with
    /// [`WaveformData`] of type `D`.
    pub(super) trait Extractable<D: WaveformData>: Sized {
        /// Parse a value of this type from parameters.  Because the parameter values are not
        /// necessarily the same as the waveform's values, we also pass three evaluation functions.
        /// If any of these evaluations returns an error, that error is paired with the name of the
        /// field that caused the error.
        fn extract_from<P: GeneralWaveformParameters, EF64, ER, EC>(
            parameters: &mut P,
            concrete_real: impl FnMut(P::Value) -> Result<f64, EF64>,
            real: impl FnMut(P::Value) -> Result<D::Real, ER>,
            complex: impl FnMut(P::Value) -> Result<D::Complex, EC>,
        ) -> Result<Self, GeneralWaveformParameterError<EF64, ER, EC>>;
    }

    /// Parse a `T1` and then parse a `T2`.
    impl<D: WaveformData, T1: Extractable<D>, T2: Extractable<D>> Extractable<D> for (T1, T2) {
        fn extract_from<P: GeneralWaveformParameters, EF64, ER, EC>(
            parameters: &mut P,
            mut concrete_real: impl FnMut(P::Value) -> Result<f64, EF64>,
            mut real: impl FnMut(P::Value) -> Result<D::Real, ER>,
            mut complex: impl FnMut(P::Value) -> Result<D::Complex, EC>,
        ) -> Result<Self, GeneralWaveformParameterError<EF64, ER, EC>> {
            let t1 = T1::extract_from(parameters, &mut concrete_real, &mut real, &mut complex)?;
            let t2 = T2::extract_from(parameters, concrete_real, real, complex)?;
            Ok((t1, t2))
        }
    }

    /// Consume and return a builtin waveform parameter that is required to be present.
    pub(super) fn mandatory<P: GeneralWaveformParameters, EF64, ER, EC, T, E>(
        parameters: &mut P,
        name: &'static str,
        parse: impl FnOnce(P::Value) -> Result<T, E>,
        value_error: impl FnOnce(&'static str, E) -> GeneralWaveformParameterError<EF64, ER, EC>,
    ) -> Result<T, GeneralWaveformParameterError<EF64, ER, EC>> {
        optional(parameters, name, parse, value_error)
            .and_then(|value| value.ok_or(GeneralWaveformParameterError::Missing(name)))
    }

    /// Consume and return a builtin waveform parameter that is not required to be present.
    pub(super) fn optional<P: GeneralWaveformParameters, EF64, ER, EC, T, E>(
        parameters: &mut P,
        name: &'static str,
        parse: impl FnOnce(P::Value) -> Result<T, E>,
        value_error: impl FnOnce(&'static str, E) -> GeneralWaveformParameterError<EF64, ER, EC>,
    ) -> Result<Option<T>, GeneralWaveformParameterError<EF64, ER, EC>> {
        parameters
            .remove_parameter(name)
            .map(|value| parse(value).map_err(|err| value_error(name, err)))
            .transpose()
    }

    /// Parse a single builtin waveform.  Checks to ensure that there are no extra parameters we
    /// haven't handled.
    pub(super) fn builtin<
        D: WaveformData,
        T: Extractable<D>,
        P: GeneralWaveformParameters,
        EF64,
        ER,
        EC,
    >(
        constructor: impl FnOnce(T) -> BuiltinWaveform<D>,
        mut parameters: P,
        concrete_real: impl FnMut(P::Value) -> Result<f64, EF64>,
        real: impl FnMut(P::Value) -> Result<D::Real, ER>,
        complex: impl FnMut(P::Value) -> Result<D::Complex, EC>,
    ) -> Result<Waveform<D>, GeneralWaveformParameterError<EF64, ER, EC>> {
        let (common_parameters, builtin) =
            Extractable::extract_from(&mut parameters, concrete_real, real, complex)?;
        let extra_parameters = parameters.into_keys();
        if extra_parameters.is_empty() {
            Ok(Waveform::Builtin {
                waveform: constructor(builtin),
                common_parameters,
            })
        } else {
            Err(GeneralWaveformParameterError::Extra(extra_parameters))
        }
    }
}
