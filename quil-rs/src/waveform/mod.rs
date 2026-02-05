//! Waveforms as found in Quil-T instructions.

use derive_where::derive_where;
use indexmap::IndexMap;
use num_complex::Complex64;

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::gen_stub_pyclass_complex_enum;

use crate::{
    expression::{EvaluationError, Expression},
    instruction::{WaveformInvocation, WaveformParameters},
    units::Cycles,
};

use self::builtin::{BuiltinWaveform, CommonBuiltinParameters};

pub mod builtin;
pub mod sampling;

#[cfg(feature = "python")]
pub(crate) mod quilpy;

pub trait WaveformType {
    type Real: Clone + PartialEq + std::fmt::Debug;
    type Complex: Clone + PartialEq + std::fmt::Debug;
}

pub enum Syntactic {}

pub enum Concrete {}

impl WaveformType for Syntactic {
    type Real = Expression;
    type Complex = Expression;
}

impl WaveformType for Concrete {
    type Real = f64;
    type Complex = Complex64;
}

/// A waveform, as specified in a [Quil-T][] [`Instruction`][crate::instruction::Instruction].
///
/// [Quil-T]: https://quil-lang.github.io/#12Annex-T--Pulse-Level-Control
#[derive_where(Clone, PartialEq, Debug)]
// #[cfg_attr(feature = "stubs", gen_stub_pyclass_complex_enum)]
// #[cfg_attr(
//     feature = "python",
//     pyo3::pyclass(module = "quil.waveforms", eq, frozen)
// )]
pub enum Waveform<T: WaveformType> {
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

impl Waveform<Syntactic> {
    pub fn new(invocation: WaveformInvocation) -> Result<Self, WaveformInvocationError> {
        let WaveformInvocation { name, parameters } = invocation;

        match name.as_ref() {
            "flat" => builtin(BuiltinWaveform::Flat, parameters)
                .map_err(|error| WaveformInvocationError { name, error }),

            "gaussian" => builtin(BuiltinWaveform::Gaussian, parameters)
                .map_err(|error| WaveformInvocationError { name, error }),

            "drag_gaussian" => builtin(BuiltinWaveform::DragGaussian, parameters)
                .map_err(|error| WaveformInvocationError { name, error }),

            "erf_square" => builtin(BuiltinWaveform::ErfSquare, parameters)
                .map_err(|error| WaveformInvocationError { name, error }),

            "hrm_gauss" => builtin(BuiltinWaveform::HermiteGaussian, parameters)
                .map_err(|error| WaveformInvocationError { name, error }),

            "boxcar_kernel" => builtin(BuiltinWaveform::BoxcarKernel, parameters)
                .map_err(|error| WaveformInvocationError { name, error }),

            _ => Ok(Self::Custom { name, parameters }),
        }
    }
}

impl<S: WaveformType> Waveform<S> {
    pub fn try_evaluate<T: WaveformType, E>(
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

#[derive(Clone, PartialEq, Eq, Debug, thiserror::Error)]
pub enum WaveformParameterError {
    #[error("missing mandatory parameter {_0}")]
    Missing(&'static str),

    #[error("parameter {_0} must be a real number known at compile time, but {_1}")]
    Nonreal(&'static str, EvaluationError),

    #[error("extra unknown parameters: {}", _0.join(", "))]
    Extra(Vec<String>),
}

#[derive(Clone, PartialEq, Eq, Debug, thiserror::Error)]
#[error("invalid invocation of waveform {name}: {error}")]
pub struct WaveformInvocationError {
    name: String,
    error: WaveformParameterError,
}

fn mandatory(
    parameters: &mut WaveformParameters,
    name: &'static str,
) -> Result<Expression, WaveformParameterError> {
    parameters
        .shift_remove(name)
        .ok_or(WaveformParameterError::Missing(name))
}

fn concrete_real(
    parameters: &mut WaveformParameters,
    name: &'static str,
) -> Result<f64, WaveformParameterError> {
    mandatory(parameters, name)?
        .to_real()
        .map_err(|err| WaveformParameterError::Nonreal(name, err))
}

trait Extractable: Sized {
    fn extract_from(parameters: &mut WaveformParameters) -> Result<Self, WaveformParameterError>;
}

impl Extractable for CommonBuiltinParameters<Syntactic> {
    fn extract_from(parameters: &mut WaveformParameters) -> Result<Self, WaveformParameterError> {
        let duration = concrete_real(parameters, "duration")?;
        let scale = parameters.shift_remove("scale");
        let phase = parameters.shift_remove("phase").map(Cycles);
        let detuning = parameters.shift_remove("detuning");

        Ok(Self {
            duration,
            scale,
            phase,
            detuning,
        })
    }
}

impl<T1: Extractable, T2: Extractable> Extractable for (T1, T2) {
    fn extract_from(parameters: &mut WaveformParameters) -> Result<Self, WaveformParameterError> {
        let t1 = T1::extract_from(parameters)?;
        let t2 = T2::extract_from(parameters)?;
        Ok((t1, t2))
    }
}

fn builtin<T: Extractable>(
    constructor: impl FnOnce(T) -> BuiltinWaveform<Syntactic>,
    mut parameters: WaveformParameters,
) -> Result<Waveform<Syntactic>, WaveformParameterError> {
    let (common_parameters, builtin) = Extractable::extract_from(&mut parameters)?;
    if parameters.is_empty() {
        Ok(Waveform::Builtin {
            waveform: constructor(builtin),
            common_parameters,
        })
    } else {
        Err(WaveformParameterError::Extra(
            parameters.into_keys().collect(),
        ))
    }
}
