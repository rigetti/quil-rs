//! Waveforms as found in Quil-T instructions.

use indexmap::IndexMap;

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::gen_stub_pyclass_complex_enum;

use crate::expression::Expression;

use self::builtin::{BuiltinWaveform, CommonBuiltinParameters};

pub mod builtin;
pub mod sampling;

#[cfg(feature = "python")]
pub(crate) mod quilpy;

/// A waveform, as specified in a [Quil-T][] [`Instruction`][crate::instruction::Instruction].
#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass_complex_enum)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.waveforms", eq, frozen)
)]
pub enum Waveform {
    /// One of the built-in waveforms `quil-rs` always knows about.
    Builtin {
        waveform: BuiltinWaveform,
        common_parameters: CommonBuiltinParameters,
    },

    /// A reference to a user-defined waveform.
    Custom {
        name: String,
        parameters: IndexMap<String, Expression>,
    },
}
