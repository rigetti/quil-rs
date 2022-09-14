// Copyright 2021 Rigetti Computing
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#[allow(clippy::module_inception)]
mod error;
mod kind;
mod internal;
mod input;

use super::lexer::{Command, Token};

pub use error::Error;
pub use kind::ErrorKind;
pub use internal::InternalParseError;
pub(crate) use input::ErrorInput;

pub type ParseError = Error<ParserErrorKind>;

/// Parsing errors specific to Quil parsing
#[allow(dead_code)]
#[derive(Debug, thiserror::Error, PartialEq)]
pub enum ParserErrorKind {
    // TODO: can this be static str?
    #[error("expected {0}, found EOF")]
    UnexpectedEOF(String),
    #[error("expected {expected}, found {actual:?}")]
    ExpectedToken { actual: Token, expected: String },

    /// Tried to parse a kind of command and couldn't
    #[error("failed to parse arguments for {command}")]
    InvalidCommand { command: Command },

    /// Unexpected start of an instruction
    #[error("expected a command or a gate")]
    NotACommandOrGate,

    /// The end of input was reached
    #[error("reached end of input")]
    EndOfInput,

    /// An instruction was encountered which is not yet supported for parsing by this library
    #[error("instruction {0} is not yet supported by this parser")]
    UnsupportedInstruction(Command),

    /// Literals specified in the input cannot be supported without loss of precision
    #[error("using this literal will result in loss of precision")]
    UnsupportedPrecision,
}