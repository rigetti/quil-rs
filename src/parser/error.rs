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

use std::fmt;
use std::fmt::Formatter;

use super::lexer::{Command, Token};
use crate::parser::lexer::LexInput;
use crate::parser::ParserInput;

/// This is a superset of `(I, nom::ErrorKind)` that includes the additional errors specified by
/// [`ErrorKind`].
#[derive(Debug)]
pub struct Error<I> {
    /// The remainder of the input stream at the time of the error.
    pub input: I,
    /// The error that occurred.
    pub error: ErrorKind,
}

impl fmt::Display for Error<LexInput<'_>> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let line = self.input.location_line();
        let column = self.input.get_utf8_column();
        let snippet = std::str::from_utf8(self.input.get_line_beginning());

        write!(f, "Error at line {line}, column {column}")?;
        if let Ok(snippet) = snippet {
            write!(f, " (\"{snippet}")?;
            if snippet.len() < self.input.len() {
                write!(f, "...")?;
            }
            write!(f, "\")")?;
        }
        write!(f, ": {}", self.error)
    }
}

impl fmt::Display for Error<ParserInput<'_>> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.input.first() {
            None => write!(f, "Error while parsing tokens: {}", self.error),
            Some(token) => write!(
                f,
                "Error while parsing tokens at line {}, column {} ({:?}): {}",
                token.line(),
                token.column(),
                token.as_token(),
                self.error,
            ),
        }
    }
}

impl std::error::Error for Error<LexInput<'_>> {}
impl std::error::Error for Error<ParserInput<'_>> {}

/// Parsing errors specific to Quil parsing
#[allow(dead_code)]
#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("expected {0}, found EOF")]
    UnexpectedEOF(String),
    #[error("expected {expected}, found {actual:?}")]
    ExpectedToken { actual: Token, expected: String },

    /// Tried to parse a kind of command and couldn't
    /// TODO: Wrap actual error, the string is a lifetime cop-out
    #[error("failed to parse arguments for {command}: {error}")]
    InvalidCommand { command: Command, error: String },

    /// Unexpected start of an instruction
    #[error("expected a command or a gate")]
    NotACommandOrGate,

    /// The end of input was reached
    #[error("reached end of input")]
    EndOfInput,

    /// An instruction was encountered which is not yet supported for parsing by this library
    #[error("unsupported instruction")]
    UnsupportedInstruction,

    /// An error occurred in an underlying nom parser.
    #[error("internal parsing error: {0:?}")]
    Parser(nom::error::ErrorKind),

    /// Literals specified in the input cannot be supported without loss of precision
    #[error("using this literal will result in loss of precision")]
    UnsupportedPrecision,
}

impl From<nom::error::ErrorKind> for ErrorKind {
    fn from(k: nom::error::ErrorKind) -> Self {
        ErrorKind::Parser(k)
    }
}

impl<I> ::nom::error::ParseError<I> for Error<I> {
    fn from_error_kind(input: I, kind: nom::error::ErrorKind) -> Self {
        Self {
            input,
            error: kind.into(),
        }
    }

    fn append(_: I, _: nom::error::ErrorKind, other: Self) -> Self {
        other
    }
}

// `nom` requires this trait to be implemented to be able to use `map_res`, even if the input and output
// types of the "conversion" are the same
impl<I> nom::error::FromExternalError<I, Error<I>> for Error<I> {
    fn from_external_error(_input: I, _kind: nom::error::ErrorKind, error: Error<I>) -> Self {
        error
    }
}
