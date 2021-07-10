/**
 * Copyright 2021 Rigetti Computing
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 **/
use std::fmt;

use super::lexer::{Command, Token};

/// This is a superset of `(I, nom::ErrorKind)` that includes the additional errors specified by
/// [`ErrorKind`].
pub struct Error<I> {
    /// The remainder of the input stream at the time of the error.
    pub input: I,
    /// The error that occurred.
    pub error: ErrorKind,
}

impl fmt::Debug for Error<&[Token]> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.input.first() {
            Some(token) => write!(f, "{:?} (at {:?})", self.error, token),
            None => write!(f, "{:?} (at EOF)", self.error),
        }
    }
}

/// Parsing errors specific to Quil parsing
#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedEOF(String),
    ExpectedToken {
        actual: Token,
        expected: String,
    },

    /// Tried to parse a kind of command and couldn't
    /// TODO: Wrap actual error, the string is a lifetime cop-out
    InvalidCommand {
        command: Command,
        error: String,
    },

    /// Unexpected start of an instruction
    NotACommandOrGate,

    /// The end of input was reached
    EndOfInput,

    /// An instruction was encountered which is not yet supported for parsing by this library
    UnsupportedInstruction,

    /// An error occurred in an underlying nom parser.
    Parser(nom::error::ErrorKind),
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
