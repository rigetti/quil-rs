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

mod leftover;
mod result;
mod syntax;

use std::error::Error;
use std::fmt;
use std::fmt::Formatter;

use crate::instruction::Instruction;
use crate::parser::{LexError, ParseError};
use crate::quil::Quil;
pub use leftover::LeftoverError;
pub use result::{disallow_leftover, map_parsed, recover};
pub use syntax::SyntaxError;

/// Errors that may occur while parsing a [`Program`](crate::program::Program).
#[derive(Debug, PartialEq)]
pub enum ParseProgramError<T> {
    InvalidCalibration {
        instruction: Instruction,
        message: String,
    },
    Syntax(SyntaxError<T>),
}

impl<T> From<LexError> for ParseProgramError<T>
where
    T: fmt::Debug,
{
    fn from(e: LexError) -> Self {
        Self::Syntax(SyntaxError::from(e))
    }
}

impl<T> From<ParseError> for ParseProgramError<T> {
    fn from(e: ParseError) -> Self {
        Self::Syntax(SyntaxError::from(e))
    }
}

impl<T> From<LeftoverError<T>> for ParseProgramError<T> {
    fn from(err: LeftoverError<T>) -> Self {
        Self::Syntax(SyntaxError::from(err))
    }
}

impl<T> From<SyntaxError<T>> for ParseProgramError<T> {
    fn from(err: SyntaxError<T>) -> Self {
        Self::Syntax(err)
    }
}

impl<T> ParseProgramError<T> {
    /// Convert the parsed output into another type.
    pub fn map_parsed<T2>(self, map: impl Fn(T) -> T2) -> ParseProgramError<T2> {
        match self {
            Self::InvalidCalibration {
                instruction,
                message,
            } => ParseProgramError::InvalidCalibration {
                instruction,
                message,
            },
            Self::Syntax(err) => ParseProgramError::Syntax(err.map_parsed(map)),
        }
    }
}

impl<T> fmt::Display for ParseProgramError<T>
where
    T: fmt::Debug + 'static,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidCalibration {
                instruction,
                message,
            } => write!(
                f,
                "invalid calibration `{instruction}`: {message}",
                instruction = instruction.to_quil_or_debug()
            ),
            Self::Syntax(err) => fmt::Display::fmt(err, f),
        }
    }
}

impl<T> Error for ParseProgramError<T>
where
    T: fmt::Debug + 'static,
{
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::InvalidCalibration { .. } => None,
            Self::Syntax(err) => Some(err),
        }
    }
}
