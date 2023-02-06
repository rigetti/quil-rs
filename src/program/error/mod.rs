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
pub use leftover::LeftoverError;
pub use result::{disallow_leftover, map_parsed, recover};
pub use syntax::SyntaxError;

/// Errors that may occur while parsing a [`Program`](crate::program::Program).
#[derive(Debug, PartialEq)]
pub enum ProgramError<T> {
    InvalidCalibration {
        instruction: Instruction,
        message: String,
    },
    RecursiveCalibration(Instruction),
    Syntax(SyntaxError<T>),
}

impl<T> From<LexError> for ProgramError<T>
where
    T: fmt::Debug,
{
    fn from(e: LexError) -> Self {
        Self::Syntax(SyntaxError::from(e))
    }
}

impl<T> From<ParseError> for ProgramError<T> {
    fn from(e: ParseError) -> Self {
        Self::Syntax(SyntaxError::from(e))
    }
}

impl<T> From<LeftoverError<T>> for ProgramError<T> {
    fn from(err: LeftoverError<T>) -> Self {
        Self::Syntax(SyntaxError::from(err))
    }
}

impl<T> From<SyntaxError<T>> for ProgramError<T> {
    fn from(err: SyntaxError<T>) -> Self {
        Self::Syntax(err)
    }
}

impl<T> ProgramError<T> {
    /// Convert the parsed output into another type.
    ///
    /// This delegates to [`LeftoverError::map_parsed`] when a [`ProgramError::Leftover`] and does
    /// nothing but change the type signature otherwise.
    pub fn map_parsed<T2>(self, map: impl Fn(T) -> T2) -> ProgramError<T2> {
        match self {
            Self::InvalidCalibration {
                instruction,
                message,
            } => ProgramError::InvalidCalibration {
                instruction,
                message,
            },
            Self::RecursiveCalibration(inst) => ProgramError::RecursiveCalibration(inst),
            Self::Syntax(err) => ProgramError::Syntax(err.map_parsed(map)),
        }
    }
}

impl<T> fmt::Display for ProgramError<T>
where
    T: fmt::Debug + 'static,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidCalibration {
                instruction,
                message,
            } => write!(f, "invalid calibration `{instruction}`: {message}"),
            Self::RecursiveCalibration(instruction) => {
                write!(f, "instruction {instruction} expands into itself")
            }
            Self::Syntax(err) => fmt::Display::fmt(err, f),
        }
    }
}

impl<T> Error for ProgramError<T>
where
    T: fmt::Debug + 'static,
{
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::InvalidCalibration { .. } => None,
            Self::RecursiveCalibration(_) => None,
            Self::Syntax(err) => Some(err),
        }
    }
}
