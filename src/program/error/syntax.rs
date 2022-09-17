// Copyright 2022 Rigetti Computing
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

use std::{error::Error, fmt};

use crate::parser::{LexError, ParseError};

use super::LeftoverError;

/// Failed to deserialize a Quil program for some reason.
///
/// # Display
///
/// The standard [`Display`](fmt::Display) implementation shows only the initial error.
/// Use the alternative syntax (e.g., `format!("{:#}", err)` instead of `format!("{}", err)`) to
/// see a backtrace of errors that caused this one.
///
/// See also [`Error`](crate::parser::Error).
#[derive(Debug, PartialEq)]
pub enum SyntaxError<T> {
    LexError(LexError),
    ParseError(ParseError),
    Leftover(LeftoverError<T>),
}

impl<T> SyntaxError<T> {
    pub fn map_parsed<T2>(self, map: impl Fn(T) -> T2) -> SyntaxError<T2> {
        match self {
            Self::LexError(err) => SyntaxError::LexError(err),
            Self::ParseError(err) => SyntaxError::ParseError(err),
            Self::Leftover(err) => SyntaxError::Leftover(err.map_parsed(map)),
        }
    }

    pub fn recover(self) -> Result<T, Self> {
        match self {
            Self::Leftover(err) => Ok(err.recover()),
            err => Err(err),
        }
    }
}

impl<T: fmt::Debug + 'static> fmt::Display for SyntaxError<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "error while parsing: {:#}", self.source().unwrap())
        } else {
            write!(f, "error while parsing: {}", self.source().unwrap())
        }
    }
}

impl<T> From<LexError> for SyntaxError<T> {
    fn from(err: LexError) -> Self {
        Self::LexError(err)
    }
}

impl<T> From<ParseError> for SyntaxError<T> {
    fn from(err: ParseError) -> Self {
        Self::ParseError(err)
    }
}

impl<T> From<LeftoverError<T>> for SyntaxError<T> {
    fn from(err: LeftoverError<T>) -> Self {
        Self::Leftover(err)
    }
}

impl<T: fmt::Debug + 'static> Error for SyntaxError<T> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::LexError(err) => Some(err),
            Self::ParseError(err) => Some(err),
            Self::Leftover(err) => Some(err),
        }
    }
}
