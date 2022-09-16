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
pub enum SyntaxError {
    LexError(LexError),
    ParseError(ParseError),
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "error while parsing: {:#}", self.source().unwrap())
        } else {
            write!(f, "error while parsing: {}", self.source().unwrap())
        }
    }
}

impl From<LexError> for SyntaxError {
    fn from(err: LexError) -> Self {
        SyntaxError::LexError(err)
    }
}

impl From<ParseError> for SyntaxError {
    fn from(err: ParseError) -> Self {
        SyntaxError::ParsingError(err)
    }
}

impl Error for SyntaxError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::LexError(err) => Some(err),
            Self::ParsingError(err) => Some(err),
        }
    }
}
