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

use crate::parser::ErrorInput;
use std::fmt;

/// The parser returned success, but there was unexpected leftover input.
///
/// This error contains the parsed item, which can be accessed using [`LeftoverError::recover`].
#[derive(Debug, PartialEq, Eq)]
pub struct LeftoverError<O> {
    line: u32,
    column: usize,
    snippet: String,
    parsed: O,
}

impl<O> fmt::Display for LeftoverError<O> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "successfully parsed {} with leftover input starting at line {}, column {} ({})",
            std::any::type_name::<O>(),
            self.line,
            self.column,
            self.snippet,
        )
    }
}

impl<O> std::error::Error for LeftoverError<O> where Self: fmt::Debug {}

impl<O> LeftoverError<O> {
    /// Create a new `LeftoverError` from the given leftover input and parsed item.
    pub(crate) fn new<I>(leftover: I, parsed: O) -> Self
    where
        I: ErrorInput,
    {
        Self {
            line: leftover.line(),
            column: leftover.column(),
            snippet: leftover.snippet(),
            parsed,
        }
    }

    /// Consumes this error and returns the parsed output.
    pub fn recover(self) -> O {
        self.parsed
    }

    /// Map the parsed output into some other type.
    pub fn map_parsed<O2>(self, map: impl FnOnce(O) -> O2) -> LeftoverError<O2> {
        let Self {
            line,
            column,
            snippet,
            parsed,
        } = self;
        let parsed = map(parsed);
        LeftoverError {
            line,
            column,
            snippet,
            parsed,
        }
    }
}
