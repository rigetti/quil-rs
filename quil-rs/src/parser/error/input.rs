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

use crate::parser::lexer::LexInput;
use crate::parser::{ParserInput, TokenWithLocation};

/// Trait for parser input that contains location information.
pub trait ErrorInput {
    /// The line where the error occurred.
    fn line(&self) -> u32;
    /// The column where the error occurred.
    fn column(&self) -> usize;
    /// A snippet of text showing the beginning of where the error occurred.
    fn snippet(&self) -> String;
    /// Whether there is anything left in the input.
    fn is_empty(&self) -> bool;
}

impl ErrorInput for LexInput<'_> {
    fn line(&self) -> u32 {
        self.location_line()
    }

    fn column(&self) -> usize {
        self.get_utf8_column()
    }

    fn snippet(&self) -> String {
        std::str::from_utf8(self.get_line_beginning())
            .map(|s| {
                if s.len() == self.len() {
                    format!("\"{s}\"")
                } else {
                    format!("\"{s}\"...")
                }
            })
            .unwrap_or_default()
    }

    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl ErrorInput for ParserInput<'_> {
    fn line(&self) -> u32 {
        self.iter().map(|token| token.line()).next().unwrap_or(1)
    }

    fn column(&self) -> usize {
        self.iter().map(|token| token.column()).next().unwrap_or(1)
    }

    fn snippet(&self) -> String {
        match self.iter().next() {
            None => String::from("EOF"),
            Some(token) => format!("{:?}", token.as_token()),
        }
    }

    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl ErrorInput for Vec<TokenWithLocation<'_>> {
    fn line(&self) -> u32 {
        self.as_slice().line()
    }

    fn column(&self) -> usize {
        self.as_slice().column()
    }

    fn snippet(&self) -> String {
        self.as_slice().snippet()
    }

    fn is_empty(&self) -> bool {
        self.as_slice().is_empty()
    }
}
