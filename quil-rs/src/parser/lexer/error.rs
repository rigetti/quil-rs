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

use crate::parser::error::{Error, InternalError};

pub(crate) type InternalLexError<'a> = InternalError<super::LexInput<'a>, LexErrorKind>;
/// An error that may occur while lexing Quil input.
pub type LexError = Error<LexErrorKind>;

/// Kinds of errors that may occur while lexing Quil input.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum LexErrorKind {
    /// Expected this particular raw string.
    #[error("expected {0:?}")]
    ExpectedString(&'static str),
    /// Expected something specific.
    #[error("expected {0}")]
    ExpectedContext(&'static str),
    /// Expected a specific character.
    #[error("expected character: '{0}'")]
    ExpectedChar(char),
    /// Encountered an unexpected EOF
    #[error("unexpected EOF while parsing")]
    UnexpectedEOF,
}
