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

use crate::parser::error::GenericParseError;

/// Kinds of errors that can occur while parsing.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum ErrorKind<E: std::error::Error> {
    /// An internal error, i.e. one that does not have a more helpful error.
    #[error(transparent)]
    Internal(GenericParseError),
    /// Another, Quil-specific parsing error.
    #[error(transparent)]
    Other(E),
}
