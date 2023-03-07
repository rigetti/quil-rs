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

use super::ErrorKind;
use crate::parser::ErrorInput;
use nom::error::{ErrorKind as NomErrorKind, ParseError};

/// Internal-only error that should be converted to an [`Error`](super::Error)
/// before being returned in a public API.
///
/// `InternalError` is an intermediate error that contains a reference to the failed input
/// along with the lower-level error `E` that describes the parsing failure. This allows us
/// to generate the more user friendly [`Error`](super::Error) once parsing completes, while
/// avoiding the performance losses of generating the user-friendly information
/// (i.e. [`line`](super::Error::line), [`column`](super::Error::column), and
/// [`snippet`](super::Error::snippet)).
///
/// Because this is an intermediate error format, it is crate-private.
#[derive(Debug)]
pub(crate) struct InternalError<I, E>
where
    I: ErrorInput,
    E: std::error::Error + Send,
{
    pub(crate) input: I,
    pub(crate) error: ErrorKind<E>,
    pub(crate) prev: Option<Box<Self>>,
}

impl<I, E> InternalError<I, E>
where
    I: ErrorInput,
    E: std::error::Error + Send,
{
    pub(crate) fn new(input: I, error: ErrorKind<E>) -> Self {
        Self {
            input,
            error,
            prev: None,
        }
    }

    pub(crate) fn from_kind(input: I, error: E) -> Self {
        Self::new(input, ErrorKind::Other(error))
    }

    pub(crate) fn with_previous(mut self, prev: Self) -> Self {
        self.prev = Some(Box::new(prev));
        self
    }
}

impl<I, E> ParseError<I> for InternalError<I, E>
where
    I: ErrorInput,

    E: std::error::Error + Send,
{
    fn from_error_kind(input: I, kind: nom::error::ErrorKind) -> Self {
        Self::new(input, ErrorKind::Internal(GenericParseError(kind)))
    }

    fn append(input: I, kind: nom::error::ErrorKind, other: Self) -> Self {
        Self::new(input, ErrorKind::Internal(GenericParseError(kind))).with_previous(other)
    }
}

// `nom` requires this trait to be implemented to be able to use `map_res`, even if the input and output
// types of the "conversion" are the same
impl<I, E> nom::error::FromExternalError<I, Self> for InternalError<I, E>
where
    I: ErrorInput,
    E: std::error::Error + Send,
{
    fn from_external_error(_input: I, _kind: nom::error::ErrorKind, error: Self) -> Self {
        error
    }
}

/// A parsing error that has not been converted to something more user-friendly.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
#[error("internal parsing error: {0:?}")]
pub struct GenericParseError(NomErrorKind);

impl GenericParseError {
    pub(crate) fn new(kind: NomErrorKind) -> Self {
        Self(kind)
    }
}
