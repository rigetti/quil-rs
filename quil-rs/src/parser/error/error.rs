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

use crate::parser::error::kind::ErrorKind;
use crate::parser::error::{ErrorInput, GenericParseError, InternalError};
use nom::error::{Error as NomError, ParseError};
use std::convert::Infallible;
use std::fmt;
use thiserror::__private::AsDynError;

/// An error that may occur while parsing.
///
/// This error contains file location information, to assist with debugging.
///
/// # Display
///
/// The [`Display`](fmt::Display) impl for this error allows for alternate outputs.
///
/// The standard output (i.e., with `format!("{}", err)`) outputs just this error, including file
/// location information.
///
/// The alternate output (i.e., with `format!("{:#}", err)`) outputs this error as well a
/// user-readable backtrace of the errors that caused this one.
///
/// When displaying errors to end-users, prefer the alternate format for easier debugging.
#[derive(Debug)]
pub struct Error<E = Infallible>
where
    E: std::error::Error + Send,
{
    line: u32,
    column: usize,
    snippet: String,
    kind: ErrorKind<E>,
    previous: Option<Box<dyn std::error::Error + 'static + Send + Sync>>,
}

impl<I, E> From<InternalError<I, E>> for Error<E>
where
    I: ErrorInput,
    E: std::error::Error + Send + Sync + 'static,
{
    fn from(internal: InternalError<I, E>) -> Self {
        let mut new = Self::internal_new(internal.input, internal.error);
        if let Some(prev) = internal.prev {
            let prev = Self::from(*prev);
            new = new.with_previous(prev);
        }
        new
    }
}

impl<E> PartialEq for Error<E>
where
    E: std::error::Error + PartialEq + Send,
{
    fn eq(&self, other: &Self) -> bool {
        self.line == other.line
            && self.column == other.column
            && self.snippet == other.snippet
            && self.kind == other.kind
            && match (&self.previous, &other.previous) {
                (None, None) => true,
                (None, Some(_)) => false,
                (Some(_), None) => false,
                (Some(left), Some(right)) => left.to_string() == right.to_string(),
            }
    }
}

impl<E> Error<E>
where
    E: std::error::Error + Send + Sync + 'static,
{
    pub(crate) fn from_nom_internal_err<I>(error: nom::Err<InternalError<I, E>>) -> nom::Err<Self>
    where
        I: ErrorInput,
    {
        error.map(Self::from)
    }

    /// Attach a previous error to this one.
    pub(crate) fn with_previous<E2>(mut self, previous: E2) -> Self
    where
        E2: std::error::Error + 'static + Send + Sync,
    {
        self.previous = Some(Box::new(previous));
        self
    }

    fn internal_new<I>(input: I, kind: ErrorKind<E>) -> Self
    where
        I: ErrorInput,
    {
        let line = input.line();
        let column = input.column();
        let snippet = input.snippet();
        Self {
            line,
            column,
            snippet,
            kind,
            previous: None,
        }
    }

    /// Create an `Error` from a nom error.
    pub(crate) fn from_nom_err<I>(err: NomError<I>) -> Self
    where
        I: ErrorInput,
    {
        let kind = ErrorKind::Internal(GenericParseError::new(err.code));
        Self::from_nom_err_with_kind(err, kind)
    }

    /// Create an `Error` by getting the input from a nom error, but set the error kind to the
    /// provided one.
    pub(crate) fn from_nom_err_with_kind<I>(err: NomError<I>, kind: ErrorKind<E>) -> Self
    where
        I: ErrorInput,
    {
        Self::internal_new(err.input, kind)
    }
}

impl<E> fmt::Display for Error<E>
where
    ErrorKind<E>: fmt::Display,
    E: std::error::Error + Send,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "at line {}, column {} ({}): {}",
            self.line, self.column, self.snippet, self.kind
        )?;
        if f.alternate() {
            if let Some(previous) = &self.previous {
                write!(f, "\n\tcause: {previous}")?;
            }
        }
        Ok(())
    }
}

impl<E> std::error::Error for Error<E>
where
    E: std::error::Error + Send,

    Self: fmt::Display + fmt::Debug,
{
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.previous.as_ref().map(|prev| prev.as_dyn_error())
    }
}

impl<I, E> ParseError<I> for Error<E>
where
    I: ErrorInput,

    E: std::error::Error + Send + Sync + 'static,
{
    fn from_error_kind(input: I, kind: nom::error::ErrorKind) -> Self {
        let nom_err = nom::error::Error::new(input, kind);
        Self::from_nom_err(nom_err)
    }

    fn append(_input: I, _kind: nom::error::ErrorKind, other: Self) -> Self {
        other
    }
}

// `nom` requires this trait to be implemented to be able to use `map_res`, even if the input and output
// types of the "conversion" are the same
impl<I, E> nom::error::FromExternalError<I, Self> for Error<E>
where
    I: ErrorInput,
    E: std::error::Error + Send + Sync,
{
    fn from_external_error(_input: I, _kind: nom::error::ErrorKind, error: Self) -> Self {
        error
    }
}

impl<I, E> nom::error::FromExternalError<I, InternalError<I, E>> for Error<E>
where
    I: ErrorInput,
    E: std::error::Error + Send + Sync + 'static,
{
    fn from_external_error(
        _input: I,
        _kind: nom::error::ErrorKind,
        error: InternalError<I, E>,
    ) -> Self {
        Self::from(error)
    }
}
