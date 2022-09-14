use std::convert::Infallible;
use std::fmt;
use nom::error::{ParseError, Error as NomError};
use thiserror::private::AsDynError;
use crate::parser::error::{ErrorInput, InternalParseError};
use crate::parser::error::kind::ErrorKind;

#[derive(Debug)]
pub struct Error<E = Infallible>
    where E: std::error::Error,
{
    line: u32,
    column: usize,
    snippet: String,
    kind: ErrorKind<E>,
    previous: Option<Box<dyn std::error::Error + 'static>>,
}

impl<E> PartialEq for Error<E>
    where E: std::error::Error + PartialEq,
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
where E: std::error::Error,
{
    pub(crate) fn from_other<I>(input: I, other: E) -> Self
    where
        I: ErrorInput,
    {
        let kind = ErrorKind::Other(other);
        Self::internal_new::<>(input, kind)
    }

    pub(crate) fn with_previous<E2>(mut self, previous: E2) -> Self
        where E2: std::error::Error + 'static,
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
        Self { line, column, snippet, kind, previous: None }
    }

    pub(crate) fn from_nom_err<I>(err: NomError<I>) -> Self
        where I: ErrorInput
    {
        let kind = ErrorKind::Internal(InternalParseError::new(err.code));
        Self::from_nom_err_with_kind(err, kind)
    }

    pub(crate) fn from_nom_err_with_kind<I>(
        err: NomError<I>,
        kind: ErrorKind<E>,
    ) -> Self
    where
        I: ErrorInput,
    {
        Self::internal_new(err.input, kind)
    }
}

impl<E> fmt::Display for Error<E>
where
    ErrorKind<E>: fmt::Display,
    E: std::error::Error
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "at line {}, column {} ({}): {}", self.line, self.column, self.snippet, self.kind)?;
        if f.alternate() {
            if let Some(previous) = &self.previous {
                write!(f, "\n\tcause: {}", previous)?;
            }
        }
        Ok(())
    }
}

impl<E> std::error::Error for Error<E>
where
    E: std::error::Error,
    Self: fmt::Display + fmt::Debug,
{
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.previous.as_ref().map(|prev| prev.as_dyn_error())
    }
}

impl<I, E> ParseError<I> for Error<E>
where
    I: ErrorInput,
    E: std::error::Error,
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
    E: std::error::Error,
{
    fn from_external_error(_input: I, _kind: nom::error::ErrorKind, error: Self) -> Self {
        error
    }
}
