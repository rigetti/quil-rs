use std::convert::Infallible;
use std::fmt;
use nom::error::{ParseError, Error as NomError};
use thiserror::private::AsDynError;
use crate::parser::error::{ErrorInput, InternalParseError};
use crate::parser::error::input::OwnedErrorInput;
use crate::parser::error::kind::ErrorKind;
use crate::parser::error::leftover::LeftoverError;

#[derive(Debug)]
pub struct Error<I, O, E=Infallible>
    where E: std::error::Error,
{
    line: u32,
    column: usize,
    snippet: String,
    kind: ErrorKind<I, O, E>,
    previous: Option<Box<dyn std::error::Error + 'static>>
}

impl<I, O, E> Error<I, O, E>
    where I: OwnedErrorInput,
          O: fmt::Debug,
          E: std::error::Error,
{
    pub(crate) fn from_other<I2>(input: I2, other: E) -> Self
        where I2: ErrorInput<OwnedInput=I>
    {
        let kind = ErrorKind::Other(other);
        Self::internal_new::<>(input, kind)
    }

    pub(crate) fn from_leftover<I2>(input: I2, leftover: I2, output: O) -> Result<O, Self>
        where I2: ErrorInput<OwnedInput=I>
    {
        if leftover.is_empty() {
            Ok(output)
        } else {
            let kind = ErrorKind::Leftover(LeftoverError::new(
                leftover.into_owned(),
                output,
            ));
            Err(Self::internal_new(input, kind))
        }
    }

    pub(crate) fn with_previous<E2>(mut self, previous: E2) -> Self
        where E2: std::error::Error,
    {
        self.previous = Some(Box::new(previous));
        self
    }

    fn internal_new<I2>(input: I2, kind: ErrorKind<I, O, E>) -> Self
        where I2: ErrorInput<OwnedInput=I>,
    {
        let line = input.line();
        let column = input.column();
        let snippet = input.snippet();
        Self { line, column, snippet, kind, previous: None }
    }

    pub(crate) fn from_nom_err<I2: ErrorInput>(err: NomError<I2>) -> Self
        where I2: ErrorInput<OwnedInput=I>
    {
        let kind = ErrorKind::Internal(InternalParseError::new(err.code));
        Self::from_nom_err_with_kind(err, kind)
    }

    pub(crate) fn from_nom_err_with_kind<I2: ErrorInput>(err: NomError<I2>, kind: ErrorKind<I, O, E>) -> Self
        where I2: ErrorInput<OwnedInput=I>
    {
        Self::internal_new(err.input, kind)
    }

    pub(crate) fn map_parsed<O2: fmt::Debug>(self, map: impl FnOnce(O) -> O2) -> Error<I, O2, E> {
        let Error { line, column, snippet, kind, previous } = self;
        let kind = if let ErrorKind::Leftover(err) = kind {
            let (leftover, parsed) = err.recover();
            ErrorKind::Leftover(LeftoverError::new(
                leftover,
                map(parsed),
            ))
        } else {
            kind.convert_not_leftover()
        };
        Error { line, column, snippet, kind, previous }
    }

    pub(crate) fn convert_not_leftover<O2>(self) -> Error<I, O2, E> {
        let Error { line, column, snippet, kind, previous } = self;
        let kind = kind.convert_not_leftover();
        Error { line, column, snippet, kind, previous }
    }
}

impl<I, O, E> fmt::Display for Error<I, O, E>
    where ErrorKind<I, O, E>: fmt::Display,
          E: std::error::Error,
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

impl<I, O, E> std::error::Error for Error<I, O, E>
    where E: std::error::Error,
          Self: fmt::Display + fmt::Debug,
{
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.previous.as_ref().map(|prev| prev.as_dyn_error())
    }
}

impl<I, I2, O, E> ParseError<I2> for Error<I, O, E>
    where I2: ErrorInput<OwnedInput=I>,
          I: OwnedErrorInput,
          O: fmt::Debug,
          E: std::error::Error,
{
    fn from_error_kind(input: I2, kind: nom::error::ErrorKind) -> Self {
        let nom_err = nom::error::Error::new(input, kind);
        Self::from_nom_err(nom_err)
    }

    fn append(_input: I2, _kind: nom::error::ErrorKind, other: Self) -> Self {
        other
        //Self::with_previous(input, Kind::from(kind), other)
    }
}

// `nom` requires this trait to be implemented to be able to use `map_res`, even if the input and output
// types of the "conversion" are the same
impl<I, I2, O, E> nom::error::FromExternalError<I2, Self> for Error<I, O, E>
    where I2: ErrorInput<OwnedInput=I>,
          O: fmt::Debug,
          E: std::error::Error,
{
    fn from_external_error(_input: I2, _kind: nom::error::ErrorKind, error: Self) -> Self {
        error
    }
}
