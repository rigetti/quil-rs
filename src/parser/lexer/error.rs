use nom::error::ErrorKind;
use crate::parser::error::{Error, OwnedStringInput};
use super::LexInput;

pub type LexError<O> = Error<OwnedStringInput, O, LexErrorKind>;

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum LexErrorKind {
    #[error("general error: {0:?}")]
    InternalError(nom::error::ErrorKind),
    #[error("expected {0:?}")]
    ExpectedString(&'static str),
    #[error("expected {0}")]
    ExpectedContext(&'static str),
}

impl From<ErrorKind> for LexErrorKind {
    fn from(kind: ErrorKind) -> Self {
        Self::InternalError(kind)
    }
}