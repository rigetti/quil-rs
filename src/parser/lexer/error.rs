use crate::parser::error::Error;

use nom::error::ErrorKind;

pub type LexError = Error<LexErrorKind>;

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum LexErrorKind {
    #[error("general error: {0:?}")]
    InternalError(ErrorKind),
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
