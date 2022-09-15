use crate::parser::error::Error;

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
}
