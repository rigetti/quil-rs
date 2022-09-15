use crate::parser::error::InternalParseError;

/// Kinds of errors that can occur while parsing.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum ErrorKind<E: std::error::Error> {
    /// An internal error, i.e. one that does not have a more helpful error.
    #[error(transparent)]
    Internal(InternalParseError),
    /// Another, Quil-specific parsing error.
    #[error(transparent)]
    Other(E),
}
