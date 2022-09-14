use crate::parser::error::InternalParseError;

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum ErrorKind<E: std::error::Error> {
    #[error(transparent)]
    Internal(InternalParseError),
    #[error(transparent)]
    Other(E),
}
