use nom::error::ErrorKind;

/// A parsing error that has not been converted to something more user-friendly.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
#[error("internal parsing error: {0:?}")]
pub struct InternalParseError(ErrorKind);

impl InternalParseError {
    pub(crate) fn new(kind: ErrorKind) -> Self {
        Self(kind)
    }
}
