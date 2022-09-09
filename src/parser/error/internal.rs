use nom::error::ErrorKind;

#[derive(Debug, thiserror::Error)]
#[error("internal parsing error: {0:?}")]
pub struct InternalParseError(ErrorKind);

impl InternalParseError {
    pub(crate) fn new(kind: ErrorKind) -> Self {
        Self(kind)
    }
}
