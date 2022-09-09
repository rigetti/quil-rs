use crate::parser::error::{ErrorInput, InternalParseError, LeftoverError};

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind<I, O, E: std::error::Error> {
    #[error(transparent)]
    Internal(InternalParseError),
    #[error(transparent)]
    Leftover(LeftoverError<I, O>),
    #[error(transparent)]
    Other(E),
}

impl<I, O, E: std::error::Error> ErrorKind<I, O, E>
    where I: ErrorInput,
{
    pub(crate) fn convert_not_leftover<O2>(self) -> ErrorKind<I, O2, E> {
        match self {
            ErrorKind::Internal(err) => ErrorKind::Internal(err),
            ErrorKind::Leftover(err) => panic!("expected error to not be LeftoverError: {}", err),
            ErrorKind::Other(err) => ErrorKind::Other(err),
        }
    }

    pub fn map_leftover_result<O2>(self, map: impl FnOnce(O) -> O2) -> ErrorKind<I, O2, E> {
        if let ErrorKind::Leftover(leftover) = self {
            ErrorKind::Leftover(leftover.map_parsed(map))
        } else {
            self.convert_not_leftover()
        }
    }
}
