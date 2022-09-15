use std::{fmt, error::Error};

use crate::parser::{LexError, ParseError};

/// Failed to parse a Quil program for some reason.
///
/// # Display
///
/// The standard [`Display`](fmt::Display) implementation shows only the initial error.
/// Use the alternative syntax (e.g., `format!("{:#}", err)` instead of `format!("{}", err)`) to
/// see a backtrace of errors that caused this one.
///
/// See also [`Error`](crate::parser::Error).
#[derive(Debug, PartialEq)]
pub enum SyntaxError {
    LexError(LexError),
    ParsingError(ParseError),
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "error while parsing: {:#}", self.source().unwrap())
        } else {
            write!(f, "error while parsing: {}", self.source().unwrap())
        }
    }
}

impl From<LexError> for SyntaxError {
    fn from(err: LexError) -> Self {
        SyntaxError::LexError(err)
    }
}

impl From<ParseError> for SyntaxError {
    fn from(err: ParseError) -> Self {
        SyntaxError::ParsingError(err)
    }
}

impl Error for SyntaxError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::LexError(err) => Some(err),
            Self::ParsingError(err) => Some(err),
        }
    }
}
