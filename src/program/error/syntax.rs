use std::{fmt, error::Error};

use crate::parser::{LexError, ParseError};

#[derive(Debug, PartialEq)]
pub enum SyntaxError {
    LexError(LexError),
    ParsingError(ParseError),
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "error while lexing: {:#}", self.source().unwrap())
        } else {
            write!(f, "error while lexing: {}", self.source().unwrap())
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
