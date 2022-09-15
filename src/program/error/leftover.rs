use std::fmt;
use crate::parser::ErrorInput;

/// The parser returned success, but there was unexpected leftover input.
///
/// This error contains the parsed item, which can be accessed using [`LeftoverError::recover`].
#[derive(Debug, PartialEq, Eq)]
pub struct LeftoverError<O> {
    line: u32,
    column: usize,
    snippet: String,
    parsed: O,
}

impl<O> fmt::Display for LeftoverError<O> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "successfully parsed {} with leftover input starting at line {}, column {} ({})",
            std::any::type_name::<O>(),
            self.line,
            self.column,
            self.snippet,
        )
    }
}

impl<O> std::error::Error for LeftoverError<O>
where
    Self: fmt::Debug,
{
}

impl<O> LeftoverError<O> {
    /// Create a new `LeftoverError` from the given leftover input and parsed item.
    pub(crate) fn new<I>(leftover: I, parsed: O) -> Self
        where I: ErrorInput,
    {
        Self { line: leftover.line(), column: leftover.column(), snippet: leftover.snippet(), parsed }
    }

    /// Consumes this error and returns the parsed output.
    pub fn recover(self) -> O {
        self.parsed
    }

    /// Map the parsed output into some other type.
    pub fn map_parsed<O2>(self, map: impl FnOnce(O) -> O2) -> LeftoverError<O2> {
        let Self { line, column, snippet, parsed } = self;
        let parsed = map(parsed);
        LeftoverError { line, column, snippet, parsed }
    }
}
