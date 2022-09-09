use std::fmt;
use serde::de::Error;
use crate::parser::error::ErrorInput;

#[derive(Debug)]
pub struct LeftoverError<I, O> {
    leftover: I,
    parsed: O,
}

impl<I, O> fmt::Display for LeftoverError<I, O>
    where I: ErrorInput,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "successfully parsed {} with leftover input starting at line {}, column {} ({})",
            std::any::type_name::<O>(),
            self.leftover.line(),
            self.leftover.column(),
            self.leftover.snippet(),
        )
    }
}

impl<I, O> std::error::Error for LeftoverError<I, O> where Self: fmt::Debug, I: ErrorInput {}

impl<I, O> LeftoverError<I, O> {
    pub(crate) fn new(leftover: I, parsed: O) -> Self {
        Self { leftover, parsed }
    }

    pub fn recover(self) -> (I, O) {
        (self.leftover, self.parsed)
    }

    pub fn map_parsed<O2>(mut self, map: impl FnOnce(O) -> O2) -> LeftoverError<I, O2> {
        let parsed = map(self.parsed);
        LeftoverError::new(self.leftover, parsed)
    }
}
