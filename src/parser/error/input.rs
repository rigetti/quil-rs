use crate::parser::lexer::LexInput;
use crate::parser::{ParserInput, TokenWithLocation};

pub trait ErrorInput {
    fn line(&self) -> u32;
    fn column(&self) -> usize;
    fn snippet(&self) -> String;
    fn is_empty(&self) -> bool;
}

impl ErrorInput for LexInput<'_> {
    fn line(&self) -> u32 {
        self.location_line()
    }

    fn column(&self) -> usize {
        self.get_utf8_column()
    }

    fn snippet(&self) -> String {
        std::str::from_utf8(self.get_line_beginning())
            .map(|s| if s.len() == self.len() {
                format!("\"{}\"", s)
            } else {
                format!("\"{}\"...", s)
            }).unwrap_or_default()
    }

    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl ErrorInput for ParserInput<'_> {
    fn line(&self) -> u32 {
        self.iter().map(|token| token.line()).next().unwrap_or(1)
    }

    fn column(&self) -> usize {
        self.iter().map(|token| token.column()).next().unwrap_or(1)
    }

    fn snippet(&self) -> String {
        match self.iter().next() {
            None => String::from("EOF"),
            Some(token) => format!("{:?}", token.as_token()),
        }
    }

    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl ErrorInput for Vec<TokenWithLocation> {
    fn line(&self) -> u32 {
        self.as_slice().line()
    }

    fn column(&self) -> usize {
        self.as_slice().column()
    }

    fn snippet(&self) -> String {
        self.as_slice().snippet()
    }

    fn is_empty(&self) -> bool {
        self.as_slice().is_empty()
    }
}