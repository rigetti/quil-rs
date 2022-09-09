use nom_locate::LocatedSpan;
use crate::parser::lexer::LexInput;
use crate::parser::{ParserInput, TokenWithLocation};

pub(crate) trait ErrorInput {
    type OwnedInput: OwnedErrorInput;
    fn line(&self) -> u32;
    fn column(&self) -> usize;
    fn snippet(&self) -> String;
    fn is_empty(&self) -> bool;
    fn into_owned(self) -> Self::OwnedInput;
}

pub(crate) trait OwnedErrorInput: ErrorInput + 'static {}
impl<I> OwnedErrorInput for I where I: ErrorInput + 'static {}

impl ErrorInput for LexInput<'_> {
    type OwnedInput = OwnedStringInput;

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

    fn into_owned(self) -> Self::OwnedInput {
        Self::OwnedInput::from(self)
    }
}

impl ErrorInput for ParserInput<'_> {
    type OwnedInput = Vec<TokenWithLocation>;
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

    fn into_owned(self) -> Self::OwnedInput {
        self.to_vec()
    }
}

impl ErrorInput for Vec<TokenWithLocation> {
    type OwnedInput = Self;

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

    fn into_owned(self) -> Self::OwnedInput {
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OwnedStringInput {
    input: String,
    line: u32,
    column: usize,
    offset: usize,
}

impl From<LexInput<'_>> for OwnedStringInput {
    fn from(input: LexInput<'_>) -> Self {
        Self {
            input: input.fragment().to_string(),
            line: input.line(),
            column: input.column(),
            offset: input.location_offset(),
        }
    }
}

impl OwnedStringInput {
    pub fn as_lex_input(&self) -> LexInput {
        // We reused values from a LocatedSpan, with the one difference of
        // converting a &str to a String. So this should be safe to do.
        unsafe {
            LocatedSpan::new_from_raw_offset(
                self.offset,
                self.line,
                &self.input,
                ()
            )
        }
    }
}

impl ErrorInput for OwnedStringInput {
    type OwnedInput = Self;

    fn line(&self) -> u32 {
        self.line
    }

    fn column(&self) -> usize {
        self.column
    }

    fn snippet(&self) -> String {
        self.as_lex_input().snippet()
    }

    fn is_empty(&self) -> bool {
        self.input.is_empty()
    }

    fn into_owned(self) -> Self::OwnedInput {
        self
    }
}