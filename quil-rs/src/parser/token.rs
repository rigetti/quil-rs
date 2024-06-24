use crate::instruction::QuotedString;
use crate::parser::lexer::{Command, DataType, LexInput, LexResult, Modifier, Operator};
use std::fmt;
use std::fmt::Formatter;

/// Wrapper for [`Token`] that includes file location information.
#[derive(Debug, Clone, PartialEq)]
pub struct TokenWithLocation<'a> {
    token: Token,
    original_input: LexInput<'a>,
}

impl PartialEq<Token> for TokenWithLocation<'_> {
    fn eq(&self, other: &Token) -> bool {
        &self.token == other
    }
}

impl TokenWithLocation<'_> {
    /// Returns a reference to the contained token.
    pub fn as_token(&self) -> &Token {
        &self.token
    }

    /// Converts this `TokenWithLocation` into the contained [`Token`].
    pub fn into_token(self) -> Token {
        self.token
    }

    /// The line that this token appears on.
    pub fn line(&self) -> u32 {
        self.original_input.location_line()
    }

    /// The column of the line this token appears on.
    pub fn column(&self) -> usize {
        self.original_input.get_utf8_column()
    }
}

impl nom::InputLength for TokenWithLocation<'_> {
    fn input_len(&self) -> usize {
        // All tokens take up exactly one place in the input token stream
        self.as_token().input_len()
    }
}

/// Wraps a parser that returns a [`Token`] and combines it with file location information.
pub(crate) fn token_with_location<'i, E, P>(
    mut parser: P,
) -> impl FnMut(LexInput<'i>) -> LexResult<'i, TokenWithLocation, E>
where
    P: nom::Parser<LexInput<'i>, Token, E>,
    E: nom::error::ParseError<LexInput<'i>>,
{
    move |input| {
        // Using this syntax because map(parser, || ...)(input) has lifetime issues for parser.
        parser.parse(input).map(|(leftover, token)| {
            (
                leftover,
                TokenWithLocation {
                    token,
                    original_input: input,
                },
            )
        })
    }
}

#[derive(Clone, PartialEq)]
pub enum Token {
    As,
    Colon,
    Comma,
    Command(Command),
    Comment(String),
    DataType(DataType),
    Float(f64),
    Identifier(String),
    Indentation,
    Integer(u64),
    Target(String),
    LBracket,
    LParenthesis,
    NonBlocking,
    Matrix,
    Modifier(Modifier),
    NewLine,
    Operator(Operator),
    Offset,
    PauliSum,
    Permutation,
    RBracket,
    RParenthesis,
    Semicolon,
    Sharing,
    String(String),
    Variable(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::As => write!(f, "AS"),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::Command(cmd) => write!(f, "{cmd}"),
            Token::Comment(comment) => write!(f, "# {comment}"),
            Token::DataType(typ) => write!(f, "{typ}"),
            Token::Float(float) => write!(f, "{float}"),
            Token::Identifier(ident) => write!(f, "{ident}"),
            Token::Indentation => write!(f, "    "),
            Token::Integer(i) => write!(f, "{i}"),
            Token::Target(label) => write!(f, "{label}"),
            Token::LBracket => write!(f, "["),
            Token::LParenthesis => write!(f, "("),
            Token::NonBlocking => write!(f, "NONBLOCKING"),
            Token::Matrix => write!(f, "MATRIX"),
            Token::Modifier(m) => write!(f, "{m}"),
            Token::NewLine => write!(f, "NEWLINE"),
            Token::Operator(op) => write!(f, "{op}"),
            Token::Offset => write!(f, "OFFSET"),
            Token::PauliSum => write!(f, "PAULI-SUM"),
            Token::Permutation => write!(f, "PERMUTATION"),
            Token::RBracket => write!(f, "]"),
            Token::RParenthesis => write!(f, ")"),
            Token::Semicolon => write!(f, ";"),
            Token::Sharing => write!(f, "SHARING"),
            Token::String(s) => write!(f, "{}", QuotedString(s)),
            Token::Variable(v) => write!(f, "{v}"),
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Token::As => write!(f, "{self}"),
            Token::Colon => write!(f, "COLON"),
            Token::Comma => write!(f, "COMMA"),
            Token::Command(cmd) => write!(f, "COMMAND({cmd})"),
            Token::Comment(comment) => write!(f, "COMMENT({comment:?})"),
            Token::DataType(typ) => write!(f, "DATATYPE({typ})"),
            Token::Float(float) => write!(f, "FLOAT({float})"),
            Token::Identifier(id) => write!(f, "IDENTIFIER({id})"),
            Token::Indentation => write!(f, "INDENT"),
            Token::Integer(i) => write!(f, "INTEGER({i})"),
            Token::Target(label) => write!(f, "@{label}"),
            Token::LBracket => write!(f, "LBRACKET"),
            Token::LParenthesis => write!(f, "LPAREN"),
            Token::NonBlocking => write!(f, "{self}"),
            Token::Matrix => write!(f, "{self}"),
            Token::Modifier(m) => write!(f, "MODIFIER({m})"),
            Token::NewLine => write!(f, "NEWLINE"),
            Token::Operator(op) => write!(f, "OPERATOR({op})"),
            Token::Offset => write!(f, "{self}"),
            Token::PauliSum => write!(f, "{self}"),
            Token::Permutation => write!(f, "{self}"),
            Token::RBracket => write!(f, "RBRACKET"),
            Token::RParenthesis => write!(f, "RPAREN"),
            Token::Semicolon => write!(f, "SEMICOLON"),
            Token::Sharing => write!(f, "{self}"),
            Token::String(s) => write!(f, "STRING({s:?})"),
            Token::Variable(v) => write!(f, "VARIABLE({v})"),
        }
    }
}

impl nom::InputLength for Token {
    fn input_len(&self) -> usize {
        // All tokens take up exactly one place in the input token stream
        1
    }
}
