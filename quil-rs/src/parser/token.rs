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
) -> impl FnMut(LexInput<'i>) -> LexResult<'i, TokenWithLocation<'i>, E>
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

/// The subset of [`Token`]s which (a) do not have arguments and (b) are keywords.  Used to ensure
/// that keyword-checking remains in sync with the definition of [`Token`].
#[derive(Debug, Copy, Clone, PartialEq, Eq, strum::Display, strum::EnumString)]
#[strum(serialize_all = "SCREAMING-KEBAB-CASE")]
pub enum KeywordToken {
    As,
    Matrix,
    #[strum(serialize = "mut")]
    Mutable,
    #[strum(serialize = "NONBLOCKING")]
    NonBlocking,
    Offset,
    PauliSum,
    Permutation,
    Sequence,
    Sharing,
}

impl From<KeywordToken> for Token {
    fn from(token: KeywordToken) -> Self {
        match token {
            KeywordToken::As => Token::As,
            KeywordToken::Matrix => Token::Matrix,
            KeywordToken::Mutable => Token::Mutable,
            KeywordToken::NonBlocking => Token::NonBlocking,
            KeywordToken::Offset => Token::Offset,
            KeywordToken::PauliSum => Token::PauliSum,
            KeywordToken::Permutation => Token::Permutation,
            KeywordToken::Sequence => Token::Sequence,
            KeywordToken::Sharing => Token::Sharing,
        }
    }
}

impl TryFrom<Token> for KeywordToken {
    type Error = ();

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        // This match is explicit so that if you add a new [`Token`] constructor you have to decide
        // if it's a keyword.
        #[deny(clippy::wildcard_enum_match_arm, clippy::wildcard_in_or_patterns)]
        match token {
            Token::As => Ok(KeywordToken::As),
            Token::Matrix => Ok(KeywordToken::Matrix),
            Token::Mutable => Ok(KeywordToken::Mutable),
            Token::Offset => Ok(KeywordToken::Offset),
            Token::PauliSum => Ok(KeywordToken::PauliSum),
            Token::Permutation => Ok(KeywordToken::Permutation),
            Token::Sequence => Ok(KeywordToken::Sequence),
            Token::Sharing => Ok(KeywordToken::Sharing),

            Token::Bang
            | Token::Colon
            | Token::Comma
            | Token::Command(_)
            | Token::Comment(_)
            | Token::DataType(_)
            | Token::Float(_)
            | Token::Identifier(_)
            | Token::Indentation
            | Token::Integer(_)
            | Token::Target(_)
            | Token::LBracket
            | Token::LParenthesis
            | Token::NonBlocking
            | Token::Modifier(_)
            | Token::NewLine
            | Token::Operator(_)
            | Token::RBracket
            | Token::RParenthesis
            | Token::Semicolon
            | Token::String(_)
            | Token::Variable(_) => Err(()),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum Token {
    As,
    Bang,
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
    Mutable,
    NewLine,
    Operator(Operator),
    Offset,
    PauliSum,
    Permutation,
    RBracket,
    RParenthesis,
    Semicolon,
    Sequence,
    Sharing,
    String(String),
    Variable(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::As => write!(f, "{}", KeywordToken::As),
            Token::Bang => write!(f, "!"),
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
            Token::NonBlocking => write!(f, "{}", KeywordToken::NonBlocking),
            Token::Matrix => write!(f, "{}", KeywordToken::Matrix),
            Token::Modifier(m) => write!(f, "{m}"),
            Token::Mutable => write!(f, "{}", KeywordToken::Mutable),
            Token::NewLine => write!(f, "NEWLINE"),
            Token::Operator(op) => write!(f, "{op}"),
            Token::Offset => write!(f, "{}", KeywordToken::Offset),
            Token::PauliSum => write!(f, "{}", KeywordToken::PauliSum),
            Token::Permutation => write!(f, "{}", KeywordToken::Permutation),
            Token::RBracket => write!(f, "]"),
            Token::RParenthesis => write!(f, ")"),
            Token::Semicolon => write!(f, ";"),
            Token::Sequence => write!(f, "{}", KeywordToken::Sequence),
            Token::Sharing => write!(f, "{}", KeywordToken::Sharing),
            Token::String(s) => write!(f, "{}", QuotedString(s)),
            Token::Variable(v) => write!(f, "{v}"),
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        #[expect(clippy::match_same_arms)]
        match self {
            Token::As => write!(f, "{self}"),
            Token::Bang => write!(f, "BANG"),
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
            Token::Mutable => write!(f, "{self}"),
            Token::NewLine => write!(f, "NEWLINE"),
            Token::Operator(op) => write!(f, "OPERATOR({op})"),
            Token::Offset => write!(f, "{self}"),
            Token::PauliSum => write!(f, "{self}"),
            Token::Permutation => write!(f, "{self}"),
            Token::RBracket => write!(f, "RBRACKET"),
            Token::RParenthesis => write!(f, "RPAREN"),
            Token::Semicolon => write!(f, "SEMICOLON"),
            Token::Sequence => write!(f, "{self}"),
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
