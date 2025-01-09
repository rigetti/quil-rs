// Copyright 2021 Rigetti Computing
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

mod error;
mod quoted_strings;
mod wrapped_parsers;

use std::str::FromStr;

use nom::{
    bytes::complete::{is_a, take_till, take_while, take_while1},
    character::complete::{digit1, one_of},
    combinator::{all_consuming, map, recognize, value},
    multi::many0,
    number::complete::double,
    sequence::{pair, preceded, terminated, tuple},
    Finish, IResult,
};
use nom_locate::LocatedSpan;
use wrapped_parsers::{alt, tag};

pub use super::token::{KeywordToken, Token, TokenWithLocation};
use crate::parser::lexer::wrapped_parsers::expecting;
use crate::parser::token::token_with_location;
pub(crate) use error::InternalLexError;
pub use error::{LexError, LexErrorKind};

#[derive(Debug, Copy, Clone, PartialEq, Eq, strum::Display, strum::EnumString)]
#[strum(serialize_all = "SCREAMING-KEBAB-CASE")]
pub enum Command {
    Add,
    And,
    Call,
    Capture,
    Convert,
    Declare,
    #[strum(to_string = "DEFCAL")]
    DefCal,
    #[strum(to_string = "DEFCIRCUIT")]
    DefCircuit,
    #[strum(to_string = "DEFFRAME")]
    DefFrame,
    #[strum(to_string = "DEFGATE")]
    DefGate,
    #[strum(to_string = "DEFWAVEFORM")]
    DefWaveform,
    Delay,
    Div,
    Eq,
    Exchange,
    Fence,
    GE,
    GT,
    Halt,
    Include,
    Ior,
    Jump,
    JumpUnless,
    JumpWhen,
    Label,
    LE,
    Load,
    LT,
    Measure,
    Move,
    Mul,
    Neg,
    Nop,
    Not,
    Pragma,
    Pulse,
    RawCapture,
    Reset,
    SetFrequency,
    SetPhase,
    SetScale,
    ShiftFrequency,
    ShiftPhase,
    SwapPhases,
    Store,
    Sub,
    Wait,
    Xor,
}

#[derive(Debug, Clone, PartialEq, Eq, strum::Display, strum::EnumString)]
#[strum(serialize_all = "UPPERCASE")]
pub enum DataType {
    Bit,
    Octet,
    Real,
    Integer,
}

#[derive(Debug, Clone, PartialEq, Eq, strum::Display, strum::EnumString)]
#[strum(serialize_all = "UPPERCASE")]
pub enum Modifier {
    Controlled,
    Dagger,
    Forked, // Not in the Quil grammar
}

#[derive(Debug, Clone, PartialEq, Eq, strum::Display)]
pub enum Operator {
    #[strum(serialize = "^")]
    Caret,
    #[strum(serialize = "-")]
    Minus,
    #[strum(serialize = "+")]
    Plus,
    #[strum(serialize = "/")]
    Slash,
    #[strum(serialize = "*")]
    Star,
}

pub type LexInput<'a> = LocatedSpan<&'a str>;
pub(crate) type InternalLexResult<'a, T = Token, E = InternalLexError<'a>> =
    IResult<LexInput<'a>, T, E>;
pub type LexResult<'a, T = Token, E = LexError> = IResult<LexInput<'a>, T, E>;

/// Completely lex a string, returning the tokens within. Panics if the string cannot be completely read.
pub(crate) fn lex(input: LexInput) -> Result<Vec<TokenWithLocation>, LexError> {
    all_consuming(_lex)(input)
        .finish()
        .map(|(_, tokens)| tokens)
        .map_err(LexError::from)
}

fn _lex(input: LexInput) -> InternalLexResult<Vec<TokenWithLocation>> {
    terminated(
        many0(alt(
            "indentation or a token preceded by whitespace",
            (lex_indent, preceded(many0(tag(" ")), lex_token)),
        )),
        many0(one_of("\n\t ")),
    )(input)
}

/// The Quil spec defines an indent as exactly 4 spaces. However, the lexer recognizes tabs as well
/// to allow for more flexible formatting.
fn lex_indent(input: LexInput) -> InternalLexResult<TokenWithLocation> {
    alt(
        "indentation",
        (
            token_with_location(value(Token::Indentation, tag("    "))),
            token_with_location(value(Token::Indentation, tag("\t"))),
        ),
    )(input)
}

fn lex_token(input: LexInput) -> InternalLexResult<TokenWithLocation> {
    alt(
        "a token",
        (
            token_with_location(lex_comment),
            token_with_location(lex_punctuation),
            token_with_location(lex_target),
            token_with_location(lex_string),
            // Operator must come before number (or it may be parsed as a prefix)
            token_with_location(lex_operator),
            token_with_location(lex_variable),
            // Identifiers must come before numbers so that `NaN`, `Inf`, and `Infinity` aren't
            // parsed as floats; Nom, as of version 7.1.1, will parse those strings,
            // case-insensitively, as floats
            token_with_location(lex_keyword_or_identifier),
            token_with_location(lex_number),
        ),
    )(input)
}

fn lex_comment(input: LexInput) -> InternalLexResult {
    let (input, _) = tag("#")(input)?;
    let (input, content) = take_till(|c| c == '\n')(input)?;
    Ok((input, Token::Comment(content.to_string())))
}

fn keyword_or_identifier(identifier: String) -> Token {
    fn parse<T: FromStr>(token: impl Fn(T) -> Token, identifier: &str) -> Result<Token, T::Err> {
        T::from_str(identifier).map(token)
    }

    parse(KeywordToken::into, &identifier)
        .or_else(|_| parse(Token::Command, &identifier))
        .or_else(|_| parse(Token::DataType, &identifier))
        .or_else(|_| parse(Token::Modifier, &identifier))
        .unwrap_or(Token::Identifier(identifier))
}

fn is_valid_identifier_leading_character(chr: char) -> bool {
    chr.is_ascii_alphabetic() || chr == '_'
}

fn is_valid_identifier_end_character(chr: char) -> bool {
    is_valid_identifier_leading_character(chr) || chr.is_ascii_digit()
}

fn is_dash(chr: char) -> bool {
    chr == '-'
}

fn lex_identifier_raw(input: LexInput) -> InternalLexResult<String> {
    expecting(
        "a valid identifier",
        map(
            tuple::<_, _, InternalLexError, _>((
                take_while1(is_valid_identifier_leading_character),
                take_while(is_valid_identifier_end_character),
                recognize(many0(pair(
                    take_while1(is_dash),
                    take_while1(is_valid_identifier_end_character),
                ))),
            )),
            |(leading, middle, trailing_dash_vars)| {
                format!("{leading}{middle}{trailing_dash_vars}")
            },
        ),
    )(input)
}

fn lex_keyword_or_identifier(input: LexInput) -> InternalLexResult {
    let (input, identifier) = lex_identifier_raw(input)?;
    let token = keyword_or_identifier(identifier);
    Ok((input, token))
}

fn lex_target(input: LexInput) -> InternalLexResult {
    let (input, _) = tag("@")(input)?;
    let (input, label) = lex_identifier_raw(input)?;
    Ok((input, Token::Target(label)))
}

fn lex_number(input: LexInput) -> InternalLexResult {
    let (input, float_string): (LexInput, LexInput) = recognize(double)(input)?;
    let integer_parse_result: IResult<LexInput, _> = all_consuming(digit1)(float_string);
    Ok((
        input,
        match integer_parse_result {
            Ok(_) => float_string
                .parse::<u64>()
                .map(Token::Integer)
                .map_err(|e| InternalLexError::from_kind(input, e.into()))
                .map_err(nom::Err::Failure)?,
            Err(_) => Token::Float(double(float_string)?.1),
        },
    ))
}

fn lex_operator(input: LexInput) -> InternalLexResult {
    use Operator::*;
    map(
        alt(
            "an operator",
            (
                value(Caret, tag("^")),
                value(Minus, tag("-")),
                value(Plus, tag("+")),
                value(Slash, tag("/")),
                value(Star, tag("*")),
            ),
        ),
        Token::Operator,
    )(input)
}

fn recognize_newlines(input: LexInput) -> InternalLexResult<LexInput> {
    alt(
        "one or more newlines",
        (
            is_a::<_, _, InternalLexError>("\n"),
            is_a::<_, _, InternalLexError>("\r\n"),
        ),
    )(input)
}

fn lex_punctuation(input: LexInput) -> InternalLexResult {
    use Token::*;
    alt(
        "punctuation",
        (
            value(Colon, tag(":")),
            value(Comma, tag(",")),
            value(
                Indentation,
                alt("four spaces or a tab character", (tag("    "), tag("\t"))),
            ),
            value(LBracket, tag("[")),
            value(LParenthesis, tag("(")),
            value(NewLine, recognize_newlines),
            value(RBracket, tag("]")),
            value(RParenthesis, tag(")")),
            value(Semicolon, tag(";")),
        ),
    )(input)
}

fn lex_string(input: LexInput) -> InternalLexResult {
    map(quoted_strings::unescaped_quoted_string, Token::String)(input)
}

fn lex_variable(input: LexInput) -> InternalLexResult {
    map(preceded(tag("%"), lex_identifier_raw), |ident| {
        Token::Variable(ident)
    })(input)
}

#[cfg(test)]
mod tests {
    use nom_locate::LocatedSpan;
    use rstest::*;

    use crate::parser::{common::tests::KITCHEN_SINK_QUIL, DataType};

    use super::{lex, Command, Operator, Token};

    #[test]
    fn comment() {
        let input = LocatedSpan::new("# hello\n#world\n#\n#");
        let tokens = lex(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Comment(" hello".to_owned()),
                Token::NewLine,
                Token::Comment("world".to_owned()),
                Token::NewLine,
                Token::Comment("".to_owned()),
                Token::NewLine,
                Token::Comment("".to_owned())
            ]
        )
    }

    #[test]
    fn keywords() {
        let input = LocatedSpan::new("DEFGATE DEFCIRCUIT JUMP-WHEN MATRIX LOAD load LOAD-MEMORY");
        let tokens = lex(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Command(Command::DefGate),
                Token::Command(Command::DefCircuit),
                Token::Command(Command::JumpWhen),
                Token::Matrix,
                Token::Command(Command::Load),
                Token::Identifier(String::from("load")),
                Token::Identifier(String::from("LOAD-MEMORY"))
            ]
        )
    }

    #[test]
    fn number() {
        let input = LocatedSpan::new("2 2i 2.0 2e3 2.0e3 (1+2i)");
        let tokens = lex(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Integer(2),
                Token::Integer(2),
                Token::Identifier("i".to_owned()),
                Token::Float(2.0),
                Token::Float(2000f64),
                Token::Float(2000f64),
                Token::LParenthesis,
                Token::Integer(1),
                Token::Operator(Operator::Plus),
                Token::Integer(2),
                Token::Identifier("i".to_owned()),
                Token::RParenthesis
            ]
        )
    }

    #[test]
    fn string() {
        let input = LocatedSpan::new("\"hello\"\n\"world\"");
        let tokens = lex(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::String("hello".to_owned()),
                Token::NewLine,
                Token::String("world".to_owned())
            ]
        )
    }

    #[test]
    fn gate_operation() {
        let input = LocatedSpan::new("I 0; RX 1\nCZ 0 1");
        let tokens = lex(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("I".to_owned()),
                Token::Integer(0),
                Token::Semicolon,
                Token::Identifier("RX".to_owned()),
                Token::Integer(1),
                Token::NewLine,
                Token::Identifier("CZ".to_owned()),
                Token::Integer(0),
                Token::Integer(1),
            ]
        )
    }

    #[test]
    fn label() {
        let input = LocatedSpan::new("@hello\n@world");
        let tokens = lex(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Target("hello".to_owned()),
                Token::NewLine,
                Token::Target("world".to_owned())
            ]
        )
    }

    #[test]
    fn indentation() {
        let input = LocatedSpan::new("    ");
        let tokens = lex(input).unwrap();
        assert_eq!(tokens, vec![Token::Indentation,])
    }

    #[test]
    fn indented_block() {
        let input = LocatedSpan::new("DEFGATE Name AS PERMUTATION:\n\t1,0\n    0,1");
        let tokens = lex(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Command(Command::DefGate),
                Token::Identifier("Name".to_owned()),
                Token::As,
                Token::Permutation,
                Token::Colon,
                Token::NewLine,
                Token::Indentation,
                Token::Integer(1),
                Token::Comma,
                Token::Integer(0),
                Token::NewLine,
                Token::Indentation,
                Token::Integer(0),
                Token::Comma,
                Token::Integer(1),
            ]
        )
    }

    #[test]
    fn surrounding_whitespace() {
        let input = LocatedSpan::new("\nI 0\n    \n");
        let tokens = lex(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::NewLine,
                Token::Identifier("I".to_owned()),
                Token::Integer(0),
                Token::NewLine,
                Token::Indentation,
                Token::NewLine
            ]
        )
    }

    #[rstest(input, expected,
        case("_", vec![Token::Identifier("_".to_string())]),
        case("a", vec![Token::Identifier("a".to_string())]),
        case("_a-2_b-2_", vec![Token::Identifier("_a-2_b-2_".to_string())]),
        case("a-2-%var", vec![
            Token::Identifier("a-2".to_string()),
            Token::Operator(Operator::Minus),
            Token::Variable("var".to_string())
        ]),
        case("BIT", vec![Token::DataType(DataType::Bit)]),
        case("BITS", vec![Token::Identifier("BITS".to_string())]),
        case("NaN", vec![Token::Identifier("NaN".to_string())]),
        case("nan", vec![Token::Identifier("nan".to_string())]),
        case("NaNa", vec![Token::Identifier("NaNa".to_string())]),
        case("nana", vec![Token::Identifier("nana".to_string())]),
        case("INF", vec![Token::Identifier("INF".to_string())]),
        case("Infinity", vec![Token::Identifier("Infinity".to_string())]),
        case("Inferior", vec![Token::Identifier("Inferior".to_string())]),
        case("-NaN", vec![Token::Operator(Operator::Minus), Token::Identifier("NaN".to_string())]),
        case("-inf", vec![Token::Operator(Operator::Minus), Token::Identifier("inf".to_string())]),
        case("-Infinity", vec![
            Token::Operator(Operator::Minus),
            Token::Identifier("Infinity".to_string())
        ]),
        case("-inferior", vec![
            Token::Operator(Operator::Minus),
            Token::Identifier("inferior".to_string())
        ]),
    )]
    fn it_lexes_identifier(input: &str, expected: Vec<Token>) {
        let input = LocatedSpan::new(input);
        let tokens = lex(input).unwrap();
        assert_eq!(tokens, expected);
    }

    #[rstest(input, not_expected,
        case("a-", vec![Token::Identifier("_-".to_string())]),
        case("-a", vec![Token::Identifier("-a".to_string())]),
        case("a\\", vec![Token::Identifier("_\\".to_string())]),
    )]
    fn it_fails_to_lex_identifier(input: &str, not_expected: Vec<Token>) {
        let input = LocatedSpan::new(input);
        if let Ok(tokens) = lex(input) {
            assert_ne!(tokens, not_expected);
        }
    }

    /// Test that an entire sample program can be lexed without failure.
    #[test]
    fn kitchen_sink() {
        let input = LocatedSpan::new(KITCHEN_SINK_QUIL);

        lex(input).unwrap();
    }
}
