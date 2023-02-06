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

use nom::{
    bytes::complete::{is_a, is_not, take_while, take_while1},
    character::complete::{digit1, one_of},
    combinator::{all_consuming, map, recognize, value},
    multi::many0,
    number::complete::double,
    sequence::{preceded, terminated, tuple},
    Finish, IResult,
};
use nom_locate::LocatedSpan;
use wrapped_parsers::{alt, tag};

pub use super::token::{Token, TokenWithLocation};
use crate::parser::lexer::wrapped_parsers::expecting;
use crate::parser::token::token_with_location;
pub(crate) use error::InternalLexError;
pub use error::{LexError, LexErrorKind};

// TODO: replace manual parsing with strum::EnumString (FromStr)?
// See: https://github.com/rigetti/quil-rs/issues/94
#[derive(Debug, Copy, Clone, PartialEq, Eq, strum::Display)]
#[strum(serialize_all = "SCREAMING-KEBAB-CASE")]
pub enum Command {
    Add,
    And,
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
    Store,
    Sub,
    Xor,
}

#[derive(Debug, Clone, PartialEq, Eq, strum::Display)]
#[strum(serialize_all = "UPPERCASE")]
pub enum DataType {
    Bit,
    Octet,
    Real,
    Integer,
}

#[derive(Debug, Clone, PartialEq, Eq, strum::Display)]
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
            (
                token_with_location(value(Token::Indentation, tag("    "))),
                preceded(many0(tag(" ")), lex_token),
            ),
        )),
        many0(one_of("\n\t ")),
    )(input)
}

fn lex_token(input: LexInput) -> InternalLexResult<TokenWithLocation> {
    alt(
        "a token",
        (
            token_with_location(lex_comment),
            token_with_location(lex_data_type),
            token_with_location(lex_modifier),
            token_with_location(lex_punctuation),
            token_with_location(lex_label),
            token_with_location(lex_string),
            // Operator must come before number (or it may be parsed as a prefix)
            token_with_location(lex_operator),
            token_with_location(lex_number),
            token_with_location(lex_variable),
            token_with_location(lex_non_blocking),
            // This should come last because it's sort of a catch all
            token_with_location(lex_command_or_identifier),
        ),
    )(input)
}

fn lex_data_type(input: LexInput) -> InternalLexResult {
    alt(
        "a data type",
        (
            value(Token::DataType(DataType::Bit), tag("BIT")),
            value(Token::DataType(DataType::Integer), tag("INTEGER")),
            value(Token::DataType(DataType::Octet), tag("OCTET")),
            value(Token::DataType(DataType::Real), tag("REAL")),
        ),
    )(input)
}

fn lex_comment(input: LexInput) -> InternalLexResult {
    let (input, _) = tag("#")(input)?;
    let (input, content) = is_not("\n")(input)?;
    Ok((input, Token::Comment(content.to_string())))
}

/// If the given identifier string matches a command keyword, return the keyword;
/// otherwise, return the original identifier as a token.
fn recognize_command_or_identifier(identifier: String) -> Token {
    use Command::*;

    match identifier.as_str() {
        "DEFGATE" => Token::Command(DefGate),
        "ADD" => Token::Command(Add),
        "AND" => Token::Command(And),
        "CONVERT" => Token::Command(Convert),
        "DIV" => Token::Command(Div),
        "EQ" => Token::Command(Eq),
        "EXCHANGE" => Token::Command(Exchange),
        "GE" => Token::Command(GE),
        "GT" => Token::Command(GT),
        "IOR" => Token::Command(Ior),
        "LE" => Token::Command(LE),
        "LOAD" => Token::Command(Load),
        "LT" => Token::Command(LT),
        "MOVE" => Token::Command(Move),
        "MUL" => Token::Command(Mul),
        "NEG" => Token::Command(Neg),
        "NOT" => Token::Command(Not),
        "STORE" => Token::Command(Store),
        "SUB" => Token::Command(Sub),
        "XOR" => Token::Command(Xor),
        "DEFCIRCUIT" => Token::Command(DefCircuit),
        "MEASURE" => Token::Command(Measure),
        "HALT" => Token::Command(Halt),
        "JUMP-WHEN" => Token::Command(JumpWhen),
        "JUMP-UNLESS" => Token::Command(JumpUnless),
        "JUMP" => Token::Command(Jump),
        "RESET" => Token::Command(Reset),
        "NOP" => Token::Command(Nop),
        "INCLUDE" => Token::Command(Include),
        "PRAGMA" => Token::Command(Pragma),
        "DECLARE" => Token::Command(Declare),
        "CAPTURE" => Token::Command(Capture),
        "DEFCAL" => Token::Command(DefCal),
        "DEFFRAME" => Token::Command(DefFrame),
        "DEFWAVEFORM" => Token::Command(DefWaveform),
        "DELAY" => Token::Command(Delay),
        "FENCE" => Token::Command(Fence),
        "PULSE" => Token::Command(Pulse),
        "RAW-CAPTURE" => Token::Command(RawCapture),
        "SET-FREQUENCY" => Token::Command(SetFrequency),
        "SET-PHASE" => Token::Command(SetPhase),
        "SET-SCALE" => Token::Command(SetScale),
        "SHIFT-FREQUENCY" => Token::Command(ShiftFrequency),
        "SHIFT-PHASE" => Token::Command(ShiftPhase),
        "LABEL" => Token::Command(Label),
        _ => Token::Identifier(identifier),
    }
}

fn is_valid_identifier_leading_character(chr: char) -> bool {
    chr.is_ascii_alphabetic() || chr == '_'
}

fn is_valid_identifier_end_character(chr: char) -> bool {
    is_valid_identifier_leading_character(chr) || chr.is_ascii_digit()
}

fn is_valid_identifier_middle_character(chr: char) -> bool {
    is_valid_identifier_end_character(chr) || chr == '-'
}

fn lex_identifier_raw(input: LexInput) -> InternalLexResult<String> {
    expecting(
        "a valid identifier",
        map(
            tuple::<_, _, InternalLexError, _>((
                take_while1(is_valid_identifier_leading_character),
                take_while(is_valid_identifier_middle_character),
            )),
            |(left, right)| format!("{left}{right}"),
        ),
    )(input)
    .and_then(|(remaining, result)| {
        if !result.ends_with(is_valid_identifier_end_character) {
            Err(nom::Err::Failure(InternalLexError::from_kind(
                input,
                LexErrorKind::ExpectedContext("valid identifier"),
            )))
        } else {
            Ok((remaining, result))
        }
    })
}

fn lex_command_or_identifier(input: LexInput) -> InternalLexResult {
    let (input, identifier) = lex_identifier_raw(input)?;
    let token = recognize_command_or_identifier(identifier);
    Ok((input, token))
}

fn lex_label(input: LexInput) -> InternalLexResult {
    let (input, _) = tag("@")(input)?;
    let (input, label) = lex_identifier_raw(input)?;
    Ok((input, Token::Label(label)))
}

fn lex_non_blocking(input: LexInput) -> InternalLexResult {
    value(Token::NonBlocking, tag("NONBLOCKING"))(input)
}

fn lex_number(input: LexInput) -> InternalLexResult {
    let (input, float_string): (LexInput, LexInput) = recognize(double)(input)?;
    let integer_parse_result: IResult<LexInput, _> = all_consuming(digit1)(float_string);
    Ok((
        input,
        match integer_parse_result {
            Ok(_) => Token::Integer(float_string.parse::<u64>().unwrap()),
            Err(_) => Token::Float(double(float_string)?.1),
        },
    ))
}

fn lex_modifier(input: LexInput) -> InternalLexResult {
    alt(
        "a modifier token",
        (
            value(Token::As, tag("AS")),
            value(Token::Matrix, tag("MATRIX")),
            value(Token::Modifier(Modifier::Controlled), tag("CONTROLLED")),
            value(Token::Modifier(Modifier::Dagger), tag("DAGGER")),
            value(Token::Modifier(Modifier::Forked), tag("FORKED")),
            value(Token::Permutation, tag("PERMUTATION")),
            value(Token::Sharing, tag("SHARING")),
        ),
    )(input)
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

    use super::{lex, Command, Operator, Token};

    #[test]
    fn comment() {
        let input = LocatedSpan::new("# hello\n#world");
        let tokens = lex(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Comment(" hello".to_owned()),
                Token::NewLine,
                Token::Comment("world".to_owned())
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
                Token::Label("hello".to_owned()),
                Token::NewLine,
                Token::Label("world".to_owned())
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
    fn whole_program() {
        let input = "DECLARE ro BIT[1]
      DEFGATE HADAMARD AS MATRIX:
      \t(1/sqrt(2)),(1/sqrt(2))
      \t(1/sqrt(2)),((-1)/sqrt(2))

      DEFGATE RX(%theta) AS MATRIX:
      \tcos((%theta/2)),((-1i)*sin((%theta/2)))
      \t((-1i)*sin((%theta/2))),cos((%theta/2))

      DEFGATE Name AS PERMUTATION:
      \t1,0
      \t0,1

      DEFCIRCUIT SIMPLE:
      \tX 0
      \tX 1

      RX 0
      CZ 0 1
      MEASURE 0 ro[0]
      RESET 0
      RESET
      CAPTURE 0 \"out\" my_waveform() iq[0]
      DEFCAL X 0:
      \tPULSE 0 \"xy\" my_waveform()

      DEFCAL RX(%theta) 0:
      \tPULSE 0 \"xy\" my_waveform()

      DEFCAL MEASURE 0 dest:
      \tDECLARE iq REAL[2]
      \tCAPTURE 0 \"out\" flat(duration: 1000000, iqs: (2+3i)) iq[0]

      DEFFRAME 0 \"xy\":
      \tSAMPLE-RATE: 3000

      DEFFRAME 0 \"xy\":
      \tDIRECTION: \"rx\"
      \tCENTER-FREQUENCY: 1000
      \tHARDWARE-OBJECT: \"some object\"
      \tINITIAL-FREQUENCY: 2000
      \tSAMPLE-RATE: 3000

      DELAY 0 100
      DELAY 0 \"xy\" 100000000
      FENCE 0
      FENCE 0 1
      PULSE 0 \"xy\" my_waveform()
      PULSE 0 1 \"cz\" my_parametrized_waveform(a: 1)
      RAW-CAPTURE 0 \"out\" 200000000 iqs[0]
      SET-FREQUENCY 0 \"xy\" 5400000000
      SET-PHASE 0 \"xy\" pi
      SET-SCALE 0 \"xy\" pi
      SHIFT-FREQUENCY 0 \"ro\" 6100000000
      SHIFT-PHASE 0 \"xy\" (-pi)
      SHIFT-PHASE 0 \"xy\" (%theta*(2/pi))
      SWAP-PHASES 2 3 \"xy\" 3 4 \"xy\"";

        let input = LocatedSpan::new(input);

        lex(input).unwrap();
    }
}
