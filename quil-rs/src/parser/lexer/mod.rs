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

use std::{num::NonZeroU8, str::FromStr};

use lexical::{
    FromLexicalWithOptions, NumberFormatBuilder, ParseFloatOptions, ParseIntegerOptions,
};
use nom::{
    bytes::complete::{is_a, take_till, take_while, take_while1},
    character::complete::one_of,
    combinator::{all_consuming, cut, map, peek, recognize, value},
    multi::many0,
    sequence::{pair, preceded, terminated, tuple},
    Finish, IResult, Slice as _,
};
use nom_locate::LocatedSpan;
use wrapped_parsers::{alt, tag, tag_no_case};

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
            token_with_location(lex_operator),
            token_with_location(lex_variable),
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

/// Create a [`lexical`] [formatting constant][lexical::NumberFormat] from a
/// [`radix`][lexical::NumberFormat::radix] (base) and an optional [prefix
/// character][lexical::NumberFormat::base_prefix] (which comes after a leading `0`).
///
/// For instance, hexadecimal literals have a radix of `16` and a prefix character of `b'x'`.
const fn number_format(radix: u8, prefix: Option<NonZeroU8>) -> u128 {
    NumberFormatBuilder::new()
        .mantissa_radix(radix)
        .exponent_base(NonZeroU8::new(radix))
        .exponent_radix(NonZeroU8::new(10))
        .base_prefix(prefix)
        .digit_separator(NonZeroU8::new(b'_'))
        .required_integer_digits(false) // Allows `.1`
        .required_fraction_digits(false) // Allows `1.`
        .required_exponent_digits(true) // Forbids `1e`
        .required_mantissa_digits(true) // Forbids `.`
        .no_positive_mantissa_sign(true) // Forbids `+1`
        .required_mantissa_sign(false) // Allows `1`, not just `-1` (though `-1` parses as `-(1)`)
        .no_exponent_notation(false) // Allows `1e3`
        .no_positive_exponent_sign(false) // Allows `1e+3`, not just `1e3`
        .required_exponent_sign(false) // Allows `1e3`, not just `1e+3`
        .no_exponent_without_fraction(false) // Allows `1e3`, not just `1.e3`
        .no_special(true) // Forbids `nan` and `inf`
        .no_integer_leading_zeros(false) // Allows `01`
        .no_float_leading_zeros(false) // Allows `01.2`
        .required_exponent_notation(false) // Allows `1.2`, not just `12e-1`
        .case_sensitive_exponent(false) // Allows `1e3` and `1E3`
        .case_sensitive_base_prefix(false) // Allows `0x1` and `0X1`
        .digit_separator_flags(true) // Allows `1__2_.3__4_e_5__6_`, but…
        .integer_leading_digit_separator(false) // Forbids `_1` (already a variable name)
        .fraction_leading_digit_separator(false) // Forbids `._1` (but buggy in lexical 7.0.5)
        .special_digit_separator(false) // Must be `false` since we forbid special floats
        .build_strict()
}

const INTEGER_OPTIONS: ParseIntegerOptions = ParseIntegerOptions::new();
const FLOAT_OPTIONS: ParseFloatOptions = ParseFloatOptions::builder()
    .exponent(b'e')
    .decimal_point(b'.')
    .build_strict();

fn lex_and_parse_number<N: FromLexicalWithOptions, const FORMAT: u128>(
    options: &'static N::Options,
) -> impl FnMut(LexInput) -> InternalLexResult<N> {
    #[inline(always)]
    fn parse<N: FromLexicalWithOptions, const FORMAT: u128>(
        input: LexInput,
        options: &'static N::Options,
    ) -> lexical::Result<(N, usize)> {
        let result @ (_, len) =
            lexical::parse_partial_with_options::<N, _, FORMAT>(input, options)?;

        // There appears to be a bug in lexical where in `0b.`, `0b` is parsed as the integer `0`
        // even though `.` is not consumed.  This check is a workaround for that.
        if const {
            NumberFormatBuilder::rebuild(FORMAT)
                .get_base_prefix()
                .is_some()
        } && len == 2
        {
            return Err(lexical::Error::EmptyInteger(2));
        }

        // There appears to be a bug in lexical where `._1` is accepted even though
        // `fraction_leading_digit_separator` is `false`.  This check is a workaround for that.
        if let Some(dot) = input.slice(..len).find("._") {
            let include_dot = dot + 1;
            let number =
                lexical::parse_with_options::<N, _, FORMAT>(input.slice(..include_dot), options)?;
            return Ok((number, include_dot));
        }

        Ok(result)
    }

    move |input| {
        let (num, len) = parse::<N, FORMAT>(input, options).map_err(|lex_err| {
            let error = InternalLexError::from_kind(input, lex_err.into());
            match lex_err {
                lexical::Error::Overflow(_) | lexical::Error::Underflow(_) => {
                    // No need to backtrack – this was a number, just a bad one
                    nom::Err::Failure(error)
                }
                _ => nom::Err::Error(error),
            }
        })?;

        Ok((input.slice(len..), num))
    }
}

fn raw_lex_integer<const PREFIX: u8, const FORMAT: u128>(
    input: LexInput,
) -> InternalLexResult<u64> {
    const {
        assert!(PREFIX <= 127, "PREFIX must be an ASCII character");
    }

    if PREFIX == 0 {
        lex_and_parse_number::<u64, FORMAT>(&INTEGER_OPTIONS)(input)
    } else {
        let (_, _) = peek(tag_no_case(std::str::from_utf8(&[b'0', PREFIX]).unwrap()))(input)?;
        cut(lex_and_parse_number::<u64, FORMAT>(&INTEGER_OPTIONS))(input)
    }
}

macro_rules! def_radix {
    ($name:ident, $radix:literal $(,)?) => {
        def_radix!($name, $radix, 0);
    };
    ($name:ident, $radix:literal, $prefix:literal $(,)?) => {
        paste::paste! {
            #[inline]
            fn [< lex_ $name _integer >](input: LexInput) -> InternalLexResult<u64> {
                raw_lex_integer::<$prefix, { number_format($radix, NonZeroU8::new($prefix)) }>(
                    input
                )
            }
        }
    };
}

def_radix!(binary, 2, b'b');
def_radix!(decimal, 10);
def_radix!(octal, 8, b'o');
def_radix!(hexadecimal, 16, b'x');

fn lex_decimal_number(input: LexInput) -> InternalLexResult {
    let parse_float = |input| {
        let (input, float) = cut(lex_and_parse_number::<f64, { number_format(10, None) }>(
            &FLOAT_OPTIONS,
        ))(input)?;

        if !float.is_finite() {
            return Err(nom::Err::Failure(InternalLexError::from_kind(
                input,
                lexical::Error::Overflow(0).into(),
            )));
        }

        Ok((input, Token::Float(float)))
    };

    if input.as_bytes().first() == Some(&b'.') {
        parse_float(input)
    } else {
        let (input_if_int, int) = lex_decimal_integer(input)?;
        if input_if_int
            .as_bytes()
            .first()
            .is_some_and(|next| b".eE".contains(next))
        {
            // Actually it was a float all along!
            parse_float(input)
        } else {
            Ok((input_if_int, Token::Integer(int)))
        }
    }
}

fn lex_number(input: LexInput) -> InternalLexResult {
    alt(
        "number",
        (
            map(lex_binary_integer, Token::Integer),
            map(lex_octal_integer, Token::Integer),
            map(lex_hexadecimal_integer, Token::Integer),
            lex_decimal_number,
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
            value(Bang, tag("!")),
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

    #[rstest]
    #[case::bin_dot("0b10.1", [Token::Integer(0b10), Token::Float(0.1)])]
    #[case::oct_dot("0o777.7", [Token::Integer(0o777), Token::Float(0.7)])]
    #[case::hex_dot("0x3.4", [Token::Integer(0x3), Token::Float(0.4)])]
    #[case::imaginary("1i", [Token::Integer(1), Token::Identifier("i".to_owned())])]
    #[case::complex(
        "1 + 2i",
        [
            Token::Integer(1),
            Token::Operator(Operator::Plus),
            Token::Integer(2),
            Token::Identifier("i".to_owned()),
        ],
    )]
    #[case::bin_imaginary("0b10i", [Token::Integer(0b10), Token::Identifier("i".to_owned())])]
    #[case::oct_imaginary("0o10i", [Token::Integer(0o10), Token::Identifier("i".to_owned())])]
    #[case::hex_imaginary("0x10i", [Token::Integer(0x10), Token::Identifier("i".to_owned())])]
    #[case::zero_dot_underscore_one("0._1", [Token::Float(0.0), Token::Identifier("_1".to_owned())])]
    #[case::zero_dot_underscore("0._", [Token::Float(0.0), Token::Identifier("_".to_owned())])]
    fn tokenization<const N: usize>(#[case] input: &str, #[case] expected: [Token; N]) {
        let tokens = lex(LocatedSpan::new(input)).expect("lexing error");
        assert_eq!(
            tokens,
            expected,
            "lexing {input:?}:\n\
             - got:      {plain_tokens:?},\n\
             - expected: {expected:?}",
            plain_tokens = tokens
                .iter()
                .map(|located| located.as_token())
                .collect::<Vec<_>>(),
        );
    }

    #[rstest]
    #[case::bin_prefix_only("0b")]
    #[case::oct_prefix_only("0o")]
    #[case::hex_prefix_only("0x")]
    #[case::bin_prefix_dot("0b.")]
    #[case::oct_prefix_dot("0o.")]
    #[case::hex_prefix_dot("0x.")]
    #[case::bin_prefix_dot_one("0b.1")]
    #[case::oct_prefix_dot_one("0o.1")]
    #[case::hex_prefix_dot_one("0x.1")]
    #[case::int_too_big("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")]
    #[case::float_too_big("1e1_000_000")]
    #[case::dot_underscore_one("._1")]
    #[case::dot_underscore("._")]
    fn bad_token(#[case] input: &str) {
        let _ = lex(LocatedSpan::new(input)).expect_err("lexing error");
    }

    macro_rules! number_test_builder {
        {
            { $($cases:tt)* }
            $(#[$meta:meta])*
            $number:literal,
            $($rest:tt)*
        } => {
            number_test_builder! {
                { $($cases)* }
                $(#[$meta])*
                { name: ($number), input: stringify!($number), expected: $number },
                $($rest)*
            }
        };

        {
            { $($cases:tt)* }
            $(#[$meta:meta])*
            lit($($piece:tt)+) => $number:literal,
            $($rest:tt)*
        } => {
            number_test_builder! {
                { $($cases)* }
                $(#[$meta])*
                {
                    name: ($($piece)+),
                    input: concat!($(stringify!($piece)),+),
                    expected: $number,
                },
                $($rest)*
            }
        };

        {
            { $($cases:tt)* }
            $(#[$meta:meta])*
            name($($piece:tt)+) => $number:literal,
            $($rest:tt)*
        } => {
            number_test_builder! {
                { $($cases)* }
                $(#[$meta])*
                {
                    name: ($($piece)+),
                    input: stringify!($number),
                    expected: $number,
                },
                $($rest)*
            }
        };

        (
            { $($cases:tt)* }
            $(#[$meta:meta])*
            {
                name: ($($name_fragment:tt)+),
                input: $input:expr,
                expected: $number:expr$(,)?
            },
            $($rest:tt)*
        ) => {
            paste::paste! {
                number_test_builder! {
                    {
                        $($cases)*
                        #[allow(
                            non_snake_case,
                            clippy::inconsistent_digit_grouping,
                            clippy::unusual_byte_groupings,
                            clippy::mixed_case_hex_literals,
                            clippy::zero_prefixed_literal,
                        )]
                        $(#[$meta])*
                        #[case::[<lex$(_$name_fragment)*>]($input, $number)]
                    }
                    $($rest)*
                }
            }
        };

        (
            {
                { $test_name:ident, $token:ident, $ty:ty }
                $($cases:tt)*
            }
        ) => {
            #[rstest]
            $($cases)*
            fn $test_name(#[case] input: &str, #[case] expected: $ty) {
                tokenization(input, [Token::$token(expected)])
            }
        }
    }

    macro_rules! integer_tests {
        ($($input:tt)*) => {
            // Ensure that stringification preserves formatting
            const _: () = assert!(matches!(
                stringify!(0x_12__34_aBCd__).as_bytes(),
                b"0x_12__34_aBCd__"
            ));

            number_test_builder! { { {integer, Integer, u64} } $($input)* }
        }
    }

    macro_rules! float_tests {
        ($($input:tt)*) => {
            number_test_builder! { { {float, Float, f64} } $($input)* }
        }
    }

    integer_tests! {
        0,
        1,
        2_,
        3__,
        4_5,
        6__7,
        8__9__10,
        0000042,
        0x0,
        0x7F,
        lit(0X7f) => 0x7f,
        0x__CaFe__B0bA__1234__,
        0o0,
        0o10,
        lit(0O10) => 0o10,
        0o__777__555__,
        0b0,
        0b101010,
        lit(0B101010) => 0b101010,
        0b__1111__0000__,
    }

    float_tests! {
        name(0dot) => 0.,
        { name: (dot0), input: ".0", expected: 0.0 },
        name(0dot0) => 0.0,
        name(1dot) => 1.,
        { name: (dot1), input: ".1", expected: 0.1 },
        name(1dot1) => 1.1,
        name(1__2__dot3__4__eplus__1__5__) => 1__2__.3__4__e+__1__5__,
        name(1__2__dot3__4__eminus__1__5__) => 1__2__.3__4__e-__1__5__,
        {
            name: (1__2__dot3__4__e__1__5__),
            input: "1__2__.3__4__e__1__5__",
            expected: 1__2__.3__4__e+__1__5__
        },
        1e5,
        { name: (1dote5), input: "1.e5", expected: 1e5 },
        { name: (dot1e5), input: ".1e5", expected: 0.1e5 },
        name(1dot1e5) => 1.1e5,
    }

    #[test]
    fn a_bunch_of_numbers() {
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
