/**
 * Copyright 2021 Rigetti Computing
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 **/
use std::collections::HashMap;

use nom::{
    branch::alt,
    combinator::{cut, map, opt, value},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, preceded, tuple},
};

use crate::{
    expected_token,
    expression::Expression,
    instruction::{
        ArithmeticOperand, AttributeValue, FrameIdentifier, GateModifier, MemoryReference, Qubit,
        ScalarType, Vector, WaveformInvocation,
    },
    parser::lexer::Operator,
    token,
};

use super::{
    error::{Error, ErrorKind},
    expression::parse_expression,
    lexer::{DataType, Modifier, Token},
    ParserInput, ParserResult,
};

/// Parse the operand of an arithmetic instruction, which may be a literal integer, literal real number, or memory reference.
pub fn parse_arithmetic_operand<'a>(input: ParserInput<'a>) -> ParserResult<'a, ArithmeticOperand> {
    alt((
        map(
            tuple((opt(token!(Operator(o))), token!(Float(v)))),
            |(op, v)| {
                let sign = match op {
                    None => 1f64,
                    Some(Operator::Minus) => -1f64,
                    _ => panic!("Implement this error"), // TODO
                };
                ArithmeticOperand::LiteralReal(sign * v)
            },
        ),
        map(
            tuple((opt(token!(Operator(o))), token!(Integer(v)))),
            |(op, v)| {
                let sign = match op {
                    None => 1,
                    Some(Operator::Minus) => -1,
                    _ => panic!("Implement this error"), // TODO
                };
                ArithmeticOperand::LiteralInteger(sign * (v as i64))
            },
        ),
        map(parse_memory_reference, |f| {
            ArithmeticOperand::MemoryReference(f)
        }),
    ))(input)
}

/// Parse a single attribute key-value pair of a frame. The value may be either a frame or an expression.
pub fn parse_frame_attribute<'a>(
    input: ParserInput<'a>,
) -> ParserResult<'a, (String, AttributeValue)> {
    let (input, _) = token!(NewLine)(input)?;
    let (input, _) = token!(Indentation)(input)?;
    let (input, key) = token!(Identifier(v))(input)?;
    let (input, _) = token!(Colon)(input)?;
    let (input, value) = alt((
        map(token!(String(v)), AttributeValue::String),
        map(parse_expression, |expression| {
            AttributeValue::Expression(expression)
        }),
    ))(input)?;
    Ok((input, (key, value)))
}

/// Parse a frame identifier, such as `0 "rf"`.
pub fn parse_frame_identifier<'a>(input: ParserInput<'a>) -> ParserResult<'a, FrameIdentifier> {
    let (input, qubits) = many1(parse_qubit)(input)?;
    let (input, name) = token!(String(v))(input)?;

    Ok((input, FrameIdentifier { name, qubits }))
}

/// Parse a gate modifier prefix, such as `CONTROLLED`.
pub fn parse_gate_modifier<'a>(input: ParserInput<'a>) -> ParserResult<'a, GateModifier> {
    let (input, token) = token!(Modifier(v))(input)?;
    Ok((
        input,
        match token {
            Modifier::Controlled => GateModifier::Controlled,
            Modifier::Dagger => GateModifier::Dagger,
            Modifier::Forked => GateModifier::Forked,
        },
    ))
}

/// Parse a reference to a memory location, such as `ro[5]`, with optional brackets (i.e, `ro` allowed).
pub fn parse_memory_reference<'a>(input: ParserInput<'a>) -> ParserResult<'a, MemoryReference> {
    let (input, name) = token!(Identifier(v))(input)?;
    let (input, index) = opt(delimited(
        token!(LBracket),
        token!(Integer(v)),
        token!(RBracket),
    ))(input)?;
    let index = index.unwrap_or(0);
    Ok((input, MemoryReference { name, index }))
}

/// Parse a reference to a memory location, such as `ro[5]` requiring the brackets (i.e, `ro` disallowed).
pub fn parse_memory_reference_with_brackets<'a>(
    input: ParserInput<'a>,
) -> ParserResult<'a, MemoryReference> {
    let (input, name) = token!(Identifier(v))(input)?;
    let (input, index) = delimited(token!(LBracket), token!(Integer(v)), token!(RBracket))(input)?;
    Ok((input, MemoryReference { name, index }))
}

/// Parse a named argument key-value pair, such as `foo: 42`.
pub fn parse_named_argument<'a>(input: ParserInput<'a>) -> ParserResult<'a, (String, Expression)> {
    let (input, (name, _, value)) =
        tuple((token!(Identifier(v)), token!(Colon), parse_expression))(input)?;
    Ok((input, (name, value)))
}

/// Parse the invocation of a waveform, such as `flat(iq: 1)`.
pub fn parse_waveform_invocation<'a>(
    input: ParserInput<'a>,
) -> ParserResult<'a, WaveformInvocation> {
    let (input, name) = parse_waveform_name(input)?;
    let (input, parameter_tuples) = opt(delimited(
        token!(LParenthesis),
        cut(separated_list0(token!(Comma), parse_named_argument)),
        token!(RParenthesis),
    ))(input)?;
    let parameter_tuples = parameter_tuples.unwrap_or_default();
    let parameters: HashMap<_, _> = parameter_tuples.into_iter().collect();

    Ok((input, WaveformInvocation { name, parameters }))
}

/// Parse a single qubit, which may be an integer (`1`), variable (`%q1`), or identifier (`q1`).
/// Per the specification, variable-named and identifier-named are valid in different locations,
/// but this parser is tolerant and accepts both as equivalent.
pub fn parse_qubit(input: ParserInput) -> ParserResult<Qubit> {
    match input.split_first() {
        None => Err(nom::Err::Error(Error {
            input,
            error: ErrorKind::UnexpectedEOF("a qubit".to_owned()),
        })),
        Some((Token::Integer(value), remainder)) => Ok((remainder, Qubit::Fixed(*value))),
        Some((Token::Variable(name), remainder)) => Ok((remainder, Qubit::Variable(name.clone()))),
        Some((Token::Identifier(name), remainder)) => {
            Ok((remainder, Qubit::Variable(name.clone())))
        }
        Some((other_token, _)) => {
            expected_token!(input, other_token, stringify!($expected_variant).to_owned())
        }
    }
}

/// Parse a variable qubit (i.e. a named qubit)
pub fn parse_variable_qubit(input: ParserInput) -> ParserResult<String> {
    match input.split_first() {
        None => Err(nom::Err::Error(Error {
            input,
            error: ErrorKind::UnexpectedEOF("a variable qubit".to_owned()),
        })),
        Some((Token::Variable(name), remainder)) => Ok((remainder, name.clone())),
        Some((Token::Identifier(name), remainder)) => Ok((remainder, name.clone())),
        Some((other_token, _)) => {
            expected_token!(input, other_token, stringify!($expected_variant).to_owned())
        }
    }
}

/// Parse a "vector" which is an integer index, such as `[0]`
pub fn parse_vector<'a>(input: ParserInput<'a>) -> ParserResult<'a, Vector> {
    let (input, data_type_token) = token!(DataType(v))(input)?;

    let data_type = match data_type_token {
        DataType::Bit => ScalarType::Bit,
        DataType::Integer => ScalarType::Integer,
        DataType::Real => ScalarType::Real,
        DataType::Octet => ScalarType::Octet,
    };

    let (input, length) = opt(delimited(
        token!(LBracket),
        token!(Integer(v)),
        token!(RBracket),
    ))(input)?;
    let length = length.unwrap_or(1);

    Ok((input, Vector { data_type, length }))
}

/// Parse a waveform name which may look like `custom` or `q20_q27_xy/sqrtiSWAP`
pub fn parse_waveform_name<'a>(input: ParserInput<'a>) -> ParserResult<'a, String> {
    use crate::parser::lexer::Operator::Slash;

    let (input, mut name) = token!(Identifier(v))(input)?;
    let (input, name_extension) =
        opt(tuple((token!(Operator(Slash)), token!(Identifier(v)))))(input)?;
    if let Some((_, extension)) = name_extension {
        name = format!("{}/{}", name, extension);
    }
    Ok((input, name))
}

/// Parse ahead past any sequence of newlines, comments, and semicolons, returning
/// once the first other token is encountered.
pub fn skip_newlines_and_comments<'a>(input: ParserInput<'a>) -> ParserResult<'a, ()> {
    let (input, _) = many0(alt((
        preceded(many0(token!(Indentation)), value((), token!(Comment(v)))),
        token!(NewLine),
        token!(Semicolon),
    )))(input)?;
    Ok((input, ()))
}

#[cfg(test)]
mod describe_skip_newlines_and_comments {
    use crate::parser::lex;

    use super::skip_newlines_and_comments;

    #[test]
    fn it_skips_indented_comment() {
        let program = "\t    # this is a comment X 0";
        let tokens = lex(program).unwrap();
        let (token_slice, _) = skip_newlines_and_comments(&tokens).unwrap();
        let (_, expected) = tokens.split_at(3);
        assert_eq!(token_slice, expected);
    }

    #[test]
    fn it_skips_comments() {
        let program = "# this is a comment \n# and another\nX 0";
        let tokens = lex(program).unwrap();
        let (token_slice, _) = skip_newlines_and_comments(&tokens).unwrap();
        let (_, expected) = tokens.split_at(4);
        assert_eq!(token_slice, expected);
    }

    #[test]
    fn it_skips_new_lines() {
        let program = "\nX 0";
        let tokens = lex(program).unwrap();
        let (token_slice, _) = skip_newlines_and_comments(&tokens).unwrap();
        let (_, expected) = tokens.split_at(1);
        assert_eq!(token_slice, expected);
    }

    #[test]
    fn it_skips_semicolons() {
        let program = ";;;;;X 0";
        let tokens = lex(program).unwrap();
        let (token_slice, _) = skip_newlines_and_comments(&tokens).unwrap();
        let (_, expected) = tokens.split_at(5);
        assert_eq!(token_slice, expected);
    }
}

#[cfg(test)]
mod tests {
    use crate::{expression::Expression, instruction::MemoryReference, parser::lex, real};

    use super::parse_waveform_invocation;

    #[test]
    fn waveform_invocation() {
        let input = "wf(a: 1.0, b: %var, c: ro[0])";
        let lexed = lex(input).unwrap();
        let (remainder, waveform) = parse_waveform_invocation(&lexed).unwrap();
        assert_eq!(remainder, &[]);
        assert_eq!(
            waveform.parameters,
            vec![
                ("a".to_owned(), Expression::Number(real!(1f64))),
                ("b".to_owned(), Expression::Variable("var".to_owned())),
                (
                    "c".to_owned(),
                    Expression::Address(MemoryReference {
                        name: "ro".to_owned(),
                        index: 0
                    })
                )
            ]
            .into_iter()
            .collect()
        )
    }
}
