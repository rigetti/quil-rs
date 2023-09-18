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

use std::{collections::HashMap, str::FromStr};

use nom::{
    branch::alt,
    combinator::{cut, map, map_res, opt, value},
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, tuple},
};

use crate::{
    expected_token,
    expression::Expression,
    instruction::{
        ArithmeticOperand, AttributeValue, BinaryOperand, ComparisonOperand, FrameIdentifier,
        GateModifier, MemoryReference, Offset, PauliGate, PauliTerm, Qubit, ScalarType, Sharing,
        Vector, WaveformInvocation,
    },
    parser::lexer::Operator,
    token,
};

use crate::parser::{InternalParseError, InternalParserResult};

use super::{
    error::ParserErrorKind,
    expression::parse_expression,
    lexer::{DataType, Modifier, Token},
    ParserInput,
};

/// Parse the operand of an arithmetic instruction, which may be a literal integer, literal real
/// number, or memory reference.
pub(crate) fn parse_arithmetic_operand<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, ArithmeticOperand> {
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
        map(parse_memory_reference, ArithmeticOperand::MemoryReference),
    ))(input)
}

/// Parse the operand of a comparison instruction, which may be a literal integer, literal real
/// number, or memory reference.
pub(crate) fn parse_comparison_operand<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, ComparisonOperand> {
    alt((
        map(
            tuple((opt(token!(Operator(o))), token!(Float(v)))),
            |(op, v)| {
                let sign = match op {
                    None => 1f64,
                    Some(Operator::Minus) => -1f64,
                    _ => panic!("Implement this error"), // TODO
                };
                ComparisonOperand::LiteralReal(sign * v)
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
                ComparisonOperand::LiteralInteger(sign * (v as i64))
            },
        ),
        map(parse_memory_reference, ComparisonOperand::MemoryReference),
    ))(input)
}

/// Parse the operand of a binary logic instruction, which may be a literal integer or memory reference.
pub(crate) fn parse_binary_logic_operand<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, BinaryOperand> {
    alt((
        map(
            tuple((opt(token!(Operator(o))), token!(Integer(v)))),
            |(op, v)| {
                let sign = match op {
                    None => 1,
                    Some(Operator::Minus) => -1,
                    _ => panic!("Implement this error"), // TODO
                };
                BinaryOperand::LiteralInteger(sign * (v as i64))
            },
        ),
        map(parse_memory_reference, BinaryOperand::MemoryReference),
    ))(input)
}

/// Parse a single attribute key-value pair of a frame. The value may be either a frame or an expression.
pub(crate) fn parse_frame_attribute<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, (String, AttributeValue)> {
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
pub(crate) fn parse_frame_identifier<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, FrameIdentifier> {
    let (input, qubits) = many1(parse_qubit)(input)?;
    let (input, name) = token!(String(v))(input)?;

    Ok((input, FrameIdentifier { name, qubits }))
}

/// Parse a gate modifier prefix, such as `CONTROLLED`.
pub(crate) fn parse_gate_modifier<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, GateModifier> {
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

/// Parse matrix used to define gate with `DEFGATE`.
pub(crate) fn parse_matrix<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, Vec<Vec<Expression>>> {
    preceded(
        token!(NewLine),
        separated_list1(
            token!(NewLine),
            preceded(
                token!(Indentation),
                separated_list0(
                    pair(token!(Comma), many0(token!(Indentation))),
                    parse_expression,
                ),
            ),
        ),
    )(input)
}

/// Parse permutation representation of a `DEFGATE` matrix.
pub(crate) fn parse_permutation<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, Vec<u64>> {
    preceded(
        token!(NewLine),
        preceded(
            token!(Indentation),
            separated_list1(token!(Comma), token!(Integer(value))),
        ),
    )(input)
}

/// Parse a [`crate::instruction::PauliWord`]. These are a special kind of a [`Token::Identifier`]
/// used in Pauli sum `DEFGATE` specifications where each identifier is made up of one or more
/// `I`, `X`, `Y`, or `Z` characters (e.g. `X`, `YY`).
fn parse_pauli_word<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, Vec<PauliGate>> {
    map_res(token!(Identifier(v)), |words| {
        let mut pauli_words: Vec<PauliGate> = Vec::new();
        for word in words.split("") {
            if word.is_empty() {
                // split("") separates every character, including empty spaces at the beginning and
                // end of string.
                continue;
            }
            pauli_words.push(PauliGate::from_str(word).map_err(|_| {
                InternalParseError::from_kind(
                    input,
                    ParserErrorKind::ExpectedCharacter {
                        expected: "(I,X,Y,Z)".to_string(),
                        actual: word.to_string(),
                    },
                )
            })?)
        }
        Ok(pauli_words)
    })(input)
}

pub(crate) fn parse_pauli_term<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, PauliTerm> {
    map_res(
        tuple((
            parse_pauli_word,
            delimited(token!(LParenthesis), parse_expression, token!(RParenthesis)),
            many1(token!(Identifier(i))),
        )),
        |(word, expression, arguments)| {
            if word.len() != arguments.len() {
                Err(InternalParseError::from_kind(
                    input,
                    ParserErrorKind::PauliTermArgumentMismatch {
                        word_length: word.len(),
                        num_args: arguments.len(),
                    },
                ))
            } else {
                Ok(PauliTerm::new(
                    word.into_iter().zip(arguments).collect(),
                    expression,
                ))
            }
        },
    )(input)
}

/// Parse Pauli sum representation of a `DEFGATE` specification.
pub(crate) fn parse_pauli_terms<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, Vec<PauliTerm>> {
    preceded(
        token!(NewLine),
        separated_list1(
            token!(NewLine),
            preceded(token!(Indentation), parse_pauli_term),
        ),
    )(input)
}

/// Parse a reference to a memory location, such as `ro[5]`, with optional brackets
/// (i.e, `ro` allowed).
pub(crate) fn parse_memory_reference<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, MemoryReference> {
    let (input, name) = token!(Identifier(v))(input)?;
    let (input, index) = opt(delimited(
        token!(LBracket),
        token!(Integer(v)),
        token!(RBracket),
    ))(input)?;
    let index = index.unwrap_or(0);
    Ok((input, MemoryReference { name, index }))
}

/// Parse a reference to a memory location, such as `ro[5]` requiring the brackets
/// (i.e, `ro` disallowed).
pub(crate) fn parse_memory_reference_with_brackets<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, MemoryReference> {
    let (input, name) = token!(Identifier(v))(input)?;
    let (input, index) = delimited(token!(LBracket), token!(Integer(v)), token!(RBracket))(input)?;
    Ok((input, MemoryReference { name, index }))
}

/// Parse a named argument key-value pair, such as `foo: 42`.
pub(crate) fn parse_named_argument<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, (String, Expression)> {
    let (input, (name, _, value)) =
        tuple((token!(Identifier(v)), token!(Colon), parse_expression))(input)?;
    Ok((input, (name, value)))
}

/// Parse the invocation of a waveform, such as `flat(iq: 1)`.
pub(crate) fn parse_waveform_invocation<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, WaveformInvocation> {
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
pub(crate) fn parse_qubit(input: ParserInput) -> InternalParserResult<Qubit> {
    match super::split_first_token(input) {
        None => Err(nom::Err::Error(InternalParseError::from_kind(
            input,
            ParserErrorKind::UnexpectedEOF("a qubit"),
        ))),
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
pub(crate) fn parse_variable_qubit(input: ParserInput) -> InternalParserResult<String> {
    match super::split_first_token(input) {
        None => Err(nom::Err::Error(InternalParseError::from_kind(
            input,
            ParserErrorKind::UnexpectedEOF("a variable qubit"),
        ))),
        Some((Token::Variable(name), remainder)) => Ok((remainder, name.clone())),
        Some((Token::Identifier(name), remainder)) => Ok((remainder, name.clone())),
        Some((other_token, _)) => {
            expected_token!(input, other_token, stringify!($expected_variant).to_owned())
        }
    }
}

fn match_data_type_token(token: DataType) -> ScalarType {
    match token {
        DataType::Bit => ScalarType::Bit,
        DataType::Integer => ScalarType::Integer,
        DataType::Real => ScalarType::Real,
        DataType::Octet => ScalarType::Octet,
    }
}

/// Parse an optional shared memory region and its offsets from a DECLARE statement.
pub(crate) fn parse_sharing<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, Option<Sharing>> {
    let (input, sharing) = opt(preceded(
        token!(Sharing),
        pair(
            token!(Identifier(v)),
            opt(preceded(
                token!(Offset),
                many1(map(
                    pair(token!(Integer(v)), token!(DataType(v))),
                    |(offset, data_type)| Offset {
                        offset,
                        data_type: match_data_type_token(data_type),
                    },
                )),
            )),
        ),
    ))(input)?;
    let sharing = sharing.map(|(name, offsets)| Sharing {
        name,
        offsets: offsets.unwrap_or_default(),
    });
    Ok((input, sharing))
}

/// Parse a "vector" which is an integer index, such as `[0]`
pub(crate) fn parse_vector<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, Vector> {
    let (input, data_type_token) = token!(DataType(v))(input)?;
    let data_type = match_data_type_token(data_type_token);

    let (input, length) = opt(delimited(
        token!(LBracket),
        token!(Integer(v)),
        token!(RBracket),
    ))(input)?;
    let length = length.unwrap_or(1);

    Ok((input, Vector { data_type, length }))
}

/// Parse a waveform name which may look like `custom` or `q20_q27_xy/sqrtiSWAP`
pub(crate) fn parse_waveform_name<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, String> {
    use crate::parser::lexer::Operator::Slash;

    let (input, mut name) = token!(Identifier(v))(input)?;
    let (input, name_extension) =
        opt(tuple((token!(Operator(Slash)), token!(Identifier(v)))))(input)?;
    if let Some((_, extension)) = name_extension {
        name = format!("{name}/{extension}");
    }
    Ok((input, name))
}

/// Parse ahead past any sequence of newlines, comments, and semicolons, returning
/// once the first other token is encountered.
pub(crate) fn skip_newlines_and_comments<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, ()> {
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

    use nom_locate::LocatedSpan;

    use super::skip_newlines_and_comments;

    #[test]
    fn it_skips_indented_comment() {
        let program = LocatedSpan::new("\t    # this is a comment X 0");
        let tokens = lex(program).unwrap();
        let (token_slice, _) = skip_newlines_and_comments(&tokens).unwrap();
        let (_, expected) = tokens.split_at(3);
        assert_eq!(token_slice, expected);
    }

    #[test]
    fn it_skips_comments() {
        let program = LocatedSpan::new("# this is a comment \n# and another\nX 0");
        let tokens = lex(program).unwrap();
        let (token_slice, _) = skip_newlines_and_comments(&tokens).unwrap();
        let (_, expected) = tokens.split_at(4);
        assert_eq!(token_slice, expected);
    }

    #[test]
    fn it_skips_new_lines() {
        let program = LocatedSpan::new("\nX 0");
        let tokens = lex(program).unwrap();
        let (token_slice, _) = skip_newlines_and_comments(&tokens).unwrap();
        let (_, expected) = tokens.split_at(1);
        assert_eq!(token_slice, expected);
    }

    #[test]
    fn it_skips_semicolons() {
        let program = LocatedSpan::new(";;;;;X 0");
        let tokens = lex(program).unwrap();
        let (token_slice, _) = skip_newlines_and_comments(&tokens).unwrap();
        let (_, expected) = tokens.split_at(5);
        assert_eq!(token_slice, expected);
    }
}

#[cfg(test)]
pub mod tests {
    use crate::{
        expression::{
            Expression, InfixExpression, InfixOperator, PrefixExpression, PrefixOperator,
        },
        instruction::{MemoryReference, PauliGate, PauliTerm},
        parser::lex,
        real,
    };

    use nom_locate::LocatedSpan;

    use super::{parse_matrix, parse_pauli_terms, parse_permutation, parse_waveform_invocation};

    /// A Quil program which uses all available instructions.
    pub const KITCHEN_SINK_QUIL: &str = "DECLARE ro BIT[1]
DEFGATE HADAMARD AS MATRIX:
\t(1/sqrt(2)),(1/sqrt(2))
\t(1/sqrt(2)),((-1)/sqrt(2))

DEFGATE RX(%theta) AS MATRIX:
\tcos((%theta/2)),((-1i)*sin((%theta/2)))
\t((-1i)*sin((%theta/2))),cos((%theta/2))

DEFGATE Name AS PERMUTATION:
\t1, 0

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

    #[test]
    fn waveform_invocation() {
        let input = LocatedSpan::new("wf(a: 1.0, b: %var, c: ro[0])");
        let lexed = lex(input).unwrap();
        let (remainder, waveform) = parse_waveform_invocation(&lexed).unwrap();
        assert!(
            remainder.is_empty(),
            "expected remainder to be empty, got {remainder:?}"
        );
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

    #[test]
    fn test_parse_matrix() {
        let input = LocatedSpan::new("\n\t1/sqrt(2), 1/sqrt(2)\n\t1/sqrt(2), -1/sqrt(2)");
        let lexed = lex(input).unwrap();
        let (remainder, matrix) = parse_matrix(&lexed).unwrap();
        assert!(
            remainder.is_empty(),
            "expected remainder to be empty, got {remainder:?}"
        );
        assert_eq!(matrix.len(), 2);
    }

    #[test]
    fn test_parse_matrix_inline_whitespace() {
        let input = LocatedSpan::new(
            "
    cis(%alpha), 0, 0, 0
    0,           1, 0, 0
    0,           0, 1, 0
    0,           0, 0, 1",
        );
        let lexed = lex(input).unwrap();
        let (remainder, matrix) = parse_matrix(&lexed).unwrap();
        assert!(
            remainder.is_empty(),
            "expected remainder to be empty, got {remainder:?}"
        );
        assert_eq!(matrix.len(), 4);
    }

    #[test]
    fn test_parse_permutation() {
        let input = LocatedSpan::new("\n\t0, 1, 2, 3, 4, 5, 7, 6");
        let lexed = lex(input).unwrap();
        let (remainder, permutation) = parse_permutation(&lexed).unwrap();
        assert!(
            remainder.is_empty(),
            "expected remainder to be empty, got {remainder:?}"
        );
        assert_eq!(permutation, vec![0, 1, 2, 3, 4, 5, 7, 6]);

        let input = LocatedSpan::new("\n\t0, 1, 2, 3, 4, 5, 7, 6\n\t0, 1, 2, 3, 4, 5, 6, 7");
        let lexed = lex(input).unwrap();
        let (remainder, permutation) = parse_permutation(&lexed).unwrap();
        assert!(!remainder.is_empty(), "multiline permutations are invalid");
        assert_eq!(permutation, vec![0, 1, 2, 3, 4, 5, 7, 6]);
    }

    #[test]
    fn test_parse_pauli_terms() {
        let input = LocatedSpan::new("\n\tZZ((-%theta)/4) p q\n\tY(%theta/4) p\n\tX(%theta/4) q");
        let lexed = lex(input).unwrap();
        let (remainder, pauli_terms) = parse_pauli_terms(&lexed).unwrap();
        assert!(remainder.is_empty());

        let expected_pauli_terms = vec![
            PauliTerm {
                arguments: vec![
                    (PauliGate::Z, "p".to_string()),
                    (PauliGate::Z, "q".to_string()),
                ],
                expression: Expression::Infix(InfixExpression {
                    left: Box::new(Expression::Prefix(PrefixExpression {
                        operator: PrefixOperator::Minus,
                        expression: Box::new(Expression::Variable("theta".to_string())),
                    })),
                    operator: InfixOperator::Slash,
                    right: Box::new(Expression::Number(real!(4.0))),
                }),
            },
            PauliTerm {
                arguments: vec![(PauliGate::Y, "p".to_string())],
                expression: Expression::Infix(InfixExpression {
                    left: Box::new(Expression::Variable("theta".to_string())),
                    operator: InfixOperator::Slash,
                    right: Box::new(Expression::Number(real!(4.0))),
                }),
            },
            PauliTerm {
                arguments: vec![(PauliGate::X, "q".to_string())],
                expression: Expression::Infix(InfixExpression {
                    left: Box::new(Expression::Variable("theta".to_string())),
                    operator: InfixOperator::Slash,
                    right: Box::new(Expression::Number(real!(4.0))),
                }),
            },
        ];

        assert_eq!(pauli_terms, expected_pauli_terms);

        let input = LocatedSpan::new("\n\tZZ((-%theta)/4) p\n");
        let lexed = lex(input).unwrap();
        assert!(
            parse_pauli_terms(&lexed).is_err(),
            "length of pauli word must match number of arguments"
        )
    }
}
