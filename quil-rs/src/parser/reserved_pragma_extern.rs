use std::str::FromStr;

use nom::{
    branch::alt,
    combinator::{map, opt},
    multi::separated_list0,
};

use crate::{
    instruction::{
        ExternDefinition, ExternParameter, ExternParameterType, ExternSignature, ReservedPragma,
        ScalarType,
    },
    token,
};

use super::{
    common::{match_data_type_token, parse_vector_with_brackets},
    InternalParserResult, ParserInput,
};

pub(super) fn parse_reserved_pragma_extern<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, crate::instruction::Instruction> {
    let (input, name) = token!(Identifier(v))(input)?;
    let (remainder, signature) = opt(token!(String(v)))(input)?;
    let signature = if let Some(signature) = signature {
        Some(
            ExternSignature::from_str(signature.as_str())
                .map_err(|e| nom::Err::Error(e.into_internal_parse_error(input)))?,
        )
    } else {
        None
    };

    let extern_definition = ExternDefinition { name, signature };

    Ok((
        remainder,
        crate::instruction::Instruction::ReservedPragma(ReservedPragma::Extern(extern_definition)),
    ))
}

/// Parse an [`ExternSignature`] from a string. Note, externs are currently defined within a
/// [`crate::instruction::Pragma] instruction as `PRAGMA EXTERN foo "signature"`; the "signature"
/// currently represents its own mini-language within the Quil specification.
///
/// Signatures are of the form:
///     `@rep[:min 0 :max 1]{@ms{Base Type}} ( @ms{Extern Parameter} @rep[:min 0]{@group{ , @ms{Extern Parameter} }} )`
///
/// For details on the signature format, see the [Quil specification "Extern Signature"](https://github.com/quil-lang/quil/blob/7f532c7cdde9f51eae6abe7408cc868fba9f91f6/specgen/spec/sec-other.s).
pub(crate) fn parse_extern_signature<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, ExternSignature> {
    let (input, return_type) = opt(token!(DataType(v)))(input)?;
    let (input, lparen) = opt(token!(LParenthesis))(input)?;
    let (input, parameters) = if lparen.is_some() {
        let (input, parameters) =
            opt(separated_list0(token!(Comma), parse_extern_parameter))(input)?;
        let (input, _) = token!(RParenthesis)(input)?;
        (input, parameters.unwrap_or_default())
    } else {
        (input, vec![])
    };

    let signature = ExternSignature {
        return_type: return_type.map(match_data_type_token),
        parameters,
    };

    Ok((input, signature))
}

fn parse_extern_parameter<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, ExternParameter> {
    let (input, name) = token!(Identifier(v))(input)?;
    let (input, _) = token!(Colon)(input)?;
    let (input, mutable) = opt(token!(Mutable))(input)?;
    let (input, data_type) = alt((
        map(
            parse_vector_with_brackets,
            ExternParameterType::FixedLengthVector,
        ),
        map(
            parse_variable_length_vector,
            ExternParameterType::VariableLengthVector,
        ),
        map(token!(DataType(v)), |data_type| {
            ExternParameterType::Scalar(match_data_type_token(data_type))
        }),
    ))(input)?;
    Ok((
        input,
        ExternParameter {
            name,
            mutable: mutable.is_some(),
            data_type,
        },
    ))
}

/// Parse a "vector", which is a [`DataType`] followed by empty brackets `[]`.
fn parse_variable_length_vector<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, ScalarType> {
    let (input, data_type_token) = token!(DataType(v))(input)?;
    let data_type = match_data_type_token(data_type_token);
    let (input, _) = token!(LBracket)(input)?;
    let (input, _) = token!(RBracket)(input)?;

    Ok((input, data_type))
}

#[cfg(test)]
mod tests {
    use crate::{
        instruction::Vector,
        parser::{lex, InternalParseError, Token},
    };

    use super::*;
    use rstest::*;

    struct ParseReservedPragmaExternTestCase {
        input: &'static str,
        remainder: Vec<Token>,
        expected: Result<crate::instruction::ExternDefinition, InternalParseError<'static>>,
    }

    impl ParseReservedPragmaExternTestCase {
        /// No signature
        fn case_01() -> Self {
            Self {
                input: "foo",
                remainder: vec![],
                expected: Ok(crate::instruction::ExternDefinition {
                    name: "foo".to_string(),
                    signature: None,
                }),
            }
        }

        /// Empty signature
        fn case_02() -> Self {
            Self {
                input: "foo \"\"",
                remainder: vec![],
                expected: Ok(crate::instruction::ExternDefinition {
                    name: "foo".to_string(),
                    signature: Some(ExternSignature {
                        return_type: None,
                        parameters: vec![],
                    }),
                }),
            }
        }

        /// Empty signature with parentheses
        fn case_03() -> Self {
            Self {
                input: "foo \"()\";",
                remainder: vec![Token::Semicolon],
                expected: Ok(crate::instruction::ExternDefinition {
                    name: "foo".to_string(),
                    signature: Some(ExternSignature {
                        return_type: None,
                        parameters: vec![],
                    }),
                }),
            }
        }

        /// Return without parameters
        fn case_04() -> Self {
            Self {
                input: "foo \"INTEGER\"",
                remainder: vec![],
                expected: Ok(crate::instruction::ExternDefinition {
                    name: "foo".to_string(),
                    signature: Some(crate::instruction::ExternSignature {
                        return_type: Some(ScalarType::Integer),
                        parameters: vec![],
                    }),
                }),
            }
        }

        /// Return with empty parentheses
        fn case_05() -> Self {
            Self {
                input: "foo \"INTEGER ()\"",
                remainder: vec![],
                expected: Ok(crate::instruction::ExternDefinition {
                    name: "foo".to_string(),
                    signature: Some(crate::instruction::ExternSignature {
                        return_type: Some(ScalarType::Integer),
                        parameters: vec![],
                    }),
                }),
            }
        }

        /// Return with parameters
        fn case_06() -> Self {
            Self {
                input: "foo \"INTEGER (bar: REAL, baz: BIT[10], biz: mut OCTET)\"",
                remainder: vec![],
                expected: Ok(crate::instruction::ExternDefinition {
                    name: "foo".to_string(),
                    signature: Some(crate::instruction::ExternSignature {
                        return_type: Some(ScalarType::Integer),
                        parameters: vec![
                            ExternParameter {
                                name: "bar".to_string(),
                                mutable: false,
                                data_type: ExternParameterType::Scalar(ScalarType::Real),
                            },
                            ExternParameter {
                                name: "baz".to_string(),
                                mutable: false,
                                data_type: ExternParameterType::FixedLengthVector(Vector {
                                    data_type: ScalarType::Bit,
                                    length: 10,
                                }),
                            },
                            ExternParameter {
                                name: "biz".to_string(),
                                mutable: true,
                                data_type: ExternParameterType::Scalar(ScalarType::Octet),
                            },
                        ],
                    }),
                }),
            }
        }

        /// Parameters without return
        fn case_07() -> Self {
            Self {
                input: "foo \"(bar: REAL, baz: BIT[10], biz : mut OCTET)\";",
                remainder: vec![Token::Semicolon],
                expected: Ok(crate::instruction::ExternDefinition {
                    name: "foo".to_string(),
                    signature: Some(crate::instruction::ExternSignature {
                        return_type: None,
                        parameters: vec![
                            ExternParameter {
                                name: "bar".to_string(),
                                mutable: false,
                                data_type: ExternParameterType::Scalar(ScalarType::Real),
                            },
                            ExternParameter {
                                name: "baz".to_string(),
                                mutable: false,
                                data_type: ExternParameterType::FixedLengthVector(Vector {
                                    data_type: ScalarType::Bit,
                                    length: 10,
                                }),
                            },
                            ExternParameter {
                                name: "biz".to_string(),
                                mutable: true,
                                data_type: ExternParameterType::Scalar(ScalarType::Octet),
                            },
                        ],
                    }),
                }),
            }
        }

        /// Variable length vector.
        fn case_08() -> Self {
            Self {
                input: "foo \"(bar : mut REAL[])\";",
                remainder: vec![Token::Semicolon],
                expected: Ok(crate::instruction::ExternDefinition {
                    name: "foo".to_string(),
                    signature: Some(crate::instruction::ExternSignature {
                        return_type: None,
                        parameters: vec![ExternParameter {
                            name: "bar".to_string(),
                            mutable: true,
                            data_type: ExternParameterType::VariableLengthVector(ScalarType::Real),
                        }],
                    }),
                }),
            }
        }
    }

    /// Test parsing of `PRAGMA EXTERN` instructions.
    #[rstest]
    #[case(ParseReservedPragmaExternTestCase::case_01())]
    #[case(ParseReservedPragmaExternTestCase::case_02())]
    #[case(ParseReservedPragmaExternTestCase::case_03())]
    #[case(ParseReservedPragmaExternTestCase::case_04())]
    #[case(ParseReservedPragmaExternTestCase::case_05())]
    #[case(ParseReservedPragmaExternTestCase::case_06())]
    #[case(ParseReservedPragmaExternTestCase::case_07())]
    #[case(ParseReservedPragmaExternTestCase::case_08())]
    fn test_parse_reserved_pragma_extern(#[case] test_case: ParseReservedPragmaExternTestCase) {
        let input = ::nom_locate::LocatedSpan::new(test_case.input);
        let tokens = lex(input).unwrap();
        match (
            test_case.expected,
            super::parse_reserved_pragma_extern(&tokens),
        ) {
            (Ok(expected), Ok((remainder, parsed))) => {
                assert_eq!(
                    parsed,
                    crate::instruction::Instruction::ReservedPragma(
                        crate::instruction::ReservedPragma::Extern(expected)
                    )
                );
                let remainder: Vec<_> = remainder.iter().map(|t| t.as_token().clone()).collect();
                assert_eq!(remainder, test_case.remainder);
            }
            (Ok(expected), Err(e)) => {
                panic!("Expected {:?}, got error: {:?}", expected, e);
            }
            (Err(expected), Ok((_, parsed))) => {
                panic!("Expected error: {:?}, got {:?}", expected, parsed);
            }
            (Err(expected), Err(found)) => {
                let expected = format!("{expected:?}");
                let found = format!("{found:?}");
                assert!(
                    found.contains(&expected),
                    "`{}` not in `{}`",
                    expected,
                    found
                );
            }
        }
    }
}
