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

use internment::ArcIntern;
use nom::combinator::opt;
use nom::multi::separated_list1;
use nom::sequence::terminated;
use num_complex::Complex64;

use crate::expression::{FunctionCallExpression, InfixExpression, PrefixExpression};
use crate::parser::InternalParserResult;
use crate::{
    expected_token,
    expression::{Expression, ExpressionFunction, InfixOperator, PrefixOperator, QuilFunction},
    imag,
    instruction::MemoryReference,
    token, unexpected_eof,
};

use super::common::parse_i;
use super::lexer::{Operator, Token};
use super::ParserInput;

#[derive(Debug, PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Sum,
    Product,
    Exponentiation,
    Call,
}

impl From<&Token> for Precedence {
    fn from(token: &Token) -> Self {
        match token {
            Token::Operator(operator) => Self::from(operator),
            // TODO: Is this used?
            Token::LParenthesis => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }
}

impl From<&Operator> for Precedence {
    fn from(operator: &Operator) -> Self {
        match operator {
            Operator::Plus | Operator::Minus => Precedence::Sum,
            Operator::Star | Operator::Slash => Precedence::Product,
            Operator::Caret => Precedence::Exponentiation,
        }
    }
}

fn get_precedence(input: ParserInput) -> Precedence {
    match super::first_token(input) {
        Some(v) => Precedence::from(v),
        None => Precedence::Lowest,
    }
}

/// Parse an expression at the head of the current input, for as long as the expression continues.
/// Return an error only if the first token(s) do not form an expression.
pub(crate) fn parse_expression(input: ParserInput) -> InternalParserResult<Expression> {
    parse(input, Precedence::Lowest)
}

/// Recursively parse an expression as long as operator precedence is satisfied.
fn parse(input: ParserInput, precedence: Precedence) -> InternalParserResult<Expression> {
    let (input, prefix) = opt(parse_prefix)(input)?;
    let (input, maybe_immediate_value) = opt(parse_immediate_value)(input)?;

    let (mut input, mut left) = maybe_immediate_value
        .map(|number| Ok((input, Expression::Number(number))))
        .unwrap_or_else(|| match super::split_first_token(input) {
            None => unexpected_eof!(input),
            Some((Token::Variable(name), remainder)) => {
                Ok((remainder, Expression::Variable(name.clone())))
            }
            Some((Token::Identifier(_), _)) => parse_expression_identifier(input),
            Some((Token::LParenthesis, remainder)) => parse_grouped_expression(remainder),
            Some((token, _)) => expected_token!(input, token, "expression".to_owned()),
        })?;

    if let Some(prefix) = prefix {
        left = Expression::Prefix(PrefixExpression {
            operator: prefix,
            expression: ArcIntern::new(left),
        });
    }

    while get_precedence(input) > precedence {
        match super::first_token(input) {
            None => return Ok((input, left)),
            Some(Token::Operator(_)) => {
                let (remainder, expression) = parse_infix(input, left)?;
                left = expression;
                input = remainder;
            }
            Some(_) => return Ok((input, left)),
        }
    }

    Ok((input, left))
}

pub(super) fn parse_immediate_value(input: ParserInput) -> InternalParserResult<Complex64> {
    match super::split_first_token(input) {
        Some((Token::Integer(value), remainder)) => {
            let (remainder, imaginary) = opt(parse_i)(remainder)?;
            match imaginary {
                None => Ok((remainder, crate::real!(*value as f64))),
                Some(_) => Ok((remainder, crate::imag!(*value as f64))),
            }
        }
        Some((Token::Float(value), remainder)) => {
            let (remainder, imaginary) = opt(parse_i)(remainder)?;
            match imaginary {
                None => Ok((remainder, crate::real!(*value))),
                Some(_) => Ok((remainder, crate::imag!(*value))),
            }
        }
        Some((token, _)) => expected_token!(input, token, "integer or float".to_owned()),
        None => unexpected_eof!(input),
    }
}

/// Identifiers have to be handled specially because some have special meaning.
///
/// In order of precedence:
///
/// 1. An identifier followed by parentheses is a function call (some names are known to Quil but
///    this doesn't affect parsing);
/// 2. An identifier followed by brackets is a memory reference;
/// 3. The constants `pi` and `i` are recognized specially;
/// 4. Any other identifier is considered to be a memory reference without index brackets.
fn parse_expression_identifier<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, Expression> {
    match super::split_first_token(input) {
        None => unexpected_eof!(input),
        Some((Token::Identifier(ident), input)) => match super::split_first_token(input) {
            Some((Token::LParenthesis, input)) => {
                let (input, arguments) = separated_list1(token!(Comma), |input| {
                    let (input, expression) = parse(input, Precedence::Lowest)?;
                    Ok((input, ArcIntern::new(expression)))
                })(input)?;
                let (input, ()) = token!(RParenthesis)(input)?;

                let function = match ident.to_lowercase().as_str() {
                    "cis" => ExpressionFunction::Builtin(QuilFunction::Cis),
                    "cos" => ExpressionFunction::Builtin(QuilFunction::Cosine),
                    "exp" => ExpressionFunction::Builtin(QuilFunction::Exponent),
                    "sin" => ExpressionFunction::Builtin(QuilFunction::Sine),
                    "sqrt" => ExpressionFunction::Builtin(QuilFunction::SquareRoot),
                    _ => ExpressionFunction::Extern(ident.to_owned()),
                };

                Ok((
                    input,
                    Expression::FunctionCall(FunctionCallExpression {
                        function,
                        arguments,
                    }),
                ))
            }

            Some((Token::LBracket, input)) => {
                let (input, index) = terminated(token!(Integer(v)), token!(RBracket))(input)?;
                Ok((
                    input,
                    Expression::Address(MemoryReference {
                        name: ident.to_owned(),
                        index,
                    }),
                ))
            }

            _ => match ident.to_lowercase().as_str() {
                "i" => Ok((input, Expression::Number(imag!(1f64)))),
                "pi" => Ok((input, Expression::PiConstant)),
                name => Ok((
                    input,
                    Expression::Address(MemoryReference {
                        name: name.to_owned(),
                        index: 0,
                    }),
                )),
            },
        },
        Some((other_token, _)) => expected_token!(input, other_token, "identifier".to_owned()),
    }
}

/// To be called following an opening parenthesis, this will parse the expression to its end
/// and then expect a closing right parenthesis.
fn parse_grouped_expression(input: ParserInput) -> InternalParserResult<Expression> {
    let (input, expression) = parse(input, Precedence::Lowest)?;
    match super::split_first_token(input) {
        None => unexpected_eof!(input),
        Some((Token::RParenthesis, remainder)) => Ok((remainder, expression)),
        Some((other_token, _)) => {
            expected_token!(input, other_token, "right parenthesis".to_owned())
        }
    }
}

/// Parse an infix operator and then the expression to the right of the operator, and return the
/// resulting infixed expression.
fn parse_infix(input: ParserInput, left: Expression) -> InternalParserResult<Expression> {
    match super::split_first_token(input) {
        None => unexpected_eof!(input),
        Some((Token::Operator(token_operator), remainder)) => {
            let expression_operator = match token_operator {
                Operator::Plus => InfixOperator::Plus,
                Operator::Minus => InfixOperator::Minus,
                Operator::Caret => InfixOperator::Caret,
                Operator::Slash => InfixOperator::Slash,
                Operator::Star => InfixOperator::Star,
            };
            let precedence = Precedence::from(token_operator);
            let (remainder, right) = parse(remainder, precedence)?;
            let infix_expression = Expression::Infix(InfixExpression {
                left: ArcIntern::new(left),
                operator: expression_operator,
                right: ArcIntern::new(right),
            });
            Ok((remainder, infix_expression))
        }
        Some((other_token, _)) => expected_token!(input, other_token, "infix operator".to_owned()),
    }
}

/// Return the prefix operator at the beginning of the input, if any.
fn parse_prefix(input: ParserInput) -> InternalParserResult<PrefixOperator> {
    match super::split_first_token(input) {
        None => unexpected_eof!(input),
        Some((Token::Operator(Operator::Minus), remainder)) => {
            Ok((remainder, PrefixOperator::Minus))
        }
        Some((other_token, _)) => expected_token!(input, other_token, "prefix operator".to_owned()),
    }
}

#[cfg(test)]
mod tests {
    use crate::expression::{
        Expression, ExpressionFunction, FunctionCallExpression, InfixExpression, InfixOperator,
        PrefixExpression, PrefixOperator, QuilFunction,
    };
    use crate::instruction::MemoryReference;
    use crate::parser::lexer::lex;
    use crate::quil::Quil;
    use crate::{imag, real};

    use internment::ArcIntern;
    use nom_locate::LocatedSpan;

    use super::parse_expression;

    macro_rules! test {
        ($name: ident, $parser: ident, $input: expr, $expected: expr) => {
            #[test]
            fn $name() {
                let input = LocatedSpan::new($input);
                let tokens = lex(input).unwrap();
                let (remainder, parsed) = $parser(&tokens).unwrap();
                assert_eq!(remainder.len(), 0);
                assert_eq!(parsed, $expected);
            }
        };
    }

    // given a function and vector of (input, expected) tuples, invoke the function on each and
    // panic on the first mismatch
    fn compare(cases: Vec<(&str, Expression)>) {
        for case in cases {
            let input = LocatedSpan::new(case.0);
            let tokens = lex(input).unwrap();
            let (remainder, parsed) = parse_expression(&tokens).unwrap();
            assert_eq!(remainder.len(), 0);
            assert_eq!(case.1, parsed);
        }
    }

    // Round-trip expressions to validate parsing & display
    #[test]
    fn display() {
        let cases = vec![
            "pi",
            "sin(pi)",
            "1+(2*3)",
            "(1+2)*3",
            "%theta",
            "cis(%theta)",
            "%a+%b",
            "(pi/2)+(1*theta[0])",
            "3 - -2",
            "extern_fn(1, 2, 5)",
        ];

        for case in cases {
            let input = LocatedSpan::new(case);
            let tokens = lex(input).unwrap();
            let (remainder, parsed) = parse_expression(&tokens).unwrap();
            assert_eq!(remainder.len(), 0);
            assert_eq!(parsed.to_quil_or_debug(), case);
        }
    }

    #[test]
    fn nullary_functions_disallowed() {
        let case = "nullary_fn()";
        let input = LocatedSpan::new(case);
        let tokens = lex(input).unwrap();
        if let Ok((remainder, result)) = parse_expression(&tokens) {
            panic!(
                concat!(
                    "expected parsing a nullary function application, \"{case}\", to fail;\n",
                    "instead it parsed as \"{result}\",\n",
                    "with remaining tokens {remainder:?}",
                ),
                case = case,
                result = result.to_quil_or_debug(),
                remainder = remainder,
            );
        }
    }

    test!(
        function_call,
        parse_expression,
        "sin(1)",
        Expression::FunctionCall(FunctionCallExpression {
            function: ExpressionFunction::Builtin(QuilFunction::Sine),
            arguments: vec![ArcIntern::new(Expression::Number(real!(1f64)))],
        })
    );

    test!(
        nested_function_call,
        parse_expression,
        "sin(sin(1))",
        Expression::FunctionCall(FunctionCallExpression {
            function: ExpressionFunction::Builtin(QuilFunction::Sine),
            arguments: vec![ArcIntern::new(Expression::FunctionCall(
                FunctionCallExpression {
                    function: ExpressionFunction::Builtin(QuilFunction::Sine),
                    arguments: vec![ArcIntern::new(Expression::Number(real!(1f64)))],
                }
            ))],
        })
    );

    test!(
        extern_call_unary,
        parse_expression,
        "extern_fn(1)",
        Expression::FunctionCall(FunctionCallExpression {
            function: ExpressionFunction::Extern("extern_fn".into()),
            arguments: vec![Expression::Number(real!(1f64)).into()],
        })
    );

    test!(
        extern_call_binary,
        parse_expression,
        "extern_fn(1, 2)",
        Expression::FunctionCall(FunctionCallExpression {
            function: ExpressionFunction::Extern("extern_fn".into()),
            arguments: vec![
                Expression::Number(real!(1f64)).into(),
                Expression::Number(real!(2f64)).into()
            ],
        })
    );

    test!(
        extern_call_ternary,
        parse_expression,
        "extern_fn(1, 2, 3)",
        Expression::FunctionCall(FunctionCallExpression {
            function: ExpressionFunction::Extern("extern_fn".into()),
            arguments: vec![
                Expression::Number(real!(1f64)).into(),
                Expression::Number(real!(2f64)).into(),
                Expression::Number(real!(3f64)).into(),
            ],
        })
    );

    test!(
        extern_call_quaternary,
        parse_expression,
        "extern_fn(1, 2, 3, 4)",
        Expression::FunctionCall(FunctionCallExpression {
            function: ExpressionFunction::Extern("extern_fn".into()),
            arguments: vec![
                Expression::Number(real!(1f64)).into(),
                Expression::Number(real!(2f64)).into(),
                Expression::Number(real!(3f64)).into(),
                Expression::Number(real!(4f64)).into(),
            ],
        })
    );

    test!(
        extern_call_nested,
        parse_expression,
        "outer(inner(x+y[1]), sin(%z), pi*(theta + 0.5))",
        Expression::FunctionCall(FunctionCallExpression {
            function: ExpressionFunction::Extern("outer".into()),
            arguments: vec![
                Expression::FunctionCall(FunctionCallExpression {
                    function: ExpressionFunction::Extern("inner".into()),
                    arguments: vec![Expression::Infix(InfixExpression {
                        left: Expression::Address(MemoryReference {
                            name: "x".to_string(),
                            index: 0,
                        })
                        .into(),
                        operator: InfixOperator::Plus,
                        right: Expression::Address(MemoryReference {
                            name: "y".to_string(),
                            index: 1,
                        })
                        .into(),
                    })
                    .into()],
                })
                .into(),
                Expression::FunctionCall(FunctionCallExpression {
                    function: ExpressionFunction::Builtin(QuilFunction::Sine),
                    arguments: vec![Expression::Variable("z".to_owned()).into()],
                })
                .into(),
                Expression::Infix(InfixExpression {
                    left: ArcIntern::new(Expression::PiConstant),
                    operator: InfixOperator::Star,
                    right: Expression::Infix(InfixExpression {
                        left: Expression::Address(MemoryReference {
                            name: "theta".to_string(),
                            index: 0,
                        })
                        .into(),
                        operator: InfixOperator::Plus,
                        right: Expression::Number(real!(0.5f64)).into(),
                    })
                    .into(),
                })
                .into()
            ],
        })
    );

    test!(
        simple_infix,
        parse_expression,
        "1+2",
        Expression::Infix(InfixExpression {
            left: ArcIntern::new(Expression::Number(real!(1f64))),
            operator: InfixOperator::Plus,
            right: ArcIntern::new(Expression::Number(real!(2f64))),
        })
    );

    test!(
        infix_with_function_call,
        parse_expression,
        "-i*sin(%theta/2)",
        Expression::Infix(InfixExpression {
            left: ArcIntern::new(Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                expression: ArcIntern::new(Expression::Number(imag!(1f64))),
            })),
            operator: InfixOperator::Star,
            right: ArcIntern::new(Expression::FunctionCall(FunctionCallExpression {
                function: ExpressionFunction::Builtin(QuilFunction::Sine),
                arguments: vec![ArcIntern::new(Expression::Infix(InfixExpression {
                    left: ArcIntern::new(Expression::Variable("theta".to_owned())),
                    operator: InfixOperator::Slash,
                    right: ArcIntern::new(Expression::Number(real!(2f64))),
                }))],
            })),
        })
    );

    test!(
        infix_parenthesized,
        parse_expression,
        "(1+2i)*%a",
        Expression::Infix(InfixExpression {
            left: ArcIntern::new(Expression::Infix(InfixExpression {
                left: ArcIntern::new(Expression::Number(real!(1f64))),
                operator: InfixOperator::Plus,
                right: ArcIntern::new(Expression::Number(imag!(2f64))),
            })),
            operator: InfixOperator::Star,
            right: ArcIntern::new(Expression::Variable("a".to_owned())),
        })
    );

    test!(
        infix_with_infix_operands_implicit_precedence,
        parse_expression,
        "pi/2 + 2*theta[0]",
        Expression::Infix(InfixExpression {
            left: ArcIntern::new(Expression::Infix(InfixExpression {
                left: ArcIntern::new(Expression::PiConstant),
                operator: InfixOperator::Slash,
                right: ArcIntern::new(Expression::Number(real!(2f64))),
            })),
            operator: InfixOperator::Plus,
            right: ArcIntern::new(Expression::Infix(InfixExpression {
                left: ArcIntern::new(Expression::Number(real!(2f64))),
                operator: InfixOperator::Star,
                right: ArcIntern::new(Expression::Address(MemoryReference {
                    name: "theta".to_string(),
                    index: 0,
                })),
            })),
        })
    );

    test!(
        infix_minus_negative,
        parse_expression,
        "-3 - -2",
        Expression::Infix(InfixExpression {
            left: ArcIntern::new(Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                expression: ArcIntern::new(Expression::Number(real!(3f64))),
            })),
            operator: InfixOperator::Minus,
            right: ArcIntern::new(Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                expression: ArcIntern::new(Expression::Number(real!(2f64)))
            }))
        })
    );

    #[test]
    fn parenthetical() {
        let cases = vec![
            (
                "1 + ( 2 + 3 )",
                Expression::Infix(InfixExpression {
                    left: ArcIntern::new(Expression::Number(real!(1f64))),
                    operator: InfixOperator::Plus,
                    right: ArcIntern::new(Expression::Infix(InfixExpression {
                        left: ArcIntern::new(Expression::Number(real!(2f64))),
                        operator: InfixOperator::Plus,
                        right: ArcIntern::new(Expression::Number(real!(3f64))),
                    })),
                }),
            ),
            (
                "1+(2+3)",
                Expression::Infix(InfixExpression {
                    left: ArcIntern::new(Expression::Number(real!(1f64))),
                    operator: InfixOperator::Plus,
                    right: ArcIntern::new(Expression::Infix(InfixExpression {
                        left: ArcIntern::new(Expression::Number(real!(2f64))),
                        operator: InfixOperator::Plus,
                        right: ArcIntern::new(Expression::Number(real!(3f64))),
                    })),
                }),
            ),
            (
                "(1+2)+3",
                Expression::Infix(InfixExpression {
                    left: ArcIntern::new(Expression::Infix(InfixExpression {
                        left: ArcIntern::new(Expression::Number(real!(1f64))),
                        operator: InfixOperator::Plus,
                        right: ArcIntern::new(Expression::Number(real!(2f64))),
                    })),
                    operator: InfixOperator::Plus,
                    right: ArcIntern::new(Expression::Number(real!(3f64))),
                }),
            ),
            (
                "(((cos(((pi))))))",
                Expression::FunctionCall(FunctionCallExpression {
                    function: ExpressionFunction::Builtin(QuilFunction::Cosine),
                    arguments: vec![ArcIntern::new(Expression::PiConstant)],
                }),
            ),
        ];

        compare(cases);
    }

    #[test]
    fn pi() {
        let cases = vec![("pi", Expression::PiConstant)];

        compare(cases);
    }

    #[test]
    fn variable() {
        let cases = vec![
            ("%theta", Expression::Variable("theta".to_owned())),
            ("%pi", Expression::Variable("pi".to_owned())),
            ("%sin", Expression::Variable("sin".to_owned())),
        ];

        compare(cases);
    }
}
