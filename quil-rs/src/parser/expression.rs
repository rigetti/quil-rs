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

use nom::combinator::opt;

use crate::expression::{FunctionCallExpression, InfixExpression, PrefixExpression};
use crate::parser::InternalParserResult;
use crate::{
    expected_token,
    expression::{Expression, ExpressionFunction, InfixOperator, PrefixOperator},
    imag,
    instruction::MemoryReference,
    parser::common::parse_memory_reference_with_brackets,
    token, unexpected_eof,
};

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
    let (mut input, mut left) = match super::split_first_token(input) {
        None => unexpected_eof!(input),
        Some((Token::Integer(value), remainder)) => {
            let (remainder, imaginary) = opt(parse_i)(remainder)?;
            match imaginary {
                None => Ok((remainder, Expression::Number(crate::real!(*value as f64)))),
                Some(_) => Ok((remainder, Expression::Number(crate::imag!(*value as f64)))),
            }
        }
        Some((Token::Float(value), remainder)) => {
            let (remainder, imaginary) = opt(parse_i)(remainder)?;
            match imaginary {
                None => Ok((remainder, Expression::Number(crate::real!(*value)))),
                Some(_) => Ok((remainder, Expression::Number(crate::imag!(*value)))),
            }
        }
        Some((Token::Variable(name), remainder)) => {
            Ok((remainder, Expression::Variable(name.clone())))
        }
        Some((Token::Identifier(_), _)) => parse_expression_identifier(input),
        Some((Token::LParenthesis, remainder)) => parse_grouped_expression(remainder),
        Some((token, _)) => {
            expected_token!(input, token, "expression".to_owned())
        }
    }?;

    if let Some(prefix) = prefix {
        left = Expression::Prefix(PrefixExpression {
            operator: prefix,
            expression: Box::new(left),
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

/// Returns successfully if the head of input is the identifier `i`, returns error otherwise.
fn parse_i(input: ParserInput) -> InternalParserResult<()> {
    match super::split_first_token(input) {
        None => unexpected_eof!(input),
        Some((Token::Identifier(v), remainder)) if v == "i" => Ok((remainder, ())),
        Some((other_token, _)) => expected_token!(input, other_token, "i".to_owned()),
    }
}

/// Given an expression function, parse the expression within its parentheses.
fn parse_function_call<'a>(
    input: ParserInput<'a>,
    function: ExpressionFunction,
) -> InternalParserResult<'a, Expression> {
    let (input, _) = token!(LParenthesis)(input)?;
    let (input, expression) = parse(input, Precedence::Lowest)?; // TODO: different precedence?
    let (input, _) = token!(RParenthesis)(input)?;
    Ok((
        input,
        Expression::FunctionCall(FunctionCallExpression {
            function,
            expression: Box::new(expression),
        }),
    ))
}

/// Identifiers have to be handled specially because some have special meaning.
///
/// By order of precedence:
///
/// 1. Memory references with brackets
/// 2. Special function and constant identifiers
/// 3. Anything else is considered to be a memory reference without index brackets
fn parse_expression_identifier(input: ParserInput) -> InternalParserResult<Expression> {
    let (input, memory_reference) = opt(parse_memory_reference_with_brackets)(input)?;
    if let Some(memory_reference) = memory_reference {
        return Ok((input, Expression::Address(memory_reference)));
    }

    match super::split_first_token(input) {
        None => unexpected_eof!(input),
        Some((Token::Identifier(ident), remainder)) => match ident.to_lowercase().as_str() {
            "cis" => parse_function_call(remainder, ExpressionFunction::Cis),
            "cos" => parse_function_call(remainder, ExpressionFunction::Cosine),
            "exp" => parse_function_call(remainder, ExpressionFunction::Exponent),
            "i" => Ok((remainder, Expression::Number(imag!(1f64)))),
            "pi" => Ok((remainder, Expression::PiConstant)),
            "sin" => parse_function_call(remainder, ExpressionFunction::Sine),
            "sqrt" => parse_function_call(remainder, ExpressionFunction::SquareRoot),
            name => Ok((
                remainder,
                Expression::Address(MemoryReference {
                    name: name.to_owned(),
                    index: 0,
                }),
            )),
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
                left: Box::new(left),
                operator: expression_operator,
                right: Box::new(right),
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
        PrefixExpression, PrefixOperator,
    };
    use crate::instruction::MemoryReference;
    use crate::parser::lexer::lex;
    use crate::quil::Quil;
    use crate::{imag, real};

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
        ];

        for case in cases {
            let input = LocatedSpan::new(case);
            let tokens = lex(input).unwrap();
            let (remainder, parsed) = parse_expression(&tokens).unwrap();
            assert_eq!(remainder.len(), 0);
            assert_eq!(parsed.to_quil_or_debug(), case);
        }
    }

    test!(
        function_call,
        parse_expression,
        "sin(1)",
        Expression::FunctionCall(FunctionCallExpression {
            function: ExpressionFunction::Sine,
            expression: Box::new(Expression::Number(real!(1f64))),
        })
    );

    test!(
        nested_function_call,
        parse_expression,
        "sin(sin(1))",
        Expression::FunctionCall(FunctionCallExpression {
            function: ExpressionFunction::Sine,
            expression: Box::new(Expression::FunctionCall(FunctionCallExpression {
                function: ExpressionFunction::Sine,
                expression: Box::new(Expression::Number(real!(1f64))),
            })),
        })
    );

    test!(
        simple_infix,
        parse_expression,
        "1+2",
        Expression::Infix(InfixExpression {
            left: Box::new(Expression::Number(real!(1f64))),
            operator: InfixOperator::Plus,
            right: Box::new(Expression::Number(real!(2f64))),
        })
    );

    test!(
        infix_with_function_call,
        parse_expression,
        "-i*sin(%theta/2)",
        Expression::Infix(InfixExpression {
            left: Box::new(Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                expression: Box::new(Expression::Number(imag!(1f64))),
            })),
            operator: InfixOperator::Star,
            right: Box::new(Expression::FunctionCall(FunctionCallExpression {
                function: ExpressionFunction::Sine,
                expression: Box::new(Expression::Infix(InfixExpression {
                    left: Box::new(Expression::Variable("theta".to_owned())),
                    operator: InfixOperator::Slash,
                    right: Box::new(Expression::Number(real!(2f64))),
                })),
            })),
        })
    );

    test!(
        infix_parenthesized,
        parse_expression,
        "(1+2i)*%a",
        Expression::Infix(InfixExpression {
            left: Box::new(Expression::Infix(InfixExpression {
                left: Box::new(Expression::Number(real!(1f64))),
                operator: InfixOperator::Plus,
                right: Box::new(Expression::Number(imag!(2f64))),
            })),
            operator: InfixOperator::Star,
            right: Box::new(Expression::Variable("a".to_owned())),
        })
    );

    test!(
        infix_with_infix_operands_implicit_precedence,
        parse_expression,
        "pi/2 + 2*theta[0]",
        Expression::Infix(InfixExpression {
            left: Box::new(Expression::Infix(InfixExpression {
                left: Box::new(Expression::PiConstant),
                operator: InfixOperator::Slash,
                right: Box::new(Expression::Number(real!(2f64))),
            })),
            operator: InfixOperator::Plus,
            right: Box::new(Expression::Infix(InfixExpression {
                left: Box::new(Expression::Number(real!(2f64))),
                operator: InfixOperator::Star,
                right: Box::new(Expression::Address(MemoryReference {
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
            left: Box::new(Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                expression: Box::new(Expression::Number(real!(3f64))),
            })),
            operator: InfixOperator::Minus,
            right: Box::new(Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                expression: Box::new(Expression::Number(real!(2f64)))
            }))
        })
    );

    #[test]
    fn parenthetical() {
        let cases = vec![
            (
                "1 + ( 2 + 3 )",
                Expression::Infix(InfixExpression {
                    left: Box::new(Expression::Number(real!(1f64))),
                    operator: InfixOperator::Plus,
                    right: Box::new(Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Number(real!(2f64))),
                        operator: InfixOperator::Plus,
                        right: Box::new(Expression::Number(real!(3f64))),
                    })),
                }),
            ),
            (
                "1+(2+3)",
                Expression::Infix(InfixExpression {
                    left: Box::new(Expression::Number(real!(1f64))),
                    operator: InfixOperator::Plus,
                    right: Box::new(Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Number(real!(2f64))),
                        operator: InfixOperator::Plus,
                        right: Box::new(Expression::Number(real!(3f64))),
                    })),
                }),
            ),
            (
                "(1+2)+3",
                Expression::Infix(InfixExpression {
                    left: Box::new(Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Number(real!(1f64))),
                        operator: InfixOperator::Plus,
                        right: Box::new(Expression::Number(real!(2f64))),
                    })),
                    operator: InfixOperator::Plus,
                    right: Box::new(Expression::Number(real!(3f64))),
                }),
            ),
            (
                "(((cos(((pi))))))",
                Expression::FunctionCall(FunctionCallExpression {
                    function: ExpressionFunction::Cosine,
                    expression: Box::new(Expression::PiConstant),
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
