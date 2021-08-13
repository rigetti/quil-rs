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
use std::f64::consts::PI;
use std::fmt;
use std::str::FromStr;

use crate::parser::{lex, parse_expression};
use crate::{imag, instruction::MemoryReference, real};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EvaluationError {
    Incomplete,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Address(MemoryReference),
    FunctionCall {
        function: ExpressionFunction,
        expression: Box<Expression>,
    },
    Infix {
        left: Box<Expression>,
        operator: InfixOperator,
        right: Box<Expression>,
    },
    Number(num_complex::Complex64),
    PiConstant,
    Prefix {
        operator: PrefixOperator,
        expression: Box<Expression>,
    },
    Variable(String),
}

/// Compute the result of an infix expression where both operands are complex.
fn calculate_infix(
    left: &num_complex::Complex64,
    operator: &InfixOperator,
    right: &num_complex::Complex64,
) -> num_complex::Complex64 {
    use InfixOperator::*;
    match operator {
        Caret => left.powc(*right),
        Plus => left + right,
        Minus => left - right,
        Slash => left / right,
        Star => left * right,
    }
}

/// Compute the result of a Quil-defined expression function where the operand is complex.
fn calculate_function(
    function: &ExpressionFunction,
    argument: &num_complex::Complex64,
) -> num_complex::Complex64 {
    use ExpressionFunction::*;
    match function {
        Sine => argument.sin(),
        Cis => argument.cos() + imag!(1f64) * argument.sin(),
        Cosine => argument.cos(),
        Exponent => argument.exp(),
        SquareRoot => argument.sqrt(),
    }
}

impl Expression {
    /// Consume the expression, simplifying it as much as possible using the values provided in the environment.
    pub fn simplify(self) -> Self {
        use Expression::*;

        let simplified = match self {
            FunctionCall {
                function,
                expression,
            } => FunctionCall {
                function,
                expression: Box::new(expression.simplify()),
            },
            Infix {
                left,
                operator,
                right,
            } => Infix {
                left: Box::new(left.simplify()),
                operator,
                right: Box::new(right.simplify()),
            },
            Prefix {
                operator,
                expression,
            } => {
                use PrefixOperator::*;
                match (&operator, expression) {
                    (Minus, expr) => Prefix {
                        operator,
                        expression: Box::new(expr.simplify()),
                    },
                    (Plus, expr) => expr.simplify(),
                }
            }
            Variable(identifier) => Variable(identifier),
            Address(memory_reference) => Address(memory_reference),
            PiConstant => PiConstant,
            Number(number) => Number(number),
        };
        if let Ok(number) = simplified.evaluate(&HashMap::new(), &HashMap::new()) {
            Number(number)
        } else {
            simplified
        }
    }

    /// Evaluate an expression, expecting that it may be fully reduced to a single complex number.
    /// If it cannot be reduced to a complex number, return an error.
    pub fn evaluate(
        &self,
        variables: &HashMap<String, num_complex::Complex64>,
        memory_references: &HashMap<&str, Vec<f64>>,
    ) -> Result<num_complex::Complex64, EvaluationError> {
        use Expression::*;

        match self {
            FunctionCall {
                function,
                expression,
            } => {
                let evaluated = expression.evaluate(variables, memory_references)?;
                Ok(calculate_function(function, &evaluated))
            }
            Infix {
                left,
                operator,
                right,
            } => {
                let left_evaluated = left.evaluate(variables, memory_references)?;
                let right_evaluated = right.evaluate(variables, memory_references)?;
                Ok(calculate_infix(&left_evaluated, operator, &right_evaluated))
            }
            Prefix {
                operator,
                expression,
            } => {
                use PrefixOperator::*;
                let value = expression.evaluate(variables, memory_references)?;
                if matches!(operator, Minus) {
                    Ok(-value)
                } else {
                    Ok(value)
                }
            }
            Variable(identifier) => match variables.get(identifier.as_str()) {
                Some(value) => Ok(*value),
                None => Err(EvaluationError::Incomplete),
            },
            Address(memory_reference) => memory_references
                .get(memory_reference.name.as_str())
                .and_then(|values| {
                    let value = values.get(memory_reference.index as usize)?;
                    Some(real!(*value))
                })
                .ok_or(EvaluationError::Incomplete),
            PiConstant => Ok(real!(PI)),
            Number(number) => Ok(*number),
        }
    }
}

impl<'a> FromStr for Expression {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let tokens = lex(s);
        let (extra, expression) =
            parse_expression(&tokens).map_err(|_| String::from("Failed to parse expression"))?;
        if extra.is_empty() {
            Ok(expression)
        } else {
            Err(format!(
                "Parsed valid expression {} but found {} extra tokens",
                expression,
                extra.len(),
            ))
        }
    }
}

/// Format a num_complex::Complex64 value in a way that omits the real or imaginary part when
/// reasonable. That is:
///
/// - When imaginary is set but real is 0, show only imaginary
/// - When imaginary is 0, show real only
/// - When both are non-zero, show with the correct operator in between
macro_rules! format_complex {
    ($value:expr) => {{
        let mut operator = String::new();
        let mut imaginary_component = String::new();

        if $value.im > 0f64 {
            operator = "+".to_owned();
            imaginary_component = format!("{:.}i", $value.im)
        } else if $value.im < 0f64 {
            imaginary_component = format!("-{:.}i", $value.im)
        }

        if imaginary_component == "" {
            format!("{:.}", $value.re)
        } else if $value.re == 0f64 {
            format!("{}", imaginary_component)
        } else {
            format!("{:.}{}{}", $value.re, operator, imaginary_component)
        }
    }};
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expression::*;
        match self {
            Address(memory_reference) => write!(f, "{}", memory_reference),
            FunctionCall {
                function,
                expression,
            } => write!(f, "{}({})", function, expression),
            Infix {
                left,
                operator,
                right,
            } => write!(f, "({}{}{})", left, operator, right),
            Number(value) => write!(f, "{}", format_complex!(value)),
            PiConstant => write!(f, "pi"),
            Prefix {
                operator,
                expression,
            } => write!(f, "({}{})", operator, expression),
            Variable(identifier) => write!(f, "%{}", identifier),
        }
    }
}

/// A function defined within Quil syntax.
#[derive(Clone, Debug, PartialEq)]
pub enum ExpressionFunction {
    Cis,
    Cosine,
    Exponent,
    Sine,
    SquareRoot,
}

impl fmt::Display for ExpressionFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ExpressionFunction::*;
        write!(
            f,
            "{}",
            match self {
                Cis => "cis",
                Cosine => "cos",
                Exponent => "exp",
                Sine => "sin",
                SquareRoot => "sqrt",
            }
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum PrefixOperator {
    Plus,
    Minus,
}

impl fmt::Display for PrefixOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use PrefixOperator::*;
        write!(
            f,
            "{}",
            match self {
                Plus => "+",
                Minus => "-",
            }
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum InfixOperator {
    Caret,
    Plus,
    Minus,
    Slash,
    Star,
}

impl fmt::Display for InfixOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use InfixOperator::*;
        write!(
            f,
            "{}",
            match self {
                Caret => "^",
                Plus => "+",
                Minus => "-",
                Slash => "/",
                Star => "*",
            }
        )
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, f64::consts::PI};

    use num_complex::Complex64;

    use crate::{
        expression::{EvaluationError, Expression, ExpressionFunction},
        real,
    };

    use super::*;

    #[test]
    fn simplify_and_evaluate() {
        use Expression::*;

        let one = real!(1.0);
        let empty_variables = HashMap::new();

        let mut variables = HashMap::new();
        variables.insert("foo".to_owned(), real!(10f64));
        variables.insert("bar".to_owned(), real!(100f64));

        let empty_memory = HashMap::new();

        let mut memory_references = HashMap::new();
        memory_references.insert("theta", vec![1.0, 2.0]);
        memory_references.insert("beta", vec![3.0, 4.0]);

        struct TestCase<'a> {
            expression: Expression,
            variables: &'a HashMap<String, Complex64>,
            memory_references: &'a HashMap<&'a str, Vec<f64>>,
            simplified: Expression,
            evaluated: Result<Complex64, EvaluationError>,
        }

        let cases: Vec<TestCase> = vec![
            TestCase {
                expression: Number(one),
                variables: &empty_variables,
                memory_references: &empty_memory,
                simplified: Number(one),
                evaluated: Ok(one),
            },
            TestCase {
                expression: Expression::Prefix {
                    operator: PrefixOperator::Minus,
                    expression: Box::new(Number(real!(1f64))),
                },
                variables: &empty_variables,
                memory_references: &empty_memory,
                simplified: Number(real!(-1f64)),
                evaluated: Ok(real!(-1f64)),
            },
            TestCase {
                expression: Expression::Variable("foo".to_owned()),
                variables: &variables,
                memory_references: &empty_memory,
                simplified: Expression::Variable("foo".to_owned()),
                evaluated: Ok(real!(10f64)),
            },
            TestCase {
                expression: Expression::from_str("%foo + %bar").unwrap(),
                variables: &variables,
                memory_references: &empty_memory,
                simplified: Expression::from_str("%foo + %bar").unwrap(),
                evaluated: Ok(real!(110f64)),
            },
            TestCase {
                expression: Expression::FunctionCall {
                    function: ExpressionFunction::Sine,
                    expression: Box::new(Expression::Number(real!(PI / 2f64))),
                },
                variables: &variables,
                memory_references: &empty_memory,
                simplified: Number(real!(1f64)),
                evaluated: Ok(real!(1f64)),
            },
            TestCase {
                expression: Expression::from_str("theta[1] * beta[0]").unwrap(),
                variables: &empty_variables,
                memory_references: &memory_references,
                simplified: Expression::from_str("theta[1] * beta[0]").unwrap(),
                evaluated: Ok(real!(6.0)),
            },
        ];

        for case in cases {
            let evaluated = case
                .expression
                .evaluate(case.variables, case.memory_references);
            assert_eq!(evaluated, case.evaluated);

            let simplified = case.expression.simplify();
            assert_eq!(simplified, case.simplified);
        }
    }
}
