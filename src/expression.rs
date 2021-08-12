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
use crate::{imag, instruction::MemoryReference, real};
use std::collections::HashMap;
use std::f64::consts::PI;
use std::fmt;
use std::hash::{Hash, Hasher};

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

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for Expression {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_string().hash(state);
    }
}

impl Eq for Expression {}

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

pub type EvaluationEnvironment = HashMap<String, num_complex::Complex64>;

impl Expression {
    /// Consume the expression, simplifying it as much as possible using the values provided in the environment.
    /// If variables are used in the expression which are not present in the environment, evaluation stops there,
    /// returning the possibly-simplified expression.
    pub fn evaluate(self, environment: &EvaluationEnvironment) -> Self {
        use Expression::*;
        match self {
            FunctionCall {
                function,
                expression,
            } => {
                let evaluated = (*expression).evaluate(environment);
                match &evaluated {
                    Number(value) => Number(calculate_function(&function, value)),
                    PiConstant => Number(calculate_function(&function, &real!(PI))),
                    _ => FunctionCall {
                        function,
                        expression: Box::new(evaluated),
                    },
                }
            }
            Infix {
                left,
                operator,
                right,
            } => {
                let left_evaluated = (*left).evaluate(environment);
                let right_evaluated = (*right).evaluate(environment);

                match (&left_evaluated, &right_evaluated) {
                    (Number(value_left), Number(value_right)) => {
                        Number(calculate_infix(value_left, &operator, value_right))
                    }
                    (PiConstant, Number(value)) => {
                        Number(calculate_infix(&real!(PI), &operator, value))
                    }
                    (Number(value), PiConstant) => {
                        Number(calculate_infix(value, &operator, &real!(PI)))
                    }
                    _ => Infix {
                        left: Box::new(left_evaluated),
                        operator,
                        right: Box::new(right_evaluated),
                    },
                }
            }
            Prefix {
                operator,
                expression,
            } => {
                use PrefixOperator::*;
                let prefixed_expression = *expression;
                match (&operator, prefixed_expression) {
                    (Minus, Number(value)) => Number(-value),
                    (Minus, PiConstant) => Number(real!(-PI)),
                    (Minus, expr) => Prefix {
                        operator,
                        expression: Box::new(expr),
                    },
                    (Plus, expr) => expr,
                }
            }
            Variable(identifier) => match environment.get(&identifier) {
                Some(value) => Number(*value),
                None => Variable(identifier),
            },
            _ => self,
        }
    }

    /// Evaluate an expression, expecting that it may be fully reduced to a single complex number.
    /// If it cannot be reduced to a complex number, return an error.
    pub fn evaluate_to_complex(
        self,
        environment: &EvaluationEnvironment,
    ) -> Result<num_complex::Complex64, EvaluationError> {
        use Expression::*;

        let result = self.evaluate(environment);
        match result {
            Number(value) => Ok(value),
            PiConstant => Ok(real!(PI)),
            _ => Err(EvaluationError::Incomplete),
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
    use super::PrefixOperator;
    use crate::{
        expression::{EvaluationError, Expression, ExpressionFunction, InfixOperator},
        real,
    };
    use num_complex::Complex64;
    use std::collections::HashSet;
    use std::{collections::HashMap, f64::consts::PI};

    #[test]
    fn evaluate() {
        use Expression::*;

        let one = Complex64::new(1f64, 0f64);
        let empty_environment = HashMap::new();

        let mut environment = HashMap::new();
        environment.insert("foo".to_owned(), real!(10f64));
        environment.insert("bar".to_owned(), real!(100f64));

        struct TestCase<'a> {
            expression: Expression,
            environment: &'a HashMap<String, Complex64>,
            evaluated_expression: Expression,
            evaluated_complex: Result<Complex64, EvaluationError>,
        }

        let cases: Vec<TestCase> = vec![
            TestCase {
                expression: Number(one),
                environment: &empty_environment,
                evaluated_expression: Number(one),
                evaluated_complex: Ok(one),
            },
            TestCase {
                expression: Expression::Prefix {
                    operator: PrefixOperator::Minus,
                    expression: Box::new(Number(real!(1f64))),
                },
                environment: &empty_environment,
                evaluated_expression: Number(real!(-1f64)),
                evaluated_complex: Ok(real!(-1f64)),
            },
            TestCase {
                expression: Expression::Variable("foo".to_owned()),
                environment: &environment,
                evaluated_expression: Number(real!(10f64)),
                evaluated_complex: Ok(real!(10f64)),
            },
            TestCase {
                expression: Expression::Infix {
                    left: Box::new(Expression::Variable("foo".to_owned())),
                    operator: InfixOperator::Plus,
                    right: Box::new(Expression::Variable("bar".to_owned())),
                },
                environment: &environment,
                evaluated_expression: Number(real!(110f64)),
                evaluated_complex: Ok(real!(110f64)),
            },
            TestCase {
                expression: Expression::FunctionCall {
                    function: ExpressionFunction::Sine,
                    expression: Box::new(Expression::Number(real!(PI / 2f64))),
                },
                environment: &environment,
                evaluated_expression: Number(real!(1f64)),
                evaluated_complex: Ok(real!(1f64)),
            },
        ];

        for case in cases {
            let evaluated = case.expression.evaluate(&case.environment);
            assert_eq!(evaluated, case.evaluated_expression);

            let evaluated_complex = evaluated.evaluate_to_complex(&case.environment);
            assert_eq!(evaluated_complex, case.evaluated_complex)
        }
    }

    #[test]
    fn hash() {
        let first = Expression::Infix {
            left: Box::new(Expression::Number(real!(1.0))),
            operator: InfixOperator::Plus,
            right: Box::new(Expression::Number(real!(2.0))),
        };
        let matching = first.clone();
        let differing = Expression::Number(real!(3.0));

        let mut set = HashSet::new();
        set.insert(first);

        assert!(set.contains(&matching));
        assert!(!set.contains(&differing))
    }

    #[test]
    #[should_panic]
    /// Not implemented yet, see https://github.com/rigetti/quil-rust/issues/27
    fn hash_commutative() {
        let first = Expression::Infix {
            left: Box::new(Expression::Number(real!(1.0))),
            operator: InfixOperator::Plus,
            right: Box::new(Expression::Number(real!(2.0))),
        };
        let second = Expression::Infix {
            left: Box::new(Expression::Number(real!(2.0))),
            operator: InfixOperator::Plus,
            right: Box::new(Expression::Number(real!(1.0))),
        };

        let mut set = HashSet::new();
        set.insert(first);

        assert!(set.contains(&second));
    }
}
