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
use std::collections::{hash_map::DefaultHasher, HashMap};
use std::f64::consts::PI;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::str::FromStr;

#[cfg(test)]
use proptest_derive::Arbitrary;

use crate::parser::{lex, parse_expression};
use crate::{imag, instruction::MemoryReference, real};

/// The different possible types of errors that could occur during expression evaluation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EvaluationError {
    /// There wasn't enough information to completely evaluate an expression.
    Incomplete,
    /// An operation expected a real number but received a complex one.
    NumberNotReal,
    /// An operation expected a number but received a different type of expression.
    NotANumber,
}

#[derive(Clone, Debug)]
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

/// Hash value helper: turn a hashable thing into a u64.
fn hash_to_u64<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

impl Hash for Expression {
    // Implemented by hand since we can't derive with f64s hidden inside.
    // Also to understand when things should be the same, like with commutativity (`1 + 2 == 2 + 1`).
    // See https://github.com/rigetti/quil-rust/issues/27
    fn hash<H: Hasher>(&self, state: &mut H) {
        use std::cmp::{max_by_key, min_by_key};
        use Expression::*;
        match self {
            Address(m) => {
                "Address".hash(state);
                m.hash(state);
            }
            FunctionCall {
                function,
                expression,
            } => {
                "FunctionCall".hash(state);
                function.hash(state);
                expression.hash(state);
            }
            Infix {
                left,
                operator,
                right,
            } => {
                "Infix".hash(state);
                operator.hash(state);
                match operator {
                    InfixOperator::Plus | InfixOperator::Star => {
                        // commutative, so put left & right in decreasing order by hash value
                        let (a, b) = (
                            min_by_key(left, right, hash_to_u64),
                            max_by_key(left, right, hash_to_u64),
                        );
                        a.hash(state);
                        b.hash(state);
                    }
                    _ => {
                        left.hash(state);
                        right.hash(state);
                    }
                }
            }
            Number(n) => {
                "Number".hash(state);
                // Skip zero values (akin to `format_complex`).
                // Also, since f64 isn't hashable, use the u64 binary representation.
                // The docs claim this is rather portable: https://doc.rust-lang.org/std/primitive.f64.html#method.to_bits
                if n.re.abs() > 0f64 {
                    n.re.to_bits().hash(state)
                }
                if n.im.abs() > 0f64 {
                    n.im.to_bits().hash(state)
                }
            }
            PiConstant => {
                "PiConstant".hash(state);
            }
            Prefix {
                operator,
                expression,
            } => {
                "Prefix".hash(state);
                operator.hash(state);
                expression.hash(state);
            }
            Variable(v) => {
                "Variable".hash(state);
                v.hash(state);
            }
        }
    }
}

impl PartialEq for Expression {
    // Partial equality by hash value
    fn eq(&self, other: &Self) -> bool {
        hash_to_u64(self) == hash_to_u64(other)
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

/// Is this a small floating point number?
#[inline(always)]
fn is_small(x: f64) -> bool {
    x.abs() < 1e-16
}

impl Expression {
    /// Consume the expression, simplifying it as much as possible.
    ///
    /// # Example
    ///
    /// ```rust
    /// use quil_rs::expression::Expression;
    /// use std::str::FromStr;
    /// use num_complex::Complex64;
    ///
    /// let expression = Expression::from_str("cos(2 * pi) + 2").unwrap().simplify();
    ///
    /// assert_eq!(expression, Expression::Number(Complex64::from(3.0)));
    /// ```
    pub fn simplify(self) -> Self {
        use Expression::*;

        let simplified = match self {
            FunctionCall {
                function,
                expression,
            } => {
                let simplified = expression.simplify();
                if let Number(number) = simplified {
                    Number(calculate_function(&function, &number))
                } else {
                    FunctionCall {
                        function,
                        expression: Box::new(simplified),
                    }
                }
            }
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
    ///
    /// # Example
    ///
    /// ```rust
    /// use quil_rs::expression::Expression;
    /// use std::str::FromStr;
    /// use std::collections::HashMap;
    /// use num_complex::Complex64;
    ///
    /// let expression = Expression::from_str("%beta + theta[0]").unwrap();
    ///
    /// let mut variables = HashMap::with_capacity(1);
    /// variables.insert(String::from("beta"), Complex64::from(1.0));
    ///
    /// let mut memory_references = HashMap::with_capacity(1);
    /// memory_references.insert("theta", vec![2.0]);
    ///
    /// let evaluated = expression.evaluate(&variables, &memory_references).unwrap();
    ///
    /// assert_eq!(evaluated, Complex64::from(3.0))
    /// ```
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

    /// Substitute an expression in the place of each matching variable.
    /// Consumes the expression and returns a new one.
    ///
    /// # Example
    ///
    /// ```rust
    /// use quil_rs::expression::Expression;
    /// use std::str::FromStr;
    /// use std::collections::HashMap;
    /// use num_complex::Complex64;
    ///
    /// let expression = Expression::from_str("%x + %y").unwrap();
    ///
    /// let mut variables = HashMap::with_capacity(1);
    /// variables.insert(String::from("x"), Expression::Number(Complex64::from(1.0)));
    ///
    /// let evaluated = expression.substitute_variables(&variables);
    ///
    /// assert_eq!(evaluated, Expression::from_str("1.0 + %y").unwrap())
    /// ```
    pub fn substitute_variables(self, variable_values: &HashMap<String, Expression>) -> Self {
        use Expression::*;

        match self {
            FunctionCall {
                function,
                expression,
            } => FunctionCall {
                function,
                expression: expression.substitute_variables(variable_values).into(),
            },
            Infix {
                left,
                operator,
                right,
            } => {
                let left = left.substitute_variables(variable_values).into();
                let right = right.substitute_variables(variable_values).into();
                Infix {
                    left,
                    operator,
                    right,
                }
            }
            Prefix {
                operator,
                expression,
            } => Prefix {
                operator,
                expression: expression.substitute_variables(variable_values).into(),
            },
            Variable(identifier) => match variable_values.get(identifier.as_str()) {
                Some(value) => value.clone(),
                None => Variable(identifier),
            },
            other => other,
        }
    }

    /// If this is a number with imaginary part "equal to" zero (of _small_ absolute value), return
    /// that number. Otherwise, error with an evaluation error of a descriptive type.
    pub fn to_real(self) -> Result<f64, EvaluationError> {
        match self {
            Expression::PiConstant => Ok(PI),
            Expression::Number(x) if is_small(x.im) => Ok(x.re),
            Expression::Number(_) => Err(EvaluationError::NumberNotReal),
            _ => Err(EvaluationError::NotANumber),
        }
    }
}

impl<'a> FromStr for Expression {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let tokens = lex(s)?;
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
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(Arbitrary))]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(Arbitrary))]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(Arbitrary))]
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
    use std::collections::HashSet;

    use num_complex::Complex64;
    use proptest::prelude::*;

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

    /// Generate an arbitrary Expression for a property test.
    /// See https://docs.rs/proptest/1.0.0/proptest/prelude/trait.Strategy.html#method.prop_recursive
    fn arb_expr() -> impl Strategy<Value = Expression> {
        use Expression::*;
        let leaf = prop_oneof![
            any::<MemoryReference>().prop_map(Address),
            (any::<f64>(), any::<f64>())
                .prop_map(|(re, im)| Number(num_complex::Complex64::new(re, im))),
            Just(PiConstant),
            ".*".prop_map(Variable),
        ];
        (leaf).prop_recursive(
            4,  // No more than 4 branch levels deep
            64, // Target around 64 total nodes
            2,  // Each "collection" is up to 2 elements
            |expr| {
                prop_oneof![
                    (any::<ExpressionFunction>(), expr.clone()).prop_map(|(function, e)| {
                        FunctionCall {
                            function,
                            expression: Box::new(e),
                        }
                    }),
                    (expr.clone(), any::<InfixOperator>(), expr.clone()).prop_map(
                        |(l, operator, r)| Infix {
                            left: Box::new(l),
                            operator,
                            right: Box::new(r)
                        }
                    ),
                    (any::<PrefixOperator>(), expr).prop_map(|(operator, e)| Prefix {
                        operator,
                        expression: Box::new(e)
                    })
                ]
            },
        )
    }

    proptest! {

        #[test]
        fn eq(a in any::<f64>(), b in any::<f64>()) {
            let first = Expression::Infix {
                left: Box::new(Expression::Number(real!(a))),
                operator: InfixOperator::Plus,
                right: Box::new(Expression::Number(real!(b))),
            };
            let matching = first.clone();
            let differing = Expression::Number(real!(a + b));
            prop_assert_eq!(&first, &matching);
            prop_assert_ne!(&first, &differing);
        }

        #[test]
        fn eq_commutative(a in any::<f64>(), b in any::<f64>()) {
            let first = Expression::Infix{
                left: Box::new(Expression::Number(real!(a))),
                operator: InfixOperator::Plus,
                right: Box::new(Expression::Number(real!(b))),
            };
            let second = Expression::Infix{
                left: Box::new(Expression::Number(real!(b))),
                operator: InfixOperator::Plus,
                right: Box::new(Expression::Number(real!(a))),
            };
            prop_assert_eq!(first, second);
        }

        #[test]
        fn hash(a in any::<f64>(), b in any::<f64>()) {
            let first = Expression::Infix {
                left: Box::new(Expression::Number(real!(a))),
                operator: InfixOperator::Plus,
                right: Box::new(Expression::Number(real!(b))),
            };
            let matching = first.clone();
            let differing = Expression::Number(real!(a + b));
            let mut set = HashSet::new();
            set.insert(first);
            assert!(set.contains(&matching));
            assert!(!set.contains(&differing))
        }

        #[test]
        fn hash_commutative(a in any::<f64>(), b in any::<f64>()) {
            let first = Expression::Infix{
                left: Box::new(Expression::Number(real!(a))),
                operator: InfixOperator::Plus,
                right: Box::new(Expression::Number(real!(b))),
            };
            let second = Expression::Infix{
                left: Box::new(Expression::Number(real!(b))),
                operator: InfixOperator::Plus,
                right: Box::new(Expression::Number(real!(a))),
            };
            let mut set = HashSet::new();
            set.insert(first);
            assert!(set.contains(&second));
        }

        #[test]
        fn eq_iff_hash_eq(x in arb_expr(), y in arb_expr()) {
            let h_x = {
                let mut s = DefaultHasher::new();
                x.hash(&mut s);
                s.finish()
            };
            let h_y = {
                let mut s = DefaultHasher::new();
                y.hash(&mut s);
                s.finish()
            };
            prop_assert_eq!(x == y, h_x == h_y);
        }

        #[test]
        fn reals_are_real(x in any::<f64>()) {
            prop_assert_eq!(Expression::Number(real!(x)).to_real(), Ok(x))
        }

        #[test]
        fn some_nums_are_real(re in any::<f64>(), im in any::<f64>()) {
            let result = Expression::Number(Complex64{re, im}).to_real();
            if is_small(im) {
                prop_assert_eq!(result, Ok(re))
            } else {
                prop_assert_eq!(result, Err(EvaluationError::NumberNotReal))
            }
        }

        #[test]
        fn no_other_exps_are_real(expr in arb_expr().prop_filter("Not numbers", |e| match e {
            Expression::Number(_) | Expression::PiConstant => false,
            _ => true,
        }
            )) {
            prop_assert_eq!(expr.to_real(), Err(EvaluationError::NotANumber))
        }

    }

    #[test]
    fn specific_to_real_tests() {
        for (input, expected) in vec![
            (Expression::PiConstant, Ok(PI)),
            (Expression::Number(Complex64 { re: 1.0, im: 0.0 }), Ok(1.0)),
            (
                Expression::Number(Complex64 { re: 1.0, im: 1.0 }),
                Err(EvaluationError::NumberNotReal),
            ),
            (
                Expression::Variable("Not a number".into()),
                Err(EvaluationError::NotANumber),
            ),
        ] {
            assert_eq!(input.to_real(), expected)
        }
    }
}
