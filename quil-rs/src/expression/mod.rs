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

use crate::{
    hash::{hash_f64, hash_to_u64},
    parser::{lex, parse_expression, ParseError},
    program::{disallow_leftover, ParseProgramError},
    {imag, instruction::MemoryReference, real},
};
use lexical::{format, to_string_with_options, WriteFloatOptions};
use nom_locate::LocatedSpan;
use num_complex::Complex64;
use once_cell::sync::Lazy;
use std::{
    collections::HashMap,
    f64::consts::PI,
    fmt,
    hash::{Hash, Hasher},
    num::NonZeroI32,
    ops::{Add, AddAssign, BitXor, BitXorAssign, Div, DivAssign, Mul, MulAssign, Sub, SubAssign},
    str::FromStr,
};

#[cfg(test)]
use proptest_derive::Arbitrary;

mod simplification;
mod simplification_2;

/// The different possible types of errors that could occur during expression evaluation.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum EvaluationError {
    #[error("There wasn't enough information to completely evaluate the expression.")]
    Incomplete,
    #[error("The operation expected a real number but received a complex one.")]
    NumberNotReal,
    #[error("The operation expected a number but received a different type of expression.")]
    NotANumber,
}

#[derive(Clone, Debug)]
pub enum Expression {
    Address(MemoryReference),
    FunctionCall(FunctionCallExpression),
    Infix(InfixExpression),
    Number(num_complex::Complex64),
    PiConstant,
    Prefix(PrefixExpression),
    Variable(String),
}

#[derive(Clone, Debug)]
pub struct FunctionCallExpression {
    pub function: ExpressionFunction,
    pub expression: Box<Expression>,
}

impl FunctionCallExpression {
    pub fn new(function: ExpressionFunction, expression: Box<Expression>) -> Self {
        Self {
            function,
            expression,
        }
    }
}

#[derive(Clone, Debug)]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub operator: InfixOperator,
    pub right: Box<Expression>,
}

impl InfixExpression {
    pub fn new(left: Box<Expression>, operator: InfixOperator, right: Box<Expression>) -> Self {
        Self {
            left,
            operator,
            right,
        }
    }
}

#[derive(Clone, Debug)]
pub struct PrefixExpression {
    pub operator: PrefixOperator,
    pub expression: Box<Expression>,
}

impl PrefixExpression {
    pub fn new(operator: PrefixOperator, expression: Box<Expression>) -> Self {
        Self {
            operator,
            expression,
        }
    }
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
            FunctionCall(FunctionCallExpression {
                function,
                expression,
            }) => {
                "FunctionCall".hash(state);
                function.hash(state);
                expression.hash(state);
            }
            Infix(InfixExpression {
                left,
                operator,
                right,
            }) => {
                "Infix".hash(state);
                operator.hash(state);
                match operator {
                    InfixOperator::Plus | InfixOperator::Star => {
                        // commutative, so put left & right in decreasing order by hash value
                        let (a, b) = (
                            min_by_key(&left, &right, hash_to_u64),
                            max_by_key(&left, &right, hash_to_u64),
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
                    hash_f64(n.re, state)
                }
                if n.im.abs() > 0f64 {
                    hash_f64(n.im, state)
                }
            }
            PiConstant => {
                "PiConstant".hash(state);
            }
            Prefix(p) => {
                "Prefix".hash(state);
                p.operator.hash(state);
                p.expression.hash(state);
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

macro_rules! impl_expr_op {
    ($name:ident, $name_assign:ident, $function:ident, $function_assign:ident, $operator:ident) => {
        impl $name for Expression {
            type Output = Self;
            fn $function(self, other: Self) -> Self {
                Expression::Infix(InfixExpression {
                    left: Box::new(self),
                    operator: InfixOperator::$operator,
                    right: Box::new(other),
                })
            }
        }
        impl $name_assign for Expression {
            fn $function_assign(&mut self, other: Self) {
                // Move out of self to avoid potentially cloning a large value
                let temp = ::std::mem::replace(self, Self::PiConstant);
                *self = temp.$function(other);
            }
        }
    };
}

impl_expr_op!(BitXor, BitXorAssign, bitxor, bitxor_assign, Caret);
impl_expr_op!(Add, AddAssign, add, add_assign, Plus);
impl_expr_op!(Sub, SubAssign, sub, sub_assign, Minus);
impl_expr_op!(Mul, MulAssign, mul, mul_assign, Star);
impl_expr_op!(Div, DivAssign, div, div_assign, Slash);

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
    /// Simplify the expression as much as possible, in-place.
    ///
    /// # Example
    ///
    /// ```rust
    /// use quil_rs::expression::Expression;
    /// use std::str::FromStr;
    /// use num_complex::Complex64;
    ///
    /// let mut expression = Expression::from_str("cos(2 * pi) + 2").unwrap();
    /// expression.simplify();
    ///
    /// assert_eq!(expression, Expression::Number(Complex64::from(3.0)));
    /// ```
    pub fn simplify(&mut self) {
        match self {
            Expression::Address(_) | Expression::Number(_) | Expression::Variable(_) => {}
            Expression::PiConstant => {
                *self = Expression::Number(Complex64::from(PI));
            }
            _ => {
                if let Ok(simpler) = simplification::run(self) {
                    *self = simpler;
                }
            }
        }
    }

    /// Consume the expression, simplifying it as much as possible.
    ///
    /// # Example
    ///
    /// ```rust
    /// use quil_rs::expression::Expression;
    /// use std::str::FromStr;
    /// use num_complex::Complex64;
    ///
    /// let simplified = Expression::from_str("cos(2 * pi) + 2").unwrap().into_simplified();
    ///
    /// assert_eq!(simplified, Expression::Number(Complex64::from(3.0)));
    /// ```
    pub fn into_simplified(mut self) -> Self {
        self.simplify();
        self
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
            FunctionCall(FunctionCallExpression {
                function,
                expression,
            }) => {
                let evaluated = expression.evaluate(variables, memory_references)?;
                Ok(calculate_function(function, &evaluated))
            }
            Infix(InfixExpression {
                left,
                operator,
                right,
            }) => {
                let left_evaluated = left.evaluate(variables, memory_references)?;
                let right_evaluated = right.evaluate(variables, memory_references)?;
                Ok(calculate_infix(&left_evaluated, operator, &right_evaluated))
            }
            Prefix(PrefixExpression {
                operator,
                expression,
            }) => {
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
            FunctionCall(FunctionCallExpression {
                function,
                expression,
            }) => Expression::FunctionCall(FunctionCallExpression {
                function,
                expression: expression.substitute_variables(variable_values).into(),
            }),
            Infix(InfixExpression {
                left,
                operator,
                right,
            }) => {
                let left = left.substitute_variables(variable_values).into();
                let right = right.substitute_variables(variable_values).into();
                Infix(InfixExpression {
                    left,
                    operator,
                    right,
                })
            }
            Prefix(PrefixExpression {
                operator,
                expression,
            }) => Prefix(PrefixExpression {
                operator,
                expression: expression.substitute_variables(variable_values).into(),
            }),
            Variable(identifier) => match variable_values.get(identifier.as_str()) {
                Some(value) => value.clone(),
                None => Variable(identifier),
            },
            other => other,
        }
    }

    /// If this is a number with imaginary part "equal to" zero (of _small_ absolute value), return
    /// that number. Otherwise, error with an evaluation error of a descriptive type.
    pub fn to_real(&self) -> Result<f64, EvaluationError> {
        match self {
            Expression::PiConstant => Ok(PI),
            Expression::Number(x) if is_small(x.im) => Ok(x.re),
            Expression::Number(_) => Err(EvaluationError::NumberNotReal),
            _ => Err(EvaluationError::NotANumber),
        }
    }
}

impl FromStr for Expression {
    type Err = ParseProgramError<Self>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let input = LocatedSpan::new(s);
        let tokens = lex(input)?;
        disallow_leftover(parse_expression(&tokens).map_err(ParseError::from_nom_internal_err))
    }
}

static FORMAT_REAL_OPTIONS: Lazy<WriteFloatOptions> = Lazy::new(|| {
    WriteFloatOptions::builder()
        .negative_exponent_break(NonZeroI32::new(-5))
        .positive_exponent_break(NonZeroI32::new(15))
        .trim_floats(true)
        .build()
        .expect("options are valid")
});

static FORMAT_IMAGINARY_OPTIONS: Lazy<WriteFloatOptions> = Lazy::new(|| {
    WriteFloatOptions::builder()
        .negative_exponent_break(NonZeroI32::new(-5))
        .positive_exponent_break(NonZeroI32::new(15))
        .trim_floats(false) // Per the quil spec, the imaginary part of a complex number is always a floating point number
        .build()
        .expect("options are valid")
});

/// Format a num_complex::Complex64 value in a way that omits the real or imaginary part when
/// reasonable. That is:
///
/// - When imaginary is set but real is 0, show only imaginary
/// - When imaginary is 0, show real only
/// - When both are non-zero, show with the correct operator in between
#[inline(always)]
fn format_complex(value: &Complex64) -> String {
    const FORMAT: u128 = format::STANDARD;
    if value.re == 0f64 && value.im == 0f64 {
        "0".to_owned()
    } else if value.im == 0f64 {
        to_string_with_options::<_, FORMAT>(value.re, &FORMAT_REAL_OPTIONS)
    } else if value.re == 0f64 {
        to_string_with_options::<_, FORMAT>(value.im, &FORMAT_IMAGINARY_OPTIONS) + "i"
    } else {
        let mut out = to_string_with_options::<_, FORMAT>(value.re, &FORMAT_REAL_OPTIONS);
        if value.im > 0f64 {
            out.push('+')
        }
        out.push_str(&to_string_with_options::<_, FORMAT>(
            value.im,
            &FORMAT_IMAGINARY_OPTIONS,
        ));
        out.push('i');
        out
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expression::*;
        match self {
            Address(memory_reference) => write!(f, "{memory_reference}"),
            FunctionCall(FunctionCallExpression {
                function,
                expression,
            }) => write!(f, "{function}({expression})"),
            Infix(InfixExpression {
                left,
                operator,
                right,
            }) => {
                format_inner_expression(f, left)?;
                write!(f, "{}", operator)?;
                format_inner_expression(f, right)
            }
            Number(value) => write!(f, "{}", format_complex(value)),
            PiConstant => write!(f, "pi"),
            Prefix(PrefixExpression {
                operator,
                expression,
            }) => {
                write!(f, "{}", operator)?;
                format_inner_expression(f, expression)
            }
            Variable(identifier) => write!(f, "%{}", identifier),
        }
    }
}

/// Utility function to wrap infix expressions that are part of an expression in parentheses, so
/// that correct precedence rules are enforced.
fn format_inner_expression(f: &mut fmt::Formatter, expression: &Expression) -> fmt::Result {
    match expression {
        Expression::Infix(InfixExpression {
            left,
            operator,
            right,
        }) => {
            write!(f, "(")?;
            format_inner_expression(f, left)?;
            write!(f, "{operator}")?;
            format_inner_expression(f, right)?;
            write!(f, ")")
        }
        _ => write!(f, "{expression}"),
    }
}

#[cfg(test)]
mod test {
    use crate::{
        expression::{
            Expression, InfixExpression, InfixOperator, PrefixExpression, PrefixOperator,
        },
        real,
    };

    #[test]
    fn formats_nested_expression() {
        let expression = Expression::Infix(InfixExpression {
            left: Box::new(Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                expression: Box::new(Expression::Number(real!(3f64))),
            })),
            operator: InfixOperator::Star,
            right: Box::new(Expression::Infix(InfixExpression {
                left: Box::new(Expression::PiConstant),
                operator: InfixOperator::Slash,
                right: Box::new(Expression::Number(real!(2f64))),
            })),
        });

        assert_eq!(expression.to_string(), "-3*(pi/2)");
    }
}

/// A function defined within Quil syntax.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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
                // NOTE: prefix Plus does nothing but cause parsing issues
                Plus => "",
                Minus => "-",
            }
        )
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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
                // NOTE: spaces included to distinguish from hyphenated identifiers
                Minus => " - ",
                Slash => "/",
                Star => "*",
            }
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{hash_map::DefaultHasher, HashSet};

    use num_complex::Complex64;
    use proptest::prelude::*;

    use crate::{
        expression::{EvaluationError, Expression, ExpressionFunction},
        real,
        reserved::ReservedToken,
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
                expression: Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    expression: Box::new(Number(real!(1f64))),
                }),
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
                expression: Expression::FunctionCall(FunctionCallExpression {
                    function: ExpressionFunction::Sine,
                    expression: Box::new(Expression::Number(real!(PI / 2f64))),
                }),
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

        for mut case in cases {
            let evaluated = case
                .expression
                .evaluate(case.variables, case.memory_references);
            assert_eq!(evaluated, case.evaluated);

            case.expression.simplify();
            assert_eq!(case.expression, case.simplified);
        }
    }

    /// Parenthesized version of [`Expression::to_string()`]
    fn parenthesized(expression: &Expression) -> String {
        use Expression::*;
        match expression {
            Address(memory_reference) => format!("({memory_reference})"),
            FunctionCall(FunctionCallExpression {
                function,
                expression,
            }) => format!("({function}({}))", parenthesized(expression)),
            Infix(InfixExpression {
                left,
                operator,
                right,
            }) => format!(
                "({}{}{})",
                parenthesized(left),
                operator,
                parenthesized(right)
            ),
            Number(value) => format!("({})", format_complex(value)),
            PiConstant => "pi".to_string(),
            Prefix(PrefixExpression {
                operator,
                expression,
            }) => format!("({}{})", operator, parenthesized(expression)),
            Variable(identifier) => format!("(%{identifier})"),
        }
    }

    // Better behaved than the auto-derived version for names
    fn arb_name() -> impl Strategy<Value = String> {
        r"[a-z][a-zA-Z0-9]{1,10}".prop_filter("Exclude reserved tokens", |t| {
            ReservedToken::from_str(t).is_err()
        })
    }

    // Better behaved than the auto-derived version re: names & indices
    fn arb_memory_reference() -> impl Strategy<Value = MemoryReference> {
        (arb_name(), (u64::MIN..u32::MAX as u64))
            .prop_map(|(name, index)| MemoryReference { name, index })
    }

    // Better behaved than the auto-derived version via arbitrary floats
    fn arb_complex64() -> impl Strategy<Value = Complex64> {
        let tau = std::f64::consts::TAU;
        ((-tau..tau), (-tau..tau)).prop_map(|(re, im)| Complex64 { re, im })
    }

    /// Generate an arbitrary Expression for a property test.
    /// See https://docs.rs/proptest/1.0.0/proptest/prelude/trait.Strategy.html#method.prop_recursive
    fn arb_expr() -> impl Strategy<Value = Expression> {
        use Expression::*;
        let leaf = prop_oneof![
            arb_memory_reference().prop_map(Address),
            arb_complex64().prop_map(Number),
            Just(PiConstant),
            arb_name().prop_map(Variable),
        ];
        (leaf).prop_recursive(
            4,  // No more than 4 branch levels deep
            64, // Target around 64 total nodes
            16, // Each "collection" is up to 16 elements
            |expr| {
                prop_oneof![
                    (any::<ExpressionFunction>(), expr.clone()).prop_map(|(function, e)| {
                        Expression::FunctionCall(FunctionCallExpression {
                            function,
                            expression: Box::new(e),
                        })
                    }),
                    (expr.clone(), any::<InfixOperator>(), expr.clone()).prop_map(
                        |(l, operator, r)| Infix(InfixExpression {
                            left: Box::new(l),
                            operator,
                            right: Box::new(r)
                        })
                    ),
                    (any::<PrefixOperator>(), expr).prop_map(|(operator, e)| Prefix(
                        PrefixExpression {
                            operator,
                            expression: Box::new(e)
                        }
                    ))
                ]
            },
        )
    }

    proptest! {

        #[test]
        fn eq(a in any::<f64>(), b in any::<f64>()) {
            let first = Expression::Infix (InfixExpression {
                left: Box::new(Expression::Number(real!(a))),
                operator: InfixOperator::Plus,
                right: Box::new(Expression::Number(real!(b))),
            } );
            let differing = Expression::Number(real!(a + b));
            prop_assert_eq!(&first, &first);
            prop_assert_ne!(&first, &differing);
        }

        #[test]
        fn eq_commutative(a in any::<f64>(), b in any::<f64>()) {
            let first = Expression::Infix(InfixExpression {
                left: Box::new(Expression::Number(real!(a))),
                operator: InfixOperator::Plus,
                right: Box::new(Expression::Number(real!(b))),
            } );
            let second = Expression::Infix(InfixExpression {
                left: Box::new(Expression::Number(real!(b))),
                operator: InfixOperator::Plus,
                right: Box::new(Expression::Number(real!(a))),
            });
            prop_assert_eq!(first, second);
        }

        #[test]
        fn hash(a in any::<f64>(), b in any::<f64>()) {
            let first = Expression::Infix (InfixExpression {
                left: Box::new(Expression::Number(real!(a))),
                operator: InfixOperator::Plus,
                right: Box::new(Expression::Number(real!(b))),
            });
            let matching = first.clone();
            let differing = Expression::Number(real!(a + b));
            let mut set = HashSet::new();
            set.insert(first);
            assert!(set.contains(&matching));
            assert!(!set.contains(&differing))
        }

        #[test]
        fn hash_commutative(a in any::<f64>(), b in any::<f64>()) {
            let first = Expression::Infix(InfixExpression {
                left: Box::new(Expression::Number(real!(a))),
                operator: InfixOperator::Plus,
                right: Box::new(Expression::Number(real!(b))),
            } );
            let second = Expression::Infix(InfixExpression {
                left: Box::new(Expression::Number(real!(b))),
                operator: InfixOperator::Plus,
                right: Box::new(Expression::Number(real!(a))),
            } );
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
        fn no_other_exps_are_real(expr in arb_expr().prop_filter("Not numbers", |e| !matches!(e, Expression::Number(_) | Expression::PiConstant))) {
            prop_assert_eq!(expr.to_real(), Err(EvaluationError::NotANumber))
        }

        #[test]
        fn complexes_are_parseable_as_expressions(value in arb_complex64()) {
            let parsed = Expression::from_str(&format_complex(&value));
            assert!(parsed.is_ok());
            let simple = parsed.unwrap().into_simplified();
            assert_eq!(Expression::Number(value), simple);
        }

        #[test]
        fn exponentiation_works_as_expected(left in arb_expr(), right in arb_expr()) {
            let expected = Expression::Infix (InfixExpression { left: Box::new(left.clone()), operator: InfixOperator::Caret, right: Box::new(right.clone()) } );
            prop_assert_eq!(left ^ right, expected);
        }

        #[test]
        fn in_place_exponentiation_works_as_expected(left in arb_expr(), right in arb_expr()) {
            let expected = Expression::Infix (InfixExpression { left: Box::new(left.clone()), operator: InfixOperator::Caret, right: Box::new(right.clone()) } );
            let mut x = left;
            x ^= right;
            prop_assert_eq!(x, expected);
        }

        #[test]
        fn addition_works_as_expected(left in arb_expr(), right in arb_expr()) {
            let expected = Expression::Infix (InfixExpression { left: Box::new(left.clone()), operator: InfixOperator::Plus, right: Box::new(right.clone()) } );
            prop_assert_eq!(left + right, expected);
        }

        #[test]
        fn in_place_addition_works_as_expected(left in arb_expr(), right in arb_expr()) {
            let expected = Expression::Infix (InfixExpression { left: Box::new(left.clone()), operator: InfixOperator::Plus, right: Box::new(right.clone()) } );
            let mut x = left;
            x += right;
            prop_assert_eq!(x, expected);
        }

        #[test]
        fn subtraction_works_as_expected(left in arb_expr(), right in arb_expr()) {
            let expected = Expression::Infix (InfixExpression { left: Box::new(left.clone()), operator: InfixOperator::Minus, right: Box::new(right.clone()) } );
            prop_assert_eq!(left - right, expected);
        }

        #[test]
        fn in_place_subtraction_works_as_expected(left in arb_expr(), right in arb_expr()) {
            let expected = Expression::Infix (InfixExpression { left: Box::new(left.clone()), operator: InfixOperator::Minus, right: Box::new(right.clone()) } );
            let mut x = left;
            x -= right;
            prop_assert_eq!(x, expected);
        }

        #[test]
        fn multiplication_works_as_expected(left in arb_expr(), right in arb_expr()) {
            let expected = Expression::Infix (InfixExpression { left: Box::new(left.clone()), operator: InfixOperator::Star, right: Box::new(right.clone()) } );
            prop_assert_eq!(left * right, expected);
        }

        #[test]
        fn in_place_multiplication_works_as_expected(left in arb_expr(), right in arb_expr()) {
            let expected = Expression::Infix (InfixExpression { left: Box::new(left.clone()), operator: InfixOperator::Star, right: Box::new(right.clone()) } );
            let mut x = left;
            x *= right;
            prop_assert_eq!(x, expected);
        }

        #[test]
        fn division_works_as_expected(left in arb_expr(), right in arb_expr()) {
            let expected = Expression::Infix (InfixExpression { left: Box::new(left.clone()), operator: InfixOperator::Slash, right: Box::new(right.clone()) } );
            prop_assert_eq!(left / right, expected);
        }

        #[test]
        fn in_place_division_works_as_expected(left in arb_expr(), right in arb_expr()) {
            let expected = Expression::Infix (InfixExpression { left: Box::new(left.clone()), operator: InfixOperator::Slash, right: Box::new(right.clone()) } );
            let mut x = left;
            x /= right;
            prop_assert_eq!(x, expected);
        }

        // Redundant clone: clippy does not correctly introspect the prop_assert_eq! macro
        #[allow(clippy::redundant_clone)]
        #[test]
        fn round_trip(e in arb_expr()) {
            let simple_e = e.clone().into_simplified();
            let s = parenthesized(&e);
            let p = Expression::from_str(&s);
            prop_assert!(p.is_ok());
            let p = p.unwrap();
            let simple_p = p.clone().into_simplified();
            prop_assert_eq!(
                simple_p.clone(),
                simple_e.clone(),
                "Simplified expressions should be equal:\nparenthesized {p} ({p:?}) extracted from {s} simplified to {simple_p}\nvs original {e} ({e:?}) simplified to {simple_e}",
                p=p,
                s=s,
                e=e,
                simple_p=simple_p,
                simple_e=simple_e
            );
        }

    }

    /// Assert that certain selected expressions are parsed and re-written to string
    /// in exactly the same way.
    #[test]
    fn specific_round_trip_tests() {
        for input in &[
            "-1*(phases[0]+phases[1])",
            "(-1*(phases[0]+phases[1]))+(-1*(phases[0]+phases[1]))",
        ] {
            let parsed = Expression::from_str(input);
            let parsed = parsed.unwrap();
            let restring = parsed.to_string();
            assert_eq!(input, &restring);
        }
    }

    #[test]
    fn specific_simplification_tests() {
        for (input, expected) in vec![
            ("pi", Expression::Number(PI.into())),
            ("pi/2", Expression::Number((PI / 2.0).into())),
            ("pi * pi", Expression::Number((PI.powi(2)).into())),
            (
                "(a[0]*2*pi)/6.283185307179586",
                Expression::Address(MemoryReference {
                    name: String::from("a"),
                    index: 0,
                }),
            ),
        ] {
            assert_eq!(
                Expression::from_str(input).unwrap().into_simplified(),
                expected
            )
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

    #[test]
    fn specific_format_complex_tests() {
        for (x, s) in &[
            (Complex64::new(0.0, 0.0), "0"),
            (Complex64::new(-0.0, 0.0), "0"),
            (Complex64::new(-0.0, -0.0), "0"),
            (Complex64::new(0.0, 1.0), "1.0i"),
            (Complex64::new(1.0, -1.0), "1-1.0i"),
            (Complex64::new(1.234, 0.0), "1.234"),
            (Complex64::new(0.0, 1.234), "1.234i"),
            (Complex64::new(-1.234, 0.0), "-1.234"),
            (Complex64::new(0.0, -1.234), "-1.234i"),
            (Complex64::new(1.234, 5.678), "1.234+5.678i"),
            (Complex64::new(-1.234, 5.678), "-1.234+5.678i"),
            (Complex64::new(1.234, -5.678), "1.234-5.678i"),
            (Complex64::new(-1.234, -5.678), "-1.234-5.678i"),
            (Complex64::new(1e100, 2e-100), "1e100+2.0e-100i"),
        ] {
            assert_eq!(format_complex(x), *s);
        }
    }
}
