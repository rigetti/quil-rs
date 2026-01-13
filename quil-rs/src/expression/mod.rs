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
    floating_point_eq, imag,
    instruction::MemoryReference,
    parser::{lex, parse_expression, ParseError},
    program::{disallow_leftover, ParseProgramError},
    quil::Quil,
    real,
};
use internment::ArcIntern;
use lexical::{format, to_string_with_options, WriteFloatOptions};
use nom_locate::LocatedSpan;
use num_complex::Complex64;
use once_cell::sync::Lazy;
use std::{
    borrow::Borrow,
    collections::HashMap,
    f64::consts::PI,
    fmt,
    hash::{Hash, Hasher},
    num::NonZeroI32,
    ops::{
        Add, AddAssign, BitXor, BitXorAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign,
    },
    str::FromStr,
};

#[cfg(test)]
use proptest_derive::Arbitrary;

#[cfg(not(feature = "python"))]
use optipy::strip_pyo3;
#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::{
    gen_stub_pyclass, gen_stub_pyclass_complex_enum, gen_stub_pyclass_enum, gen_stub_pymethods,
};
#[cfg(feature = "python")]
pub(crate) mod quilpy;

mod simplification;

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

/// The type of Quil expressions.
///
/// Quil expressions take advantage of *structural sharing*; if a Quil expression contains the same
/// subexpression twice, such as `x + y` in `(x + y) * (x + y)`, the two children of the `*` node
/// will be the *same pointer*.  This is implemented through *interning*, also known as
/// *hash-consing*; the recursive references to child nodes are done via [`ArcIntern<Expression>`]s.
/// Creating an [`ArcIntern`] from an `Expression` will always return the same pointer for the same
/// expression.
///
/// The structural sharing means that equality, cloning, and hashing on Quil expressions are all
/// very cheap, as they do not need to be recursive: equality of [`ArcIntern`]s is a single-pointer
/// comparison, cloning of [`ArcIntern`]s is a pointer copy and an atomic increment, and hashing of
/// [`ArcIntern`]s hashes a single pointer.  It is also very cheap to key [`HashMap`]s by an
/// [`ArcIntern<Expression>`], which can allow for cheap memoization of operations on `Expression`s.
///
/// The structural sharing also means that Quil expressions are fundamentally *immutable*; it is
/// *impossible* to get an owned or `&mut` reference to the child `Expression`s of any `Expression`,
/// as the use of interning means there may be multiple references to that `Expression` at any time.
///
/// Note that when comparing Quil expressions, any embedded NaNs are treated as *equal* to other
/// NaNs, not unequal, in contravention of the IEEE 754 spec.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass_complex_enum)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.expression", eq, frozen, hash)
)]
#[cfg_attr(not(feature = "python"), strip_pyo3)]
pub enum Expression {
    Address(MemoryReference),
    FunctionCall(FunctionCallExpression),
    Infix(InfixExpression),
    Number(Complex64),
    // Developer note: In Rust, this could be just `PiConstant`,
    // but to be compatible with PyO3's "complex enums",
    // it has to be an empty tuple variant.
    // The same restriction applies to empty tuples in `Instruction`.
    #[pyo3(name = "Pi")]
    PiConstant(),
    Prefix(PrefixExpression),
    Variable(String),
}

#[cfg(test)]
impl proptest::prelude::Arbitrary for Expression {
    type Parameters = ();
    type Strategy = proptest::prelude::BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        use self::proptest_helpers::{arb_complex64, arb_expr_custom_leaves, arb_name};
        use proptest::prelude::*;

        let () = args;

        arb_expr_custom_leaves(any::<MemoryReference>, arb_name, arb_complex64).boxed()
    }
}

/// The type of function call Quil expressions, e.g. `sin(e)`.
///
/// Quil expressions take advantage of *structural sharing*, which is why the `expression` here is
/// wrapped in an [`ArcIntern`]; for more details, see the documentation for [`Expression`].
///
/// Note that when comparing Quil expressions, any embedded NaNs are treated as *equal* to other
/// NaNs, not unequal, in contravention of the IEEE 754 spec.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.expression", eq, frozen, hash, subclass)
)]
#[cfg_attr(not(feature = "python"), strip_pyo3)]
pub struct FunctionCallExpression {
    #[pyo3(get)]
    pub function: ExpressionFunction,
    pub expression: ArcIntern<Expression>,
}

impl FunctionCallExpression {
    pub fn new(function: ExpressionFunction, expression: ArcIntern<Expression>) -> Self {
        Self {
            function,
            expression,
        }
    }
}

/// The type of infix Quil expressions, e.g. `e1 + e2`.
///
/// Quil expressions take advantage of *structural sharing*, which is why the `left` and `right`
/// expressions here are wrapped in [`ArcIntern`]s; for more details, see the documentation for
/// [`Expression`].
///
/// Note that when comparing Quil expressions, any embedded NaNs are treated as *equal* to other
/// NaNs, not unequal, in contravention of the IEEE 754 spec.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.expression", eq, frozen, hash, subclass)
)]
#[cfg_attr(not(feature = "python"), strip_pyo3)]
pub struct InfixExpression {
    pub left: ArcIntern<Expression>,
    #[pyo3(get)]
    pub operator: InfixOperator,
    pub right: ArcIntern<Expression>,
}

impl InfixExpression {
    pub fn new(
        left: ArcIntern<Expression>,
        operator: InfixOperator,
        right: ArcIntern<Expression>,
    ) -> Self {
        Self {
            left,
            operator,
            right,
        }
    }
}

/// The type of prefix Quil expressions, e.g. `-e`.
///
/// Quil expressions take advantage of *structural sharing*, which is why the `expression` here is
/// wrapped in an [`ArcIntern`]; for more details, see the documentation for [`Expression`].
///
/// Note that when comparing Quil expressions, any embedded NaNs are treated as *equal* to other
/// NaNs, not unequal, in contravention of the IEEE 754 spec.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.expression", eq, frozen, hash, subclass)
)]
#[cfg_attr(not(feature = "python"), strip_pyo3)]
pub struct PrefixExpression {
    #[pyo3(get)]
    pub operator: PrefixOperator,
    pub expression: ArcIntern<Expression>,
}

impl PrefixExpression {
    pub fn new(operator: PrefixOperator, expression: ArcIntern<Expression>) -> Self {
        Self {
            operator,
            expression,
        }
    }
}

// TODO (#458): PartialEq/Eq is inconsistent with Hash.
impl PartialEq for Expression {
    // Implemented by hand since we can't derive with f64s hidden inside.
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Address(left), Self::Address(right)) => left == right,
            (Self::Infix(left), Self::Infix(right)) => left == right,
            (Self::Number(left), Self::Number(right)) => {
                floating_point_eq::complex64::eq(*left, *right)
            }
            (Self::Prefix(left), Self::Prefix(right)) => left == right,
            (Self::FunctionCall(left), Self::FunctionCall(right)) => left == right,
            (Self::Variable(left), Self::Variable(right)) => left == right,
            (Self::PiConstant(), Self::PiConstant()) => true,

            // This explicit or-pattern ensures that we'll get a compilation error if
            // `Expression` grows another constructor.
            (
                Self::Address(_)
                | Self::Infix(_)
                | Self::Number(_)
                | Self::Prefix(_)
                | Self::FunctionCall(_)
                | Self::Variable(_)
                | Self::PiConstant(),
                _,
            ) => false,
        }
    }
}

// Implemented by hand since we can't derive with f64s hidden inside.
impl Eq for Expression {}

impl Hash for Expression {
    // Implemented by hand since we can't derive with f64s hidden inside.
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Address(m) => {
                "Address".hash(state);
                m.hash(state);
            }
            Self::FunctionCall(FunctionCallExpression {
                function,
                expression,
            }) => {
                "FunctionCall".hash(state);
                function.hash(state);
                expression.hash(state);
            }
            Self::Infix(InfixExpression {
                left,
                operator,
                right,
            }) => {
                "Infix".hash(state);
                operator.hash(state);
                left.hash(state);
                right.hash(state);
            }
            Self::Number(n) => {
                "Number".hash(state);
                floating_point_eq::complex64::hash(*n, state);
            }
            Self::PiConstant() => {
                "PiConstant()".hash(state);
            }
            Self::Prefix(p) => {
                "Prefix".hash(state);
                p.operator.hash(state);
                p.expression.hash(state);
            }
            Self::Variable(v) => {
                "Variable".hash(state);
                v.hash(state);
            }
        }
    }
}

macro_rules! impl_expr_op {
    ($name:ident, $name_assign:ident, $function:ident, $function_assign:ident, $operator:ident) => {
        impl $name for Expression {
            type Output = Self;
            fn $function(self, other: Self) -> Self {
                Self::Infix(InfixExpression {
                    left: ArcIntern::new(self),
                    operator: InfixOperator::$operator,
                    right: ArcIntern::new(other),
                })
            }
        }

        impl $name<ArcIntern<Expression>> for Expression {
            type Output = Self;
            fn $function(self, other: ArcIntern<Expression>) -> Self {
                Self::Infix(InfixExpression {
                    left: ArcIntern::new(self),
                    operator: InfixOperator::$operator,
                    right: other,
                })
            }
        }

        impl $name<Expression> for ArcIntern<Expression> {
            type Output = Expression;
            fn $function(self, other: Expression) -> Expression {
                Expression::Infix(InfixExpression {
                    left: self,
                    operator: InfixOperator::$operator,
                    right: ArcIntern::new(other),
                })
            }
        }

        impl $name_assign for Expression {
            fn $function_assign(&mut self, other: Self) {
                // Move out of self to avoid potentially cloning a large value
                let temp = ::std::mem::replace(self, Self::PiConstant());
                *self = temp.$function(other);
            }
        }

        impl $name_assign<ArcIntern<Expression>> for Expression {
            fn $function_assign(&mut self, other: ArcIntern<Expression>) {
                // Move out of self to avoid potentially cloning a large value
                let temp = ::std::mem::replace(self, Self::PiConstant());
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

impl Neg for Expression {
    type Output = Self;

    fn neg(self) -> Self {
        Expression::Prefix(PrefixExpression {
            operator: PrefixOperator::Minus,
            expression: ArcIntern::new(self),
        })
    }
}

/// Compute the result of an infix expression where both operands are complex.
#[inline]
pub(crate) fn calculate_infix(
    left: Complex64,
    operator: InfixOperator,
    right: Complex64,
) -> Complex64 {
    use InfixOperator::*;
    match operator {
        Caret => left.powc(right),
        Plus => left + right,
        Minus => left - right,
        Slash => left / right,
        Star => left * right,
    }
}

/// Compute the result of a Quil-defined expression function where the operand is complex.
#[inline]
pub(crate) fn calculate_function(function: ExpressionFunction, argument: Complex64) -> Complex64 {
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
            Expression::PiConstant() => {
                *self = Expression::Number(Complex64::from(PI));
            }
            _ => *self = simplification::run(self),
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
    pub fn evaluate<K1, K2>(
        &self,
        variables: &HashMap<K1, Complex64>,
        memory_references: &HashMap<K2, Vec<f64>>,
    ) -> Result<Complex64, EvaluationError>
    where
        K1: Borrow<str> + Hash + Eq,
        K2: Borrow<str> + Hash + Eq,
    {
        use Expression::*;

        match self {
            FunctionCall(FunctionCallExpression {
                function,
                expression,
            }) => {
                let evaluated = expression.evaluate(variables, memory_references)?;
                Ok(calculate_function(*function, evaluated))
            }
            Infix(InfixExpression {
                left,
                operator,
                right,
            }) => {
                let left_evaluated = left.evaluate(variables, memory_references)?;
                let right_evaluated = right.evaluate(variables, memory_references)?;
                Ok(calculate_infix(left_evaluated, *operator, right_evaluated))
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
            Variable(identifier) => match variables.get(identifier) {
                Some(&value) => Ok(value),
                None => Err(EvaluationError::Incomplete),
            },
            Address(memory_reference) => memory_references
                .get(memory_reference.name.as_str())
                .and_then(|values| {
                    let value = values.get(memory_reference.index as usize)?;
                    Some(real!(*value))
                })
                .ok_or(EvaluationError::Incomplete),
            PiConstant() => Ok(real!(PI)),
            Number(number) => Ok(*number),
        }
    }

    /// Substitute an expression in the place of each matching variable.
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
    #[must_use]
    pub fn substitute_variables<K>(&self, variable_values: &HashMap<K, Expression>) -> Self
    where
        K: Borrow<str> + Hash + Eq,
    {
        use Expression::*;

        match self {
            FunctionCall(FunctionCallExpression {
                function,
                expression,
            }) => Expression::FunctionCall(FunctionCallExpression {
                function: *function,
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
                    operator: *operator,
                    right,
                })
            }
            Prefix(PrefixExpression {
                operator,
                expression,
            }) => Prefix(PrefixExpression {
                operator: *operator,
                expression: expression.substitute_variables(variable_values).into(),
            }),
            Variable(identifier) => match variable_values.get(identifier) {
                Some(value) => value.clone(),
                None => Variable(identifier.clone()),
            },
            other => other.clone(),
        }
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[cfg_attr(feature = "python", pyo3::pymethods)]
impl Expression {
    /// If this is a number with imaginary part "equal to" zero (of _small_ absolute value), return
    /// that number. Otherwise, error with an evaluation error of a descriptive type.
    pub fn to_real(&self) -> Result<f64, EvaluationError> {
        match self {
            Expression::PiConstant() => Ok(PI),
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
pub(crate) fn format_complex(value: &Complex64) -> String {
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

impl Quil for Expression {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> Result<(), crate::quil::ToQuilError> {
        use Expression::*;
        match self {
            Address(memory_reference) => memory_reference.write(f, fall_back_to_debug),
            FunctionCall(FunctionCallExpression {
                function,
                expression,
            }) => {
                write!(f, "{function}(")?;
                expression.write(f, fall_back_to_debug)?;
                write!(f, ")")?;
                Ok(())
            }
            Infix(InfixExpression {
                left,
                operator,
                right,
            }) => {
                format_inner_expression(f, fall_back_to_debug, left)?;
                write!(f, "{operator}")?;
                format_inner_expression(f, fall_back_to_debug, right)
            }
            Number(value) => write!(f, "{}", format_complex(value)).map_err(Into::into),
            PiConstant() => write!(f, "pi").map_err(Into::into),
            Prefix(PrefixExpression {
                operator,
                expression,
            }) => {
                write!(f, "{operator}")?;
                format_inner_expression(f, fall_back_to_debug, expression)
            }
            Variable(identifier) => write!(f, "%{identifier}").map_err(Into::into),
        }
    }
}

/// Utility function to wrap infix expressions that are part of an expression in parentheses, so
/// that correct precedence rules are enforced.
fn format_inner_expression(
    f: &mut impl std::fmt::Write,
    fall_back_to_debug: bool,
    expression: &Expression,
) -> crate::quil::ToQuilResult<()> {
    match expression {
        Expression::Infix(InfixExpression {
            left,
            operator,
            right,
        }) => {
            write!(f, "(")?;
            format_inner_expression(f, fall_back_to_debug, left)?;
            write!(f, "{operator}")?;
            format_inner_expression(f, fall_back_to_debug, right)?;
            write!(f, ")")?;
            Ok(())
        }
        _ => expression.write(f, fall_back_to_debug),
    }
}

#[cfg(test)]
mod test {
    use crate::{
        expression::{
            Expression, InfixExpression, InfixOperator, PrefixExpression, PrefixOperator,
        },
        quil::Quil,
        real,
    };

    use internment::ArcIntern;

    #[test]
    fn formats_nested_expression() {
        let expression = Expression::Infix(InfixExpression {
            left: ArcIntern::new(Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                expression: ArcIntern::new(Expression::Number(real!(3f64))),
            })),
            operator: InfixOperator::Star,
            right: ArcIntern::new(Expression::Infix(InfixExpression {
                left: ArcIntern::new(Expression::PiConstant()),
                operator: InfixOperator::Slash,
                right: ArcIntern::new(Expression::Number(real!(2f64))),
            })),
        });

        assert_eq!(expression.to_quil_or_debug(), "-3*(pi/2)");
    }
}

/// A function defined within Quil syntax.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass_enum)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(
        module = "quil.expression",
        eq,
        frozen,
        hash,
        str,
        rename_all = "SCREAMING_SNAKE_CASE"
    )
)]
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
#[cfg_attr(feature = "stubs", gen_stub_pyclass_enum)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(
        module = "quil.expression",
        eq,
        frozen,
        hash,
        str,
        rename_all = "SCREAMING_SNAKE_CASE"
    )
)]
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
#[cfg_attr(feature = "stubs", gen_stub_pyclass_enum)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(
        module = "quil.expression",
        eq,
        frozen,
        hash,
        str,
        rename_all = "SCREAMING_SNAKE_CASE"
    )
)]
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

/// Convenience constructors for creating [`ArcIntern<Expression>`]s out of other
/// [`ArcIntern<Expression>`]s.
pub mod interned {
    use super::*;

    macro_rules! atoms {
        ($($func:ident: $ctor:ident($($typ:ty)?)),+ $(,)?) => {
            $(
                #[doc = concat!(
                    "A wrapper around [`Expression::",
                    stringify!($ctor),
                    "`] that returns an [`ArcIntern<Expression>`]."
                )]
                #[inline(always)]
                pub fn $func($(value: $typ)?) -> ArcIntern<Expression> {
                    // Using `std::convert::identity` lets us refer to `$typ` and thus make the
                    // constructor argument optional
                    ArcIntern::new(Expression::$ctor($(std::convert::identity::<$typ>(value))?))
                }
            )+
        };
    }

    macro_rules! expression_wrappers {
        ($($func:ident: $atom:ident($ctor:ident { $($field:ident: $field_ty:ty),*$(,)? })),+ $(,)?) => {
            paste::paste! { $(
                #[doc = concat!(
                    "A wrapper around [`Expression::", stringify!([<$func:camel>]), "`] ",
                    "that takes the contents of the inner expression type as arguments directly ",
                    "and returns an [`ArcIntern<Expression>`].",
                    "\n\n",
                    "See also [`", stringify!($atom), "`].",
                )]
                #[inline(always)]
                pub fn $func($($field: $field_ty),*) -> ArcIntern<Expression> {
                    $atom($ctor { $($field),* })
                }
            )+ }
        };
    }

    macro_rules! function_wrappers {
        ($($func:ident: $ctor:ident),+ $(,)?) => {
            $(
                #[doc = concat!(
                    "Create an <code>[ArcIntern]&lt;[Expression]&gt;</code> representing ",
                    "`", stringify!($func), "(expression)`.",
                    "\n\n",
                    "A wrapper around [`Expression::FunctionCall`] with ",
                    "[`ExpressionFunction::", stringify!($ctor), "`].",
                )]
                #[inline(always)]
                pub fn $func(expression: ArcIntern<Expression>) -> ArcIntern<Expression> {
                    function_call(ExpressionFunction::$ctor, expression)
                }
            )+
        };
    }

    macro_rules! infix_wrappers {
        ($($func:ident: $ctor:ident ($op:tt)),+ $(,)?) => {
            $(
                #[doc = concat!(
                    "Create an <code>[ArcIntern]&lt;[Expression]&gt;</code> representing ",
                    "`left ", stringify!($op), " right`.",
                    "\n\n",
                    "A wrapper around [`Expression::Infix`] with ",
                    "[`InfixOperator::", stringify!($ctor), "`].",
                )]
                #[inline(always)]
                pub fn $func(
                    left: ArcIntern<Expression>,
                    right: ArcIntern<Expression>,
                ) -> ArcIntern<Expression> {
                    infix(left, InfixOperator::$ctor, right)
                }
            )+
        };
    }

    macro_rules! prefix_wrappers {
        ($($func:ident: $ctor:ident ($op:tt)),+ $(,)?) => {
            $(
                #[doc = concat!(
                    "Create an <code>[ArcIntern]&lt;[Expression]&gt;</code> representing ",
                    "`", stringify!($op), "expression`.",
                    "\n\n",
                    "A wrapper around [`Expression::Prefix`] with ",
                    "[`PrefixOperator::", stringify!($ctor), "`].",
                )]
                #[inline(always)]
                pub fn $func(expression: ArcIntern<Expression>) -> ArcIntern<Expression> {
                    prefix(PrefixOperator::$ctor, expression)
                }
            )+
        };
    }

    atoms! {
        address: Address(MemoryReference),
        function_call_expr: FunctionCall(FunctionCallExpression),
        infix_expr: Infix(InfixExpression),
        pi: PiConstant(),
        number: Number(Complex64),
        prefix_expr: Prefix(PrefixExpression),
        variable: Variable(String),
    }

    expression_wrappers! {
        function_call: function_call_expr(FunctionCallExpression {
            function: ExpressionFunction,
            expression: ArcIntern<Expression>,
        }),

        infix: infix_expr(InfixExpression {
            left: ArcIntern<Expression>,
            operator: InfixOperator,
            right: ArcIntern<Expression>,
        }),

        prefix: prefix_expr(PrefixExpression {
            operator: PrefixOperator,
            expression: ArcIntern<Expression>,
        }),
    }

    function_wrappers! {
        cis: Cis,
        cos: Cosine,
        exp: Exponent,
        sin: Sine,
        sqrt: SquareRoot,
    }

    infix_wrappers! {
        add: Plus (+),
        sub: Minus (-),
        mul: Star (*),
        div: Slash (/),
        pow: Caret (^),
    }

    prefix_wrappers! {
        unary_plus: Plus (+),
        neg: Minus (-),
    }
}

#[cfg(test)]
pub mod proptest_helpers {
    use super::*;

    use std::f64::consts::TAU;

    use proptest::prelude::*;

    use crate::reserved::ReservedToken;

    pub fn arb_name() -> impl Strategy<Value = String> {
        r"[A-Za-z_]([A-Za-z0-9_-]*[A-Za-z0-9_])?".prop_filter("Exclude reserved tokens", |t| {
            ReservedToken::from_str(t).is_err()
        })
    }

    // Better behaved than the auto-derived version via arbitrary floats
    pub fn arb_complex64() -> impl Strategy<Value = Complex64> {
        ((-TAU..TAU), (-TAU..TAU)).prop_map(|(re, im)| Complex64 { re, im })
    }

    /// Filter an [`Expression`] to not be constantly zero.
    pub fn arb_expr_nonzero(
        strat: impl Strategy<Value = Expression>,
    ) -> impl Strategy<Value = Expression> {
        strat.prop_filter("Exclude constantly-zero expressions", |expr| {
            expr.clone().into_simplified() != Expression::Number(Complex64::new(0.0, 0.0))
        })
    }

    /// Generate an arbitrary [`Expression`] for a property test, with custom leaf generation.
    pub fn arb_expr_custom_leaves<
        MemRefStrat: Strategy<Value = MemoryReference> + 'static,
        VariableStrat: Strategy<Value = String> + 'static,
        ComplexStrat: Strategy<Value = Complex64> + 'static,
    >(
        mut arb_memory_reference: impl FnMut() -> MemRefStrat,
        mut arb_variable: impl FnMut() -> VariableStrat,
        mut arb_complex64: impl FnMut() -> ComplexStrat,
    ) -> impl Strategy<Value = Expression> {
        use Expression::*;
        let leaf = prop_oneof![
            arb_memory_reference().prop_map(Address),
            arb_complex64().prop_map(Number),
            Just(PiConstant()),
            arb_variable().prop_map(Variable),
        ];
        leaf.prop_recursive(
            4,  // No more than 4 branch levels deep
            64, // Target around 64 total nodes
            16, // Each "collection" is up to 16 elements
            |expr| {
                let inner = expr.clone();
                prop_oneof![
                    (any::<ExpressionFunction>(), expr.clone()).prop_map(|(function, e)| {
                        Expression::FunctionCall(FunctionCallExpression {
                            function,
                            expression: ArcIntern::new(e),
                        })
                    }),
                    (expr.clone(), any::<InfixOperator>())
                        .prop_flat_map(move |(left, operator)| {
                            (
                                Just(left),
                                Just(operator),
                                // Avoid division by 0 so that we can reliably assert equality
                                if let InfixOperator::Slash = operator {
                                    arb_expr_nonzero(inner.clone()).boxed()
                                } else {
                                    inner.clone().boxed()
                                },
                            )
                        })
                        .prop_map(|(l, operator, r)| {
                            Infix(InfixExpression {
                                left: ArcIntern::new(l),
                                operator,
                                right: ArcIntern::new(r),
                            })
                        }),
                    (any::<PrefixOperator>(), expr).prop_map(|(operator, e)| {
                        Prefix(PrefixExpression {
                            operator,
                            expression: ArcIntern::new(e),
                        })
                    }),
                ]
            },
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::collections::{hash_map::DefaultHasher, HashSet};

    use proptest::prelude::*;

    use super::proptest_helpers::*;

    /// Hash value helper: turn a hashable thing into a u64.
    #[inline]
    fn hash_to_u64<T: Hash>(t: &T) -> u64 {
        let mut s = DefaultHasher::new();
        t.hash(&mut s);
        s.finish()
    }

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
                    expression: ArcIntern::new(Number(real!(1f64))),
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
                    expression: ArcIntern::new(Expression::Number(real!(PI / 2f64))),
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
            Address(memory_reference) => memory_reference.to_quil_or_debug(),
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
            PiConstant() => "pi".to_string(),
            Prefix(PrefixExpression {
                operator,
                expression,
            }) => format!("({}{})", operator, parenthesized(expression)),
            Variable(identifier) => format!("(%{identifier})"),
        }
    }

    proptest! {
        #[test]
        fn eq(a: f64, b: f64) {
            let first = Expression::Infix (InfixExpression {
                left: ArcIntern::new(Expression::Number(real!(a))),
                operator: InfixOperator::Plus,
                right: ArcIntern::new(Expression::Number(real!(b))),
            } );
            let differing = Expression::Number(real!(a + b));
            prop_assert_eq!(&first, &first);
            prop_assert_ne!(&first, &differing);
        }

        #[test]
        fn hash(a: f64, b: f64) {
            let first = Expression::Infix (InfixExpression {
                left: ArcIntern::new(Expression::Number(real!(a))),
                operator: InfixOperator::Plus,
                right: ArcIntern::new(Expression::Number(real!(b))),
            });
            let matching = first.clone();
            let differing = Expression::Number(real!(a + b));
            let mut set = HashSet::new();
            set.insert(first);
            assert!(set.contains(&matching));
            assert!(!set.contains(&differing))
        }

        #[test]
        fn eq_iff_hash_eq(x: Expression, y: Expression) {
            prop_assert_eq!(x == y, hash_to_u64(&x) == hash_to_u64(&y));
        }

        #[test]
        fn reals_are_real(x: f64) {
            prop_assert_eq!(Expression::Number(real!(x)).to_real(), Ok(x))
        }

        #[test]
        fn some_nums_are_real(re: f64, im: f64) {
            let result = Expression::Number(Complex64{re, im}).to_real();
            if is_small(im) {
                prop_assert_eq!(result, Ok(re))
            } else {
                prop_assert_eq!(result, Err(EvaluationError::NumberNotReal))
            }
        }

        #[test]
        fn no_other_exps_are_real(expr in any::<Expression>().prop_filter("Not numbers", |e| !matches!(e, Expression::Number(_) | Expression::PiConstant()))) {
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
        fn exponentiation_works_as_expected(left: Expression, right: Expression) {
            let expected = Expression::Infix (InfixExpression { left: ArcIntern::new(left.clone()), operator: InfixOperator::Caret, right: ArcIntern::new(right.clone()) } );
            prop_assert_eq!(left ^ right, expected);
        }

        #[test]
        fn in_place_exponentiation_works_as_expected(left: Expression, right: Expression) {
            let expected = Expression::Infix (InfixExpression { left: ArcIntern::new(left.clone()), operator: InfixOperator::Caret, right: ArcIntern::new(right.clone()) } );
            let mut x = left;
            x ^= right;
            prop_assert_eq!(x, expected);
        }

        #[test]
        fn addition_works_as_expected(left: Expression, right: Expression) {
            let expected = Expression::Infix (InfixExpression { left: ArcIntern::new(left.clone()), operator: InfixOperator::Plus, right: ArcIntern::new(right.clone()) } );
            prop_assert_eq!(left + right, expected);
        }

        #[test]
        fn in_place_addition_works_as_expected(left: Expression, right: Expression) {
            let expected = Expression::Infix (InfixExpression { left: ArcIntern::new(left.clone()), operator: InfixOperator::Plus, right: ArcIntern::new(right.clone()) } );
            let mut x = left;
            x += right;
            prop_assert_eq!(x, expected);
        }

        #[test]
        fn subtraction_works_as_expected(left: Expression, right: Expression) {
            let expected = Expression::Infix (InfixExpression { left: ArcIntern::new(left.clone()), operator: InfixOperator::Minus, right: ArcIntern::new(right.clone()) } );
            prop_assert_eq!(left - right, expected);
        }

        #[test]
        fn in_place_subtraction_works_as_expected(left: Expression, right: Expression) {
            let expected = Expression::Infix (InfixExpression { left: ArcIntern::new(left.clone()), operator: InfixOperator::Minus, right: ArcIntern::new(right.clone()) } );
            let mut x = left;
            x -= right;
            prop_assert_eq!(x, expected);
        }

        #[test]
        fn multiplication_works_as_expected(left: Expression, right: Expression) {
            let expected = Expression::Infix (InfixExpression { left: ArcIntern::new(left.clone()), operator: InfixOperator::Star, right: ArcIntern::new(right.clone()) } );
            prop_assert_eq!(left * right, expected);
        }

        #[test]
        fn in_place_multiplication_works_as_expected(left: Expression, right: Expression) {
            let expected = Expression::Infix (InfixExpression { left: ArcIntern::new(left.clone()), operator: InfixOperator::Star, right: ArcIntern::new(right.clone()) } );
            let mut x = left;
            x *= right;
            prop_assert_eq!(x, expected);
        }


        // Avoid division by 0 so that we can reliably assert equality
        #[test]
        fn division_works_as_expected(left: Expression, right in arb_expr_nonzero(any::<Expression>())) {
            let expected = Expression::Infix (InfixExpression { left: ArcIntern::new(left.clone()), operator: InfixOperator::Slash, right: ArcIntern::new(right.clone()) } );
            prop_assert_eq!(left / right, expected);
        }

        // Avoid division by 0 so that we can reliably assert equality
        #[test]
        fn in_place_division_works_as_expected(left: Expression, right in arb_expr_nonzero(any::<Expression>())) {
            let expected = Expression::Infix (InfixExpression { left: ArcIntern::new(left.clone()), operator: InfixOperator::Slash, right: ArcIntern::new(right.clone()) } );
            let mut x = left;
            x /= right;
            prop_assert_eq!(x, expected);
        }

        // Redundant clone: clippy does not correctly introspect the prop_assert_eq! macro
        #[allow(clippy::redundant_clone)]
        #[test]
        fn round_trip(e: Expression) {
            let simple_e = e.clone().into_simplified();
            let s = parenthesized(&e);
            let p = Expression::from_str(&s);
            prop_assert!(p.is_ok());
            let p = p.unwrap();
            let simple_p = p.clone().into_simplified();

            prop_assert_eq!(
                &simple_p,
                &simple_e,
                "Simplified expressions should be equal:\nparenthesized {p} ({p:?}) extracted from {s} simplified to {simple_p}\nvs original {e} ({e:?}) simplified to {simple_e}",
                p=p.to_quil_or_debug(),
                s=s,
                e=e.to_quil_or_debug(),
                simple_p=simple_p.to_quil_or_debug(),
                simple_e=simple_e.to_quil_or_debug()
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
            let restring = parsed.to_quil_or_debug();
            assert_eq!(input, &restring);
        }
    }

    #[test]
    fn test_nan_is_equal() {
        let left = Expression::Number(f64::NAN.into());
        let right = left.clone();
        assert_eq!(left, right);
    }

    #[test]
    fn specific_simplification_tests() {
        for (input, expected) in [
            ("pi", Expression::Number(PI.into())),
            ("pi/2", Expression::Number((PI / 2.0).into())),
            ("pi * pi", Expression::Number((PI.powi(2)).into())),
            ("1.0/(1.0-1.0)", Expression::Number(f64::NAN.into())),
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
        for (input, expected) in [
            (Expression::PiConstant(), Ok(PI)),
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
