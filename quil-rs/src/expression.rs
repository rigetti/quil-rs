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

use lexical::{format, to_string_with_options, WriteFloatOptions};
use nom_locate::LocatedSpan;
use num_complex::Complex64;
use once_cell::sync::Lazy;
use std::collections::{hash_map::DefaultHasher, HashMap};
use std::f64::consts::PI;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::num::NonZeroI32;
use std::ops::{
    Add, AddAssign, BitXor, BitXorAssign, Div, DivAssign, Mul, MulAssign, Sub, SubAssign,
};
use std::str::FromStr;

#[cfg(test)]
use proptest_derive::Arbitrary;

use crate::parser::{lex, parse_expression, ParseError};
use crate::program::{disallow_leftover, ParseProgramError};
use crate::{imag, instruction::MemoryReference, real};

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
                    n.re.to_bits().hash(state)
                }
                if n.im.abs() > 0f64 {
                    n.im.to_bits().hash(state)
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
                let result = self.clone().$function(other);
                *self = result;
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

/// A submodule whose sole purpose is to contain complex machinery for simplifying [`Expression`]s.
mod simplification {
    use super::{
        format_complex, is_small, Expression, ExpressionFunction, FunctionCallExpression,
        InfixExpression, InfixOperator, MemoryReference, PrefixExpression, PrefixOperator,
    };
    use egg::{define_language, rewrite as rw, FromOp, Id, Language, RecExpr};
    use once_cell::sync::Lazy;
    use ordered_float::OrderedFloat;
    use std::{
        ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign},
        str::FromStr,
    };
    use symbolic_expressions::{parser, IntoSexp, Sexp, SexpError};

    /// Simplify an [`Expression`]:
    /// - turn it into a [`Sexp`],
    /// - parse that into a [`RecExpr<Expr>`],
    /// - let [`egg`] simplify the recursive expression as best as it can,
    /// - and turn that back into an [`Expression`] (by way of another [`Sexp`]).
    pub(super) fn run(expression: &Expression) -> Result<Expression, SimplificationError> {
        let sexp = expression.into_sexp();
        let recexpr = parse_expr(&sexp)?;
        let runner = egg::Runner::default().with_expr(&recexpr).run(&(*RULES));
        let root = runner.roots[0];
        let (_, best) = egg::Extractor::new(&runner.egraph, egg::AstSize).find_best(root);
        let simpler_sexp =
            parser::parse_str(&best.to_string()).map_err(SimplificationError::SexpFromRecExpr)?;
        Expression::try_from(&simpler_sexp)
    }

    /// All the myriad ways simplifying an [`Expression`] can fail.
    #[derive(Debug, thiserror::Error)]
    pub enum SimplificationError {
        #[error("Unable to parse Sexp from given RecExpr: {0:?}")]
        SexpFromRecExpr(#[from] SexpError),
        #[error("Can't make anything from an empty Sexp")]
        EmptySexp,
        #[error("Invalid string for a complex number: {0}")]
        ComplexParsingError(#[from] num_complex::ParseComplexError<std::num::ParseFloatError>),
        #[error("Unexpected unary expr: {0}")]
        UnexpectedUnaryExpr(String),
        #[error("Expected a valid index: {0}")]
        IndexExpected(#[from] std::num::ParseIntError),
        #[error("Unexpected infix operation: {0}")]
        UnexpectedInfixOp(String),
        #[error("Unexpected list Sexp: {0:#?}")]
        UnexpectedListSexp(Sexp),
        #[error("Unknown operation: {0}")]
        UnknownOp(String),
    }

    impl IntoSexp for Expression {
        fn into_sexp(&self) -> Sexp {
            match self {
                Expression::Address(memory_reference) => {
                    let mut s = Sexp::start("address");
                    s.push(&memory_reference.name);
                    s.push(memory_reference.index.to_string());
                    s
                }
                Expression::FunctionCall(f) => {
                    let mut s = Sexp::start(&f.function.to_string());
                    s.push(&*f.expression);
                    s
                }
                Expression::Infix(i) => {
                    let mut s = Sexp::start(&i.operator.to_string().trim());
                    s.push(&*i.left);
                    s.push(&*i.right);
                    s
                }
                Expression::Number(n) => format_complex(n).into(),
                Expression::PiConstant => "pi".into(),
                Expression::Prefix(p) => {
                    let mut s = Sexp::start(match p.operator {
                        PrefixOperator::Plus => "pos",
                        PrefixOperator::Minus => "neg",
                    });
                    s.push(&*p.expression);
                    s
                }
                Expression::Variable(s) => format!("%{s}").into(),
            }
        }
    }

    impl From<&Expression> for Sexp {
        fn from(expression: &Expression) -> Self {
            expression.into_sexp()
        }
    }

    impl TryFrom<&Sexp> for Expression {
        type Error = SimplificationError;
        fn try_from(sexp: &Sexp) -> Result<Self, Self::Error> {
            match sexp {
                Sexp::Empty => Err(SimplificationError::EmptySexp),
                Sexp::String(s) => match s.as_str() {
                    "pi" => Ok(Expression::PiConstant),
                    _ => num_complex::Complex64::from_str(s)
                        .map(Expression::Number)
                        .map_err(SimplificationError::ComplexParsingError),
                },
                Sexp::List(ss) => match &ss[..] {
                    [Sexp::String(s), e] => {
                        let expression = Expression::try_from(e)?.into();
                        match s.as_str() {
                            "cis" => Ok(Expression::FunctionCall(FunctionCallExpression {
                                function: ExpressionFunction::Cis,
                                expression,
                            })),
                            "cos" => Ok(Expression::FunctionCall(FunctionCallExpression {
                                function: ExpressionFunction::Cosine,
                                expression,
                            })),
                            "exp" => Ok(Expression::FunctionCall(FunctionCallExpression {
                                function: ExpressionFunction::Exponent,
                                expression,
                            })),
                            "sin" => Ok(Expression::FunctionCall(FunctionCallExpression {
                                function: ExpressionFunction::Sine,
                                expression,
                            })),
                            "sqrt" => Ok(Expression::FunctionCall(FunctionCallExpression {
                                function: ExpressionFunction::SquareRoot,
                                expression,
                            })),
                            "pos" => Ok(Expression::Prefix(PrefixExpression {
                                operator: PrefixOperator::Plus,
                                expression,
                            })),
                            "neg" => Ok(Expression::Prefix(PrefixExpression {
                                operator: PrefixOperator::Minus,
                                expression,
                            })),
                            _ => Err(SimplificationError::UnexpectedUnaryExpr(s.to_string())),
                        }
                    }
                    [Sexp::String(s), Sexp::String(name), Sexp::String(y)]
                        if s.as_str() == "address" =>
                    {
                        let index = y
                            .parse::<u64>()
                            .map_err(SimplificationError::IndexExpected)?;
                        Ok(Expression::Address(MemoryReference {
                            name: name.clone(),
                            index,
                        }))
                    }
                    [Sexp::String(s), x, y] => {
                        let left = Expression::try_from(x)?.into();
                        let right = Expression::try_from(y)?.into();
                        let operator = match s.as_str() {
                            "^" => InfixOperator::Caret,
                            "*" => InfixOperator::Star,
                            "/" => InfixOperator::Slash,
                            "+" => InfixOperator::Plus,
                            "-" => InfixOperator::Minus,
                            _ => return Err(SimplificationError::UnexpectedInfixOp(s.to_string())),
                        };
                        Ok(Expression::Infix(InfixExpression {
                            left,
                            right,
                            operator,
                        }))
                    }
                    _ => Err(SimplificationError::UnexpectedListSexp(sexp.clone())),
                },
            }
        }
    }

    /// An [`egg`]-friendly complex number.
    /// We can't just use `num_complex::Complex64`, because we need `Ord` and `Hash`.
    ///
    /// Fun fact, there is no total ordering on the complex numbers; however, the derived thing
    /// here will work for our purposes.
    ///
    /// https://en.wikipedia.org/wiki/Complex_number#Ordering
    #[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
    struct Complex {
        re: OrderedFloat<f64>,
        im: OrderedFloat<f64>,
    }

    impl From<num_complex::Complex64> for Complex {
        fn from(x: num_complex::Complex64) -> Self {
            Self {
                re: x.re.into(),
                im: x.im.into(),
            }
        }
    }

    impl From<Complex> for num_complex::Complex64 {
        fn from(x: Complex) -> Self {
            Self {
                re: x.re.into(),
                im: x.im.into(),
            }
        }
    }

    impl std::str::FromStr for Complex {
        type Err = ();
        fn from_str(s: &str) -> Result<Self, Self::Err> {
            num_complex::Complex64::from_str(s)
                .map(Self::from)
                .map_err(|_| ())
        }
    }

    impl std::fmt::Display for Complex {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            f.write_str(&format_complex(&num_complex::Complex64::from(*self)))
        }
    }

    impl Complex {
        const I: Self = Self {
            re: OrderedFloat(0.0),
            im: OrderedFloat(1.0),
        };
        const PI: Self = Self {
            re: OrderedFloat(std::f64::consts::PI),
            im: OrderedFloat(0.0),
        };
        const ZERO: Self = Self {
            re: OrderedFloat(0.0),
            im: OrderedFloat(0.0),
        };
        fn close(&self, other: Self) -> bool {
            is_small((*self - other).abs())
        }
        fn abs(self) -> f64 {
            num_complex::Complex64::from(self).norm()
        }
        fn cis(self) -> Self {
            let z = num_complex::Complex64::from(self);
            Self::from(z.cos() + num_complex::Complex64::from(Self::I) * z.sin())
        }
        fn cos(self) -> Self {
            Self::from(num_complex::Complex64::from(self).cos())
        }
        fn exp(self) -> Self {
            Self::from(num_complex::Complex64::from(self).exp())
        }
        fn pow(self, e: Self) -> Self {
            Self::from(num_complex::Complex64::from(self).powc(e.into()))
        }
        fn sin(self) -> Self {
            Self::from(num_complex::Complex64::from(self).sin())
        }
        fn sqrt(self) -> Self {
            Self::from(num_complex::Complex64::from(self).sqrt())
        }
    }

    impl Neg for Complex {
        type Output = Self;
        fn neg(self) -> Self::Output {
            Self::from(-num_complex::Complex64::from(self))
        }
    }

    macro_rules! impl_via_num_complex {
        ($name:ident, $name_assign:ident, $function:ident, $function_assign:ident) => {
            impl $name for Complex {
                type Output = Self;
                fn $function(self, other: Self) -> Self {
                    Self::from(
                        num_complex::Complex64::from(self)
                            .$function(num_complex::Complex64::from(other)),
                    )
                }
            }
            impl $name_assign for Complex {
                fn $function_assign(&mut self, other: Self) {
                    *self = Self::from(
                        num_complex::Complex64::from(*self)
                            .$function(num_complex::Complex64::from(other)),
                    );
                }
            }
        };
    }

    impl_via_num_complex!(Add, AddAssign, add, add_assign);
    impl_via_num_complex!(Sub, SubAssign, sub, sub_assign);
    impl_via_num_complex!(Mul, MulAssign, mul, mul_assign);
    impl_via_num_complex!(Div, DivAssign, div, div_assign);

    define_language! {
        /// An [`egg`]-friendly version of [`Expression`]s, this language allows us to manipulate
        /// and simplify terms.
        enum Expr {
            // Numbers
            "pi" = Pi,
            Number(Complex),
            // Functions
            "cis" = Cis(Id),
            "cos" = Cos(Id),
            "exp" = Exp(Id),
            "sin" = Sin(Id),
            "sqrt" = Sqrt(Id),
            // Prefix arithmetic
            "pos" = Pos(Id),
            "neg" = Neg(Id),
            // Infix arithmetic
            "^" = Pow([Id; 2]),
            "*" = Mul([Id; 2]),
            "/" = Div([Id; 2]),
            "+" = Add([Id; 2]),
            "-" = Sub([Id; 2]),
            // Address
            "address" = Address([Id; 2]),
            // Variables
            Symbol(egg::Symbol),
        }
    }

    /// Parse the [`Sexp`] into a [`RecExpr<Expr>`], avoiding needless stringification.
    ///
    /// See https://docs.rs/egg/0.9.4/src/egg/language.rs.html#545
    fn parse_expr(sexp: &Sexp) -> Result<RecExpr<Expr>, SimplificationError> {
        fn parse_sexp_into(
            sexp: &Sexp,
            expr: &mut RecExpr<Expr>,
        ) -> Result<Id, SimplificationError> {
            match sexp {
                Sexp::Empty => Err(SimplificationError::EmptySexp),
                Sexp::String(s) => {
                    let node = Expr::from_op(s, vec![])
                        .map_err(|e| SimplificationError::UnknownOp(format!("{e:#?}")))?;
                    Ok(expr.add(node))
                }
                Sexp::List(list) if list.is_empty() => Err(SimplificationError::EmptySexp),
                Sexp::List(list) => match &list[0] {
                    Sexp::Empty => Err(SimplificationError::EmptySexp), // should be unreachable
                    list @ Sexp::List(..) => {
                        Err(SimplificationError::UnexpectedListSexp(list.clone()))
                    }
                    Sexp::String(op) => {
                        let arg_ids: Vec<Id> = list[1..]
                            .iter()
                            .map(|s| parse_sexp_into(s, expr))
                            .collect::<Result<_, _>>()?;
                        let node = Expr::from_op(op, arg_ids)
                            .map_err(|e| SimplificationError::UnknownOp(format!("{e:#?}")))?;
                        Ok(expr.add(node))
                    }
                },
            }
        }
        let mut expr = RecExpr::default();
        parse_sexp_into(sexp, &mut expr)?;
        Ok(expr)
    }

    /// Our analysis will perform arithmetic simplification (largely, constant folding) on our
    /// language.
    #[derive(Default)]
    struct Arithmetic;
    type EGraph = egg::EGraph<Expr, Arithmetic>;

    /// Our analysis will perform constant folding on our language.
    impl egg::Analysis<Expr> for Arithmetic {
        /// Constant values
        type Data = Option<Complex>;

        /// Pull the (possible) [`Self::Data`] from the given expression.
        fn make(egraph: &EGraph, enode: &Expr) -> Self::Data {
            let x = |id: &Id| egraph[*id].data.as_ref();
            match enode {
                Expr::Pi => Some(Complex::PI),
                Expr::Number(c) => Some(*c),
                Expr::Cis(id) => Some(x(id)?.cis()),
                Expr::Cos(id) => Some(x(id)?.cos()),
                Expr::Exp(id) => Some(x(id)?.exp()),
                Expr::Sin(id) => Some(x(id)?.sin()),
                Expr::Sqrt(id) => Some(x(id)?.sqrt()),
                Expr::Pos(id) => Some(*x(id)?),
                Expr::Neg(id) => Some(-*x(id)?),
                Expr::Pow([base, power]) => Some(x(base)?.pow(*x(power)?)),
                Expr::Mul([left, right]) => Some(*x(left)? * *x(right)?),
                Expr::Div([left, right]) => Some(*x(left)? / *x(right)?),
                Expr::Add([left, right]) => Some(*x(left)? + *x(right)?),
                Expr::Sub([left, right]) => Some(*x(left)? - *x(right)?),
                _ => None,
            }
        }

        /// Merge two pieces of data with the same value.
        fn merge(&mut self, to: &mut Self::Data, from: Self::Data) -> egg::DidMerge {
            egg::merge_option(to, from, |a, b| {
                assert!(a.close(b), "Merged non-equal constants");
                egg::DidMerge(false, false)
            })
        }

        /// Update the graph to equate and simplify constant values.
        fn modify(egraph: &mut EGraph, id: Id) {
            if let Some(c) = egraph[id].data {
                let value = if c.close(Complex::PI) {
                    Expr::Pi
                } else {
                    Expr::Number(c)
                };
                let added = egraph.add(value);
                egraph.union(id, added);
                egraph[id].nodes.retain(|n| n.is_leaf());
            }
        }
    }

    /// Is the variable equivalent to zero in the given circumstances?
    fn is_not_zero(var: &str) -> impl Fn(&mut EGraph, Id, &egg::Subst) -> bool {
        let key = var.parse().unwrap();
        move |egraph, _, subst| {
            egraph[subst[key]]
                .data
                .as_ref()
                .map(|value| !value.close(Complex::ZERO))
                .unwrap_or(false)
        }
    }

    /// Rewrite terms of our [`Expr`] language by reducing with our [`Arithmetic`] analysis.
    type Rewrite = egg::Rewrite<Expr, Arithmetic>;

    /// Instantiate our rewrite rules for simplifying [`Expr`] terms.
    static RULES: Lazy<Vec<Rewrite>> = Lazy::new(|| {
        vec![
            // largely copied from https://github.com/egraphs-good/egg/blob/main/tests/math.rs
            // and https://github.com/herbie-fp/herbie/blob/main/egg-herbie/src/rules.rs

            // addition & subtraction
            rw!("zero-add"      ; "(+ ?a 0)"                => "?a"),
            rw!("comm-add"      ; "(+ ?a ?b)"               => "(+ ?b ?a)"),
            rw!("comm-mul"      ; "(* ?a ?b)"               => "(* ?b ?a)"),
            rw!("assoc-add"     ; "(+ ?a (+ ?b ?c))"        => "(+ (+ ?a ?b) ?c)"),
            rw!("assoc-mul"     ; "(* ?a (* ?b ?c))"        => "(* (* ?a ?b) ?c)"),
            rw!("sub-canon"     ; "(- ?a ?b)"               => "(+ ?a (* -1 ?b))"),
            rw!("sub-canon-2"   ; "(- ?a ?b)"               => "(+ ?a (neg ?b))"),
            rw!("cancel-sub"    ; "(- ?a ?a)"               => "0"),
            // multiplication & division
            rw!("div-canon"     ; "(/ ?a ?b)"               => "(* ?a (^ ?b -1))" if is_not_zero("?b")),
            rw!("zero-mul"      ; "(* ?a 0)"                => "0"),
            rw!("one-mul"       ; "(* ?a 1)"                => "?a"),
            rw!("cancel-div"    ; "(/ ?a ?a)"               => "1" if is_not_zero("?a")),
            // + - * /
            rw!("distribute"    ; "(* ?a (+ ?b ?c))"        => "(+ (* ?a ?b) (* ?a ?c))"),
            rw!("factor"        ; "(+ (* ?a ?b) (* ?a ?c))" => "(* ?a (+ ?b ?c))"),
            // pow & sqrt
            rw!("pow0"          ; "(^ ?a 0)"                => "1" if is_not_zero("?a")),
            rw!("pow1"          ; "(^ ?a 1)"                => "?a"),
            rw!("pow2"          ; "(^ ?a 2)"                => "(* ?a ?a)"),
            rw!("pow2-neg"      ; "(^ (neg ?a) 2)"          => "(* ?a ?a)"),
            rw!("pow2-sqrt"     ; "(^ (sqrt ?a) 2)"         => "?a"),
            rw!("sqrt-pow2"     ; "(sqrt (^ ?a 2))"         => "?a"),
            rw!("pow1/2"        ; "(^ ?a 0.5)"              => "(sqrt ?a)"),
            rw!("pow-recip"     ; "(^ ?a -1)"               => "(/ 1 ?a)" if is_not_zero("?a")),
            rw!("recip-mul-div" ; "(* ?a (/ 1 ?a))"         => "1" if is_not_zero("?a")),
            rw!("pow-mul"       ; "(* (^ ?a ?b) (^ ?a ?c))" => "(^ ?a (+ ?b ?c))"),
            rw!("mul-pow"       ; "(^ ?a (+ ?b ?c))"        => "(* (^ ?a ?b) (^ ?a ?c))"),
            // pos and neg
            rw!("pos-canon"     ; "(pos ?a)"                => "?a"),
            rw!("neg-canon"     ; "(neg ?a)"                => "-?a"),
            // exp
            rw!("exp-zero"      ; "(exp 0)"                 => "1"),
            rw!("exp-neg"       ; "(exp (neg ?a))"          => "(/ 1 (exp ?a))"),
            // trig
            rw!("cis-zero"      ; "(cis 0)"                 => "1"),
            rw!("cis-pi"        ; "(cis pi)"                => "-1"),
            rw!("cos-zero"      ; "(cos 0)"                 => "1"),
            rw!("cos-pi"        ; "(cos pi)"                => "-1"),
            rw!("cos-+pi"       ; "(cos (+ ?a pi)))"        => "(neg (cos ?a))"),
            rw!("cos-+pi/2"     ; "(cos (+ ?a (/ pi 2)))"   => "(neg (sin ?a))"),
            rw!("sin-zero"      ; "(sin 0)"                 => "0"),
            rw!("sin-pi"        ; "(sin pi)"                => "0"),
            rw!("sin-+pi"       ; "(sin (+ ?a pi))"         => "(neg (sin ?a))"),
            rw!("sin-+pi/2"     ; "(sin (+ ?a (/ pi 2)))"   => "(cos ?a)"),
            rw!("cos-neg"       ; "(cos (neg ?a))"          => "(cos ?a)"),
            rw!("sin-neg"       ; "(sin (neg ?a))"          => "(neg (sin ?a))"),
            // cos^2 + sin^2 = 1
            rw!("cos-sin-sum"   ; "(+ (* (cos ?a) (cos ?a)) (* (sin ?a) (sin ?a)))" => "1"),
            rw!("sub-1-cos"     ; "(- (* (cos ?a) (cos ?a)) 1)" => "(neg (* (sin ?a) (sin ?a)))"),
            rw!("sub-1-sin"     ; "(- (* (sin ?a) (sin ?a)) 1)" => "(neg (* (cos ?a) (cos ?a)))"),
        ]
    });

    #[cfg(test)]
    mod tests {
        use super::*;
        use rstest::rstest;

        egg::test_fn! {
            docstring_example,
            &RULES,
            "(+ (cos (* 2 pi)) 2)" => "3"
        }

        egg::test_fn! {
            issue_208_1,
            &RULES,
            "(* 0 theta)" => "0"
        }

        egg::test_fn! {
            issue_208_2,
            &RULES,
            "(/ theta 1)" => "theta"
        }

        egg::test_fn! {
            issue_208_3,
            &RULES,
            "(/ (* theta 5) 5)" => "theta"
        }

        egg::test_fn! {
            memory_ref,
            &RULES,
            "theta[0]" => "theta[0]"
        }

        egg::test_fn! {
            var,
            &RULES,
            "%foo" => "%foo"
        }

        egg::test_fn! {
            prefix_neg,
            &RULES,
            "(neg -1)" => "1"
        }

        egg::test_fn! {
            neg_sub,
            &RULES,
            "(neg (- 1 2))" => "1"
        }

        egg::test_fn! {
            neg_imag,
            &RULES,
            "(neg 9.48e42i)" => "-9.48e42i"
        }

        #[rstest]
        #[case("cos(2 * pi) + 2", "(+ (cos (* 2 pi)) 2)")]
        #[case("pi", "pi")]
        #[case("-(1 - 2)", "(neg (- 1 2))")]
        #[case("-9.48e42i^A[9]", "(^ (neg 9.48e42i) (address A 9))")]
        #[case("x^1-2i", "(^ (address x 0) 1.0-2.0i)")]
        fn test_sexp(#[case] input: &str, #[case] expected: &str) {
            let parsed = Expression::from_str(&input);
            assert!(parsed.is_ok());
            let sexp = Sexp::from(&parsed.unwrap());
            assert_eq!(sexp.to_string(), expected);
        }
    }
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
            Expression::Address(_)
            | Expression::Number(_)
            | Expression::PiConstant
            | Expression::Variable(_) => {}
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
        }) => write!(f, "({left}{operator}{right})"),
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
                // NOTE: prefix Plus does nothing, and it causes parsing issues
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

    // Better behaved than the auto-derived version re: names
    fn arb_memory_reference() -> impl Strategy<Value = MemoryReference> {
        (r"[a-zA-Z][a-zA-Z0-9]*", any::<u64>())
            .prop_map(|(name, index)| MemoryReference { name, index })
    }

    fn arb_complex64() -> impl Strategy<Value = Complex64> {
        any::<(f64, f64)>().prop_map(|(re, im)| Complex64 { re, im })
    }

    /// Generate an arbitrary Expression for a property test.
    /// See https://docs.rs/proptest/1.0.0/proptest/prelude/trait.Strategy.html#method.prop_recursive
    fn arb_expr() -> impl Strategy<Value = Expression> {
        use Expression::*;
        let leaf = prop_oneof![
            arb_memory_reference().prop_map(Address),
            arb_complex64().prop_map(Number),
            Just(PiConstant),
            r"[a-zA-Z][a-zA-Z0-9]*".prop_map(Variable),
        ];
        (leaf).prop_recursive(
            4,  // No more than 4 branch levels deep
            64, // Target around 64 total nodes
            2,  // Each "collection" is up to 2 elements
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
            dbg!((&value, &simple));
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

        #[test]
        fn round_trip(e in arb_expr()) {
            let s = e.to_string();
            let p = Expression::from_str(&s);
            dbg!((&e, &s, &p));
            prop_assert!(p.is_ok());
            prop_assert_eq!(p.unwrap().into_simplified(), e.into_simplified());
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
