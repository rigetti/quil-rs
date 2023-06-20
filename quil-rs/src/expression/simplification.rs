/// Complex machinery for simplifying [`Expression`]s.
use crate::expression::{
    format_complex, hash_to_u64, imag, is_small, real, Expression, ExpressionFunction,
    FunctionCallExpression, InfixExpression, InfixOperator, MemoryReference, PrefixExpression,
    PrefixOperator,
};
use egg::{define_language, rewrite as rw, FromOp, Id, Language, RecExpr};
use once_cell::sync::Lazy;
use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign},
    str::FromStr,
};
use symbolic_expressions::{IntoSexp, Sexp};

/// Simplify an [`Expression`]:
/// - turn it into a [`Sexp`],
/// - parse that into a [`RecExpr<Expr>`],
/// - let [`egg`] simplify the recursive expression as best as it can,
/// - and turn that back into an [`Expression`] (by way of another [`Sexp`]).
pub(super) fn run(expression: &Expression) -> Result<Expression, SimplificationError> {
    let sexp = expression.into_sexp();
    let recexpr = sexp_to_recexpr(&sexp)?;
    let runner = egg::Runner::default().with_expr(&recexpr).run(&(*RULES));
    let root = runner.roots[0];
    let (_, best) = egg::Extractor::new(&runner.egraph, egg::AstSize).find_best(root);
    let simpler_sexp = recexpr_to_sexp(best)?;
    Expression::try_from(&simpler_sexp)
}

/// All the myriad ways simplifying an [`Expression`] can fail.
#[derive(Debug, thiserror::Error)]
pub enum SimplificationError {
    #[error("Invalid string for a complex number: {0}")]
    ComplexParsingError(#[from] num_complex::ParseComplexError<std::num::ParseFloatError>),
    #[error("Cycle found in recursive expression: {0}")]
    CycleInRecExpr(String),
    #[error("Can't make anything from an empty Sexp")]
    EmptySexp,
    #[error("Expected a valid index: {0}")]
    IndexExpected(#[from] std::num::ParseIntError),
    #[error("Unexpected infix operation: {0}")]
    UnexpectedInfixOp(String),
    #[error("Unexpected list Sexp: {0:#?}")]
    UnexpectedListSexp(Sexp),
    #[error("Unexpected unary expr: {0}")]
    UnexpectedUnaryExpr(String),
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
                let mut s = Sexp::start(i.operator.to_string().trim());
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
                x if x.starts_with('%') => Ok(Expression::Variable(s[1..].to_string())),
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
/// Fun fact, there is no total ordering on the complex numbers; however, the implementations
/// here are good enough for our purposes.
///
/// https://en.wikipedia.org/wiki/Complex_number#Ordering
#[derive(Debug, Default, Clone, Copy)]
struct Complex(num_complex::Complex64);

impl Hash for Complex {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Skip zero values (akin to `format_complex`).
        // Also, since f64 isn't hashable, use the u64 binary representation.
        // The docs claim this is rather portable: https://doc.rust-lang.org/std/primitive.f64.html#method.to_bits
        if self.0.re.abs() > 0f64 {
            self.0.re.to_bits().hash(state)
        }
        if self.0.im.abs() > 0f64 {
            self.0.im.to_bits().hash(state)
        }
    }
}

impl PartialEq for Complex {
    // Partial equality by hash value
    fn eq(&self, other: &Self) -> bool {
        hash_to_u64(self) == hash_to_u64(other)
    }
}

impl Eq for Complex {}

impl PartialOrd for Complex {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Typical ordering, but with NAN as the biggest value; borrowed the idea from ordered-float
#[inline(always)]
fn _fcmp(x: f64, y: f64) -> Ordering {
    if let Some(ordering) = x.partial_cmp(&y) {
        ordering
    } else {
        match (x.is_nan(), y.is_nan()) {
            (true, true) => Ordering::Equal,
            (false, true) => Ordering::Less,
            (true, false) => Ordering::Greater,
            (false, false) => unreachable!("These floats should be partially comparable"),
        }
    }
}

/// lexicographic ordering with NAN as the biggest value
impl Ord for Complex {
    fn cmp(&self, other: &Self) -> Ordering {
        match (_fcmp(self.0.re, other.0.re), _fcmp(self.0.im, other.0.im)) {
            (Ordering::Less, _) => Ordering::Less,
            (Ordering::Greater, _) => Ordering::Greater,
            (Ordering::Equal, other) => other,
        }
    }
}

impl std::str::FromStr for Complex {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        num_complex::Complex64::from_str(s)
            .map(Self)
            .map_err(|_| ())
    }
}

impl std::fmt::Display for Complex {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(&format_complex(&self.0))
    }
}

macro_rules! impl_via_inner {
    ($function:ident) => {
        fn $function(self) -> Self {
            Self(self.0.$function())
        }
    };
    ($trait:ident, $function:ident) => {
        impl $trait for Complex {
            type Output = Self;
            fn $function(self) -> Self::Output {
                Self(self.0.$function())
            }
        }
    };
    ($trait:ident, $trait_assign:ident, $function:ident, $function_assign:ident) => {
        impl $trait for Complex {
            type Output = Self;
            fn $function(self, other: Self) -> Self {
                Self(self.0.$function(other.0))
            }
        }
        impl $trait_assign for Complex {
            fn $function_assign(&mut self, other: Self) {
                *self = Self(self.0.$function(other.0))
            }
        }
    };
}

impl_via_inner!(Neg, neg);
impl_via_inner!(Add, AddAssign, add, add_assign);
impl_via_inner!(Sub, SubAssign, sub, sub_assign);
impl_via_inner!(Mul, MulAssign, mul, mul_assign);
impl_via_inner!(Div, DivAssign, div, div_assign);

impl Complex {
    const ZERO: Self = Self(real!(0.0));
    const PI: Self = Self(real!(std::f64::consts::PI));
    fn close(&self, other: Self) -> bool {
        is_small((*self - other).abs())
    }
    fn abs(self) -> f64 {
        self.0.norm()
    }
    fn cis(self) -> Self {
        // num_complex::Complex64::cis takes a float :-(
        Self(self.0.cos() + imag!(1.0) * self.0.sin())
    }
    fn pow(self, other: Self) -> Self {
        Self(self.0.powc(other.0))
    }
    impl_via_inner!(cos);
    impl_via_inner!(exp);
    impl_via_inner!(sin);
    impl_via_inner!(sqrt);
}

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
fn sexp_to_recexpr(sexp: &Sexp) -> Result<RecExpr<Expr>, SimplificationError> {
    fn sexp_into(sexp: &Sexp, expr: &mut RecExpr<Expr>) -> Result<Id, SimplificationError> {
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
                inner_list @ Sexp::List(..) => {
                    Err(SimplificationError::UnexpectedListSexp(inner_list.clone()))
                }
                Sexp::String(op) => {
                    let arg_ids: Vec<Id> = list[1..]
                        .iter()
                        .map(|s| sexp_into(s, expr))
                        .collect::<Result<_, _>>()?;
                    let node = Expr::from_op(op, arg_ids)
                        .map_err(|e| SimplificationError::UnknownOp(format!("{e:#?}")))?;
                    Ok(expr.add(node))
                }
            },
        }
    }
    let mut expr = RecExpr::default();
    sexp_into(sexp, &mut expr)?;
    Ok(expr)
}

/// Parse the [`RecExpr<Expr>`] into a [`Sexp`], avoiding (some) needless stringification.
///
/// See https://docs.rs/egg/latest/src/egg/language.rs.html#470
fn recexpr_to_sexp(expr: RecExpr<Expr>) -> Result<Sexp, SimplificationError> {
    fn recexpr_into(nodes: &[Expr], i: usize) -> Result<Sexp, SimplificationError> {
        let op = Sexp::String(nodes[i].to_string());
        if nodes[i].is_leaf() {
            Ok(op)
        } else {
            let mut list = vec![op];
            for child in nodes[i].children().iter().map(|i| usize::from(*i)) {
                if child < i {
                    list.push(recexpr_into(nodes, child)?);
                } else {
                    return Err(SimplificationError::CycleInRecExpr(format!("{nodes:?}")));
                }
            }
            Ok(Sexp::List(list))
        }
    }
    let nodes = expr.as_ref();
    recexpr_into(nodes, nodes.len() - 1)
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
            Expr::Address(_) | Expr::Symbol(_) => None,
        }
    }

    /// Merge two pieces of data with the same value.
    fn merge(&mut self, to: &mut Self::Data, from: Self::Data) -> egg::DidMerge {
        egg::merge_option(to, from, |_a, _b| egg::DidMerge(false, false))
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

fn is_number(var: &str) -> impl Fn(&mut EGraph, Id, &egg::Subst) -> bool {
    let key = var.parse().unwrap();
    move |egraph, _, subst| egraph[subst[key]].data.as_ref().is_some()
}

/// Rewrite terms of our [`Expr`] language by reducing with our [`Arithmetic`] analysis.
type Rewrite = egg::Rewrite<Expr, Arithmetic>;

/// Instantiate our rewrite rules for simplifying [`Expr`] terms.
static RULES: Lazy<Vec<Rewrite>> = Lazy::new(|| {
    vec![
        // Largely copied from https://github.com/egraphs-good/egg/blob/82c00e970f0bc1fbfe90ce6dc3c3c79ee919c933/tests/math.rs
        // and https://github.com/herbie-fp/herbie/blob/2052806f2ffe0d46bc2e151dd096b127c39e12bd/egg-herbie/src/rules.rs

        // addition & subtraction
        rw!("add-zero"      ; "(+ ?a 0)"                => "?a"),
        rw!("zero-add"      ; "(+ 0 ?a)"                => "?a"),
        rw!("cancel-sub"    ; "(- ?a ?a)"               => "0"),
        // multiplication & division
        rw!("mul-zero"      ; "(* ?a 0)"                => "0"),
        rw!("zero-mul"      ; "(* 0 ?a)"                => "0"),
        rw!("one-mul"       ; "(* 1 ?a)"                => "?a"),
        rw!("mul-one"       ; "(* ?a 1)"                => "?a"),
        rw!("one-div"       ; "(/ ?a 1)"                => "?a"),
        rw!("cancel-div"    ; "(/ ?a ?a)"               => "1" if is_not_zero("?a")),
        // + & *
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
        rw!("div-mul"       ; "(/ ?a (* ?b ?a))"        => "?b" if is_not_zero("?a")),
        rw!("div-mul-2"     ; "(/ (* ?b ?a) ?a)"        => "?b" if is_not_zero("?a")),
        rw!("mul-div"       ; "(* ?a (/ ?b ?a))"        => "?b" if is_not_zero("?a")),
        rw!("mul-div-2"     ; "(* (/ ?b ?a) ?a)"        => "?b" if is_not_zero("?a")),
        rw!("pow-mul"       ; "(* (^ ?a ?b) (^ ?a ?c))" => "(^ ?a (+ ?b ?c))"),
        rw!("mul-pow"       ; "(^ ?a (+ ?b ?c))"        => "(* (^ ?a ?b) (^ ?a ?c))"),
        // pos and neg
        rw!("pos-canon"     ; "(pos ?a)"                => "?a"),
        rw!("sub-neg"       ; "(- ?a (neg ?b))"         => "(+ ?a ?b)"),
        rw!("neg-canon"     ; "(neg ?a)"                => "-?a" if is_number("?a")),
        // exp
        rw!("exp-zero"      ; "(exp 0)"                 => "1"),
        rw!("exp-neg"       ; "(exp (neg ?a))"          => "(/ 1 (exp ?a))"),
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

    egg::test_fn! {
        pow_neg_address,
        &RULES,
        "(^ (neg 9.48e42i) (address A 9))" => "(^ -9.48e42i (address A 9))"
    }

    #[rstest]
    #[case("cos(2 * pi) + 2", "(+ (cos (* 2 pi)) 2)")]
    #[case("pi", "pi")]
    #[case("-(1 - 2)", "(neg (- 1 2))")]
    #[case("-9.48e42i^A[9]", "(^ (neg 9.48e42i) (address A 9))")]
    fn test_sexp(#[case] input: &str, #[case] expected: &str) {
        let parsed = Expression::from_str(input);
        assert!(parsed.is_ok());
        let sexp = Sexp::from(&parsed.unwrap());
        assert_eq!(sexp.to_string(), expected);
    }
}
