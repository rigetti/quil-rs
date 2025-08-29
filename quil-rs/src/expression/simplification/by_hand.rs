//! Complex machinery for simplifying [`Expression`]s.

use std::{cmp::min_by_key, collections::HashMap};

use internment::ArcIntern;

use crate::expression::{
    calculate_function, calculate_infix, interned, real, Expression, ExpressionFunction,
    FunctionCallExpression, InfixExpression, InfixOperator, PrefixExpression, PrefixOperator,
};

/// Simplify an [`Expression`].
pub fn run(expression: &Expression) -> Expression {
    Simplifier::new()
        .simplify(ArcIntern::new(expression.clone()), LIMIT)
        .as_ref()
        .clone()
}

/// Keep stack sizes under control
///
/// Note(@genos): If this limit is allowed to be too large (100, in local testing on my laptop),
/// the recursive nature of `simplify` and friends (below) will build up large callstacks and then
/// crash with an "I've overflowed my stack" error. Except for exceedingly large expressions
/// (`the_big_one` test case in `mod.rs`, for example), a larger limit here doesn't seem to be of
/// practical value in anecdotal testing.
const LIMIT: Limit = Limit(10);

/// We use a separate type for the limit so that we can enforce the use of saturating subtraction,
/// but we don't use [`std::num::Saturating`] so that we can write that in terms of literals.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(transparent)]
struct Limit(u64);

impl std::cmp::PartialEq<u64> for Limit {
    #[inline(always)]
    fn eq(&self, other: &u64) -> bool {
        self.0 == *other
    }
}

impl std::ops::Sub<u64> for Limit {
    type Output = Self;

    #[inline(always)]
    fn sub(self, rhs: u64) -> Self::Output {
        Limit(self.0.saturating_sub(rhs))
    }
}

#[derive(Debug, Clone)]
struct Simplifier {
    simplify_cache: HashMap<ArcIntern<Expression>, ArcIntern<Expression>>,
    size_cache: HashMap<ArcIntern<Expression>, usize>,
}

/// Useful for debugging
fn debug_cache<V, D: std::fmt::Display, W: std::io::Write>(
    cache_name: &str,
    cache: &HashMap<ArcIntern<Expression>, V>,
    display: impl Fn(&V) -> D,
    w: &mut W,
) -> std::io::Result<()> {
    use crate::quil::Quil as _;

    writeln!(w, "{cache_name} cache:")?;
    for (k, v) in cache {
        writeln!(w, "    {} => {}", k.to_quil_or_debug(), display(v))?;
    }

    Ok(())
}

impl Simplifier {
    fn new() -> Self {
        Self {
            simplify_cache: HashMap::new(),
            size_cache: HashMap::new(),
        }
    }

    /// Useful for debugging
    #[allow(dead_code)]
    fn debug_caches(&self, w: &mut impl std::io::Write) -> std::io::Result<()> {
        self.debug_simplify_cache(w)?;
        writeln!(w)?;
        self.debug_size_cache(w)?;
        Ok(())
    }

    /// Useful for debugging
    fn debug_simplify_cache(&self, w: &mut impl std::io::Write) -> std::io::Result<()> {
        use crate::quil::Quil as _;
        debug_cache(
            "Simplification",
            &self.simplify_cache,
            |v| v.to_quil_or_debug(),
            w,
        )
    }

    /// Useful for debugging
    fn debug_size_cache(&self, w: &mut impl std::io::Write) -> std::io::Result<()> {
        debug_cache("Size", &self.size_cache, |v| *v, w)
    }

    /// Helper: in simplification, we'll bias towards smaller expressions
    fn size(&mut self, expr: ArcIntern<Expression>) -> usize {
        if let Some(size) = self.size_cache.get(&expr) {
            return *size;
        }

        let result = match expr.as_ref() {
            Expression::Address(_)
            | Expression::Number(_)
            | Expression::PiConstant()
            | Expression::Variable(_) => 1,
            Expression::FunctionCall(FunctionCallExpression {
                function: _,
                expression,
            }) => 1 + self.size(expression.clone()),
            Expression::Infix(InfixExpression {
                left,
                operator: _,
                right,
            }) => 1 + self.size(left.clone()) + self.size(right.clone()),
            Expression::Prefix(PrefixExpression {
                operator: _,
                expression,
            }) => 1 + self.size(expression.clone()),
        };

        self.size_cache.insert(expr, result);

        result
    }

    /// Helper: using `size`, pick the smaller of two expressions
    fn smaller(
        &mut self,
        expr1: ArcIntern<Expression>,
        expr2: ArcIntern<Expression>,
    ) -> ArcIntern<Expression> {
        min_by_key(expr1, expr2, |e| self.size(e.clone()))
    }

    /// Recursively simplify an [`Expression`] by hand, breaking into cases to make things more
    /// manageable.  Terminates the simplification when `limit` reaches `0`.
    ///
    /// The `limit` decreases whenever we take a step deeper within the expression.  The special
    /// case simplification functions, the ones that do all the work, don't need to decrement the
    /// limit at the top of the function.
    ///
    /// Invariant: Never returns [`Expression::PiConstant()`].
    fn simplify(&mut self, e: ArcIntern<Expression>, limit: Limit) -> ArcIntern<Expression> {
        if let Some(simplified) = self.simplify_cache.get(&e) {
            return simplified.clone();
        }

        let result = match e.as_ref() {
            // Even at a limit of 0, replace `pi` with 3.14….  This gets us the extremely useful
            // invariant that `Expression::PiConstant()` can never be returned from `simplify`.
            Expression::PiConstant() => ArcIntern::new(Expression::Number(PI)),

            // We're out of gas; this is as simplified as things get
            _ if limit == 0 => e.clone(),

            // Atoms don't need to be simplified
            Expression::Address(_) | Expression::Number(_) | Expression::Variable(_) => e.clone(),

            // Simplify recursively
            Expression::FunctionCall(FunctionCallExpression {
                function,
                expression,
            }) => self.simplify_function_call(*function, expression.clone(), limit - 1),
            Expression::Infix(InfixExpression {
                left,
                operator,
                right,
            }) => self.simplify_infix(left.clone(), *operator, right.clone(), limit - 1),
            Expression::Prefix(PrefixExpression {
                operator,
                expression,
            }) => self.simplify_prefix(*operator, expression.clone(), limit - 1),
        };

        self.simplify_cache.insert(e, result.clone());

        result
    }
}

const ZERO: num_complex::Complex64 = real!(0.0);
const ONE: num_complex::Complex64 = real!(1.0);
const TWO: num_complex::Complex64 = real!(2.0);
const PI: num_complex::Complex64 = real!(std::f64::consts::PI);

#[inline]
fn is_zero(x: num_complex::Complex64) -> bool {
    x.norm() < 1e-10
}

#[inline]
fn is_one(x: num_complex::Complex64) -> bool {
    is_zero(x - 1.0)
}

/// Check if both arguments are of the form "something * x" for the _same_ x.
fn mul_matches(left_ax: &ArcIntern<Expression>, right_ax: &ArcIntern<Expression>) -> bool {
    match (left_ax.as_ref(), right_ax.as_ref()) {
        (
            Expression::Infix(InfixExpression {
                left: ll,
                operator: InfixOperator::Star,
                right: lr,
            }),
            Expression::Infix(InfixExpression {
                left: rl,
                operator: InfixOperator::Star,
                right: rr,
            }),
        ) => ll == rl || ll == rr || lr == rl || lr == rr,
        _ => false,
    }
}

impl Simplifier {
    /// Simplify a function call inside an `Expression`.
    fn simplify_function_call(
        &mut self,
        function: ExpressionFunction,
        expression: ArcIntern<Expression>,
        limit: Limit,
    ) -> ArcIntern<Expression> {
        // Evaluate numbers and π, pass through otherwise.  Since [`Self::simplify`] cannot return a
        // literal π, we only need to worry about the number case
        let expression = self.simplify(expression, limit);

        if let Expression::Number(x) = expression.as_ref() {
            interned::number(calculate_function(function, *x))
        } else {
            interned::function_call(function, expression)
        }
    }

    /// Simplify an infix expression inside an `Expression`
    fn simplify_infix(
        &mut self,
        left: ArcIntern<Expression>,
        operator: InfixOperator,
        right: ArcIntern<Expression>,
        limit: Limit,
    ) -> ArcIntern<Expression> {
        let left = self.simplify(left, limit);
        let right = self.simplify(right, limit);

        // There are … many cases here
        match (left.as_ref(), operator, right.as_ref()) {
            //----------------------------------------------------------------
            // First: only diving one deep, pattern matching on the operation
            // (Constant folding and cancellations)
            //----------------------------------------------------------------

            // ## Addition and Subtraction ##

            // Adding with zero
            (Expression::Number(x), InfixOperator::Plus, _) if is_zero(*x) => right,
            (_, InfixOperator::Plus, Expression::Number(x)) if is_zero(*x) => left,

            // Subtracting with zero
            (Expression::Number(x), InfixOperator::Minus, _) if is_zero(*x) => {
                self.simplify(interned::neg(right), limit - 1)
            }
            (_, InfixOperator::Minus, Expression::Number(y)) if is_zero(*y) => left,

            // Subtracting two equal items; equality comparison is more efficient on the `ArcIntern`s
            (_, InfixOperator::Minus, _) if left == right => interned::number(ZERO),

            // ## Multiplication and Division ##

            // Multiplication with zero
            (Expression::Number(x), InfixOperator::Star, _)
            | (_, InfixOperator::Star, Expression::Number(x))
                if is_zero(*x) =>
            {
                interned::number(ZERO)
            }

            // Multiplication with one
            (Expression::Number(x), InfixOperator::Star, _) if is_one(*x) => right,
            (_, InfixOperator::Star, Expression::Number(x)) if is_one(*x) => left,

            // Division with zero
            (Expression::Number(x), InfixOperator::Slash, _) if is_zero(*x) => {
                interned::number(ZERO)
            }
            (_, InfixOperator::Slash, Expression::Number(y)) if is_zero(*y) => {
                interned::number(real!(f64::NAN))
            }

            // Division by one
            (_, InfixOperator::Slash, Expression::Number(y)) if is_one(*y) => left,

            // Division of two equal items; equality comparison is more efficient on the `ArcIntern`s
            (_, InfixOperator::Slash, _) if left == right => interned::number(ONE),

            // ## Exponentiation ##

            // Exponentiation with zero (0⁰ = 1)
            (Expression::Number(x), InfixOperator::Caret, _) if is_zero(*x) => {
                interned::number(ZERO)
            }
            (_, InfixOperator::Caret, Expression::Number(y)) if is_zero(*y) => {
                interned::number(ONE)
            }

            // Exponentiation with one
            (Expression::Number(x), InfixOperator::Caret, _) if is_one(*x) => interned::number(ONE),
            (_, InfixOperator::Caret, Expression::Number(y)) if is_one(*y) => left,

            // ## Two numbers ##

            // Since [`Self::simplify`] cannot return [`Expression::PiConstant()`], we only need to
            // handle true numbers here.
            (Expression::Number(x), _, Expression::Number(y)) => {
                interned::number(calculate_infix(*x, operator, *y))
            }

            //----------------------------------------------------------------
            // Second: dealing with negation in subexpressions
            //----------------------------------------------------------------

            // Addition with negation
            // a + (-b) = (-b) + a = a - b
            (
                _,
                InfixOperator::Plus,
                Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    expression,
                }),
            ) => self.simplify(interned::sub(left, expression.clone()), limit - 1),

            (
                Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    expression,
                }),
                InfixOperator::Plus,
                _,
            ) => self.simplify(interned::sub(right, expression.clone()), limit - 1),

            // Subtraction with negation

            // a - (-b) = a + b
            (
                _,
                InfixOperator::Minus,
                Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    expression,
                }),
            ) => self.simplify(interned::add(left, expression.clone()), limit - 1),

            // -expression - right = smaller of [(-expression) - right, -(expression + right)]
            (
                Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    expression,
                }),
                InfixOperator::Minus,
                _,
            ) => {
                // Recreate the original structure but having simplified the contents
                let original_structure = interned::sub(left.clone(), right.clone());

                // Factor out the negation
                let inner_sum = self.simplify(interned::add(expression.clone(), right), limit - 1);
                let outer_neg = self.simplify(interned::neg(inner_sum), limit - 1);

                self.smaller(original_structure, outer_neg)
            }

            // Multiplication and division with negation

            // Double negative: (-a) ⋇ (-b) = a ⋇ b
            (
                Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    expression: a,
                }),
                InfixOperator::Star | InfixOperator::Slash,
                Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    expression: b,
                }),
            ) => self.simplify(interned::infix(a.clone(), operator, b.clone()), limit - 1),

            // (-a) / a = a / (-a) = -1
            (
                _,
                InfixOperator::Slash,
                Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    expression,
                }),
            ) if &left == expression => interned::number(-ONE),
            (
                Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    expression,
                }),
                InfixOperator::Slash,
                _,
            ) if expression == &right => interned::number(-ONE),

            // a ⋇ (-b) = (-a) ⋇ b, pick the shorter
            (
                _,
                InfixOperator::Star | InfixOperator::Slash,
                Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    expression,
                }),
            ) => {
                let original = interned::mul(left.clone(), right.clone());
                let neg_left = self.simplify(interned::neg(left), limit - 1);
                let new = self.simplify(
                    interned::infix(neg_left, operator, expression.clone()),
                    limit - 1,
                );
                self.smaller(original, new)
            }

            // (-a) ⋇ b = a ⋇ (-b), pick the shorter
            (
                Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    expression,
                }),
                InfixOperator::Star | InfixOperator::Slash,
                _,
            ) => {
                let original = interned::mul(left.clone(), right.clone());
                let neg_right = self.simplify(interned::neg(right), limit - 1);
                let new = self.simplify(
                    interned::infix(expression.clone(), operator, neg_right),
                    limit - 1,
                );
                self.smaller(original, new)
            }

            //----------------------------------------------------------------
            // Third: Affine relationships
            //----------------------------------------------------------------

            // (a1 * x + b1) + (a2 * x + b2) = (a1 + a2) * x + (b1 + b2)
            //
            // Apologies for this one; I couldn't get the compiler to let me match two levels deep in a
            // recursive data type, and `if let` in a match guard isn't stabilized.
            (
                Expression::Infix(InfixExpression {
                    left: left_ax,
                    operator: InfixOperator::Plus,
                    right: left_b,
                }),
                InfixOperator::Plus,
                Expression::Infix(InfixExpression {
                    left: right_ax,
                    operator: InfixOperator::Plus,
                    right: right_b,
                }),
            ) if mul_matches(left_ax, right_ax) => {
                let Expression::Infix(InfixExpression {
                    left: ll,
                    operator: InfixOperator::Star,
                    right: lr,
                }) = left_ax.as_ref()
                else {
                    unreachable!("This is handled by mul_matches")
                };
                let Expression::Infix(InfixExpression {
                    left: rl,
                    operator: InfixOperator::Star,
                    right: rr,
                }) = right_ax.as_ref()
                else {
                    unreachable!("This is handled by mul_matches")
                };
                let (left_a, right_a, x) = if ll == rl {
                    (lr, rr, ll)
                } else if ll == rr {
                    (lr, rl, ll)
                } else if lr == rl {
                    (ll, rr, lr)
                } else {
                    (ll, rl, rr)
                };

                let sum_as =
                    self.simplify(interned::add(left_a.clone(), right_a.clone()), limit - 1);
                let sum_bs =
                    self.simplify(interned::add(left_b.clone(), right_b.clone()), limit - 1);
                let mul_as_x = self.simplify(interned::mul(sum_as, x.clone()), limit - 1);
                self.simplify(interned::add(mul_as_x, sum_bs), limit - 1)
            }

            // (a1 * x) + (a2 * x) = (a1 + a2) * x
            (
                Expression::Infix(InfixExpression {
                    left: left_a,
                    operator: InfixOperator::Star,
                    right: left_x,
                }),
                InfixOperator::Plus,
                Expression::Infix(InfixExpression {
                    left: right_a,
                    operator: InfixOperator::Star,
                    right: right_x,
                }),
            ) if left_x == right_x => {
                let sum_as =
                    self.simplify(interned::add(left_a.clone(), right_a.clone()), limit - 1);
                self.simplify(interned::mul(sum_as, left_x.clone()), limit - 1)
            }

            // (x + b1) + (x + b2) = 2 * x + (b1 + b2)
            (
                Expression::Infix(InfixExpression {
                    left: left_x,
                    operator: InfixOperator::Plus,
                    right: left_b,
                }),
                InfixOperator::Plus,
                Expression::Infix(InfixExpression {
                    left: right_x,
                    operator: InfixOperator::Plus,
                    right: right_b,
                }),
            ) if left_x == right_x => {
                let two_x = self.simplify(
                    interned::mul(interned::number(TWO), left_x.clone()),
                    limit - 1,
                );
                let sum_bs =
                    self.simplify(interned::add(left_b.clone(), right_b.clone()), limit - 1);
                self.simplify(interned::add(two_x, sum_bs), limit - 1)
            }

            //----------------------------------------------------------------
            // Fourth: commutation, association, distribution
            //----------------------------------------------------------------

            // Right associativity:
            // - Addition: a + (b + c) = (a + b) + c
            // - Multiplication: a * (b * c) = (a * b) * c
            // In both cases, pick the shorter
            (
                _,
                InfixOperator::Plus | InfixOperator::Star,
                Expression::Infix(InfixExpression {
                    left: b,
                    operator: inner_operator,
                    right: c,
                }),
            ) if operator == *inner_operator => {
                let original = interned::infix(left.clone(), operator, right.clone());
                let ab = self.simplify(interned::infix(left, operator, b.clone()), limit - 1);
                let new = self.simplify(interned::infix(ab, operator, c.clone()), limit - 1);
                self.smaller(original, new)
            }

            // Right "associativity" (not really):
            // - Subtraction (not really): a - (b - c) = (a + c) - b
            // - Division (not really): a / (b / c) = (a * c) / b
            // In both cases, pick the shorter
            (
                _,
                InfixOperator::Minus | InfixOperator::Slash,
                Expression::Infix(InfixExpression {
                    left: b,
                    operator: inner_operator,
                    right: c,
                }),
            ) if operator == *inner_operator => {
                let inverse = if operator == InfixOperator::Minus {
                    InfixOperator::Plus
                } else {
                    InfixOperator::Star
                };

                let original = interned::infix(left.clone(), operator, right.clone());
                let ac = self.simplify(interned::infix(left, inverse, c.clone()), limit - 1);
                let new = self.simplify(interned::infix(ac, operator, b.clone()), limit - 1);
                self.smaller(original, new)
            }

            // Left associativity and "associativity":
            // - Addition: (a + b) + c = a + (b + c)
            // - Multiplication: (a * b) * c = a * (b * c)
            // - Subtraction (not really): (a - b) - c = a - (b + c)
            // - Division (not really): (a / b) / c = a / (b * c)
            // In all cases, pick the shorter
            (
                Expression::Infix(InfixExpression {
                    left: a,
                    operator: inner_operator,
                    right: b,
                }),
                InfixOperator::Plus
                | InfixOperator::Star
                | InfixOperator::Minus
                | InfixOperator::Slash,
                _,
            ) if operator == *inner_operator => {
                let rhs_operator = match operator {
                    InfixOperator::Minus => InfixOperator::Plus,
                    InfixOperator::Slash => InfixOperator::Star,
                    _ => operator,
                };

                let original = interned::infix(left.clone(), operator, right.clone());
                let bc = self.simplify(interned::infix(b.clone(), operator, right), limit - 1);
                let new = self.simplify(interned::infix(a.clone(), rhs_operator, bc), limit - 1);
                self.smaller(original, new)
            }

            // Right distribution: a * (b + c) = (a * b) + (a * c)
            (
                _,
                InfixOperator::Star,
                Expression::Infix(InfixExpression {
                    left: b,
                    operator: InfixOperator::Plus,
                    right: c,
                }),
            ) => {
                let original = interned::mul(left.clone(), right.clone());
                let ab = self.simplify(interned::mul(left.clone(), b.clone()), limit - 1);
                let ac = self.simplify(interned::mul(left, c.clone()), limit - 1);
                let new = self.simplify(interned::add(ab, ac), limit - 1);
                self.smaller(original, new)
            }

            // Left distribution: (a + b) * c = (a * c) + (a * b)
            (
                Expression::Infix(InfixExpression {
                    left: a,
                    operator: InfixOperator::Plus,
                    right: b,
                }),
                InfixOperator::Star,
                _,
            ) => {
                let original = interned::mul(left.clone(), right.clone());
                let ac = self.simplify(interned::mul(a.clone(), right.clone()), limit - 1);
                let bc = self.simplify(interned::mul(b.clone(), right), limit - 1);
                let new = self.simplify(interned::add(ac, bc), limit - 1);
                self.smaller(original, new)
            }

            //----------------------------------------------------------------
            // Fifth: other parenthesis manipulation
            //----------------------------------------------------------------

            // Mul inside Div on left with cancellation: (a * b) / a = (b * a) / a = b
            (
                Expression::Infix(
                    InfixExpression {
                        left: same,
                        operator: InfixOperator::Star,
                        right: other,
                    }
                    | InfixExpression {
                        left: other,
                        operator: InfixOperator::Star,
                        right: same,
                    },
                ),
                InfixOperator::Slash,
                _,
            ) if &right == same => other.clone(),

            // Mul inside Div on right with cancellation: a / (a * b) = a / (b * a) = 1 / b
            (
                _,
                InfixOperator::Slash,
                Expression::Infix(InfixExpression {
                    left: same,
                    operator: InfixOperator::Star,
                    right: other,
                }),
            )
            | (
                _,
                InfixOperator::Slash,
                Expression::Infix(InfixExpression {
                    left: other,
                    operator: InfixOperator::Star,
                    right: same,
                }),
            ) if &left == same => self.simplify(
                interned::div(interned::number(ONE), other.clone()),
                limit - 1,
            ),

            // Mul inside Div on left: (a * b) / c = a * (b / c), pick the shorter
            (
                Expression::Infix(InfixExpression {
                    left: multiplier,
                    operator: InfixOperator::Star,
                    right: multiplicand,
                }),
                InfixOperator::Slash,
                _,
            ) => {
                let original = interned::div(left.clone(), right.clone());
                let new_multiplicand =
                    self.simplify(interned::div(multiplicand.clone(), right), limit - 1);
                let new = self.simplify(
                    interned::mul(multiplier.clone(), new_multiplicand),
                    limit - 1,
                );
                self.smaller(original, new)
            }

            // Mul inside Div on right: a / (b * c) = (a / b) / c, pick the shorter
            (
                _,
                InfixOperator::Slash,
                Expression::Infix(InfixExpression {
                    left: multiplier,
                    operator: InfixOperator::Star,
                    right: multiplicand,
                }),
            ) => {
                let original = interned::div(left.clone(), right.clone());
                let new_multiplier =
                    self.simplify(interned::div(left, multiplier.clone()), limit - 1);
                let new = self.simplify(
                    interned::div(new_multiplier, multiplicand.clone()),
                    limit - 1,
                );
                self.smaller(original, new)
            }

            // Div inside Mul on left with cancellation: (b / a) * a = b
            (
                Expression::Infix(InfixExpression {
                    left: other,
                    operator: InfixOperator::Slash,
                    right: same,
                }),
                InfixOperator::Star,
                _,
            ) if same == &right => other.clone(),

            // Div inside Mul on right with cancellation: a * (b / a) = b
            (
                _,
                InfixOperator::Star,
                Expression::Infix(InfixExpression {
                    left: other,
                    operator: InfixOperator::Slash,
                    right: same,
                }),
            ) if &left == same => other.clone(),

            //----------------------------------------------------------------
            // Sixth: catch-all if no other patterns match
            //----------------------------------------------------------------
            _ => interned::infix(left, operator, right),
        }
    }

    /// Simplify a prefix expression inside an `Expression`
    fn simplify_prefix(
        &mut self,
        operator: PrefixOperator,
        expr: ArcIntern<Expression>,
        limit: Limit,
    ) -> ArcIntern<Expression> {
        // Remove +
        // Push - into numbers ([`Self::simplify`] cannot return π)
        // Remove double negation
        // Pass through otherwise

        let expr = self.simplify(expr, limit);

        match operator {
            PrefixOperator::Plus => expr,

            PrefixOperator::Minus => match expr.as_ref() {
                Expression::Number(x) => interned::number(-*x),

                Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    expression: inner,
                }) => inner.clone(),

                _ => interned::neg(expr),
            },
        }
    }
}
