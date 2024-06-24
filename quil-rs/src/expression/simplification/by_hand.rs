/// Complex machinery for simplifying [`Expression`]s.
use crate::expression::{
    imag, real, Expression, ExpressionFunction, FunctionCallExpression, InfixExpression,
    InfixOperator, PrefixExpression, PrefixOperator,
};
use std::cmp::min_by_key;

/// Simplify an [`Expression`].
pub(super) fn run(expression: &Expression) -> Expression {
    simplify(expression, LIMIT)
}

/// Keep stack sizes under control
///
/// Note(@genos): If this limit is allowed to be too large (100, in local testing on my laptop),
/// the recursive nature of `simplify` and friends (below) will build up large callstacks and then
/// crash with an "I've overflowed my stack" error. Except for exceedingly large expressions
/// (`the_big_one` test case in `mod.rs`, for example), a larger limit here doesn't seem to be of
/// practical value in anecdotal testing.
const LIMIT: u64 = 10;

/// Recursively simplify an [`Expression`] by hand, breaking into cases to make things more
/// manageable.
fn simplify(e: &Expression, limit: u64) -> Expression {
    if limit == 0 {
        // bail
        e.clone()
    } else {
        match e {
            Expression::Address(_) | Expression::Number(_) | Expression::Variable(_) => e.clone(),
            Expression::FunctionCall(FunctionCallExpression {
                function,
                expression,
            }) => simplify_function_call(*function, expression, limit - 1),
            Expression::PiConstant => Expression::Number(std::f64::consts::PI.into()),
            Expression::Infix(InfixExpression {
                left,
                operator,
                right,
            }) => simplify_infix(left, *operator, right, limit - 1),
            Expression::Prefix(PrefixExpression {
                operator,
                expression,
            }) => simplify_prefix(*operator, expression, limit - 1),
        }
    }
}

const PI: num_complex::Complex64 = real!(std::f64::consts::PI);
const ZERO: num_complex::Complex64 = real!(0.0);
const ONE: num_complex::Complex64 = real!(1.0);
const TWO: num_complex::Complex64 = real!(2.0);

/// Simplify a function call inside an `Expression`, terminating the recursion if `limit` has reached zero.
fn simplify_function_call(func: ExpressionFunction, expr: &Expression, limit: u64) -> Expression {
    if limit == 0 {
        // bail
        Expression::FunctionCall(FunctionCallExpression {
            function: func,
            expression: expr.clone().into(),
        })
    } else {
        // Evaluate numbers and π
        // Pass through otherwise
        match (func, simplify(expr, limit - 1)) {
            (ExpressionFunction::Cis, Expression::Number(x)) => {
                // num_complex::Complex64::cis only accepts f64
                Expression::Number(x.cos() + imag!(1.0) * x.sin())
            }
            (ExpressionFunction::Cis, Expression::PiConstant) => Expression::Number(-ONE),
            (ExpressionFunction::Cosine, Expression::Number(x)) => Expression::Number(x.cos()),
            (ExpressionFunction::Cosine, Expression::PiConstant) => Expression::Number(-ONE),
            (ExpressionFunction::Exponent, Expression::Number(x)) => Expression::Number(x.exp()),
            (ExpressionFunction::Exponent, Expression::PiConstant) => Expression::Number(PI.exp()),
            (ExpressionFunction::Sine, Expression::Number(x)) => Expression::Number(x.sin()),
            (ExpressionFunction::Sine, Expression::PiConstant) => Expression::Number(PI.sin()),
            (ExpressionFunction::SquareRoot, Expression::Number(x)) => Expression::Number(x.sqrt()),
            (ExpressionFunction::SquareRoot, Expression::PiConstant) => {
                Expression::Number(PI.sqrt())
            }
            (function, expression) => Expression::FunctionCall(FunctionCallExpression {
                function,
                expression: expression.into(),
            }),
        }
    }
}

#[inline]
fn is_zero(x: num_complex::Complex64) -> bool {
    x.norm() < 1e-10
}

#[inline]
fn is_one(x: num_complex::Complex64) -> bool {
    is_zero(x - 1.0)
}

/// Helper: in simplification, we'll bias towards smaller expressions
fn size(expr: &Expression) -> usize {
    match expr {
        Expression::Address(_)
        | Expression::Number(_)
        | Expression::PiConstant
        | Expression::Variable(_) => 1,
        Expression::FunctionCall(FunctionCallExpression {
            function: _,
            expression,
        }) => 1 + size(expression),
        Expression::Infix(InfixExpression {
            left,
            operator: _,
            right,
        }) => 1 + size(left) + size(right),
        Expression::Prefix(PrefixExpression {
            operator: _,
            expression,
        }) => 1 + size(expression),
    }
}

// It's verbose to go alone! Take this.
macro_rules! infix {
    ($left:expr, $op:expr, $right:expr) => {
        Expression::Infix(InfixExpression {
            left: $left.into(),
            operator: $op,
            right: $right.into(),
        })
    };
}
macro_rules! add {
    ($left:expr, $right:expr) => {
        infix!($left, InfixOperator::Plus, $right)
    };
}
macro_rules! sub {
    ($left:expr, $right:expr) => {
        infix!($left, InfixOperator::Minus, $right)
    };
}
macro_rules! mul {
    ($left:expr, $right:expr) => {
        infix!($left, InfixOperator::Star, $right)
    };
}
macro_rules! div {
    ($left:expr, $right:expr) => {
        infix!($left, InfixOperator::Slash, $right)
    };
}

/// Check if both arguments are of the form "something * x" for the _same_ x.
fn mul_matches(left_ax: &Expression, right_ax: &Expression) -> bool {
    match (left_ax, right_ax) {
        (
            Expression::Infix(InfixExpression {
                left: ref ll,
                operator: InfixOperator::Star,
                right: ref lr,
            }),
            Expression::Infix(InfixExpression {
                left: ref rl,
                operator: InfixOperator::Star,
                right: ref rr,
            }),
        ) => ll == rl || ll == rr || lr == rl || lr == rr,
        _ => false,
    }
}

/// Simplify an infix expression inside an `Expression`, terminating the recursion if `limit` has reached
/// zero.
fn simplify_infix(l: &Expression, op: InfixOperator, r: &Expression, limit: u64) -> Expression {
    if limit == 0 {
        // bail
        Expression::Infix(InfixExpression {
            left: l.clone().into(),
            operator: op,
            right: r.clone().into(),
        })
    } else {
        // There are … many cases here
        match (simplify(l, limit - 1), op, simplify(r, limit - 1)) {
            //----------------------------------------------------------------
            // First: only diving one deep, pattern matching on the operation
            // (Constant folding and cancellations, mostly)
            //----------------------------------------------------------------

            // Addition and Subtraction

            // Adding with zero
            (Expression::Number(x), InfixOperator::Plus, other)
            | (other, InfixOperator::Plus, Expression::Number(x))
                if is_zero(x) =>
            {
                other
            }
            // Adding numbers or π
            (Expression::Number(x), InfixOperator::Plus, Expression::Number(y)) => {
                Expression::Number(x + y)
            }
            (Expression::Number(x), InfixOperator::Plus, Expression::PiConstant)
            | (Expression::PiConstant, InfixOperator::Plus, Expression::Number(x)) => {
                Expression::Number(PI + x)
            }
            (Expression::PiConstant, InfixOperator::Plus, Expression::PiConstant) => {
                Expression::Number(2.0 * PI)
            }

            // Subtracting with zero
            (Expression::Number(x), InfixOperator::Minus, right) if is_zero(x) => {
                simplify_prefix(PrefixOperator::Minus, &right, limit - 1)
            }
            (left, InfixOperator::Minus, Expression::Number(y)) if is_zero(y) => left,
            // Subtracting self
            (left, InfixOperator::Minus, right) if left == right => Expression::Number(ZERO),
            // Subtracting numbers or π (π - π already covered)
            (Expression::Number(x), InfixOperator::Minus, Expression::Number(y)) => {
                Expression::Number(x - y)
            }
            (Expression::Number(x), InfixOperator::Minus, Expression::PiConstant) => {
                Expression::Number(x - PI)
            }
            (Expression::PiConstant, InfixOperator::Minus, Expression::Number(y)) => {
                Expression::Number(PI - y)
            }

            // Multiplication and Division

            // Multiplication with zero
            (Expression::Number(x), InfixOperator::Star, _)
            | (_, InfixOperator::Star, Expression::Number(x))
                if is_zero(x) =>
            {
                Expression::Number(ZERO)
            }
            // Multiplication with one
            (Expression::Number(x), InfixOperator::Star, other)
            | (other, InfixOperator::Star, Expression::Number(x))
                if is_one(x) =>
            {
                other
            }
            // Multiplying with numbers or π
            (Expression::Number(x), InfixOperator::Star, Expression::Number(y)) => {
                Expression::Number(x * y)
            }
            (Expression::Number(x), InfixOperator::Star, Expression::PiConstant)
            | (Expression::PiConstant, InfixOperator::Star, Expression::Number(x)) => {
                Expression::Number(PI * x)
            }
            (Expression::PiConstant, InfixOperator::Star, Expression::PiConstant) => {
                Expression::Number(PI * PI)
            }

            // Division with zero
            (Expression::Number(x), InfixOperator::Slash, _) if is_zero(x) => {
                Expression::Number(ZERO)
            }
            (_, InfixOperator::Slash, Expression::Number(y)) if is_zero(y) => {
                Expression::Number(real!(f64::NAN))
            }
            // Division with one
            (left, InfixOperator::Slash, Expression::Number(y)) if is_one(y) => left,
            // Division with self
            (left, InfixOperator::Slash, right) if left == right => Expression::Number(ONE),
            // Division with numbers or π (π / π already covered)
            (Expression::Number(x), InfixOperator::Slash, Expression::Number(y)) => {
                Expression::Number(x / y)
            }
            (Expression::Number(x), InfixOperator::Slash, Expression::PiConstant) => {
                Expression::Number(x / PI)
            }
            (Expression::PiConstant, InfixOperator::Slash, Expression::Number(y)) => {
                Expression::Number(PI / y)
            }

            // Exponentiation

            // Exponentiation with zero
            (Expression::Number(x), InfixOperator::Caret, _) if is_zero(x) => {
                Expression::Number(ZERO)
            }
            (_, InfixOperator::Caret, Expression::Number(y)) if is_zero(y) => {
                Expression::Number(ONE)
            }
            // Exponentiation with one
            (Expression::Number(x), InfixOperator::Caret, _) if is_one(x) => {
                Expression::Number(ONE)
            }
            (left, InfixOperator::Caret, Expression::Number(y)) if is_one(y) => left,
            // Exponentiation with numbers or π
            (Expression::Number(x), InfixOperator::Caret, Expression::Number(y)) => {
                Expression::Number(x.powc(y))
            }
            (Expression::Number(x), InfixOperator::Caret, Expression::PiConstant) => {
                Expression::Number(x.powc(PI))
            }
            (Expression::PiConstant, InfixOperator::Caret, Expression::Number(y)) => {
                Expression::Number(PI.powc(y))
            }
            (Expression::PiConstant, InfixOperator::Caret, Expression::PiConstant) => {
                Expression::Number(PI.powc(PI))
            }

            //----------------------------------------------------------------
            // Second: dealing with negation in subexpressions
            //----------------------------------------------------------------

            // Addition with negation
            // a + (-b) = (-b) + a = a - b
            (
                ref other,
                InfixOperator::Plus,
                Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    ref expression,
                }),
            )
            | (
                Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    ref expression,
                }),
                InfixOperator::Plus,
                ref other,
            ) => simplify_infix(other, InfixOperator::Minus, expression, limit - 1),

            // Subtraction with negation

            // a - (-b) = a + b
            (
                ref left,
                InfixOperator::Minus,
                Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    ref expression,
                }),
            ) => simplify_infix(left, InfixOperator::Plus, expression, limit - 1),

            // -expression - right = smaller of [(-expression) - right, -(expression + right)]
            (
                ref left @ Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    ref expression,
                }),
                InfixOperator::Minus,
                ref right,
            ) => {
                let original = sub!(left.clone(), right.clone());
                let new = simplify_prefix(
                    PrefixOperator::Minus,
                    &simplify_infix(expression, InfixOperator::Plus, right, limit - 1),
                    limit - 1,
                );
                min_by_key(original, new, size)
            }

            // Multiplication with negation

            // Double negative: (-a) * (-b) = a * b
            (
                Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    expression: ref a,
                }),
                InfixOperator::Star,
                Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    expression: ref b,
                }),
            ) => simplify_infix(a, InfixOperator::Star, b, limit - 1),

            // a * (-b) = (-a) * b, pick the shorter
            (
                ref left,
                InfixOperator::Star,
                ref right @ Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    ref expression,
                }),
            ) => {
                let original = mul!(left.clone(), right.clone());
                let neg_left = simplify_prefix(PrefixOperator::Minus, left, limit - 1);
                let new = simplify_infix(&neg_left, InfixOperator::Star, expression, limit - 1);
                min_by_key(original, new, size)
            }
            // (-a) * b = a * (-b), pick the shorter
            (
                ref left @ Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    ref expression,
                }),
                InfixOperator::Star,
                ref right,
            ) => {
                let original = mul!(left.clone(), right.clone());
                let neg_right = simplify_prefix(PrefixOperator::Minus, right, limit - 1);
                let new = simplify_infix(expression, InfixOperator::Star, &neg_right, limit - 1);
                min_by_key(original, new, size)
            }

            // Division with negation

            // Double negative: (-a) / (-b) = a / b
            (
                Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    expression: ref a,
                }),
                InfixOperator::Slash,
                Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    expression: ref b,
                }),
            ) => simplify_infix(a, InfixOperator::Slash, b, limit - 1),

            // (-a) / a = a / (-a) = -1
            (
                ref other,
                InfixOperator::Slash,
                Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    ref expression,
                }),
            )
            | (
                Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    ref expression,
                }),
                InfixOperator::Slash,
                ref other,
            ) if *other == **expression => Expression::Number(-ONE),

            // a / (-b) = (-a) / b, pick the shorter
            (
                ref left,
                InfixOperator::Slash,
                ref right @ Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    ref expression,
                }),
            ) => {
                let original = div!(left.clone(), right.clone());
                let neg_left = simplify_prefix(PrefixOperator::Minus, left, limit - 1);
                let new = simplify_infix(&neg_left, InfixOperator::Slash, expression, limit - 1);
                min_by_key(original, new, size)
            }

            // (-a) / b = a / (-b), pick the shorter
            (
                ref left @ Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    ref expression,
                }),
                InfixOperator::Slash,
                ref right,
            ) => {
                let original = div!(left.clone(), right.clone());
                let neg_right = simplify_prefix(PrefixOperator::Minus, right, limit - 1);
                let new = simplify_infix(expression, InfixOperator::Slash, &neg_right, limit - 1);
                min_by_key(original, new, size)
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
                    left: ref left_ax,
                    operator: InfixOperator::Plus,
                    right: ref left_b,
                }),
                InfixOperator::Plus,
                Expression::Infix(InfixExpression {
                    left: ref right_ax,
                    operator: InfixOperator::Plus,
                    right: ref right_b,
                }),
            ) if mul_matches(left_ax, right_ax) => {
                let &Expression::Infix(InfixExpression {
                    left: ref ll,
                    operator: InfixOperator::Star,
                    right: ref lr,
                }) = &**left_ax
                else {
                    unreachable!("This is handled by mul_matches")
                };
                let &Expression::Infix(InfixExpression {
                    left: ref rl,
                    operator: InfixOperator::Star,
                    right: ref rr,
                }) = &**right_ax
                else {
                    unreachable!("This is handled by mul_matches")
                };
                let (left_a, right_a, x) = if **ll == **rl {
                    (lr, rr, ll)
                } else if **ll == **rr {
                    (lr, rl, ll)
                } else if **lr == **rl {
                    (ll, rr, lr)
                } else {
                    (ll, rl, rr)
                };
                simplify_infix(
                    &simplify_infix(
                        &simplify_infix(left_a, InfixOperator::Plus, right_a, limit - 1),
                        InfixOperator::Star,
                        x,
                        limit - 1,
                    ),
                    InfixOperator::Plus,
                    &simplify_infix(left_b, InfixOperator::Plus, right_b, limit - 1),
                    limit - 1,
                )
            }

            // (a1 * x) + (a2 * x) = (a1 + a2) * x
            (
                Expression::Infix(InfixExpression {
                    left: ref left_a,
                    operator: InfixOperator::Star,
                    right: ref left_x,
                }),
                InfixOperator::Plus,
                Expression::Infix(InfixExpression {
                    left: ref right_a,
                    operator: InfixOperator::Star,
                    right: ref right_x,
                }),
            ) if left_x == right_x => simplify_infix(
                &simplify_infix(left_a, InfixOperator::Plus, right_a, limit - 1),
                InfixOperator::Star,
                left_x,
                limit - 1,
            ),

            // (x + b1) + (x + b2) = x + (b1 + b2)
            (
                Expression::Infix(InfixExpression {
                    left: ref left_x,
                    operator: InfixOperator::Plus,
                    right: ref left_b,
                }),
                InfixOperator::Plus,
                Expression::Infix(InfixExpression {
                    left: ref right_x,
                    operator: InfixOperator::Plus,
                    right: ref right_b,
                }),
            ) if left_x == right_x => simplify_infix(
                &simplify_infix(
                    &Expression::Number(TWO),
                    InfixOperator::Star,
                    left_x,
                    limit - 1,
                ),
                InfixOperator::Plus,
                &simplify_infix(left_b, InfixOperator::Plus, right_b, limit - 1),
                limit - 1,
            ),

            //----------------------------------------------------------------
            // Fourth: commutation, association, distribution
            //----------------------------------------------------------------

            // Addition associative, right: a + (b + c) = (a + b) + c, pick the shorter
            (
                ref a,
                InfixOperator::Plus,
                ref right @ Expression::Infix(InfixExpression {
                    left: ref b,
                    operator: InfixOperator::Plus,
                    right: ref c,
                }),
            ) => {
                let original = add!(a.clone(), right.clone());
                let new_ab = simplify_infix(a, InfixOperator::Plus, b, limit - 1);
                let new = simplify_infix(&new_ab, InfixOperator::Plus, c, limit - 1);
                min_by_key(original, new, size)
            }

            // Addition associative, left: (a + b) + c = a + (b + c), pick the shorter
            (
                ref left @ Expression::Infix(InfixExpression {
                    left: ref a,
                    operator: InfixOperator::Plus,
                    right: ref b,
                }),
                InfixOperator::Plus,
                ref c,
            ) => {
                let original = add!(left.clone(), c.clone());
                let bc = simplify_infix(b, InfixOperator::Plus, c, limit - 1);
                let new = simplify_infix(a, InfixOperator::Plus, &bc, limit - 1);
                min_by_key(original, new, size)
            }

            // Multiplication associative, right: a * (b * c) = (a * b) * c, pick the shorter
            (
                ref a,
                InfixOperator::Star,
                ref right @ Expression::Infix(InfixExpression {
                    left: ref b,
                    operator: InfixOperator::Star,
                    right: ref c,
                }),
            ) => {
                let original = mul!(a.clone(), right.clone());
                let ab = simplify_infix(a, InfixOperator::Star, b, limit - 1);
                let new = simplify_infix(&ab, InfixOperator::Star, c, limit - 1);
                min_by_key(original, new, size)
            }

            // Multiplication associative, left: (a * b) * c = a * (b * c), pick the shorter
            (
                ref left @ Expression::Infix(InfixExpression {
                    left: ref a,
                    operator: InfixOperator::Star,
                    right: ref b,
                }),
                InfixOperator::Star,
                ref c,
            ) => {
                let original = mul!(left.clone(), c.clone());
                let bc = simplify_infix(b, InfixOperator::Star, c, limit - 1);
                let new = simplify_infix(a, InfixOperator::Star, &bc, limit - 1);
                min_by_key(original, new, size)
            }

            // Subtraction "associative" (not really), right: a - (b - c) = (a + c) - b
            (
                ref a,
                InfixOperator::Minus,
                ref right @ Expression::Infix(InfixExpression {
                    left: ref b,
                    operator: InfixOperator::Minus,
                    right: ref c,
                }),
            ) => {
                let original = sub!(a.clone(), right.clone());
                let ac = simplify_infix(a, InfixOperator::Plus, c, limit - 1);
                let new = simplify_infix(&ac, InfixOperator::Minus, b, limit - 1);
                min_by_key(original, new, size)
            }

            // Division "associative" (not really), right: a / (b / c) = (a * c) / b
            (
                ref a,
                InfixOperator::Slash,
                ref right @ Expression::Infix(InfixExpression {
                    left: ref b,
                    operator: InfixOperator::Slash,
                    right: ref c,
                }),
            ) => {
                let original = div!(a.clone(), right.clone());
                let ac = simplify_infix(a, InfixOperator::Star, c, limit - 1);
                let new = simplify_infix(&ac, InfixOperator::Slash, b, limit - 1);
                min_by_key(original, new, size)
            }

            // Division "associative" (not really), left: (a / b) / c = a / (b * c)
            (
                ref left @ Expression::Infix(InfixExpression {
                    left: ref a,
                    operator: InfixOperator::Slash,
                    right: ref b,
                }),
                InfixOperator::Slash,
                ref c,
            ) => {
                let original = div!(left.clone(), c.clone());
                let bc = simplify_infix(b, InfixOperator::Star, c, limit - 1);
                let new = simplify_infix(a, InfixOperator::Slash, &bc, limit - 1);
                min_by_key(original, new, size)
            }

            // Right distribution: a * (b + c) = (a * b) + (a * c)
            (
                ref a,
                InfixOperator::Star,
                ref right @ Expression::Infix(InfixExpression {
                    left: ref b,
                    operator: InfixOperator::Plus,
                    right: ref c,
                }),
            ) => {
                let original = mul!(a.clone(), right.clone());
                let ab = simplify_infix(a, InfixOperator::Star, b, limit - 1);
                let ac = simplify_infix(a, InfixOperator::Star, c, limit - 1);
                let new = simplify_infix(&ab, InfixOperator::Plus, &ac, limit - 1);
                min_by_key(original, new, size)
            }

            // Left distribution: (a + b) * c = (a * c) + (a * b)
            (
                ref left @ Expression::Infix(InfixExpression {
                    left: ref a,
                    operator: InfixOperator::Plus,
                    right: ref b,
                }),
                InfixOperator::Star,
                ref c,
            ) => {
                let original = mul!(left.clone(), c.clone());
                let ac = simplify_infix(a, InfixOperator::Star, c, limit - 1);
                let bc = simplify_infix(b, InfixOperator::Star, c, limit - 1);
                let new = simplify_infix(&ac, InfixOperator::Plus, &bc, limit - 1);
                min_by_key(original, new, size)
            }

            //----------------------------------------------------------------
            // Fifth: other parenthesis manipulation
            //----------------------------------------------------------------

            // Mul inside Div on left with cancellation
            (
                Expression::Infix(InfixExpression {
                    left: ref same_1,
                    operator: InfixOperator::Star,
                    right: ref other,
                }),
                InfixOperator::Slash,
                ref same_2,
            )
            | (
                Expression::Infix(InfixExpression {
                    left: ref other,
                    operator: InfixOperator::Star,
                    right: ref same_1,
                }),
                InfixOperator::Slash,
                ref same_2,
            ) if **same_1 == *same_2 => simplify(other, limit - 1),

            // Mul inside Div on right with cancellation
            (
                ref same_1,
                InfixOperator::Slash,
                Expression::Infix(InfixExpression {
                    left: ref same_2,
                    operator: InfixOperator::Star,
                    right: ref other,
                }),
            )
            | (
                ref same_1,
                InfixOperator::Slash,
                Expression::Infix(InfixExpression {
                    left: ref other,
                    operator: InfixOperator::Star,
                    right: ref same_2,
                }),
            ) if *same_1 == **same_2 => simplify_infix(
                &Expression::Number(ONE),
                InfixOperator::Slash,
                other,
                limit - 1,
            ),

            // Mul inside Div on left
            (
                ref numerator @ Expression::Infix(InfixExpression {
                    left: ref multiplier,
                    operator: InfixOperator::Star,
                    right: ref multiplicand,
                }),
                InfixOperator::Slash,
                ref denominator,
            ) => {
                let original = div!(numerator.clone(), denominator.clone());
                let new_multiplicand =
                    simplify_infix(multiplicand, InfixOperator::Slash, denominator, limit - 1);
                let new = simplify_infix(
                    multiplier,
                    InfixOperator::Star,
                    &new_multiplicand,
                    limit - 1,
                );
                min_by_key(original, new, size)
            }

            // Mul inside Div on right
            (
                ref numerator,
                InfixOperator::Slash,
                ref denominator @ Expression::Infix(InfixExpression {
                    left: ref multiplier,
                    operator: InfixOperator::Star,
                    right: ref multiplicand,
                }),
            ) => {
                let original = div!(numerator.clone(), denominator.clone());
                let new_multiplier =
                    simplify_infix(numerator, InfixOperator::Slash, multiplier, limit - 1);
                let new = simplify_infix(
                    &new_multiplier,
                    InfixOperator::Star,
                    multiplicand,
                    limit - 1,
                );
                min_by_key(original, new, size)
            }

            // Div inside Mul with cancellation
            (
                Expression::Infix(InfixExpression {
                    left: ref other,
                    operator: InfixOperator::Slash,
                    right: ref same_1,
                }),
                InfixOperator::Star,
                ref same_2,
            )
            | (
                ref same_2,
                InfixOperator::Star,
                Expression::Infix(InfixExpression {
                    left: ref other,
                    operator: InfixOperator::Slash,
                    right: ref same_1,
                }),
            ) if **same_1 == *same_2 => simplify(other, limit - 1),

            //----------------------------------------------------------------
            // Sixth: catch-all if no other patterns match
            //----------------------------------------------------------------
            (left, operator, right) => Expression::Infix(InfixExpression {
                left: left.into(),
                operator,
                right: right.into(),
            }),
        }
    }
}

/// Simplify a prefix expression inside an `Expression`, terminating the recursion if `limit` has reached zero.
fn simplify_prefix(op: PrefixOperator, expr: &Expression, limit: u64) -> Expression {
    if limit == 0 {
        // bail
        Expression::Prefix(PrefixExpression {
            operator: op,
            expression: expr.clone().into(),
        })
    } else {
        // Remove +
        // Push - into numbers & π
        // Pass through otherwise
        match (op, simplify(expr, limit - 1)) {
            (PrefixOperator::Plus, expression) => expression,
            (PrefixOperator::Minus, Expression::Number(x)) => Expression::Number(-x),
            (PrefixOperator::Minus, Expression::PiConstant) => Expression::Number(-PI),
            (operator, expression) => Expression::Prefix(PrefixExpression {
                operator,
                expression: expression.into(),
            }),
        }
    }
}
