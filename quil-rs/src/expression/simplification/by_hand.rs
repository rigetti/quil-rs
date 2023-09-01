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
/// If this limit is allowed to be too large (100, in local testing on my laptop), the recursive
/// nature of `simplify` and friends (below) will build up large callstacks and then crash with an
/// "I've overflowed my stack" error. Except for exceedingly large expressions (`the_big_one` test
/// case in `mod.rs`, for example), a larger limit here doesn't seem to be of practical value in
/// anecdotal testing.
const LIMIT: u64 = 10;

/// Recursively simplify an [`Expression`] by hand, breaking into cases to make things more
/// manageable.
fn simplify(e: &Expression, limit: u64) -> Expression {
    if limit == 0 {
        // bail
        return e.clone();
    }
    match e {
        Expression::Address(_) | Expression::Number(_) | Expression::Variable(_) => e.clone(),
        Expression::FunctionCall(FunctionCallExpression {
            function,
            expression,
        }) => simplify_function_call(*function, expression, limit.saturating_sub(1)),
        Expression::PiConstant => Expression::Number(std::f64::consts::PI.into()),
        Expression::Infix(InfixExpression {
            left,
            operator,
            right,
        }) => simplify_infix(left, *operator, right, limit.saturating_sub(1)),
        Expression::Prefix(PrefixExpression {
            operator,
            expression,
        }) => simplify_prefix(*operator, expression, limit.saturating_sub(1)),
    }
}

const PI: num_complex::Complex64 = real!(std::f64::consts::PI);
const ZERO: num_complex::Complex64 = real!(0.0);
const ONE: num_complex::Complex64 = real!(1.0);
const TWO: num_complex::Complex64 = real!(2.0);
const I: num_complex::Complex64 = imag!(1.0);

fn simplify_function_call(func: ExpressionFunction, expr: &Expression, limit: u64) -> Expression {
    if limit == 0 {
        // bail
        return Expression::FunctionCall(FunctionCallExpression {
            function: func,
            expression: expr.clone().into(),
        });
    }
    // Evaluate numbers and π
    // Pass through otherwise
    match (func, simplify(expr, limit.saturating_sub(1))) {
        (ExpressionFunction::Cis, Expression::Number(x)) => {
            // num_complex::Complex64::cis only accpets f64 :-(
            Expression::Number(x.cos() + I * x.sin())
        }
        (ExpressionFunction::Cis, Expression::PiConstant) => Expression::Number(-ONE),
        (ExpressionFunction::Cosine, Expression::Number(x)) => Expression::Number(x.cos()),
        (ExpressionFunction::Cosine, Expression::PiConstant) => Expression::Number(-ONE),
        (ExpressionFunction::Exponent, Expression::Number(x)) => Expression::Number(x.exp()),
        (ExpressionFunction::Exponent, Expression::PiConstant) => Expression::Number(PI.exp()),
        (ExpressionFunction::Sine, Expression::Number(x)) => Expression::Number(x.sin()),
        (ExpressionFunction::Sine, Expression::PiConstant) => Expression::Number(PI.sin()),
        (ExpressionFunction::SquareRoot, Expression::Number(x)) => Expression::Number(x.sqrt()),
        (ExpressionFunction::SquareRoot, Expression::PiConstant) => Expression::Number(PI.sqrt()),
        (function, expression) => Expression::FunctionCall(FunctionCallExpression {
            function,
            expression: expression.into(),
        }),
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

// Are these both of the form something * x for the _same_ x?
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
        ) => **ll == **rl || **ll == **rr || **lr == **rl || **lr == **rr,
        _ => false,
    }
}

fn simplify_infix(l: &Expression, op: InfixOperator, r: &Expression, limit: u64) -> Expression {
    if limit == 0 {
        // bail
        return Expression::Infix(InfixExpression {
            left: l.clone().into(),
            operator: op,
            right: r.clone().into(),
        });
    }
    // There are … many cases here
    match (
        simplify(l, limit.saturating_sub(1)),
        op,
        simplify(r, limit.saturating_sub(1)),
    ) {
        //----------------------------------------------------------------
        // First: only diving one deep, pattern matching on the operation
        // (Constant folding and cancellations, mostly)
        //----------------------------------------------------------------

        // Addition and Subtraction

        // Adding with zero
        (Expression::Number(x), InfixOperator::Plus, right) if is_zero(x) => right,
        (left, InfixOperator::Plus, Expression::Number(x)) if is_zero(x) => left,
        // Adding numbers or π
        (Expression::Number(x), InfixOperator::Plus, Expression::Number(y)) => {
            Expression::Number(x + y)
        }
        (Expression::Number(x), InfixOperator::Plus, Expression::PiConstant) => {
            Expression::Number(x + PI)
        }
        (Expression::PiConstant, InfixOperator::Plus, Expression::Number(y)) => {
            Expression::Number(PI + y)
        }
        (Expression::PiConstant, InfixOperator::Plus, Expression::PiConstant) => {
            Expression::Number(2.0 * PI)
        }

        // Subtracting with zero
        (Expression::Number(x), InfixOperator::Minus, right) if is_zero(x) => {
            simplify_prefix(PrefixOperator::Minus, &right, limit.saturating_sub(2))
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
        (Expression::Number(x), InfixOperator::Star, _) if is_zero(x) => Expression::Number(ZERO),
        (_, InfixOperator::Star, Expression::Number(y)) if is_zero(y) => Expression::Number(ZERO),
        // Multiplication with one
        (Expression::Number(x), InfixOperator::Star, right) if is_one(x) => right,
        (left, InfixOperator::Star, Expression::Number(y)) if is_one(y) => left,
        // Multiplying with numbers or π
        (Expression::Number(x), InfixOperator::Star, Expression::Number(y)) => {
            Expression::Number(x * y)
        }
        (Expression::Number(x), InfixOperator::Star, Expression::PiConstant) => {
            Expression::Number(x * PI)
        }
        (Expression::PiConstant, InfixOperator::Star, Expression::Number(y)) => {
            Expression::Number(PI * y)
        }
        (Expression::PiConstant, InfixOperator::Star, Expression::PiConstant) => {
            Expression::Number(PI * PI)
        }

        // Division with zero
        (Expression::Number(x), InfixOperator::Slash, _) if is_zero(x) => Expression::Number(ZERO),
        (_, InfixOperator::Slash, Expression::Number(y)) if is_zero(y) => {
            Expression::Number(real!(f64::NAN)) // TODO Is this OK?
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
        (Expression::Number(x), InfixOperator::Caret, _) if is_zero(x) => Expression::Number(ZERO),
        (_, InfixOperator::Caret, Expression::Number(y)) if is_zero(y) => Expression::Number(ONE),
        // Exponentiation with one
        (Expression::Number(x), InfixOperator::Caret, _) if is_one(x) => Expression::Number(ONE),
        (left, InfixOperator::Caret, Expression::Number(y)) if is_one(y) => left,

        //----------------------------------------------------------------
        // Next: dealing with negation in subexpressions
        //----------------------------------------------------------------

        // Addition with negation
        (
            ref left,
            InfixOperator::Plus,
            Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                ref expression,
            }),
        ) => simplify_infix(
            left,
            InfixOperator::Minus,
            expression,
            limit.saturating_sub(1),
        ),
        (
            Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                ref expression,
            }),
            InfixOperator::Plus,
            ref right,
        ) => simplify_infix(
            right,
            InfixOperator::Minus,
            expression,
            limit.saturating_sub(1),
        ),

        // Subtraction with negation
        (
            ref left,
            InfixOperator::Minus,
            Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                ref expression,
            }),
        ) => simplify_infix(
            left,
            InfixOperator::Plus,
            expression,
            limit.saturating_sub(1),
        ),
        (
            // -expression - right => smaller of (-expression) - right & -(expression + right)
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
                &simplify_infix(
                    expression,
                    InfixOperator::Plus,
                    right,
                    limit.saturating_sub(2),
                ),
                limit.saturating_sub(1),
            );
            min_by_key(original, new, size)
        }

        // Multiplication with negation
        (
            Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                expression: ref left,
            }),
            InfixOperator::Star,
            Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                expression: ref right,
            }),
        ) => simplify_infix(left, InfixOperator::Star, right, limit.saturating_sub(1)),
        (
            ref left,
            InfixOperator::Star,
            ref right @ Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                ref expression,
            }),
        ) => {
            let original = mul!(left.clone(), right.clone());
            let neg_left = simplify_prefix(PrefixOperator::Minus, left, limit.saturating_sub(1));
            let new = simplify_infix(
                &neg_left,
                InfixOperator::Star,
                expression,
                limit.saturating_sub(1),
            );
            min_by_key(original, new, size)
        }
        (
            ref left @ Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                ref expression,
            }),
            InfixOperator::Star,
            ref right,
        ) => {
            let original = mul!(left.clone(), right.clone());
            let neg_right = simplify_prefix(PrefixOperator::Minus, right, limit.saturating_sub(1));
            let new = simplify_infix(
                expression,
                InfixOperator::Star,
                &neg_right,
                limit.saturating_sub(1),
            );
            min_by_key(original, new, size)
        }

        // Division with negation
        (
            Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                expression: ref left,
            }),
            InfixOperator::Slash,
            Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                expression: ref right,
            }),
        ) => simplify_infix(left, InfixOperator::Slash, right, limit.saturating_sub(1)),
        (
            ref left,
            InfixOperator::Slash,
            Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                ref expression,
            }),
        ) if *left == **expression => Expression::Number(-ONE),
        (
            ref left,
            InfixOperator::Slash,
            ref right @ Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                ref expression,
            }),
        ) => {
            let original = div!(left.clone(), right.clone());
            let neg_left = simplify_prefix(PrefixOperator::Minus, left, limit.saturating_sub(1));
            let new = simplify_infix(
                &neg_left,
                InfixOperator::Slash,
                expression,
                limit.saturating_sub(1),
            );
            min_by_key(original, new, size)
        }
        (
            Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                ref expression,
            }),
            InfixOperator::Slash,
            ref right,
        ) if **expression == *right => Expression::Number(-ONE),
        (
            ref left @ Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                ref expression,
            }),
            InfixOperator::Slash,
            ref right,
        ) => {
            let original = div!(left.clone(), right.clone());
            let neg_right = simplify_prefix(PrefixOperator::Minus, right, limit.saturating_sub(1));
            let new = simplify_infix(
                expression,
                InfixOperator::Slash,
                &neg_right,
                limit.saturating_sub(1),
            );
            min_by_key(original, new, size)
        }

        //----------------------------------------------------------------
        // Also: Affine relationships
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
                    &simplify_infix(
                        left_a,
                        InfixOperator::Plus,
                        right_a,
                        limit.saturating_sub(3),
                    ),
                    InfixOperator::Star,
                    x,
                    limit.saturating_sub(2),
                ),
                InfixOperator::Plus,
                &simplify_infix(
                    left_b,
                    InfixOperator::Plus,
                    right_b,
                    limit.saturating_sub(2),
                ),
                limit.saturating_sub(1),
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
            &simplify_infix(
                left_a,
                InfixOperator::Plus,
                right_a,
                limit.saturating_sub(2),
            ),
            InfixOperator::Star,
            left_x,
            limit.saturating_sub(1),
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
            &mul!(Expression::Number(TWO), left_x.clone()),
            InfixOperator::Plus,
            &simplify_infix(
                left_b,
                InfixOperator::Plus,
                right_b,
                limit.saturating_sub(2),
            ),
            limit.saturating_sub(1),
        ),

        //----------------------------------------------------------------
        // After that: commutation, association, distribution
        //----------------------------------------------------------------

        // Addition Associative, right
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
            let new_ab = simplify_infix(a, InfixOperator::Plus, b, limit.saturating_sub(1));
            let new = simplify_infix(&new_ab, InfixOperator::Plus, c, limit.saturating_sub(1));
            min_by_key(original, new, size)
        }

        // Addition Associative, left
        (
            ref left @ Expression::Infix(InfixExpression {
                left: ref a,
                operator: InfixOperator::Plus,
                right: ref b,
            }),
            InfixOperator::Plus,
            ref c,
        ) => {
            let original = mul!(left.clone(), c.clone());
            let bc = simplify_infix(b, InfixOperator::Plus, c, limit.saturating_sub(1));
            let new = simplify_infix(a, InfixOperator::Plus, &bc, limit.saturating_sub(1));
            min_by_key(original, new, size)
        }

        // Multiplication Associative, right
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
            let ab = simplify_infix(a, InfixOperator::Star, b, limit.saturating_sub(1));
            let new = simplify_infix(&ab, InfixOperator::Star, c, limit.saturating_sub(1));
            min_by_key(original, new, size)
        }

        // Multiplication Associative, left
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
            let bc = simplify_infix(b, InfixOperator::Star, c, limit.saturating_sub(1));
            let new = simplify_infix(a, InfixOperator::Star, &bc, limit.saturating_sub(1));
            min_by_key(original, new, size)
        }

        // Subtraction "association" (not really), right
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
            let new_left = simplify_infix(a, InfixOperator::Plus, c, limit.saturating_sub(1));
            let new = simplify_infix(&new_left, InfixOperator::Minus, b, limit.saturating_sub(1));
            min_by_key(original, new, size)
        }

        // Division "association" (not really), right
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
            let new_left = simplify_infix(a, InfixOperator::Star, c, limit.saturating_sub(1));
            let new = simplify_infix(&new_left, InfixOperator::Slash, b, limit.saturating_sub(1));
            min_by_key(original, new, size)
        }

        // Right distribution
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
            let ab = simplify_infix(a, InfixOperator::Star, b, limit.saturating_sub(1));
            let ac = simplify_infix(a, InfixOperator::Star, c, limit.saturating_sub(1));
            let new = simplify_infix(&ab, InfixOperator::Plus, &ac, limit.saturating_sub(1));
            min_by_key(original, new, size)
        }

        // Left distribution
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
            let ac = simplify_infix(a, InfixOperator::Star, c, limit.saturating_sub(1));
            let bc = simplify_infix(b, InfixOperator::Star, c, limit.saturating_sub(1));
            let new = simplify_infix(&ac, InfixOperator::Plus, &bc, limit.saturating_sub(1));
            min_by_key(original, new, size)
        }

        //----------------------------------------------------------------
        // Finally: other parenthesis manipulation
        //----------------------------------------------------------------

        // Mul inside Div on left, multiplicand = denominator
        (
            Expression::Infix(InfixExpression {
                left: ref multiplier,
                operator: InfixOperator::Star,
                right: ref multiplicand,
            }),
            InfixOperator::Slash,
            ref denominator,
        ) if **multiplicand == *denominator => *multiplier.clone(),

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
            let new_multiplicand = simplify_infix(
                multiplicand,
                InfixOperator::Slash,
                denominator,
                limit.saturating_sub(1),
            );
            let new = simplify_infix(
                multiplier,
                InfixOperator::Star,
                &new_multiplicand,
                limit.saturating_sub(1),
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
            let new_multiplier = simplify_infix(
                numerator,
                InfixOperator::Slash,
                multiplier,
                limit.saturating_sub(1),
            );
            let new = simplify_infix(
                &new_multiplier,
                InfixOperator::Star,
                multiplicand,
                limit.saturating_sub(1),
            );
            min_by_key(original, new, size)
        }

        // Div inside Mul on left, denominator = multiplicand
        (
            Expression::Infix(InfixExpression {
                left: ref numerator,
                operator: InfixOperator::Slash,
                right: ref denominator,
            }),
            InfixOperator::Star,
            ref multiplicand,
        ) if **denominator == *multiplicand => *numerator.clone(),

        // Div inside Mul on right, denominator = multiplicand
        (
            ref multiplicand,
            InfixOperator::Star,
            Expression::Infix(InfixExpression {
                left: ref numerator,
                operator: InfixOperator::Slash,
                right: ref denominator,
            }),
        ) if **denominator == *multiplicand => *numerator.clone(),

        // Catch-all
        (left, operator, right) => Expression::Infix(InfixExpression {
            left: left.into(),
            operator,
            right: right.into(),
        }),
    }
}

fn simplify_prefix(op: PrefixOperator, expr: &Expression, limit: u64) -> Expression {
    if limit == 0 {
        // bail
        return Expression::Prefix(PrefixExpression {
            operator: op,
            expression: expr.clone().into(),
        });
    }
    // Remove +
    // Push - into numbers & π
    // Pass through otherwise
    match (op, simplify(expr, limit.saturating_sub(1))) {
        (PrefixOperator::Plus, expression) => expression,
        (PrefixOperator::Minus, Expression::Number(x)) => Expression::Number(-x),
        (PrefixOperator::Minus, Expression::PiConstant) => Expression::Number(-PI),
        (operator, expression) => Expression::Prefix(PrefixExpression {
            operator,
            expression: expression.into(),
        }),
    }
}
