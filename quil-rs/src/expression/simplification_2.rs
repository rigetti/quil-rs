/// Complex machinery for simplifying [`Expression`]s.
use crate::expression::{
    imag, real, Expression, ExpressionFunction, FunctionCallExpression, InfixExpression,
    InfixOperator, PrefixExpression, PrefixOperator,
};
use std::cmp::min_by_key;

/// Simplify an [`Expression`].
pub(super) fn run(expression: &Expression) -> Expression {
    simplify(expression)
}

/// Recursively simplify an [`Expression`], breaking into cases to make things more manageable.
fn simplify(e: &Expression) -> Expression {
    match e {
        Expression::Address(_)
        | Expression::Number(_)
        | Expression::PiConstant
        | Expression::Variable(_) => e.clone(),
        Expression::FunctionCall(FunctionCallExpression {
            function,
            expression,
        }) => simplify_function_call(function, expression),
        Expression::Infix(InfixExpression {
            left,
            operator,
            right,
        }) => simplify_infix(left, operator, right),
        Expression::Prefix(PrefixExpression {
            operator,
            expression,
        }) => simplify_prefix(operator, expression),
    }
}

const PI: num_complex::Complex64 = real!(std::f64::consts::PI);
const ZERO: num_complex::Complex64 = real!(0.0);
const ONE: num_complex::Complex64 = real!(1.0);
const I: num_complex::Complex64 = imag!(1.0);

fn simplify_function_call(func: &ExpressionFunction, expr: &Expression) -> Expression {
    // Evaluate numbers and π
    // Pass through otherwise
    match (func, simplify(expr)) {
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
            function: *function,
            expression: expression.into(),
        }),
    }
}

#[inline]
fn is_zero(x: &num_complex::Complex64) -> bool {
    x.norm() < 1e-10
}

#[inline]
fn is_one(x: &num_complex::Complex64) -> bool {
    (x - 1.0).norm() < 1e-10
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

fn simplify_infix(l: &Expression, op: &InfixOperator, r: &Expression) -> Expression {
    // There are … many cases here
    match (&simplify(l), op, &simplify(r)) {
        //----------------------------------------------------------------
        // First: only diving one deep, pattern matching on the operation
        // (Constant folding and cancellations, mostly)
        //----------------------------------------------------------------

        // Addition and Subtraction

        // Adding with zero
        (Expression::Number(x), InfixOperator::Plus, right) if is_zero(x) => right.clone(),
        (left, InfixOperator::Plus, Expression::Number(x)) if is_zero(x) => left.clone(),
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
            simplify_prefix(&PrefixOperator::Minus, right)
        }
        (left, InfixOperator::Minus, Expression::Number(y)) if is_zero(y) => left.clone(),
        // Subtracting self
        (left, InfixOperator::Minus, right) if left == right => Expression::Number(real!(0.0)),
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
        (Expression::Number(x), InfixOperator::Star, right) if is_one(x) => right.clone(),
        (left, InfixOperator::Star, Expression::Number(y)) if is_one(y) => left.clone(),
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
        (left, InfixOperator::Slash, Expression::Number(y)) if is_one(y) => left.clone(),
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
        (left, InfixOperator::Caret, Expression::Number(y)) if is_one(y) => left.clone(),

        //----------------------------------------------------------------
        // Next: dealing with negation in subexpressions
        //----------------------------------------------------------------

        // Addition with negation
        (
            left,
            InfixOperator::Plus,
            Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                ref expression,
            }),
        ) => simplify_infix(left, &InfixOperator::Minus, expression),
        (
            Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                ref expression,
            }),
            InfixOperator::Plus,
            right,
        ) => simplify_infix(right, &InfixOperator::Minus, expression),

        // Subtraction with negation
        (
            left,
            InfixOperator::Minus,
            Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                ref expression,
            }),
        ) => simplify_infix(left, &InfixOperator::Plus, expression),
        (
            // -expression - right => smaller of (-expression) - right & -(expression + right)
            left @ &Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                ref expression,
            }),
            InfixOperator::Minus,
            right,
        ) => {
            let original = sub!(left.clone(), right.clone());
            let new = simplify_prefix(
                &PrefixOperator::Minus,
                &add!(expression.clone(), right.clone()),
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
        ) => simplify_infix(left, &InfixOperator::Star, right),
        (
            left,
            InfixOperator::Star,
            right @ &Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                ref expression,
            }),
        ) => {
            let original = mul!(left.clone(), right.clone());
            let neg_left = simplify_prefix(&PrefixOperator::Minus, left);
            let new = mul!(neg_left, expression.clone());
            min_by_key(original, new, size)
        }
        (
            left @ &Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                ref expression,
            }),
            InfixOperator::Star,
            right,
        ) => {
            let original = mul!(left.clone(), right.clone());
            let neg_right = simplify_prefix(&PrefixOperator::Minus, right);
            let new = mul!(expression.clone(), neg_right);
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
        ) => simplify_infix(left, &InfixOperator::Slash, right),
        (
            left,
            InfixOperator::Slash,
            Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                ref expression,
            }),
        ) if *left == **expression => Expression::Number(-ONE),
        (
            left,
            InfixOperator::Slash,
            right @ &Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                ref expression,
            }),
        ) => {
            let original = div!(left.clone(), right.clone());
            let neg_left = simplify_prefix(&PrefixOperator::Minus, left);
            let new = div!(neg_left, expression.clone());
            min_by_key(original, new, size)
        }
        (
            Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                ref expression,
            }),
            InfixOperator::Slash,
            right,
        ) if **expression == *right => Expression::Number(-ONE),
        (
            left @ &Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                ref expression,
            }),
            InfixOperator::Slash,
            right,
        ) => {
            let original = div!(left.clone(), right.clone());
            let neg_right = simplify_prefix(&PrefixOperator::Minus, right);
            let new = div!(expression.clone(), neg_right);
            min_by_key(original, new, size)
        }

        //----------------------------------------------------------------
        // Also: Affine relationships
        //----------------------------------------------------------------

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
        ) if left_x == right_x => {
            mul!(
                simplify_infix(left_a, &InfixOperator::Plus, right_a),
                left_x.clone()
            )
        }

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
        ) if left_x == right_x => {
            add!(
                mul!(Expression::Number(ONE + ONE), left_x.clone()),
                simplify_infix(left_b, &InfixOperator::Plus, right_b)
            )
        }

        //----------------------------------------------------------------
        // After that: association, distribution
        //----------------------------------------------------------------

        // Addition Associative, right
        (
            a,
            InfixOperator::Plus,
            right @ Expression::Infix(InfixExpression {
                left: ref b,
                operator: InfixOperator::Plus,
                right: ref c,
            }),
        ) => {
            let original = add!(a.clone(), right.clone());
            let new_ab = simplify_infix(a, &InfixOperator::Plus, b);
            let new = simplify_infix(&new_ab, &InfixOperator::Plus, c);
            min_by_key(original, new, size)
        }

        // Multipliation Associative, right
        (
            a,
            InfixOperator::Star,
            right @ Expression::Infix(InfixExpression {
                left: ref b,
                operator: InfixOperator::Star,
                right: ref c,
            }),
        ) => {
            let original = mul!(a.clone(), right.clone());
            let new_ab = simplify_infix(a, &InfixOperator::Star, b);
            let new = simplify_infix(&new_ab, &InfixOperator::Star, c);
            min_by_key(original, new, size)
        }

        // Subtraction "association" (not really)
        (
            a,
            InfixOperator::Minus,
            right @ Expression::Infix(InfixExpression {
                left: ref b,
                operator: InfixOperator::Minus,
                right: ref c,
            }),
        ) => {
            let original = sub!(a.clone(), right.clone());
            let new_left = simplify_infix(a, &InfixOperator::Plus, c);
            let new = simplify_infix(&new_left, &InfixOperator::Minus, b);
            min_by_key(original, new, size)
        }

        // Division "association" (not really)
        (
            a,
            InfixOperator::Slash,
            right @ Expression::Infix(InfixExpression {
                left: ref b,
                operator: InfixOperator::Slash,
                right: ref c,
            }),
        ) => {
            let original = div!(a.clone(), right.clone());
            let new_left = simplify_infix(a, &InfixOperator::Star, c);
            let new = simplify_infix(&new_left, &InfixOperator::Slash, b);
            min_by_key(original, new, size)
        }

        // Right distribution
        (
            a,
            InfixOperator::Star,
            right @ Expression::Infix(InfixExpression {
                left: ref b,
                operator: InfixOperator::Plus,
                right: ref c,
            }),
        ) => {
            let original = mul!(a.clone(), right.clone());
            let ab = simplify_infix(a, &InfixOperator::Star, b);
            let ac = simplify_infix(a, &InfixOperator::Star, c);
            let new = simplify_infix(&ab, &InfixOperator::Plus, &ac);
            min_by_key(original, new, size)
        }

        // Left distribution
        (
            left @ Expression::Infix(InfixExpression {
                left: ref a,
                operator: InfixOperator::Plus,
                right: ref b,
            }),
            InfixOperator::Star,
            c,
        ) => {
            let original = mul!(left.clone(), c.clone());
            let ac = simplify_infix(a, &InfixOperator::Star, c);
            let bc = simplify_infix(b, &InfixOperator::Star, c);
            let new = simplify_infix(&ac, &InfixOperator::Plus, &bc);
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
            denominator,
        ) if **multiplicand == *denominator => *multiplier.clone(),

        // Mul inside Div on left
        (
            numerator @ &Expression::Infix(InfixExpression {
                left: ref multiplier,
                operator: InfixOperator::Star,
                right: ref multiplicand,
            }),
            InfixOperator::Slash,
            denominator,
        ) => {
            let original = div!(numerator.clone(), denominator.clone());
            let new_multiplicand = simplify_infix(multiplicand, &InfixOperator::Slash, denominator);
            let new = simplify_infix(&multiplier, &InfixOperator::Star, &new_multiplicand);
            min_by_key(original, new, size)
        }

        // Mul inside Div on right
        (
            numerator,
            InfixOperator::Slash,
            denominator @ &Expression::Infix(InfixExpression {
                left: ref multiplier,
                operator: InfixOperator::Star,
                right: ref multiplicand,
            }),
        ) => {
            let original = div!(numerator.clone(), denominator.clone());
            let new_multiplier = simplify_infix(numerator, &InfixOperator::Slash, multiplier);
            let new = simplify_infix(&new_multiplier, &InfixOperator::Star, multiplicand);
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
            multiplicand,
        ) if **denominator == *multiplicand => *numerator.clone(),

        // Div inside Mul on right, denominator = multiplicand
        (
            multiplicand,
            InfixOperator::Star,
            Expression::Infix(InfixExpression {
                left: ref numerator,
                operator: InfixOperator::Slash,
                right: ref denominator,
            }),
        ) if **denominator == *multiplicand => *numerator.clone(),

        // Catch-all
        (left, operator, right) => Expression::Infix(InfixExpression {
            left: left.clone().into(),
            operator: *operator,
            right: right.clone().into(),
        }),
    }
}

fn simplify_prefix(op: &PrefixOperator, expr: &Expression) -> Expression {
    // Remove +
    // Push - into numbers & π
    // Pass through otherwise
    match (op, simplify(expr)) {
        (PrefixOperator::Plus, expression) => expression,
        (PrefixOperator::Minus, Expression::Number(x)) => Expression::Number(-x),
        (PrefixOperator::Minus, Expression::PiConstant) => Expression::Number(-PI),
        (operator, expression) => Expression::Prefix(PrefixExpression {
            operator: *operator,
            expression: expression.into(),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    macro_rules! test_simplify {
        ($name:ident, $input:expr, $expected:expr) => {
            #[test]
            fn $name() {
                let parsed_input = Expression::from_str($input);
                assert!(parsed_input.is_ok(), "Parsing input `{}` failed!", $input);
                let parsed_expected = Expression::from_str($expected);
                assert!(
                    parsed_expected.is_ok(),
                    "Parsing expected expression `{}` failed!",
                    $expected
                );
                let output = simplify(&parsed_input.unwrap());
                assert_eq!(
                    parsed_expected.unwrap(),
                    output,
                    "Simplifying `{}` yielded `{}` instead of the expected `{}`",
                    $input,
                    output,
                    $expected
                );
            }
        };
    }

    test_simplify! {
        docstring_example,
        "cos(2 * pi) + 2",
        "3"
    }

    test_simplify! {
        issue_208_1,
        "0 * theta[0]",
        "0"
    }

    test_simplify! {
        issue_208_2,
        "theta[0] / 1",
        "theta[0]"
    }

    test_simplify! {
        issue_208_3,
        "(theta[0] * 5) / 5",
        "theta"
    }

    test_simplify! {
        memory_ref,
        "theta[0]",
        "theta[0]"
    }

    test_simplify! {
        var,
        "%foo",
        "%foo"
    }

    test_simplify! {
        prefix_neg,
        "-(-1)",
        "1"
    }

    test_simplify! {
        sub_neg,
        "2 - (-1)",
        "3"
    }

    test_simplify! {
        neg_sub,
        "-(1 - 2)",
        "1"
    }

    test_simplify! {
        fold_constant_mul,
        "2 * pi",
        "6.283185307179586"
    }

    test_simplify! {
        fold_constant_mul_div,
        "(2 * pi) / 6.283185307179586",
        "1"
    }

    test_simplify! {
        fold_constant_mul_div_2,
        "2 * (pi / 6.283185307179586)",
        "1"
    }

    test_simplify! {
        fold_constant_mul_div_with_ref,
        "((a[0] * 2) * pi) / 6.283185307179586",
        "a[0]"
    }

    test_simplify! {
        fold_constant_mul_div_with_ref_2,
        "a[0] * (2 * pi) / 6.283185307179586",
        "a[0]"
    }

    test_simplify! {
        fold_constant_mul_div_with_ref_3,
        "a[0] * (2 * (pi / 6.283185307179586))",
        "a[0]"
    }

    test_simplify! {
        affine,
        "(2 * x[0] + 3) + (4 * x[0] + 5)",
        "6 * x[0] + 8"
    }

    test_simplify! {
        affine_2,
        "2 * x[0] + (4 * x[0] + 5)",
        "6 * x[0] + 5"
    }

    test_simplify! {
        affine_3,
        "2 * x[0] + 4 * x[0]",
        "6 * x[0]"
    }

    test_simplify! {
        affine_4,
        "(x[0] + 3) + (x[0] + 5)",
        "2 * x[0] + 8"
    }
}
