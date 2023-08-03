/// Complex machinery for simplifying [`Expression`]s.
use crate::expression::{
    imag, real, Expression, ExpressionFunction, FunctionCallExpression, InfixExpression,
    InfixOperator, PrefixExpression, PrefixOperator,
};

const PI: num_complex::Complex64 = real!(std::f64::consts::PI);
const ZERO: num_complex::Complex64 = real!(0.0);
const ONE: num_complex::Complex64 = real!(1.0);
const I: num_complex::Complex64 = imag!(1.0);

/// Simplify an [`Expression`].
pub(super) fn run(expression: Expression) -> Expression {
    simplify(expression)
}

fn simplify(e: Expression) -> Expression {
    match e {
        Expression::Address(_)
        | Expression::Number(_)
        | Expression::PiConstant
        | Expression::Variable(_) => e,
        Expression::FunctionCall(FunctionCallExpression {
            function,
            expression,
        }) => simplify_function_call(function, *expression),
        Expression::Infix(InfixExpression {
            left,
            operator,
            right,
        }) => simplify_infix(*left, operator, *right),
        Expression::Prefix(PrefixExpression {
            operator,
            expression,
        }) => simplify_prefix(operator, *expression),
    }
}

fn simplify_function_call(func: ExpressionFunction, expr: Expression) -> Expression {
    // Evaluate numbers and π
    // Pass through otherwise
    match (func, simplify(expr)) {
        (ExpressionFunction::Cis, Expression::Number(x)) => {
            // num_complex::Complex64::cis only accpets f64 :-(
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
    (x - 1.0).norm() < 1e-10
}

fn simplify_infix(l: Expression, op: InfixOperator, r: Expression) -> Expression {
    // There are … many cases here
    match (simplify(l), op, simplify(r)) {
        // + & -

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
            simplify(Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                expression: right.into(),
            }))
        }
        (left, InfixOperator::Minus, Expression::Number(y)) if is_zero(y) => left,
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

        // * & /

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

        // Catch-all
        (left, operator, right) => Expression::Infix(InfixExpression {
            left: left.into(),
            operator,
            right: right.into(),
        }),
    }
}

fn simplify_prefix(op: PrefixOperator, expr: Expression) -> Expression {
    // Remove +
    // Push - into numbers & π
    // Pass through otherwise
    match (op, simplify(expr)) {
        (PrefixOperator::Plus, expression) => expression,
        (PrefixOperator::Minus, Expression::Number(x)) => Expression::Number(-x),
        (PrefixOperator::Minus, Expression::PiConstant) => Expression::Number(real!(-PI)),
        (operator, expression) => Expression::Prefix(PrefixExpression {
            operator,
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
                let output = simplify(parsed_input.unwrap());
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
        neg_sub,
        "-(1 - 2)",
        "1"
    }

    test_simplify! {
        pow_neg_address,
        "(-(9.48e42i))^A[9]",
        "(-9.48e42i)^A[9]"
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
        fold_constant_mul_div_with_ref,
        "((a[0] * 2) * pi) / 6.283185307179586",
        "a[0]"
    }
}
