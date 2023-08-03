/// Complex machinery for simplifying [`Expression`]s.
use crate::expression::{
    imag, is_small, real, Expression, ExpressionFunction, FunctionCallExpression, InfixExpression,
    InfixOperator, PrefixExpression, PrefixOperator,
};
use std::f64::consts::PI;

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
    // Evaluate numbers and π; pass through otherwise
    match (func, simplify(expr)) {
        (ExpressionFunction::Cis, Expression::Number(x)) => {
            // num_complex::Complex::<f64>::cis only accpets f64 :-(
            Expression::Number(x.cos() + imag!(1f64) * x.sin())
        }
        (ExpressionFunction::Cis, Expression::PiConstant) => Expression::Number(real!(-1f64)),
        (ExpressionFunction::Cosine, Expression::Number(x)) => Expression::Number(x.cos()),
        (ExpressionFunction::Cosine, Expression::PiConstant) => Expression::Number(real!(-1f64)),
        (ExpressionFunction::Exponent, Expression::Number(x)) => Expression::Number(x.exp()),
        (ExpressionFunction::Exponent, Expression::PiConstant) => {
            Expression::Number(real!(PI).exp())
        }
        (ExpressionFunction::Sine, Expression::Number(x)) => Expression::Number(x.sin()),
        (ExpressionFunction::Sine, Expression::PiConstant) => Expression::Number(real!(PI).sin()),
        (ExpressionFunction::SquareRoot, Expression::Number(x)) => Expression::Number(x.sqrt()),
        (ExpressionFunction::SquareRoot, Expression::PiConstant) => {
            Expression::Number(real!(PI).sqrt())
        }
        (function, expression) => Expression::FunctionCall(FunctionCallExpression {
            function,
            expression: expression.into(),
        }),
    }
}

fn simplify_infix(l: Expression, op: InfixOperator, r: Expression) -> Expression {
    match (simplify(l), op, simplify(r)) {
        (left, operator, right) => Expression::Infix(InfixExpression {
            left: left.into(),
            operator,
            right: right.into(),
        }),
    }
}

fn simplify_prefix(op: PrefixOperator, expr: Expression) -> Expression {
    // Remove +; push - into numbers & π; pass through otherwise
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
        ($name:ident, $input:expr, $expected:expr$(,)?) => {
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
        neg_imag,
        "-(9.48e42i)",
        "-9.48e42i"
    }

    test_simplify! {
        pow_neg_address,
        "(-(9.48e42i))^A[9]",
        "(-9.48e42i)^A[9]",
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
