use quil_rs::expression::ExpressionFunction;
use rigetti_pyo3::py_wrap_union_enum;

py_wrap_union_enum! {
    PyExpressionFunction(ExpressionFunction) as "ExpressionFunction" {
        cis: Cis,
        cosine: Cosine,
        exponent: Exponent,
        sine: Sine,
        square_root: SquareRoot
    }
}

// TODO: This macro needs to implement IntoPy for fieldless enums
// py_wrap_data_struct! {
//     PyFunctionCallExpression(FunctionCallExpression) as "FunctionCallExpression" {
//         function: ExpressionFunction => PyExpressionFunction
//         // expression: Box<Expression> => PyExpression,
//     }
// }

// use crate::py_wrap_data_struct;
// use quil_rs::expression::Expression;
//
// py_wrap_union_enum! {
//     PyExpression(Expression) as "Expression" {
//         address: MemoryReference => PyMemoryReference,
//         ...
//     }
// }
