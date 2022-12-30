use quil_rs::expression::{Expression, ExpressionFunction, FunctionCallExpression};
use rigetti_pyo3::{py_wrap_data_struct, py_wrap_type, py_wrap_union_enum};

use super::memory_reference::PyMemoryReference;

py_wrap_union_enum! {
    PyExpressionFunction(ExpressionFunction) as "ExpressionFunction" {
        cis: Cis,
        cosine: Cosine,
        exponent: Exponent,
        sine: Sine,
        square_root: SquareRoot
    }
}

py_wrap_data_struct! {
   PyFunctionCallExpression(FunctionCallExpression) as "FunctionCallExpression" {
        function: ExpressionFunction => PyExpressionFunction
        // expression: Box<Expression> => PyExpression,
    }
}

py_wrap_union_enum! {
    PyExpression(Expression) as "Expression" {
        address: Address => PyMemoryReference,
        function_call: FunctionCall => PyFunctionCallExpression
    }
}

pub type Expressions = Vec<Expression>;
py_wrap_type! {
    #[derive(Debug)]
    PyExpressions(Expressions) as "Expressions"
}
