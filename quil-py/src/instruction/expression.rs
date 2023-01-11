use quil_rs::expression::{
    Expression, ExpressionFunction, FunctionCallExpression, InfixExpression, InfixOperator,
};
use rigetti_pyo3::{py_wrap_data_struct, py_wrap_union_enum};

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

// TODO: Keeping for reference, implemented locally via `py_wrap_type!`
// impl ToPython<PyExpression> for &Box<Expression> {
//     fn to_python(&self, _py: Python) -> PyResult<PyExpression> {
//         Ok(self.as_ref().into())
//     }
// }

// impl PyTryFrom<PyExpression> for Box<Expression> {
//     fn py_try_from(_py: Python<'_>, item: &PyExpression) -> PyResult<Box<Expression>> {
//         Ok(Box::new(Expression::from(item.clone())))
//     }
// }

py_wrap_data_struct! {
   PyFunctionCallExpression(FunctionCallExpression) as "FunctionCallExpression" {
        function: ExpressionFunction => PyExpressionFunction,
        expression: Box<Expression> => PyExpression
    }
}

py_wrap_union_enum! {
    PyExpression(Expression) as "Expression" {
        address: Address => PyMemoryReference,
        function_call: FunctionCall => PyFunctionCallExpression
    }
}

py_wrap_union_enum! {
    PyInfixOperator(InfixOperator) as "InfixOperator" {
        caret: Caret,
        plus: Plus,
        minus: Minus,
        slash: Slash,
        star: Star
    }
}

py_wrap_data_struct! {
    PyInfixExpression(InfixExpression) as "InfixExpression" {
        left: Box<Expression> => PyExpression,
        operator: InfixOperator => PyInfixOperator,
        right: Box<Expression> => PyExpression
    }
}
