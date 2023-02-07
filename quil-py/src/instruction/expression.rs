use std::str::FromStr;

use pyo3::{exceptions::PyValueError, pymethods, PyResult, Python};
use quil_rs::expression::{
    Expression, ExpressionFunction, FunctionCallExpression, InfixExpression, InfixOperator,
};
use rigetti_pyo3::{py_wrap_data_struct, py_wrap_union_enum, ToPython};

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

#[pymethods]
impl PyExpression {
    #[staticmethod]
    fn parse_from_str(py: Python<'_>, expression: String) -> PyResult<Self> {
        Expression::from_str(&expression)
            .map_err(|e| PyValueError::new_err(e.to_string()))?
            .to_python(py)
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
