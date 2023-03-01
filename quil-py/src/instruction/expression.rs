use std::str::FromStr;

use pyo3::types::PyComplex;
use quil_rs::expression::{
    Expression, ExpressionFunction, FunctionCallExpression, InfixExpression, InfixOperator,
    PrefixExpression, PrefixOperator,
};

use rigetti_pyo3::{
    impl_from_str, impl_parse, impl_repr, impl_str, py_wrap_data_struct, py_wrap_union_enum,
    pyo3::{exceptions::PyValueError, pymethods, types::PyString, Py, PyResult, Python},
    ToPython,
};

use super::PyMemoryReference;

py_wrap_union_enum! {
    PyExpression(Expression) as "Expression" {
        address: Address => PyMemoryReference,
        function_call: FunctionCall => PyFunctionCallExpression,
        infix: Infix => PyInfixExpression,
        number: Number => Py<PyComplex>,
        pi: PiConstant,
        prefix: Prefix => PyPrefixExpression,
        variable: Variable => Py<PyString>
    }
}
impl_repr!(PyExpression);
impl_str!(PyExpression);

py_wrap_union_enum! {
    PyExpressionFunction(ExpressionFunction) as "ExpressionFunction" {
        cis: Cis,
        cosine: Cosine,
        exponent: Exponent,
        sine: Sine,
        square_root: SquareRoot
    }
}
impl_repr!(PyExpressionFunction);
impl_str!(PyExpressionFunction);

py_wrap_data_struct! {
   PyFunctionCallExpression(FunctionCallExpression) as "FunctionCallExpression" {
        function: ExpressionFunction => PyExpressionFunction,
        expression: Box<Expression> => PyExpression
    }
}
impl_repr!(PyFunctionCallExpression);

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
impl_repr!(PyInfixOperator);
impl_str!(PyInfixOperator);

py_wrap_data_struct! {
    PyInfixExpression(InfixExpression) as "InfixExpression" {
        left: Box<Expression> => PyExpression,
        operator: InfixOperator => PyInfixOperator,
        right: Box<Expression> => PyExpression
    }
}
impl_repr!(PyInfixExpression);

py_wrap_union_enum! {
    PyPrefixOperator(PrefixOperator) as "PrefixOperator" {
        plus: Plus,
        minus: Minus
    }
}

py_wrap_data_struct! {
    PyPrefixExpression(PrefixExpression) as "PrefixExpression" {
        operator: PrefixOperator => PyPrefixOperator,
        expression: Box<Expression> => PyExpression
    }
}
