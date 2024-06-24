use std::collections::HashMap;

use quil_rs::expression::{
    Expression, ExpressionFunction, FunctionCallExpression, InfixExpression, InfixOperator,
    PrefixExpression, PrefixOperator,
};

use rigetti_pyo3::{
    create_init_submodule, impl_from_str, impl_hash, impl_parse, impl_repr, impl_str,
    num_complex::Complex64,
    py_wrap_data_struct, py_wrap_error, py_wrap_simple_enum, py_wrap_union_enum,
    pyo3::{
        exceptions::PyValueError,
        pymethods,
        types::{PyComplex, PyString},
        Py, PyResult, Python,
    },
    wrap_error, PyTryFrom, PyWrapper, PyWrapperMut, ToPython, ToPythonError,
};

use crate::{impl_eq, impl_to_quil, instruction::PyMemoryReference};

wrap_error!(RustEvaluationError(quil_rs::expression::EvaluationError));
py_wrap_error!(quil, RustEvaluationError, EvaluationError, PyValueError);
wrap_error!(RustParseExpressionError(quil_rs::program::ParseProgramError<Expression>));
py_wrap_error!(
    quil,
    RustParseExpressionError,
    ParseExpressionError,
    PyValueError
);

py_wrap_union_enum! {
    #[derive(Debug, Hash, PartialEq, Eq)]
    #[pyo3(module="quil.expression")]
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
impl_to_quil!(PyExpression);
impl_from_str!(PyExpression, RustParseExpressionError);
impl_hash!(PyExpression);
impl_parse!(PyExpression);
impl_eq!(PyExpression);

#[pymethods]
impl PyExpression {
    pub fn simplify(&mut self) {
        self.as_inner_mut().simplify()
    }

    pub fn into_simplified(&self, py: Python<'_>) -> PyResult<Self> {
        self.as_inner().clone().into_simplified().to_python(py)
    }

    pub fn evaluate(
        &self,
        variables: HashMap<String, Complex64>,
        memory_references: HashMap<&str, Vec<f64>>,
    ) -> PyResult<Complex64> {
        self.as_inner()
            .evaluate(&variables, &memory_references)
            .map_err(RustEvaluationError::from)
            .map_err(RustEvaluationError::to_py_err)
    }

    pub fn substitute_variables(
        &self,
        py: Python<'_>,
        variable_values: HashMap<String, PyExpression>,
    ) -> PyResult<Self> {
        Ok(PyExpression(self.as_inner().clone().substitute_variables(
            &HashMap::<String, Expression>::py_try_from(py, &variable_values)?,
        )))
    }

    pub fn to_real(&self) -> PyResult<f64> {
        self.as_inner()
            .to_real()
            .map_err(RustEvaluationError::from)
            .map_err(RustEvaluationError::to_py_err)
    }

    pub fn __add__(&self, other: PyExpression) -> Self {
        PyExpression(self.as_inner().clone() + other.as_inner().clone())
    }

    pub fn __sub__(&self, other: PyExpression) -> Self {
        PyExpression(self.as_inner().clone() - other.as_inner().clone())
    }

    pub fn __mul__(&self, other: PyExpression) -> Self {
        PyExpression(self.as_inner().clone() * other.as_inner().clone())
    }

    pub fn __truediv__(&self, other: PyExpression) -> Self {
        PyExpression(self.as_inner().clone() / other.as_inner().clone())
    }
}

py_wrap_data_struct! {
    #[pyo3(subclass)]
    #[derive(Debug)]
    PyFunctionCallExpression(FunctionCallExpression) as "FunctionCallExpression" {
        function: ExpressionFunction => PyExpressionFunction,
        expression: Box<Expression> => PyExpression
    }
}
impl_repr!(PyFunctionCallExpression);

#[pymethods]
impl PyFunctionCallExpression {
    #[new]
    pub fn new(
        py: Python<'_>,
        function: PyExpressionFunction,
        expression: PyExpression,
    ) -> PyResult<Self> {
        Ok(PyFunctionCallExpression(FunctionCallExpression::new(
            ExpressionFunction::py_try_from(py, &function)?,
            Box::<Expression>::py_try_from(py, &expression)?,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug)]
    #[pyo3(subclass)]
    PyInfixExpression(InfixExpression) as "InfixExpression" {
        left: Box<Expression> => PyExpression,
        operator: InfixOperator => PyInfixOperator,
        right: Box<Expression> => PyExpression
    }
}
impl_repr!(PyInfixExpression);

#[pymethods]
impl PyInfixExpression {
    #[new]
    pub fn new(
        py: Python<'_>,
        left: PyExpression,
        operator: PyInfixOperator,
        right: PyExpression,
    ) -> PyResult<Self> {
        Ok(PyInfixExpression(InfixExpression::new(
            Box::<Expression>::py_try_from(py, &left)?,
            InfixOperator::py_try_from(py, &operator)?,
            Box::<Expression>::py_try_from(py, &right)?,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug)]
    #[pyo3(subclass)]
    PyPrefixExpression(PrefixExpression) as "PrefixExpression" {
        operator: PrefixOperator => PyPrefixOperator,
        expression: Box<Expression> => PyExpression
    }
}

#[pymethods]
impl PyPrefixExpression {
    #[new]
    pub fn new(
        py: Python<'_>,
        operator: PyPrefixOperator,
        expression: PyExpression,
    ) -> PyResult<Self> {
        Ok(PyPrefixExpression(PrefixExpression::new(
            PrefixOperator::py_try_from(py, &operator)?,
            Box::<Expression>::py_try_from(py, &expression)?,
        )))
    }
}

py_wrap_simple_enum! {
    #[derive(Debug, PartialEq, Eq, Hash)]
    PyExpressionFunction(ExpressionFunction) as "ExpressionFunction" {
        Cis,
        Cosine,
        Exponent,
        Sine,
        SquareRoot
    }
}
impl_repr!(PyExpressionFunction);
impl_str!(PyExpressionFunction);
impl_hash!(PyExpressionFunction);
impl_eq!(PyExpressionFunction);

py_wrap_simple_enum! {
    #[derive(Debug, PartialEq, Eq, Hash)]
    PyPrefixOperator(PrefixOperator) as "PrefixOperator" {
        Plus,
        Minus
    }
}
impl_repr!(PyPrefixOperator);
impl_str!(PyPrefixOperator);
impl_hash!(PyPrefixOperator);
impl_eq!(PyPrefixOperator);

py_wrap_simple_enum! {
    #[derive(Debug, PartialEq, Eq, Hash)]
    PyInfixOperator(InfixOperator) as "InfixOperator" {
        Caret,
        Plus,
        Minus,
        Slash,
        Star
    }
}
impl_repr!(PyInfixOperator);
impl_str!(PyInfixOperator);
impl_hash!(PyInfixOperator);
impl_eq!(PyInfixOperator);

create_init_submodule! {
    classes: [PyExpression, PyFunctionCallExpression, PyInfixExpression, PyPrefixExpression, PyExpressionFunction, PyPrefixOperator, PyInfixOperator],
    errors: [EvaluationError, ParseExpressionError],
}
