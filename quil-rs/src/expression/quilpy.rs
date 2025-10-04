use pyo3::{prelude::*, types::PyTuple};

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::gen_stub_pymethods;

use super::*;
use crate::quilpy::{errors::ValueError, fix_complex_enums, impl_repr};

#[pymodule]
#[pyo3(name = "expression", module = "quil", submodule)]
pub(crate) fn init_submodule(m: &Bound<'_, PyModule>) -> PyResult<()> {
    use crate::quilpy::errors;

    let py = m.py();
    m.add("EvaluationError", py.get_type::<errors::EvaluationError>())?;
    m.add(
        "ParseExpressionError",
        py.get_type::<errors::ParseExpressionError>(),
    )?;

    m.add_class::<Expression>()?;
    m.add_class::<ExpressionFunction>()?;
    m.add_class::<FunctionCallExpression>()?;
    m.add_class::<InfixExpression>()?;
    m.add_class::<InfixOperator>()?;
    m.add_class::<PrefixExpression>()?;
    m.add_class::<PrefixOperator>()?;

    fix_complex_enums!(py, Expression);

    Ok(())
}

impl_repr!(Expression);
impl_repr!(ExpressionFunction);
impl_repr!(FunctionCallExpression);
impl_repr!(InfixExpression);
impl_repr!(InfixOperator);
impl_repr!(PrefixExpression);
impl_repr!(PrefixOperator);

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Expression {
    /// Return an expression derived from this one, simplified as much as possible.
    #[pyo3(name = "into_simplified")]
    fn py_into_simplified(&self) -> Self {
        self.clone().into_simplified()
    }

    /// Evaluate an expression, expecting that it may be fully reduced to a single complex number.
    ///
    /// If it cannot be reduced to a complex number, this raises an error.
    #[pyo3(name = "evaluate")]
    fn py_evaluate(
        &self,
        variables: HashMap<String, Complex64>,
        memory_references: HashMap<String, Vec<f64>>,
    ) -> PyResult<Complex64> {
        Ok(self.evaluate(&variables, &memory_references)?)
    }

    /// Substitute an expression in the place of each matching variable.
    #[pyo3(name = "substitute_variables")]
    fn py_substitute_variables(&self, variable_values: HashMap<String, Expression>) -> Self {
        self.substitute_variables(&variable_values)
    }

    fn __add__(&self, other: Expression) -> Self {
        self.clone() + other
    }

    fn __sub__(&self, other: Expression) -> Self {
        self.clone() - other
    }

    fn __mul__(&self, other: Expression) -> Self {
        self.clone() * other
    }

    fn __truediv__(&self, other: Expression) -> Self {
        self.clone() / other
    }

    /// Parse an ``Expression`` from a string.
    ///
    /// Raises a ``ParseExpressionError`` error if the string isn't a valid Quil expression.
    #[staticmethod]
    fn parse(input: &str) -> PyResult<Self> {
        Ok(<Self as std::str::FromStr>::from_str(input)?)
    }

    #[gen_stub(override_return_type(
        type_repr = "tuple[MemoryReference | FunctionCallExpression | InfixExpression | complex | PrefixExpression | str]"
    ))]
    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        match self {
            Self::Address(value) => (value.clone(),).into_pyobject(py),
            Self::FunctionCall(value) => (value.clone(),).into_pyobject(py),
            Self::Infix(value) => (value.clone(),).into_pyobject(py),
            Self::Number(value) => (value,).into_pyobject(py),
            Self::PiConstant() => (Self::PiConstant(),).into_pyobject(py),
            Self::Prefix(value) => (value.clone(),).into_pyobject(py),
            Self::Variable(value) => (value.clone(),).into_pyobject(py),
        }
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl InfixExpression {
    #[new]
    fn __new__(left: Expression, operator: InfixOperator, right: Expression) -> Self {
        Self::new(ArcIntern::new(left), operator, ArcIntern::new(right))
    }

    fn __getnewargs__(&self) -> (Expression, InfixOperator, Expression) {
        (self.left(), self.operator, self.right())
    }

    #[getter]
    fn left(&self) -> Expression {
        (*self.left).clone()
    }

    #[getter]
    fn right(&self) -> Expression {
        (*self.right).clone()
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PrefixExpression {
    #[new]
    fn __new__(operator: PrefixOperator, expression: Expression) -> Self {
        Self::new(operator, ArcIntern::new(expression))
    }

    fn __getnewargs__(&self) -> (PrefixOperator, Expression) {
        (self.operator, self.expression())
    }

    #[getter]
    fn expression(&self) -> Expression {
        (*self.expression).clone()
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl FunctionCallExpression {
    #[new]
    fn __new__(function: ExpressionFunction, expression: Expression) -> Self {
        Self::new(function, ArcIntern::new(expression))
    }

    fn __getnewargs__(&self) -> (ExpressionFunction, Expression) {
        (self.function, self.expression())
    }

    #[getter]
    fn expression(&self) -> Expression {
        (*self.expression).clone()
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl ExpressionFunction {
    #[new]
    fn __new__(value: isize) -> PyResult<Self> {
        match value {
            val if val == Self::Cis as isize => Ok(Self::Cis),
            val if val == Self::Cosine as isize => Ok(Self::Cosine),
            val if val == Self::Exponent as isize => Ok(Self::Exponent),
            val if val == Self::Sine as isize => Ok(Self::Sine),
            val if val == Self::SquareRoot as isize => Ok(Self::SquareRoot),
            _ => Err(ValueError::new_err("unknown value")),
        }
    }

    fn __getnewargs__(&self) -> (isize,) {
        (*self as isize,)
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PrefixOperator {
    #[new]
    fn __new__(value: isize) -> PyResult<Self> {
        match value {
            val if val == Self::Plus as isize => Ok(Self::Plus),
            val if val == Self::Minus as isize => Ok(Self::Minus),
            _ => Err(ValueError::new_err("unknown value")),
        }
    }

    fn __getnewargs__(&self) -> (isize,) {
        (*self as isize,)
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl InfixOperator {
    #[new]
    fn __new__(value: isize) -> PyResult<Self> {
        match value {
            val if val == Self::Caret as isize => Ok(Self::Caret),
            val if val == Self::Plus as isize => Ok(Self::Plus),
            val if val == Self::Minus as isize => Ok(Self::Minus),
            val if val == Self::Slash as isize => Ok(Self::Slash),
            val if val == Self::Star as isize => Ok(Self::Star),
            _ => Err(ValueError::new_err("unknown value")),
        }
    }

    fn __getnewargs__(&self) -> (isize,) {
        (*self as isize,)
    }
}
