use pyo3::prelude::*;

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::gen_stub_pymethods;

use super::*;
use crate::impl_repr;

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
    Ok(())
}

impl_repr!(Expression);
impl_repr!(ExpressionFunction);
impl_repr!(FunctionCallExpression);
impl_repr!(InfixExpression);
impl_repr!(InfixOperator);
impl_repr!(PrefixExpression);
impl_repr!(PrefixOperator);

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Expression {
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
