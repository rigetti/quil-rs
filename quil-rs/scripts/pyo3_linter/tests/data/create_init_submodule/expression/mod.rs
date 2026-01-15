#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pyclass_complex_enum};

#[cfg(feature = "python")]
pub(crate) mod quilpy;

#[cfg_attr(feature = "stubs", gen_stub_pyclass_complex_enum)]
#[cfg_attr(feature = "python", pyo3::pyclass(module = "quil.expression"))]
pub enum Expression {
    Number(f64),
    Variable(String),
}

#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(feature = "python", pyo3::pyclass(module = "quil.expression"))]
pub struct ExpressionFunction {
    pub name: String,
}

#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(feature = "python", pyo3::pyclass(module = "quil.expression"))]
pub struct FunctionCallExpression {
    pub function: ExpressionFunction,
}
