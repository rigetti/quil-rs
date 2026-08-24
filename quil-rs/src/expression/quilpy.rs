use numpy::{PyArray, PyArrayDescr, PyArrayDescrMethods};
use pyo3::{
    exceptions::{PyDeprecationWarning, PyNotImplementedError},
    prelude::*,
    types::{PyAnyMethods, PyComplex, PyFloat, PyInt},
    IntoPyObjectExt,
};
use rigetti_pyo3::{create_init_submodule, impl_repr};

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::gen_stub_pymethods;

use super::*;
use crate::quilpy::{
    errors::{self, ValueError},
    impl_newargs, impl_to_quil, IntoNewArgs, NewArgs,
};

create_init_submodule! {
    classes: [
        ExpressionFunction,
        FunctionCallExpression,
        InfixExpression,
        InfixOperator,
        PrefixExpression,
        PrefixOperator
    ],
    complex_enums: [ Expression ],
    errors: [ errors::EvaluationError, errors::ParseExpressionError ],
}

pub(crate) fn post_init(m: &Bound<'_, PyModule>) -> PyResult<()> {
    use crate::quilpy::union;

    let py = m.py();

    // Add singleton instances to the module.
    // m.add("Pi", PiType::__new__(py)?)?;

    // Add TypeAliases for use in annotations.
    // These are mainly for PyQuil backwards-compatibility.
    #[expect(non_snake_case)]
    let ExpressionValueDesignator = union!(py, PyInt, PyFloat, PyComplex)?;
    m.add("ExpressionValueDesignator", &ExpressionValueDesignator)?;
    m.add(
        "ExpressionDesignator",
        py.get_type::<Expression>()
            .bitor(&ExpressionValueDesignator)?,
    )?;
    m.add(
        "ParameterDesignator",
        union!(py, Expression, MemoryReference, PyInt, PyFloat, PyComplex)?,
    )?;

    // TODO
    // ParameterSubstitutionsMapDesignator = Mapping[Expression.Variable | MemoryReference, ExpressionValueDesignator]

    Ok(())
}

impl_repr!(Expression);
impl_repr!(ExpressionFunction);
impl_repr!(FunctionCallExpression);
impl_repr!(InfixExpression);
impl_repr!(InfixOperator);
impl_repr!(PrefixExpression);
impl_repr!(PrefixOperator);

impl_to_quil!(Expression);

/// A type that can be converted into an `Expression`,
/// for use as a parameter in methods and functions exposed through Python bindings.
#[derive(Debug, Clone, FromPyObject)]
pub(crate) enum ExpressionLike {
    Variable(String),
    MemoryReference(MemoryReference),
    Expression(Expression),
    Int(i64),
    Float(f64),
    Complex(Complex64),
}

impl From<ExpressionLike> for Expression {
    fn from(value: ExpressionLike) -> Self {
        match value {
            ExpressionLike::Variable(name) => Expression::Variable(name),
            ExpressionLike::MemoryReference(memref) => Expression::Address(memref),
            ExpressionLike::Expression(expr) => expr,
            ExpressionLike::Int(v) => Expression::Number(Complex64::new(v as f64, 0.0)),
            ExpressionLike::Float(v) => Expression::Number(v.into()),
            ExpressionLike::Complex(v) => Expression::Number(v),
        }
    }
}

/// A key in a parameter substitution map.
///
/// This allows Python users to provide a single dictionary when calling `Expression.substitute`,
/// rather than needing to provide separate dictionaries for variables versus memory references.
#[derive(FromPyObject, Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum SubstitutionKey {
    Variable(String),
    MemoryReference(MemoryReference),
}

#[derive(FromPyObject, Debug, Clone)]
pub(crate) enum SubstitutionValue {
    Variable(Complex64),
    Memory(Vec<f64>),
}

impl From<SubstitutionKey> for Expression {
    fn from(key: SubstitutionKey) -> Self {
        match key {
            SubstitutionKey::Variable(name) => Expression::Variable(name),
            SubstitutionKey::MemoryReference(memref) => Expression::Address(memref),
        }
    }
}

/// The (potentially partial) result of evaluating an expression with substitutions.
///
/// This is a wrapper around `Expression` with a custom `IntoPyObject` implementation
/// to enable returning either a `Complex64` or an `Expression` to Python.
#[derive(IntoPyObject)]
enum Evaluated {
    Full(Complex64),
    Partial(Expression),
}

#[cfg(feature = "stubs")]
mod stubs {
    use pyo3_stub_gen::{derive::gen_methods_from_python, impl_stub_type, type_alias};

    #[allow(clippy::wildcard_imports)]
    use super::*;

    impl_stub_type!(ExpressionLike = Expression | MemoryReference | String | i64 | f64 | Complex64);
    impl_stub_type!(SubstitutionKey = String | MemoryReference);
    impl_stub_type!(SubstitutionValue = Complex64 | Vec<Complex64>);
    impl_stub_type!(Evaluated = Expression | Complex64);

    type_alias!(
        "quil._quil.expression",
        ExpressionValueDesignator = i64 | f64 | Complex64
    );
    type_alias!(
        "quil._quil.expression",
        ExpressionDesignator = Expression | i64 | f64 | Complex64
    );
    type_alias!(
        "quil._quil.expression",
        ParameterDesignator = ExpressionLike
    );

    pyo3_stub_gen::inventory::submit! {
        gen_methods_from_python! {
            r#"
            import builtins
            import typing

            class Expression:
                @overload
                def evaluate(
                    self,
                    variables: typing.Optional[typing.Mapping[builtins.str, builtins.complex]] = None,
                    memory_references: typing.Optional[typing.Mapping[builtins.str, builtins.list[builtins.float]]] = None,
                    /,
                    partial: typing.Literal[False] = False
                ) -> complex:
                    ...
            "#
        }
    }
}

impl_newargs!(
    ExpressionArgs = MemoryReference
        | FunctionCallExpression
        | InfixExpression
        | Complex64
        | PrefixExpression
        | String
);

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Expression {
    /// Create a new `Expression`.
    ///
    /// This constructor accepts existing `Expression` objects, `MemoryReference`s,
    /// numberic types (int, float, complex), or strings, which are interpreted as variable names.
    #[new]
    fn __new__(expression: ExpressionLike) -> Self {
        expression.into()
    }

    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<NewArgs<'py, ExpressionArgs>> {
        match self {
            Self::Address(value) => value.clone().into_new_args(py),
            Self::FunctionCall(value) => value.clone().into_new_args(py),
            Self::Infix(value) => value.clone().into_new_args(py),
            Self::Number(value) => value.into_new_args(py),
            Self::PiConstant() => Self::PiConstant().into_new_args(py),
            Self::Prefix(value) => value.clone().into_new_args(py),
            Self::Variable(value) => value.into_new_args(py),
        }
    }

    /// Return an expression derived from this one, simplified as much as possible.
    #[pyo3(name = "into_simplified")]
    fn py_into_simplified(&self) -> Self {
        self.clone().into_simplified()
    }

    /// Evaluate an expression, expecting that it may be fully reduced to a single complex number.
    ///
    /// By default, if it cannot be reduced to a complex number, this raises an error;
    /// pass the keyword-only parameter `partial=True` to allow partial evaluation,
    /// returning an `Expression` with the applied mappings.
    ///
    /// The `variables` should be a mapping of variable names to complex values,
    /// and `memory_references` should be a mapping of memory reference names to lists of floats.
    /// If not provided, they'll default to an empty mapping.
    ///
    /// # Examples
    ///
    /// If the `Expression` has no variables or memory references, no mappings are needed:
    ///
    /// ```python
    /// from quil.expression import Expression
    ///
    /// expr = Expression.parse("1 + 2 * 3")
    /// assert expr.evaluate() == 7.0+0.0j
    /// ```
    ///
    /// With variables and memory references, you can provide mappings to evaluate the expression:
    ///
    /// ```python
    /// from quil.expression import Expression
    ///
    /// expr = Expression.parse("%beta + theta[0] * theta[1]")
    /// evaluated = expr.evaluate(
    ///     variables={"beta": 1.0+0.0j},
    ///     memory_references={"theta": [2.0, 3.0]},
    /// )
    /// assert evaluated == 7.0+0.0j
    /// ```
    ///
    /// If the expression cannot be fully evaluated, you can allow partial evaluation:
    ///
    /// ```python
    /// from quil.expression import Expression
    ///
    /// expr = Expression.parse("%beta + theta[0] * theta[1]")
    ///
    /// evaluated = expr.evaluate(variables={"beta": 1.0+0.0j}, partial=True)
    /// assert evaluated == Expression.parse("1.0 + theta[0] * theta[1]")
    ///
    /// evaluated = expr.evaluate(memory_references={"theta": [2.0, 3.0]}, partial=True)
    /// assert evaluated == Expression.parse("%beta + 6")
    /// ```
    #[pyo3(name = "evaluate", signature = (variables=None, memory_references=None, /, partial=false))]
    fn py_evaluate(
        &self,
        variables: Option<HashMap<String, Complex64>>,
        memory_references: Option<HashMap<String, Vec<f64>>>,
        partial: bool,
    ) -> PyResult<Evaluated> {
        let variables = variables.unwrap_or_default();
        let memory_references = memory_references.unwrap_or_default();

        match self.evaluate_partial(&variables, &memory_references) {
            Expression::PiConstant() => Ok(Evaluated::Full(Complex64::new(PI, 0.0))),
            Expression::Number(c) => Ok(Evaluated::Full(c)),
            other => {
                if partial {
                    Ok(Evaluated::Partial(other))
                } else {
                    Err(EvaluationError::Incomplete)?
                }
            }
        }
    }

    /// Substitute an expression in the place of each matching variable.
    ///
    /// # Example
    ///
    /// ```python
    /// from quil.expression import Expression
    ///
    /// expression = Expression.parse("%x + %y")
    /// evaluated = expression.substitute_variables({"x": Expression.Number(1.0j)})
    /// assert evaluated == Expression.parse("1.0 + %y")
    /// ```
    #[pyo3(name = "substitute_variables")]
    fn py_substitute_variables(&self, variable_values: HashMap<String, ExpressionLike>) -> Self {
        self.substitute_variables_impl(&variable_values)
    }

    /// Explicitly evaluate as much of ``expr`` as possible, using substitutions from `d`.
    ///
    /// This method is deprecated; use `evaluate(..., partial=True)` instead,
    /// as it is more explicit and efficient.
    ///
    /// This supports substitution of both parameters and memory references.
    /// Each memory reference must be individually assigned a value at each memory offset to be substituted.
    ///
    /// :param d: Numerical substitutions for parameters or memory references.
    ///
    /// Returns a complex number (if possible) or a partially simplified `Expression`.
    #[pyo3(name = "substitute", signature = (d=None, /))]
    #[pyo3(warn(
        message = "`substitute` is deprecated; use `evaluate(..., partial=True)` instead.",
        category = PyDeprecationWarning
    ))]
    fn py_substitute(
        &self,
        d: Option<HashMap<SubstitutionKey, SubstitutionValue>>,
    ) -> PyResult<Evaluated> {
        // Split and validate the substitution dictionary.
        let d = d.unwrap_or_default();
        let mut variables = HashMap::new();
        let mut memory_references = HashMap::new();
        for (key, value) in d {
            match (key, value) {
                (SubstitutionKey::Variable(name), SubstitutionValue::Variable(value)) => {
                    variables.insert(name, value);
                }
                (SubstitutionKey::MemoryReference(memref), SubstitutionValue::Memory(values)) => {
                    memory_references.insert(memref.name, values);
                }
                (SubstitutionKey::Variable(name), SubstitutionValue::Memory(values)) => {
                    memory_references.insert(name, values);
                }
                (SubstitutionKey::MemoryReference(memref), SubstitutionValue::Variable(_)) => {
                    return Err(ValueError::new_err(format!(
                        "Expected a list of floats for memory reference '{memref}', but got a variable substitution"
                    )));
                }
            }
        }

        self.py_evaluate(Some(variables), Some(memory_references), true)
    }

    fn __add__(&self, other: ExpressionLike) -> Self {
        self.clone() + Expression::from(other)
    }

    fn __radd__(&self, other: ExpressionLike) -> Self {
        Expression::from(other) + self.clone()
    }

    fn __sub__(&self, other: ExpressionLike) -> Self {
        self.clone() - Expression::from(other)
    }

    fn __rsub__(&self, other: ExpressionLike) -> Self {
        Expression::from(other) - self.clone()
    }

    fn __mul__(&self, other: ExpressionLike) -> Self {
        self.clone() * Expression::from(other)
    }

    fn __rmul__(&self, other: ExpressionLike) -> Self {
        Expression::from(other) * self.clone()
    }

    fn __truediv__(&self, other: ExpressionLike) -> Self {
        self.clone() / Expression::from(other)
    }

    fn __rtruediv__(&self, other: ExpressionLike) -> Self {
        Expression::from(other) / self.clone()
    }

    /// Raise `self` to a complex power.
    ///
    /// Note: the `modulo` argument is not supported and will raise an error if provided.
    // In Quil, the Caret ('^') operator is used for exponentiation.
    // In Rust, it's implemented via Xor so that users can write `expr1 ^ expr2`,
    // but in PyQuil, it used the '**' operator, the standard in Python for exponentiation,
    // and we carry that forward here by implementing `__pow__` instead of `__xor__`.
    // Technically that means it can also be used with `pow`, which permits an optional `modulo`,
    // but that doesn't make sense here, so we raise an error if `modulo` is provided.
    fn __pow__<'py>(
        &self,
        exponent: ExpressionLike,
        modulo: Option<Bound<'py, PyAny>>,
    ) -> PyResult<Self> {
        if modulo.is_some() {
            return Err(PyNotImplementedError::new_err(
                "`modulo` is not supported for `Expression`",
            ));
        }
        Ok(self.clone() ^ Expression::from(exponent))
    }

    fn __rpow__<'py>(
        &self,
        base: ExpressionLike,
        modulo: Option<Bound<'py, PyAny>>,
    ) -> PyResult<Self> {
        if modulo.is_some() {
            return Err(PyNotImplementedError::new_err(
                "`modulo` is not supported for `Expression`",
            ));
        }
        Ok(Expression::from(base) ^ self.clone())
    }

    fn __pos__(slf: PyRef<'_, Self>) -> PyRef<'_, Self> {
        slf
    }

    fn __neg__(&self) -> Self {
        -(self.clone())
    }

    /// Simplify the expression to a float.
    ///
    /// Expression simplification can be slow, especially for large recursive expressions.
    /// This will raise an error if simplification doesn't result in a real number.
    fn __float__(&self) -> PyResult<f64> {
        Ok(self.clone().into_simplified().to_real()?)
    }

    /// Simplify the expression to a complex number.
    ///
    /// Expression simplification can be slow, especially for large recursive expressions.
    /// This will raise an error if simplification doesn't result in a complex number.
    fn __complex__(&self) -> PyResult<Complex64> {
        match self.clone().into_simplified() {
            Expression::PiConstant() => Ok(PI.into()),
            Expression::Number(c) => Ok(c),
            _ => Err(EvaluationError::NotANumber)?,
        }
    }

    /// Convert this `Expression` into a `numpy` array.
    ///
    /// If `dtype` is `object`, this returns an object array containing this `Expression` object.
    /// Given other non-none `dtype`s, this simplifies the `Expression` to a complex number
    /// and converts the array to the target `dtype`, raising an exception if either step fails.
    ///
    /// If `dtype` is `None`, this attempts to simplify the `Expression` to a complex number
    /// and return an array with that value, but if simplification isn't possible,
    /// this falls back to returning an object array containing this `Expression` object.
    ///
    /// Note that the expression simplification can be slow, especially for large recursive expressions.
    ///
    /// # Example
    ///
    /// ```python
    /// import numpy as np
    /// from quil.expression import Expression
    ///
    /// expr = Expression.parse("cis(pi / 2) + %x")
    ///
    /// ```
    #[gen_stub(override_return_type(type_repr = "numpy.ndarray", imports = ("numpy")))]
    #[pyo3(signature = (dtype=None, copy=None))]
    fn __array__<'py>(
        slf: Bound<'py, Self>,
        py: Python<'py>,
        #[gen_stub(override_type(type_repr = "numpy.dtype | None", imports = ("numpy")))]
        dtype: Option<Bound<'py, PyArrayDescr>>,
        copy: Option<bool>,
    ) -> PyResult<Bound<'py, PyAny>> {
        if let Some(false) = copy {
            return Err(ValueError::new_err(
                "`copy=False` is not supported for `Expression.__array__`",
            ));
        }

        fn to_obj_array<'py>(py: Python<'py>, slf: Bound<'py, Expression>) -> Bound<'py, PyAny> {
            let as_any = slf.unbind().into_any();
            PyArray::from_owned_object_array(py, ndarray::array![as_any]).into_any()
        }

        let arr = match dtype {
            None => {
                // Without a dtype, attempt to simplify the expression to a complex number.
                // If that isn't possible, fallback to the object representation.
                if let Ok(simplified) = slf.borrow().__complex__() {
                    PyArray::from_owned_array(py, ndarray::array![simplified]).into_any()
                } else {
                    to_obj_array(py, slf)
                }
            }
            Some(dtype) => {
                if dtype.is_equiv_to(&PyArrayDescr::object(py)) {
                    to_obj_array(py, slf)
                } else if dtype.is_equiv_to(&numpy::dtype::<f64>(py)) {
                    PyArray::from_owned_array(py, ndarray::array![slf.get().__float__()?])
                        .into_any()
                } else {
                    PyArray::from_owned_array(py, ndarray::array![slf.get().__complex__()?])
                        .call_method1(pyo3::intern!(py, "astype"), (dtype,))?
                        .into_any()
                }
            }
        };

        Ok(arr)
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
