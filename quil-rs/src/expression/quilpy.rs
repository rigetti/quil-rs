use numpy::{PyArray, PyArrayDescr, PyArrayDescrMethods};
use pyo3::{
    exceptions::{PyDeprecationWarning, PyNotImplementedError},
    prelude::*,
    types::{PyAnyMethods, PyComplex, PyFloat, PyInt},
    IntoPyObjectExt, PyClass,
};
use rigetti_pyo3::{create_init_submodule, impl_repr};

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pyfunction, gen_stub_pymethods};

use super::*;
use crate::{
    pickleable_new,
    quilpy::{
        errors::{self, ValueError},
        impl_to_quil,
        singleton::{py_singleton, PyModuleSingletonExt},
    },
};
use macros::*;

create_init_submodule! {
    classes: [
        ExpressionFunction,
        FunctionCallExpression,
        InfixExpression,
        InfixOperator,
        PrefixExpression,
        PrefixOperator,

        // Python-specific types without a direct Rust equivalent.
        PyExpression,
        Address,
        Number,
        Parameter,
        PiType,
    ],
    errors: [ errors::EvaluationError, errors::ParseExpressionError ],
    funcs: [
        quil_sin,
        quil_cos,
        quil_cis,
        quil_exp,
        quil_sqrt
    ],
}

pub(crate) fn post_init(m: &Bound<'_, PyModule>) -> PyResult<()> {
    use crate::quilpy::union;

    let py = m.py();

    m.add_singleton::<PiType>()?;

    // Add TypeAliases for use in annotations.
    // These are mainly for PyQuil backwards-compatibility.
    #[expect(non_snake_case)]
    let ExpressionValueDesignator = union!(py, PyInt, PyFloat, PyComplex)?;
    m.add("ExpressionValueDesignator", &ExpressionValueDesignator)?;
    m.add(
        "ExpressionDesignator",
        py.get_type::<PyExpression>()
            .bitor(&ExpressionValueDesignator)?,
    )?;
    m.add(
        "ParameterDesignator",
        union!(py, PyExpression, MemoryReference, PyInt, PyFloat, PyComplex)?,
    )?;

    // TODO
    // ParameterSubstitutionsMapDesignator = Mapping[Parameter | MemoryReference, ExpressionValueDesignator]

    Ok(())
}

/// Superclass for all [`Expression`] variants in Python.
///
/// The subclasses of this class are the various Quil expression types.
///
/// ```python
/// >>> from quil.expression import Expression, Number
/// >>> n = Number(1.0j)
/// >>> isinstance(n, Number)
/// True
/// >>> isinstance(n, Expression)
/// True
/// ```
///
/// You can check for different expression variants and destructure them using `match`:
///
/// ```python
/// match x:
///     case Number(value):
///         print("A number!")
///     case x if x is Pi:
///         print("pi!")
///     case Expression():
///         print("Some other expression!")
///     case _:
///         print("Not an expression!")
/// ```
///
// Rust Developer Notes:
//
// Rather than expose the `Expression` complex enum directly,
// we annotate each variant's inner type as `#[pyclass(extends = PyExpression)]`.
// `Expression::Number` and `Expression::Variable` don't have a dedicated Rust struct
// so we introduce the `Number` and `Parameter` newtypes here purely for the Python bindings.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(
    name = "Expression",
    module = "quil._quil.expression",
    subclass,
    skip_from_py_object,
    frozen,
    eq,
    hash
)]
pub struct PyExpression;

/// A type that can be converted into an [`Expression`],
/// for use as a parameter in methods and functions exposed through Python bindings.
#[derive(Debug, Clone, FromPyObject)]
// Generally we don't want to return this as a Python type,
// but the stub generator needs this when constructing default values in signatures.
#[cfg_attr(feature = "stubs", derive(IntoPyObject))]
pub(crate) enum ExpressionLike {
    Variable(String), // Ensure this is first so we convert to variables rather than memrefs
    Number(Complex64),
    PiConstant(PiType),
    MemoryReference(MemoryReference), // TODO: rename Address
    FunctionCall(FunctionCallExpression),
    Infix(InfixExpression),
    Prefix(PrefixExpression),
}

impl TryFrom<&Bound<'_, PyExpression>> for Expression {
    type Error = PyErr;

    fn try_from(value: &Bound<'_, PyExpression>) -> Result<Self, Self::Error> {
        if let Ok(slf) = value.cast::<Parameter>() {
            Ok(Expression::Variable(slf.get().name.clone()))
        } else if let Ok(slf) = value.cast::<Number>() {
            Ok(Expression::Number(slf.get().value))
        } else if value.cast::<PiType>().is_ok() {
            Ok(Expression::PiConstant)
        } else if let Ok(slf) = value.cast::<Address>() {
            Ok(Expression::Address(slf.get().memory_reference.clone()))
        } else if let Ok(slf) = value.cast::<FunctionCallExpression>() {
            Ok(Expression::FunctionCall(slf.get().clone()))
        } else if let Ok(slf) = value.cast::<InfixExpression>() {
            Ok(Expression::Infix(slf.get().clone()))
        } else if let Ok(slf) = value.cast::<PrefixExpression>() {
            Ok(Expression::Prefix(slf.get().clone()))
        } else {
            Err(PyNotImplementedError::new_err(
                "unknown Expression subclass",
            ))
        }
    }
}

impl<'a, 'py> FromPyObject<'a, 'py> for Expression {
    type Error = PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        obj.extract::<ExpressionLike>().map(Into::into)
    }
}

impl From<ExpressionLike> for Expression {
    fn from(value: ExpressionLike) -> Self {
        match value {
            ExpressionLike::Variable(name) => Expression::Variable(name),
            ExpressionLike::Number(v) => Expression::Number(v),
            ExpressionLike::PiConstant(_) => Expression::PiConstant,
            ExpressionLike::MemoryReference(memref) => Expression::Address(memref),
            ExpressionLike::FunctionCall(expr) => Expression::FunctionCall(expr),
            ExpressionLike::Infix(expr) => Expression::Infix(expr),
            ExpressionLike::Prefix(expr) => Expression::Prefix(expr),
        }
    }
}

/// Convert an [`Expression`] into an [`ExpressionLike`].
///
/// Occassionally, it's useful to call a Python-exposed function that takes an `ExpressionLike`
/// using an existing `Expression`. Ideally, we'd be able to take an `impl Into<Expression>`,
/// but since such functions cannot be used with PyO3, we need to convert it instead.
impl From<Expression> for ExpressionLike {
    fn from(value: Expression) -> Self {
        match value {
            Expression::Variable(name) => ExpressionLike::Variable(name),
            Expression::Number(n) => ExpressionLike::Number(n),
            Expression::PiConstant => ExpressionLike::PiConstant(PiType),
            Expression::Address(memref) => ExpressionLike::MemoryReference(memref),
            Expression::FunctionCall(expr) => ExpressionLike::FunctionCall(expr),
            Expression::Infix(expr) => ExpressionLike::Infix(expr),
            Expression::Prefix(expr) => ExpressionLike::Prefix(expr),
        }
    }
}

/// Allow [`Expression`]s to be returned from Python-exposed functions and methods.
impl<'py> IntoPyObject<'py> for Expression {
    type Target = PyAny;
    type Output = Bound<'py, Self::Target>;
    type Error = PyErr;

    fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
        match self {
            Expression::Address(memory_reference) => {
                Address { memory_reference }.into_bound_py_any(py)
            }
            Expression::Variable(name) => Parameter { name }.into_bound_py_any(py),
            Expression::Number(value) => Number { value }.into_bound_py_any(py),
            Expression::PiConstant => PiType.into_bound_py_any(py),
            Expression::FunctionCall(value) => value.into_bound_py_any(py),
            Expression::Infix(value) => value.into_bound_py_any(py),
            Expression::Prefix(value) => value.into_bound_py_any(py),
        }
    }
}

impl_repr!(ExpressionFunction);
impl_repr!(FunctionCallExpression);
impl_repr!(InfixExpression);
impl_repr!(InfixOperator);
impl_repr!(PrefixExpression);
impl_repr!(PrefixOperator);
impl_repr!(Address);
impl_repr!(Number);
impl_repr!(Parameter);
impl_repr!(PiType);

impl_to_quil!(
    FunctionCallExpression,
    InfixExpression,
    PrefixExpression,
    Address,
    Number,
    Parameter,
    PiType,
);

py_expression!(Address);
py_expression!(FunctionCallExpression);
py_expression!(InfixExpression);
py_expression!(Number);
py_expression!(Parameter);
py_expression!(PrefixExpression);

/// Convert a bound `#[pyclass]` into a `numpy` object array holding just that instance.
fn to_obj_array<'py, T: PyClass>(obj: &Bound<'py, T>) -> Bound<'py, PyAny> {
    let py = obj.py();
    let as_any = obj.to_owned().unbind().into_any();
    PyArray::from_owned_object_array(py, ndarray::array![as_any]).into_any()
}

impl AsView for ExpressionLike {
    fn as_view(&self) -> ExpressionView<'_> {
        match self {
            ExpressionLike::Variable(name) => ExpressionView::Variable(name),
            ExpressionLike::Number(value) => ExpressionView::Number(*value),
            ExpressionLike::PiConstant(_) => ExpressionView::PiConstant,
            ExpressionLike::MemoryReference(memref) => ExpressionView::Address(memref),
            ExpressionLike::FunctionCall(expr) => ExpressionView::FunctionCall(expr),
            ExpressionLike::Infix(expr) => ExpressionView::Infix(expr),
            ExpressionLike::Prefix(expr) => ExpressionView::Prefix(expr),
        }
    }
}

impl AsView for &Bound<'_, PyExpression> {
    fn as_view(&self) -> ExpressionView<'_> {
        if let Ok(obj) = self.cast::<Parameter>() {
            ExpressionView::Variable(&obj.get().name)
        } else if let Ok(obj) = self.cast::<Number>() {
            ExpressionView::Number(obj.get().value)
        } else if self.cast::<PiType>().is_ok() {
            ExpressionView::PiConstant
        } else if let Ok(obj) = self.cast::<Address>() {
            ExpressionView::Address(&obj.get().memory_reference)
        } else if let Ok(obj) = self.cast::<FunctionCallExpression>() {
            ExpressionView::FunctionCall(obj.get())
        } else if let Ok(obj) = self.cast::<InfixExpression>() {
            ExpressionView::Infix(obj.get())
        } else if let Ok(obj) = self.cast::<PrefixExpression>() {
            ExpressionView::Prefix(obj.get())
        } else {
            // TODO: Determine a way to prove this can't happen at compile time.
            // That is, prove that the above covers all subclasses of `PyExpression`.
            panic!("Unknown Expression subclass");
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PyExpression {
    /// Parse an ``Expression`` from a string.
    ///
    /// Raises a ``ParseExpressionError`` error if the string isn't a valid Quil expression.
    #[staticmethod]
    fn parse(input: &str) -> PyResult<Expression> {
        Ok(Expression::from_str(input)?)
    }

    /// Return a new expression derived from this one, simplified as much as possible.
    #[pyo3(name = "into_simplified")]
    fn py_into_simplified(slf: &Bound<'_, Self>) -> PyResult<Expression> {
        Ok(Expression::try_from(slf)?.into_simplified())
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
    #[pyo3(name = "evaluate", signature = (variables=None, memory_references=None, /, partial=false))]
    fn py_evaluate(
        slf: &Bound<'_, Self>,
        variables: Option<HashMap<String, Complex64>>,
        memory_references: Option<HashMap<String, Vec<f64>>>,
        partial: bool,
    ) -> PyResult<Evaluated> {
        let variables = variables.unwrap_or_default();
        let memory_references = memory_references.unwrap_or_default();

        match Expression::try_from(slf)?.evaluate_partial(&variables, &memory_references) {
            Expression::PiConstant => Ok(Evaluated::Full(Complex64::new(PI, 0.0))),
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
    #[pyo3(name = "substitute_variables")]
    fn py_substitute_variables(
        slf: &Bound<'_, Self>,
        variable_values: HashMap<String, ExpressionLike>,
    ) -> PyResult<Expression> {
        Ok(Expression::try_from(slf)?.substitute_variables_impl(&variable_values))
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
        slf: &Bound<'_, Self>,
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

        PyExpression::py_evaluate(slf, Some(variables), Some(memory_references), true)
    }

    fn __add__(slf: &Bound<'_, Self>, other: ExpressionLike) -> PyResult<Expression> {
        Ok(Expression::try_from(slf)? + Expression::from(other))
    }

    fn __radd__(slf: &Bound<'_, Self>, other: ExpressionLike) -> PyResult<Expression> {
        Ok(Expression::from(other) + Expression::try_from(slf)?)
    }

    fn __sub__(slf: &Bound<'_, Self>, other: ExpressionLike) -> PyResult<Expression> {
        Ok(Expression::try_from(slf)? - Expression::from(other))
    }

    fn __rsub__(slf: &Bound<'_, Self>, other: ExpressionLike) -> PyResult<Expression> {
        Ok(Expression::from(other) - Expression::try_from(slf)?)
    }

    fn __mul__(slf: &Bound<'_, Self>, other: ExpressionLike) -> PyResult<Expression> {
        Ok(Expression::try_from(slf)? * Expression::from(other))
    }

    fn __rmul__(slf: &Bound<'_, Self>, other: ExpressionLike) -> PyResult<Expression> {
        Ok(Expression::from(other) * Expression::try_from(slf)?)
    }

    fn __truediv__(slf: &Bound<'_, Self>, other: ExpressionLike) -> PyResult<Expression> {
        Ok(Expression::try_from(slf)? / Expression::from(other))
    }

    fn __rtruediv__(slf: &Bound<'_, Self>, other: ExpressionLike) -> PyResult<Expression> {
        Ok(Expression::from(other) / Expression::try_from(slf)?)
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
        slf: &Bound<'_, Self>,
        exponent: ExpressionLike,
        modulo: Option<Bound<'py, PyAny>>,
    ) -> PyResult<Expression> {
        if modulo.is_some() {
            return Err(PyNotImplementedError::new_err(
                "`modulo` is not supported for `Expression`",
            ));
        }
        Ok(Expression::try_from(slf)? ^ Expression::from(exponent))
    }

    fn __rpow__<'py>(
        slf: &Bound<'_, Self>,
        base: ExpressionLike,
        modulo: Option<Bound<'py, PyAny>>,
    ) -> PyResult<Expression> {
        if modulo.is_some() {
            return Err(PyNotImplementedError::new_err(
                "`modulo` is not supported for `Expression`",
            ));
        }
        Ok(Expression::from(base) ^ Expression::try_from(slf)?)
    }

    fn __pos__(slf: PyRef<'_, Self>) -> PyRef<'_, Self> {
        slf
    }

    fn __neg__(slf: &Bound<'_, Self>) -> PyResult<Expression> {
        Ok(-(Expression::try_from(slf)?))
    }

    /// Simplify the expression to a float.
    ///
    /// Expression simplification can be slow, especially for large recursive expressions.
    /// This will raise an error if simplification doesn't result in a real number.
    fn __float__(slf: &Bound<'_, Self>) -> PyResult<f64> {
        Ok(Expression::try_from(slf)?.into_simplified().to_real()?)
    }

    /// Simplify the expression to a complex number.
    ///
    /// Expression simplification can be slow, especially for large recursive expressions.
    /// This will raise an error if simplification doesn't result in a complex number.
    fn __complex__(slf: &Bound<'_, Self>) -> PyResult<Complex64> {
        match Expression::try_from(slf)?.into_simplified() {
            Expression::PiConstant => Ok(PI.into()),
            Expression::Number(c) => Ok(c),
            _ => Err(EvaluationError::NotANumber)?,
        }
    }

    /// Convert this expression into a `numpy` array.
    ///
    /// If `dtype` is `object`, this returns an object array containing this object.
    /// Given other non-none `dtype`s, this simplifies the expression to a complex number
    /// and converts the array to the target `dtype`, raising an exception if either step fails.
    ///
    /// If `dtype` is `None`, this attempts to simplify the expression to a complex number
    /// and return an array with that value, but if simplification isn't possible,
    /// this falls back to returning an object array containing this object.
    ///
    /// Note that the expression simplification can be slow, especially for large recursive expressions.
    #[gen_stub(override_return_type(type_repr = "numpy.ndarray", imports = ("numpy")))]
    #[pyo3(signature = (dtype=None, copy=None))]
    fn __array__<'py>(
        slf: &Bound<'py, Self>,
        py: Python<'py>,
        #[gen_stub(override_type(type_repr = "numpy.dtype | None", imports = ("numpy")))]
        dtype: Option<Bound<'py, PyArrayDescr>>,
        copy: Option<bool>,
    ) -> PyResult<Bound<'py, PyAny>> {
        if let Some(false) = copy {
            return Err(ValueError::new_err(
                "`copy=False` is not supported for `__array__`",
            ));
        }

        let arr = match dtype {
            None => {
                // Without a dtype, attempt to simplify the expression to a complex number.
                // If that isn't possible, fallback to the object representation.
                if let Ok(simplified) = PyExpression::__complex__(slf) {
                    PyArray::from_owned_array(py, ndarray::array![simplified]).into_any()
                } else {
                    to_obj_array(slf)
                }
            }
            Some(dtype) => {
                if dtype.is_equiv_to(&PyArrayDescr::object(py)) {
                    to_obj_array(slf)
                } else if dtype.is_equiv_to(&numpy::dtype::<f64>(py)) {
                    PyArray::from_owned_array(py, ndarray::array![PyExpression::__float__(slf)?])
                        .into_any()
                } else {
                    PyArray::from_owned_array(py, ndarray::array![PyExpression::__complex__(slf)?])
                        .call_method1(pyo3::intern!(py, "astype"), (dtype,))?
                        .into_any()
                }
            }
        };

        Ok(arr)
    }
}

/// The type of the `Expression::PiConstant` variant, i.e. the literal `pi`.
///
/// The singleton instance of this type is exposed in Python as ``Pi``.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil._quil.expression", extends = PyExpression, eq, frozen, hash, from_py_object)
)]
#[cfg_attr(not(feature = "python"), strip_pyo3)]
pub(crate) struct PiType;

py_singleton!(PiType, "Pi", |py| {
    PyClassInitializer::from(PyExpression).add_subclass(PiType)
});

#[cfg(feature = "stubs")]
pyo3_stub_gen::module_variable!("quil._quil.expression", "Pi", PiType);

impl From<PiType> for Expression {
    fn from(_: PiType) -> Self {
        Expression::PiConstant
    }
}

impl AsView for PiType {
    fn as_view(&self) -> ExpressionView<'_> {
        ExpressionView::PiConstant
    }
}

impl AsView for Bound<'_, PiType> {
    fn as_view(&self) -> ExpressionView<'_> {
        ExpressionView::PiConstant
    }
}

/// A Quil formal parameter within an expression, e.g. ``%theta``.
///
/// This corresponds to the [`Expression::Variable`] variant.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil._quil.expression", extends = PyExpression, eq, frozen, hash, skip_from_py_object, get_all)
)]
#[cfg_attr(not(feature = "python"), strip_pyo3)]
pub(crate) struct Parameter {
    name: String, // TODO: consider replacing this with `Py<PyString>`.
}

/// Wrapper around [`Parameter`] with type stubs that indicate it the method accepts `str`s, too.
#[derive(Clone, Debug, Hash, PartialEq, Eq, FromPyObject)]
#[cfg_attr(feature = "stubs", derive(IntoPyObject))]
pub(crate) struct ParameterLike(Parameter);

impl From<ParameterLike> for Parameter {
    fn from(value: ParameterLike) -> Self {
        value.0
    }
}

impl<'a, 'py> FromPyObject<'a, 'py> for Parameter {
    type Error = PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        if let Ok(value) = obj.cast::<Parameter>() {
            Ok(value.get().clone())
        } else {
            Ok(Self {
                name: obj.extract::<String>()?,
            })
        }
    }
}

impl From<Parameter> for Expression {
    fn from(val: Parameter) -> Self {
        Expression::Variable(val.name)
    }
}

impl From<Parameter> for String {
    fn from(val: Parameter) -> Self {
        val.name
    }
}

impl AsView for Parameter {
    fn as_view(&self) -> ExpressionView<'_> {
        ExpressionView::Variable(&self.name)
    }
}

impl AsView for Bound<'_, Parameter> {
    fn as_view(&self) -> ExpressionView<'_> {
        ExpressionView::Variable(&self.get().name)
    }
}

pickleable_new! {
    impl Parameter {
        fn __new__(name: String);
    }
}

/// A memory reference embedded in an expression, e.g. ``theta[0]``.
///
/// This corresponds to the [`Expression::Address`] variant.
/// It's a distinct type from [`MemoryReference`] because a [`MemoryReference`] properly
/// is a kind of Quil instruction (and hence a `quil` [`Instruction`]),
/// whereas an [`Address`] is the use of a memory reference within an expression.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil._quil.expression", extends = PyExpression, eq, frozen, hash, from_py_object, get_all)
)]
#[cfg_attr(not(feature = "python"), strip_pyo3)]
pub(crate) struct Address {
    /// The underlying memory reference, e.g. `theta[0]`.
    memory_reference: MemoryReference,
}

impl From<Address> for Expression {
    fn from(val: Address) -> Self {
        Expression::Address(val.memory_reference)
    }
}

impl AsView for Address {
    fn as_view(&self) -> ExpressionView<'_> {
        ExpressionView::Address(&self.memory_reference)
    }
}

impl AsView for Bound<'_, Address> {
    fn as_view(&self) -> ExpressionView<'_> {
        ExpressionView::Address(&self.get().memory_reference)
    }
}

pickleable_new! {
    impl Address {
        fn __new__(memory_reference: MemoryReference);
    }
}

/// A literal, constant number embedded in an expression, e.g. ``1.0`` or ``2i``.
///
/// This corresponds to the [`Expression::Number`] variant.
#[derive(Copy, Clone, Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(
        module = "quil._quil.expression",
        extends = PyExpression,
        eq, frozen, hash, from_py_object, get_all
    )
)]
#[cfg_attr(not(feature = "python"), strip_pyo3)]
pub(crate) struct Number {
    /// The underlying complex value.
    value: Complex64,
}

impl From<Number> for Expression {
    fn from(val: Number) -> Self {
        Expression::Number(val.value)
    }
}

impl AsView for Number {
    fn as_view(&self) -> ExpressionView<'_> {
        ExpressionView::Number(self.value)
    }
}

impl AsView for Bound<'_, Number> {
    fn as_view(&self) -> ExpressionView<'_> {
        ExpressionView::Number(self.get().value)
    }
}

pickleable_new! {
    impl Number {
        fn __new__(value: Complex64);
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self.as_view() == other.as_view()
    }
}

impl Eq for Number {}

impl Hash for Number {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_view().hash(state)
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl InfixExpression {
    // TODO: Augment `pickleable_new` to handle conversions like the kinds used here:
    // - extract `ExpressionLike` and use its TypeStub impl
    // - convert into `ArcIntern<Expression>`
    // - enable `__getnewargs__` to convert to `Expression`

    #[new]
    fn __new__(left: ExpressionLike, operator: InfixOperator, right: ExpressionLike) -> Self {
        Self::new(
            ArcIntern::new(Expression::from(left)),
            operator,
            ArcIntern::new(Expression::from(right)),
        )
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

impl AsView for InfixExpression {
    fn as_view(&self) -> ExpressionView<'_> {
        ExpressionView::Infix(self)
    }
}

impl AsView for Bound<'_, InfixExpression> {
    fn as_view(&self) -> ExpressionView<'_> {
        ExpressionView::Infix(self.get())
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PrefixExpression {
    #[new]
    fn __new__(operator: PrefixOperator, expression: ExpressionLike) -> Self {
        Self::new(operator, ArcIntern::new(Expression::from(expression)))
    }

    fn __getnewargs__(&self) -> (PrefixOperator, Expression) {
        (self.operator, self.expression())
    }

    #[getter]
    fn expression(&self) -> Expression {
        (*self.expression).clone()
    }
}

impl AsView for PrefixExpression {
    fn as_view(&self) -> ExpressionView<'_> {
        ExpressionView::Prefix(self)
    }
}

impl AsView for Bound<'_, PrefixExpression> {
    fn as_view(&self) -> ExpressionView<'_> {
        ExpressionView::Prefix(self.get())
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl FunctionCallExpression {
    #[new]
    fn __new__(function: ExpressionFunction, expression: ExpressionLike) -> Self {
        Self::new(function, ArcIntern::new(Expression::from(expression)))
    }

    fn __getnewargs__(&self) -> (ExpressionFunction, Expression) {
        (self.function, self.expression())
    }

    #[getter]
    fn expression(&self) -> Expression {
        (*self.expression).clone()
    }
}

impl AsView for FunctionCallExpression {
    fn as_view(&self) -> ExpressionView<'_> {
        ExpressionView::FunctionCall(self)
    }
}

impl AsView for Bound<'_, FunctionCallExpression> {
    fn as_view(&self) -> ExpressionView<'_> {
        ExpressionView::FunctionCall(self.get())
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

/// Wrapper for [`Expression`] that forces type stubs to use the fully-qualified name.
///
/// Used to force `from quil import _quil.expression` and `_quil.expression.Expression`
/// in places that would otherwise be `expression.Expression`;
/// since there are several struct fields named `expression`,
/// the latter would otherwise cause `mypy` and other type checkers to reject the stubs
/// because of the name collision.
#[derive(Debug, Clone, Hash, Eq, PartialEq, IntoPyObject)]
pub(crate) struct QualifiedExpression(Expression);

impl<'a, 'py> FromPyObject<'a, 'py> for QualifiedExpression {
    type Error = PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> PyResult<Self> {
        Ok(Self(obj.extract::<ExpressionLike>()?.into()))
    }
}

impl From<QualifiedExpression> for Expression {
    fn from(value: QualifiedExpression) -> Self {
        value.0
    }
}

impl From<Expression> for QualifiedExpression {
    fn from(value: Expression) -> Self {
        Self(value)
    }
}

impl std::borrow::Borrow<Expression> for QualifiedExpression {
    fn borrow(&self) -> &Expression {
        &self.0
    }
}

impl std::borrow::BorrowMut<Expression> for QualifiedExpression {
    fn borrow_mut(&mut self) -> &mut Expression {
        &mut self.0
    }
}

#[cfg_attr(
    feature = "stubs",
    gen_stub_pyfunction(module = "quil._quil.expression")
)]
#[pyfunction]
pub(crate) fn quil_sin(expression: ExpressionLike) -> Expression {
    Expression::FunctionCall(FunctionCallExpression::new(
        ExpressionFunction::Sine,
        ArcIntern::new(expression.into()),
    ))
}

#[cfg_attr(
    feature = "stubs",
    gen_stub_pyfunction(module = "quil._quil.expression")
)]
#[pyfunction]
pub(crate) fn quil_cos(expression: ExpressionLike) -> Expression {
    Expression::FunctionCall(FunctionCallExpression::new(
        ExpressionFunction::Cosine,
        ArcIntern::new(expression.into()),
    ))
}

#[cfg_attr(
    feature = "stubs",
    gen_stub_pyfunction(module = "quil._quil.expression")
)]
#[pyfunction]
pub(crate) fn quil_cis(expression: ExpressionLike) -> Expression {
    Expression::FunctionCall(FunctionCallExpression::new(
        ExpressionFunction::Cis,
        ArcIntern::new(expression.into()),
    ))
}

#[cfg_attr(
    feature = "stubs",
    gen_stub_pyfunction(module = "quil._quil.expression")
)]
#[pyfunction]
pub(crate) fn quil_exp(expression: ExpressionLike) -> Expression {
    Expression::FunctionCall(FunctionCallExpression::new(
        ExpressionFunction::Exponent,
        ArcIntern::new(expression.into()),
    ))
}

#[cfg_attr(
    feature = "stubs",
    gen_stub_pyfunction(module = "quil._quil.expression")
)]
#[pyfunction]
pub(crate) fn quil_sqrt(expression: ExpressionLike) -> Expression {
    Expression::FunctionCall(FunctionCallExpression::new(
        ExpressionFunction::SquareRoot,
        ArcIntern::new(expression.into()),
    ))
}

mod macros {
    /// Implement [`IntoPyObject`] and `From<$T> for PyClassInitializer<$T>`
    /// for a `#[pyclass(extends = PyExpression)]`.
    macro_rules! py_expression {
        ($T:ty) => {
            impl From<$T> for PyClassInitializer<$T> {
                fn from(value: $T) -> Self {
                    PyClassInitializer::from(PyExpression).add_subclass(value)
                }
            }

            impl<'py> IntoPyObject<'py> for $T {
                type Target = Self;
                type Output = Bound<'py, Self::Target>;
                type Error = PyErr;

                fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
                    Ok(Py::new(py, PyClassInitializer::from(self))?.into_bound(py))
                }
            }
        };
    }

    pub(super) use py_expression;
}

#[cfg(feature = "stubs")]
pub(crate) mod stubs {
    use pyo3_stub_gen::{impl_stub_type, runtime::PyRuntimeType, type_alias, PyStubType, TypeInfo};

    #[allow(clippy::wildcard_imports)]
    use super::*;
    use std::collections::HashSet;

    impl PyStubType for QualifiedExpression {
        fn type_output() -> TypeInfo {
            TypeInfo {
                name: "_quil.expression.Expression".to_string(),
                source_module: None,
                import: HashSet::from(["quil._quil.expression".into()]),
                type_refs: HashMap::new(),
            }
        }
    }

    impl PyRuntimeType for QualifiedExpression {
        fn runtime_type_object(py: Python<'_>) -> PyResult<Bound<'_, PyAny>> {
            <PyExpression as PyRuntimeType>::runtime_type_object(py)
        }
    }

    impl PyStubType for ParameterLike {
        fn type_input() -> TypeInfo {
            <Parameter as PyStubType>::type_input() | <String as PyStubType>::type_input()
        }

        fn type_output() -> TypeInfo {
            <Parameter as PyStubType>::type_output()
        }
    }

    impl PyRuntimeType for ParameterLike {
        fn runtime_type_object(py: Python<'_>) -> PyResult<Bound<'_, PyAny>> {
            <Parameter as PyRuntimeType>::runtime_type_object(py)
        }
    }

    impl_stub_type!(Expression = PyExpression);
    impl_stub_type!(
        ExpressionLike = QualifiedExpression | MemoryReference | String | i64 | f64 | Complex64
    );
    impl_stub_type!(SubstitutionKey = String | MemoryReference);
    impl_stub_type!(SubstitutionValue = Complex64 | Vec<Complex64>);
    impl_stub_type!(Evaluated = Expression | Complex64);

    // Developer Note: Proper aliases MUST be added to the module in `post_init`!
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
}
