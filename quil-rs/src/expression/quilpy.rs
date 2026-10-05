use numpy::{PyArray, PyArrayDescr, PyArrayDescrMethods};
use pyo3::{
    exceptions::{PyDeprecationWarning, PyNotImplementedError},
    prelude::*,
    sync::PyOnceLock,
    types::{PyAnyMethods, PyComplex, PyFloat, PyInt, PyString},
    CastError, IntoPyObjectExt, PyClass, PyTypeCheck,
};
use rigetti_pyo3::{create_init_submodule, impl_repr};

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pyfunction, gen_stub_pymethods};

use super::*;
use crate::{
    quilpy::{
        errors::{self, ValueError},
        impl_to_quil,
        singleton::{py_singleton, PyModuleSingletonExt},
    },
};

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
        union!(
            py,
            PyExpression,
            MemoryReference,
            PyInt,
            PyFloat,
            PyComplex
        )?,
    )?;

    // TODO
    // ParameterSubstitutionsMapDesignator = Mapping[Parameter | MemoryReference, ExpressionValueDesignator]

    Ok(())
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

/// A type that can be converted into an `Expression`,
/// for use as a parameter in methods and functions exposed through Python bindings.
#[derive(Debug, Clone, FromPyObject)]
// Generally we don't want to return this as a Python type,
// but the stub generator needs this when constructing default values in signatures.
#[cfg_attr(feature = "stubs", derive(IntoPyObject))]
pub(crate) enum ExpressionLike {
    Variable(String), // Ensure this is first so we convert to variables rather than memrefs
    MemoryReference(MemoryReference),
    // Expression must come before Number, since Expression implements __complex__.
    Expression(Expression),
    Number(Complex64),
}

impl From<ExpressionLike> for Expression {
    fn from(value: ExpressionLike) -> Self {
        match value {
            ExpressionLike::Variable(name) => Expression::Variable(name),
            ExpressionLike::MemoryReference(memref) => Expression::Address(memref),
            ExpressionLike::Expression(expr) => expr,
            ExpressionLike::Number(v) => Expression::Number(v),
        }
    }
}

/// Wrap an [`Expression`] in an [`ExpressionLike`].
///
/// Occassionally, it's useful to call a Python-exposed function that takes an `ExpressionLike`
/// using an existing `Expression`. Ideally, we'd be able to take an `impl Into<Expression>`,
/// but since such functions cannot be used with PyO3, we need to convert instead.
impl From<Expression> for ExpressionLike {
    fn from(value: Expression) -> Self {
        ExpressionLike::Expression(value)
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

/// Wrapper for `Expression` that forces type stubs to use the fully-qualified name.
///
/// Used to force `from quil import _quil.expression` and `_quil.expression.Expression`
/// in places that would otherwise be `expression.Expression`;
/// since there are several struct fields named `expression`,
/// the latter would otherwise cause `mypy` and other type checkers to reject the stubs
/// because of the name collision.
#[derive(Debug, Clone, Hash, Eq, PartialEq, FromPyObject, IntoPyObject)]
pub(crate) struct QualifiedExpression(Expression);

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

#[cfg(feature = "stubs")]
pub(crate) mod stubs {
    use pyo3_stub_gen::{
        impl_stub_type, runtime::PyRuntimeType, type_alias, PyStubType, TypeInfo,
    };

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
            Ok(py.get_type::<PyExpression>().into_any())
        }
    }

    impl_stub_type!(
        ExpressionLike = QualifiedExpression | MemoryReference | String | i64 | f64 | Complex64
    );
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

    // Note: unlike the prior `Expression` complex-enum API, `evaluate`'s more precise
    // `partial=False -> complex` overload is not replicated here for each concrete subclass;
    // the generated stub instead shows the broader `Expression | complex` return type.
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
///     case PiType():
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
// we annotate each variant's inner type as `#[pyclass(extends = PyExpression)]`
// and rely on `PyExpression: Default` so each subclass's `#[new]` can just return `Self`.
//
// `Expression::Number` and `Expression::Variable` don't have a dedicated Rust struct
// (they wrap `Complex64` and `String` respectively), so we introduce the `Number` and
// `Parameter` newtypes here purely for the Python bindings.
//
// TODO(migration-guide): `Expression(x)` is no longer a valid constructor; use the
// concrete subclass directly instead, e.g. `Number(1.0j)`, `Parameter("theta")`,
// `Address(memory_reference)`, or `Expression.parse(...)`.
#[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(
    name = "Expression",
    module = "quil._quil.expression",
    subclass,
    from_py_object,
    frozen,
    eq,
    hash
)]
pub struct PyExpression;

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
}

#[cfg(feature = "stubs")]
pyo3_stub_gen::impl_stub_type!(Expression = PyExpression);

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

impl Quil for PiType {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> Result<(), crate::quil::ToQuilError> {
        Expression::PiConstant.write(writer, fall_back_to_debug)
    }
}

impl From<PiType> for Expression {
    fn from(_: PiType) -> Self {
        Expression::PiConstant
    }
}

/// A Quil formal parameter within an expression, e.g. ``%theta``.
///
/// This corresponds to the [`Expression::Variable`] variant.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(name = "Parameter", module = "quil._quil.expression", extends = PyExpression, eq, frozen, hash, from_py_object)
)]
#[cfg_attr(not(feature = "python"), strip_pyo3)]
pub(crate) struct Parameter {
    #[pyo3(get)]
    name: String,
}

impl From<Parameter> for Expression {
    fn from(val: Parameter) -> Self {
        Expression::Variable(val.name)
    }
}

impl Quil for Parameter {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> Result<(), crate::quil::ToQuilError> {
        Expression::Variable(self.name.clone()).write(writer, fall_back_to_debug)
    }
}

/// A memory reference embedded in an expression, e.g. ``theta[0]``.
///
/// This corresponds to the [`Expression::Address`] variant. It's a distinct type from
/// [`MemoryReference`] (rather than making `MemoryReference` itself extend [`PyExpression`])
/// because a `pyclass` can only have a single Python base class, and `MemoryReference` is
/// independently exposed as an `Instruction`-related type.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(name = "Address", module = "quil._quil.expression", extends = PyExpression, eq, frozen, hash, from_py_object)
)]
#[cfg_attr(not(feature = "python"), strip_pyo3)]
pub(crate) struct Address(pub(crate) MemoryReference);

impl From<Address> for Expression {
    fn from(val: Address) -> Self {
        Expression::Address(val.0)
    }
}

impl Quil for Address {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> Result<(), crate::quil::ToQuilError> {
        self.0.write(writer, fall_back_to_debug)
    }
}

/// A literal, constant number embedded in an expression, e.g. ``1.0`` or ``2i``.
///
/// This corresponds to the [`Expression::Number`] variant.
#[derive(Copy, Clone, Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(name = "Number", module = "quil._quil.expression", extends = PyExpression, eq, frozen, hash, from_py_object)
)]
#[cfg_attr(not(feature = "python"), strip_pyo3)]
pub(crate) struct Number(pub(crate) Complex64);

// Implemented by hand since we can't derive `PartialEq`/`Hash` with an `f64` hidden inside.
impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        floating_point_eq::complex64::eq(self.0, other.0)
    }
}

impl Eq for Number {}

impl Hash for Number {
    fn hash<H: Hasher>(&self, state: &mut H) {
        floating_point_eq::complex64::hash(self.0, state);
    }
}

impl From<Number> for Expression {
    fn from(val: Number) -> Self {
        Expression::Number(val.0)
    }
}

impl Quil for Number {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> Result<(), crate::quil::ToQuilError> {
        Expression::Number(self.0).write(writer, fall_back_to_debug)
    }
}

impl Quil for FunctionCallExpression {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> Result<(), crate::quil::ToQuilError> {
        Expression::from(self.clone()).write(writer, fall_back_to_debug)
    }
}

impl Quil for InfixExpression {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> Result<(), crate::quil::ToQuilError> {
        Expression::from(self.clone()).write(writer, fall_back_to_debug)
    }
}

impl Quil for PrefixExpression {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> Result<(), crate::quil::ToQuilError> {
        Expression::from(self.clone()).write(writer, fall_back_to_debug)
    }
}

py_expression!(Address);
py_expression!(FunctionCallExpression);
py_expression!(InfixExpression);
py_expression!(Number);
py_expression!(Parameter);
py_expression!(PrefixExpression);

// NOTE: we don't use the `pickleable_new!` macro here (unlike most other simple newtypes),
// since its default (no-body) arm currently has a pre-existing bug that produces spurious
// `TryFrom<&Bound<'_, T>>` trait errors for types with a single `String`/etc. field; this bug
// affects other unrelated types in this crate too and is out of scope for this change.
#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Parameter {
    #[new]
    fn __new__(name: String) -> Self {
        Self { name }
    }

    fn __getnewargs__(&self) -> (String,) {
        (self.name.clone(),)
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Address {
    #[new]
    fn __new__(memory_reference: MemoryReference) -> Self {
        Self(memory_reference)
    }

    fn __getnewargs__(&self) -> (MemoryReference,) {
        (self.0.clone(),)
    }

    /// The underlying memory reference, e.g. `theta[0]`.
    #[getter]
    fn memory_reference(&self) -> MemoryReference {
        self.0.clone()
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Number {
    #[new]
    fn __new__(value: Complex64) -> Self {
        Self(value)
    }

    fn __getnewargs__(&self) -> (Complex64,) {
        (self.0,)
    }

    /// The underlying complex value.
    #[getter]
    fn value(&self) -> Complex64 {
        self.0
    }
}

/// Convert a `#[pyclass(extends = PyExpression)]` instance into a `numpy` object array.
#[cfg(feature = "python")]
fn expression_obj_array<'py, T: PyClass>(py: Python<'py>, slf: Bound<'py, T>) -> Bound<'py, PyAny> {
    let as_any = slf.unbind().into_any();
    PyArray::from_owned_object_array(py, ndarray::array![as_any]).into_any()
}

/// Implement the methods shared by all [`Expression`] variants
/// (`evaluate`, `substitute_variables`, arithmetic operators, etc.) for each of the given
/// `#[pyclass(extends = PyExpression)]` types.
///
/// These can't be defined once on `PyExpression` itself, because each concrete subclass
/// is a genuinely distinct Python class (not just a tag on a shared one), so each needs
/// its own copy of these methods, all of which delegate to `Expression`'s own logic.
macro_rules! impl_expression_methods {
    ($($T:ty),+ $(,)?) => {
        $(
        #[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
        #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
        #[pymethods]
        impl $T {
            /// Return a new expression derived from this one, simplified as much as possible.
            #[pyo3(name = "into_simplified")]
            fn py_into_simplified(&self) -> Expression {
                Expression::from(self.clone()).into_simplified()
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
                &self,
                variables: Option<HashMap<String, Complex64>>,
                memory_references: Option<HashMap<String, Vec<f64>>>,
                partial: bool,
            ) -> PyResult<Evaluated> {
                let variables = variables.unwrap_or_default();
                let memory_references = memory_references.unwrap_or_default();

                match Expression::from(self.clone()).evaluate_partial(&variables, &memory_references) {
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
            fn py_substitute_variables(&self, variable_values: HashMap<String, ExpressionLike>) -> Expression {
                Expression::from(self.clone()).substitute_variables_impl(&variable_values)
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

            fn __add__(&self, other: ExpressionLike) -> Expression {
                Expression::from(self.clone()) + Expression::from(other)
            }

            fn __radd__(&self, other: ExpressionLike) -> Expression {
                Expression::from(other) + Expression::from(self.clone())
            }

            fn __sub__(&self, other: ExpressionLike) -> Expression {
                Expression::from(self.clone()) - Expression::from(other)
            }

            fn __rsub__(&self, other: ExpressionLike) -> Expression {
                Expression::from(other) - Expression::from(self.clone())
            }

            fn __mul__(&self, other: ExpressionLike) -> Expression {
                Expression::from(self.clone()) * Expression::from(other)
            }

            fn __rmul__(&self, other: ExpressionLike) -> Expression {
                Expression::from(other) * Expression::from(self.clone())
            }

            fn __truediv__(&self, other: ExpressionLike) -> Expression {
                Expression::from(self.clone()) / Expression::from(other)
            }

            fn __rtruediv__(&self, other: ExpressionLike) -> Expression {
                Expression::from(other) / Expression::from(self.clone())
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
            ) -> PyResult<Expression> {
                if modulo.is_some() {
                    return Err(PyNotImplementedError::new_err(
                        "`modulo` is not supported for `Expression`",
                    ));
                }
                Ok(Expression::from(self.clone()) ^ Expression::from(exponent))
            }

            fn __rpow__<'py>(
                &self,
                base: ExpressionLike,
                modulo: Option<Bound<'py, PyAny>>,
            ) -> PyResult<Expression> {
                if modulo.is_some() {
                    return Err(PyNotImplementedError::new_err(
                        "`modulo` is not supported for `Expression`",
                    ));
                }
                Ok(Expression::from(base) ^ Expression::from(self.clone()))
            }

            fn __pos__(slf: PyRef<'_, Self>) -> PyRef<'_, Self> {
                slf
            }

            fn __neg__(&self) -> Expression {
                -(Expression::from(self.clone()))
            }

            /// Simplify the expression to a float.
            ///
            /// Expression simplification can be slow, especially for large recursive expressions.
            /// This will raise an error if simplification doesn't result in a real number.
            fn __float__(&self) -> PyResult<f64> {
                Ok(Expression::from(self.clone()).into_simplified().to_real()?)
            }

            /// Simplify the expression to a complex number.
            ///
            /// Expression simplification can be slow, especially for large recursive expressions.
            /// This will raise an error if simplification doesn't result in a complex number.
            fn __complex__(&self) -> PyResult<Complex64> {
                match Expression::from(self.clone()).into_simplified() {
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
                slf: Bound<'py, Self>,
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
                        if let Ok(simplified) = slf.borrow().__complex__() {
                            PyArray::from_owned_array(py, ndarray::array![simplified]).into_any()
                        } else {
                            expression_obj_array(py, slf)
                        }
                    }
                    Some(dtype) => {
                        if dtype.is_equiv_to(&PyArrayDescr::object(py)) {
                            expression_obj_array(py, slf)
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
        }
        )*
    };
}

impl_expression_methods!(
    Address,
    FunctionCallExpression,
    InfixExpression,
    Number,
    Parameter,
    PiType,
    PrefixExpression,
);

/// Implement [`IntoPyObject`] and [`FromPyObject`] for [`Expression`],
/// dispatching to/from the concrete subclass for each variant.
impl<'py> IntoPyObject<'py> for Expression {
    type Target = PyAny;
    type Output = Bound<'py, Self::Target>;
    type Error = PyErr;

    fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
        match self {
            Expression::Address(value) => Address(value).into_bound_py_any(py),
            Expression::FunctionCall(value) => value.into_bound_py_any(py),
            Expression::Infix(value) => value.into_bound_py_any(py),
            Expression::Number(value) => Number(value).into_bound_py_any(py),
            Expression::PiConstant => PiType.into_bound_py_any(py),
            Expression::Prefix(value) => value.into_bound_py_any(py),
            Expression::Variable(name) => Parameter { name }.into_bound_py_any(py),
        }
    }
}

impl<'a, 'py> FromPyObject<'a, 'py> for Expression {
    type Error = PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        if let Ok(value) = obj.cast::<Address>() {
            Ok(Expression::Address(value.get().0.clone()))
        } else if let Ok(value) = obj.cast::<FunctionCallExpression>() {
            Ok(Expression::FunctionCall(value.get().clone()))
        } else if let Ok(value) = obj.cast::<InfixExpression>() {
            Ok(Expression::Infix(value.get().clone()))
        } else if let Ok(value) = obj.cast::<Number>() {
            Ok(Expression::Number(value.get().0))
        } else if obj.cast::<PiType>().is_ok() {
            Ok(Expression::PiConstant)
        } else if let Ok(value) = obj.cast::<PrefixExpression>() {
            Ok(Expression::Prefix(value.get().clone()))
        } else if let Ok(value) = obj.cast::<Parameter>() {
            Ok(Expression::Variable(value.get().name.clone()))
        } else {
            Err(CastError::new(obj, PyExpression::classinfo_object(obj.py())))?
        }
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl InfixExpression {
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
