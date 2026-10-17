use numpy::{PyArray, PyArrayDescr, PyArrayDescrMethods};
use pyo3::{
    exceptions::{PyDeprecationWarning, PyNotImplementedError},
    prelude::*,
    types::{PyAnyMethods, PyComplex, PyFloat, PyInt},
    CastError, IntoPyObjectExt, PyClass, PyTraverseError, PyTypeCheck, PyVisit,
};
use rigetti_pyo3::{create_init_submodule, impl_repr};

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pyfunction, gen_stub_pymethods};

use super::*;
use crate::{
    pickleable_new,
    quil::Quil,
    quilpy::{
        errors::{self, ValueError},
        impl_match_args,
        memory::{PyMemRef, TryAsBorrowed},
        pickleable_enum,
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
        Number,
        Parameter,
        PiType,
        Address, // TODO: Make DeclarationAt or something work here instead.
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

/// All the things that can be converted into an [`Expression`],
/// for use as a parameter in methods and functions exposed through Python bindings.
///
/// This is distinct from both [`Expression`] and [`PyExpression`]:
///
/// - `ExpressionLike` unifies all the things that can be taken from Python
///   and used in a context that requires an `Expression`.
/// - `Expression` is the Rust-specific enum responsible for actual implementation logic;
///   it typically shouldn't be the type of a parameter in Python-exposed methods and functions.
/// - `PyExpression` is the base-class for all Python-exposed Expression types.
///   If extracted, it has NO associated data, so it's mostly only useful for type-checking,
///   but it can be used as `Bound<'_, PyExpression>`, which is needed
///   for its own `#[pymethods]` block to provide methods for all `Expression` variants.
///   You can use `Expression::try_from` to get the actual `Expression` variant.
///
// TODO: Now that `Expression` is _not_ a PyClass, can I use it as an input parameter?
// FromPyObject is implemented manually to handle specifics around PyExpression subclassing.
#[derive(Debug, Clone)]
// Generally we don't want to return this as a Python type,
// but the stub generator needs this when constructing default values in signatures.
#[cfg_attr(feature = "stubs", derive(IntoPyObject))]
pub(crate) enum ExpressionLike {
    Variable(String),
    PiConstant(PiType),
    Number(Complex64),
    MemoryReference(MemoryReference), // TODO: rename Address
    FunctionCall(FunctionCallExpression),
    Infix(InfixExpression),
    Prefix(PrefixExpression),
}

impl<'a, 'py> FromPyObject<'a, 'py> for ExpressionLike {
    type Error = CastError<'a, 'py>;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        if let Ok(expr_obj) = obj.cast::<PyExpression>() {
            if let Ok(expr) = Expression::try_from(std::ops::Deref::deref(&expr_obj)) {
                return Ok(expr.into());
            }
        }

        // Use `extract` here so we allow e.g. complex, float, int, numpy scalars.
        if let Ok(value) = obj.extract::<Complex64>() {
            return Ok(ExpressionLike::Number(value));
        }

        // Ensure this is first so we convert to variables rather than memrefs
        if let Ok(value) = obj.extract::<String>() {
            return Ok(ExpressionLike::Variable(value));
        }

        if let Ok(value) = obj.extract::<PyMemRef<'py>>() {
            return Ok(ExpressionLike::MemoryReference(value.into()));
        }

        Err(CastError::new(
            obj,
            <PyExpression as PyTypeCheck>::classinfo_object(obj.py()),
        ))
    }
}

/// Extract an [`ExpressionView`] from a Python object, which must be a [`PyExpression`] subclass,
/// and more specifically, a subclass that this crate knows about.
impl<'a, 'py> FromPyObject<'a, 'py> for ExpressionView<'a> {
    type Error = CastError<'a, 'py>;

    // TODO: Handle more Memory types -- likely drop Address in favor of PyMemRef or something.
    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        if let Ok(obj) = obj.cast_exact::<Parameter>() {
            Ok(obj.get().as_view())
        } else if obj.cast_exact::<PiType>().is_ok() {
            Ok(ExpressionView::PiConstant)
        } else if let Ok(obj) = obj.cast_exact::<Number>() {
            Ok(obj.get().as_view())
        } else if let Ok(obj) = obj.cast_exact::<Address>() {
            Ok(obj.get().as_view())
        } else if let Ok(obj) = obj.cast_exact::<FunctionCallExpression>() {
            Ok(obj.get().as_view())
        } else if let Ok(obj) = obj.cast_exact::<InfixExpression>() {
            Ok(obj.get().as_view())
        } else if let Ok(obj) = obj.cast_exact::<PrefixExpression>() {
            Ok(obj.get().as_view())
        } else {
            // It's possible to get here if:
            // - A Python user subclasses `Expression` (which we don't support).
            // - A Rust developer adds a new `#[pyclass(extends = PyExpression)]`
            //   but forgets to update this implementation.
            Err(CastError::new(
                obj,
                <PyExpression as PyTypeCheck>::classinfo_object(obj.py()),
            ))
        }
    }
}

impl<'a, 'py> TryFrom<&'a Bound<'py, PyExpression>> for Expression {
    type Error = CastError<'a, 'py>;

    fn try_from(value: &'a Bound<'py, PyExpression>) -> Result<Self, Self::Error> {
        // Note: the `cast_exact` assumes we don't create new subclasses of these.
        // They currently can't be subclassed (neither in Rust nor in Python),
        // but there's no easy way to catch that at compile time;
        // there are unit tests for this currently, but that's not a perfect solution.
        // TODO(testing): Actually add unit tests that ensure these can't be subclassed.
        if let Ok(slf) = value.cast_exact::<FunctionCallExpression>() {
            Ok(Expression::FunctionCall(slf.get().clone()))
        } else if let Ok(slf) = value.cast_exact::<InfixExpression>() {
            Ok(Expression::Infix(slf.get().clone()))
        } else if let Ok(slf) = value.cast_exact::<PrefixExpression>() {
            Ok(Expression::Prefix(slf.get().clone()))
        } else if value.cast_exact::<PiType>().is_ok() {
            Ok(Expression::PiConstant)
        } else if let Ok(slf) = value.cast_exact::<Number>() {
            Ok(Expression::Number(slf.get().value))
        } else if let Ok(slf) = value.cast_exact::<Parameter>() {
            Ok(Expression::Variable(slf.get().name.clone()))
        } else if let Ok(slf) = value.cast_exact::<Address>() {
            Ok(Expression::Address(
                slf.get().memory_reference.get().clone(),
            ))
        } else {
            Err(CastError::new(
                value.as_any().as_borrowed(),
                <PyExpression as PyTypeCheck>::classinfo_object(value.py()),
            ))
        }
    }
}

/// [`Expression`] implements [`FromPyObject`] so it can be used within other Python-exposed types.
///
// TODO: Determine if this should be true:
// Note that when used, the Python object must be a [`PyExpression`] subclass instance,
// not just something that can be converted into an [`Expression`].
// Use [`ExpressionLike`] if you also want that.
// Currently, we _do_ convert other if they can be, and maybe that is the right way to go?
// It means that we don't have an easy way (in Rust) to explicitly say
// "this Python-exposed function requires an existing (Py)Expression instance",
// but perhaps we don't have any reason for that.
// Should we even have `ExpressionLike`?
impl<'a, 'py> FromPyObject<'a, 'py> for Expression {
    type Error = CastError<'a, 'py>;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        if let Ok(obj) = obj.cast::<PyExpression>() {
            if let Ok(obj) = Expression::try_from(std::ops::Deref::deref(&obj)) {
                return Ok(obj);
            }
        }

        // If the above claim should be true, this is the right thing to return immediately.
        // Err(CastError::new(obj, <PyExpression as PyTypeCheck>::classinfo_object(obj.py())))

        // Otherwise, route through ExpressionLike.
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
            Expression::Address(memory_reference) => Address {
                memory_reference: memory_reference.into_pyobject(py)?.unbind(),
            }
            .into_bound_py_any(py),
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
impl_repr!(InfixOperator);
impl_repr!(PrefixOperator);

// These all inherit `to_quil`, `to_quil_or_debug`, and `__repr__` from `PyExpression`.
py_expression!(Address);
py_expression!(FunctionCallExpression);
py_expression!(InfixExpression);
py_expression!(Number);
py_expression!(Parameter);
// PyType is implemented as a singleton and implemented manually.
py_expression!(PrefixExpression);

// TODO: We likely could take a similar approach with Instruction subclasses.
// The main advantage is that those methods show up on the base class.
// It's unclear how impactful this is on performance, but that might be worth benchmarking.

/// Convert a bound `#[pyclass]` into a `numpy` object array holding just that instance.
fn to_obj_array<'py, T: PyClass>(obj: &Bound<'py, T>) -> Bound<'py, PyAny> {
    let py = obj.py();
    let as_any = obj.to_owned().unbind().into_any();
    PyArray::from_owned_object_array(py, ndarray::arr0(as_any)).into_any()
}

/// Convert a scalar value into a `numpy` array holding just that value.
fn scalar_array<'py, T: numpy::Element>(py: Python<'py>, value: T) -> Bound<'py, PyAny> {
    PyArray::from_owned_array(py, ndarray::arr0(value)).into_any()
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

/*
impl<T: PyClass<BaseType = PyExpression>> AsView for Bound<'_, T> {
    fn as_view(&self) -> ExpressionView<'_> {
        let result = if let Ok(obj) = self.cast_exact::<Parameter>() {
            obj.get().as_view()
        } else if self.cast_exact::<PiType>().is_ok() {
            ExpressionView::PiConstant
        } else if let Ok(obj) = self.cast_exact::<Number>() {
            obj.get().as_view()
        } else if let Ok(obj) = self.cast_exact::<Address>() {
            obj.get().as_view()
        } else if let Ok(obj) = self.cast_exact::<FunctionCallExpression>() {
            obj.get().as_view()
        } else if let Ok(obj) = self.cast_exact::<InfixExpression>() {
            obj.get().as_view()
        } else if let Ok(obj) = self.cast_exact::<PrefixExpression>() {
            obj.get().as_view()
        } else {
            // There are two ways to trigger this panic:
            // - In Python, subclass `Expression` (which we don't support) and get it here.
            //   This will typically manifest as an exception, or else crash the interpretter;
            //   either way, the user will see a message telling them don't subclass `Expression`.
            // - In Rust, add a new `#[pyclass(extends = PyExpression)]`
            //   but forget to update this implementation.
            //   That should not be possible due to the `PythonExpression` bound.
            panic!("Unknown Expression subclass");
        };

        result
    }
}
*/

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PyExpression {
    #[new]
    #[pyo3(signature = (arg, /))]
    fn __new__(arg: ExpressionLike) -> Expression {
        arg.into()
    }

    /// Parse an ``Expression`` from a string.
    ///
    /// Raises a ``ParseExpressionError`` error if the string isn't a valid Quil expression.
    #[staticmethod]
    fn parse(input: &str) -> PyResult<Expression> {
        Ok(Expression::from_str(input)?)
    }

    // TODO: Benchmark if it's faster to have these implemented on the individual subclasses.
    // I suspect doing it that way may be better for performance
    // because it certainly avoids constructing the actual `Expression` enum.
    fn to_quil(slf: &Bound<'_, Self>) -> PyResult<String> {
        Ok(slf
            .as_borrowed()
            .extract::<ExpressionView<'_>>()?
            .to_quil()?)
    }

    fn to_quil_or_debug(slf: &Bound<'_, Self>) -> PyResult<String> {
        Ok(slf
            .as_borrowed()
            .extract::<ExpressionView<'_>>()?
            .to_quil_or_debug())
    }

    fn __repr__(slf: &Bound<'_, Self>) -> PyResult<String> {
        Ok(format!("{:?}", Expression::try_from(slf)?))
    }

    fn __str__(slf: &Bound<'_, Self>) -> PyResult<String> {
        Self::to_quil_or_debug(slf)
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
        variables: Option<HashMap<Parameter, Complex64>>,
        memory_references: Option<HashMap<MemRefKey<'_>, Vec<f64>>>,
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
        variable_values: HashMap<Parameter, ExpressionLike>,
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
        let mut memory_references: HashMap<_, MemoryMap> = HashMap::new();
        for (key, value) in d {
            match (key, value) {
                (
                    SubstitutionKey::Str(name) | SubstitutionKey::Parameter(Parameter { name }),
                    SubstitutionValue::Variable(value),
                ) => {
                    variables.insert(name, value);
                }
                (SubstitutionKey::Str(name), SubstitutionValue::Memory(values)) => {
                    memory_references.insert(name, MemoryMap::Contiguous(values));
                }
                (SubstitutionKey::MemoryReference(memref), SubstitutionValue::Memory(values)) => {
                    // We should probably handle the case where memref.index() != 0,
                    // but for now, we'll assume it does.
                    memory_references
                        .insert(memref.name().to_string(), MemoryMap::Contiguous(values));
                }
                (SubstitutionKey::MemoryReference(memref), SubstitutionValue::Variable(value)) => {
                    if !(value.is_finite() && value.im == 0.0) {
                        return Err(ValueError::new_err(format!(
                            "Expected a real number for MemoryReference '{memref}', but got a complex number: {value}"
                        )));
                    }
                    let value = value.re;
                    let idx = memref.index() as usize;

                    // See if we've already seen this memory reference in the substitutions.
                    // If so, add this variable substitution to the existing mapping.
                    // Otherwise, create a new mapping for this memory reference.
                    memory_references
                        .entry(memref.name().to_string())
                        .and_modify(|existing| {
                            match existing {
                                MemoryMap::Contiguous(existing_values) => {
                                    if idx == existing_values.len() {
                                        existing_values.push(value);
                                    } else if idx < existing_values.len() {
                                        existing_values[idx] = value;
                                    } else {
                                        // Swap to a disjoint mapping if the index is not contiguous.
                                        let mut new_map = HashMap::new();
                                        for (i, v) in existing_values.iter().enumerate() {
                                            new_map.insert(i, *v);
                                        }
                                        new_map.insert(idx, value);
                                        *existing = MemoryMap::Disjoint(new_map);
                                    }
                                }
                                MemoryMap::Disjoint(existing_values) => {
                                    existing_values.insert(idx, value);
                                }
                            }
                        })
                        .or_insert_with(|| match memref.index() {
                            0 => MemoryMap::Contiguous(vec![value]),
                            _ => MemoryMap::Disjoint(HashMap::from([(idx, value)])),
                        });
                }
                (SubstitutionKey::Parameter(param), SubstitutionValue::Memory(_)) => {
                    return Err(ValueError::new_err(format!(
                        "Expected a single complex value for Parameter '{param}', but got a memory reference substitution"
                    )));
                }
            }
        }

        match Expression::try_from(slf)?.evaluate_partial(&variables, &memory_references) {
            Expression::PiConstant => Ok(Evaluated::Full(Complex64::new(PI, 0.0))),
            Expression::Number(c) => Ok(Evaluated::Full(c)),
            other => Ok(Evaluated::Partial(other)),
        }
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
        Expression::try_from(slf)?.into_complex()
    }

    /// Simplify the expression to a complex number, if possible.
    ///
    /// This may be slow for large recursive expressions.
    ///
    /// This will raise an error if simplification doesn't result in a complex number.
    /// Use `into_simplified` to get a simplified `Expression`, or use `evaluate(partial=True)`
    /// to get a complex number when possible or a partially simplified `Expression` otherwise.
    /// Note that the latter in particular accepts variable and memory reference substitutions,
    /// which can be used to simplify an `Expression` that contains such references.
    ///
    /// This is equivalent to `complex(expr)` or `expr.__complex__()`.
    fn into_complex(slf: &Bound<'_, Self>) -> PyResult<Complex64> {
        Expression::try_from(slf)?.into_complex()
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
                    scalar_array(py, simplified)
                } else {
                    to_obj_array(slf)
                }
            }
            Some(dtype) => {
                if dtype.is_equiv_to(&PyArrayDescr::object(py)) {
                    to_obj_array(slf)
                } else if dtype.is_equiv_to(&numpy::dtype::<f64>(py)) {
                    scalar_array(py, PyExpression::__float__(slf)?)
                } else {
                    scalar_array(py, PyExpression::__complex__(slf)?)
                }
            }
        };

        Ok(arr)
    }
}

impl Expression {
    /// Convert this [`Expression`] into a complex number, if possible.
    ///
    /// This may be slow for large recursive expressions.
    /// This will return an error if simplification doesn't result in a complex number.
    /// Use [`Expression::into_simplified`] to get a simplified [`Expression`],
    /// or use [`Expression::evaluate_partial`] if substitutions are desired.
    pub(crate) fn into_complex(self) -> PyResult<Complex64> {
        match self.into_simplified() {
            Expression::PiConstant => Ok(PI.into()),
            Expression::Number(c) => Ok(c),
            _ => Err(EvaluationError::NotANumber)?,
        }
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

// TODO(migration-guide): In PyQuil v4, there wasn't a dedicated `Pi` type.
// To convert to/from the `Expression::PiConstant` variant, it used `numpy.pi` instead.
// It's perhaps worth noting that the `__str__`/`__repr__` implementatinos of `numpy.pi`
// are string representations of its floating point value (`"3.14..."`).
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PiType {
    fn __str__(&self) -> &'static str {
        "π"
    }

    fn __repr__(&self) -> &'static str {
        "Pi"
    }
}

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
#[derive(Clone, Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil._quil.expression", extends = PyExpression, eq, frozen, hash, skip_from_py_object, get_all)
)]
#[cfg_attr(not(feature = "python"), strip_pyo3)]
pub(crate) struct Parameter {
    name: String, // TODO: replace this with `Py<PyString>` if possible.
}

impl std::fmt::Display for Parameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.name)
    }
}

impl std::borrow::Borrow<str> for Parameter {
    fn borrow(&self) -> &str {
        &self.name
    }
}

// Ensure that `Parameter` hashes and compares exactly like `String`.
// This is a requirement for it to implement Borrow<str> in a valid way.
impl std::hash::Hash for Parameter {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }
}

impl Eq for Parameter {}
impl PartialEq for Parameter {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

/// Wrapper around [`Parameter`] with type stubs that indicate it the method accepts `str`s, too.
#[derive(Clone, Debug, Hash, PartialEq, Eq, FromPyObject)]
#[cfg_attr(feature = "stubs", derive(IntoPyObject))]
pub(crate) struct ParameterLike(Parameter);

impl From<String> for Parameter {
    fn from(name: String) -> Self {
        Self { name }
    }
}

impl From<ParameterLike> for Parameter {
    fn from(value: ParameterLike) -> Self {
        value.0
    }
}

impl From<ParameterLike> for String {
    fn from(value: ParameterLike) -> Self {
        value.0.name
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

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Parameter {
    fn __str__(&self) -> String {
        format!("%{}", self.name)
    }

    fn __repr__(&self) -> String {
        format!(r#"Parameter("{}")"#, self.name)
    }
}

/// A memory reference embedded in an expression, e.g. ``theta[0]``.
///
/// This corresponds to the [`Expression::Address`] variant.
/// It's a distinct type from [`MemoryReference`] because a [`MemoryReference`] properly
/// is a kind of Quil instruction (and hence a `quil` [`Instruction`]),
/// whereas an [`Address`] is the use of a memory reference within an expression.
#[derive(Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil._quil.expression", extends = PyExpression, eq, frozen, hash, skip_from_py_object, get_all)
)]
#[cfg_attr(not(feature = "python"), strip_pyo3)]
pub(crate) struct Address {
    /// The underlying memory reference, e.g. `theta[0]`.
    pub(crate) memory_reference: Py<MemoryReference>,
}
// TODO: Can this be united with `PyMemRef`?

impl FromPyObject<'_, '_> for Address {
    type Error = PyErr;

    fn extract(obj: Borrowed<'_, '_, PyAny>) -> Result<Self, Self::Error> {
        let memref = if let Ok(value) = obj.cast::<Address>() {
            Ok(value.get().memory_reference.clone_ref(obj.py()))
        } else if let Ok(memref) = obj.cast::<MemoryReference>() {
            Ok(memref.to_owned().unbind())
        } else {
            Err(CastError::new(
                obj,
                <Address as PyTypeCheck>::classinfo_object(obj.py()),
            ))
        }?;
        Ok(Self {
            memory_reference: memref,
        })
    }
}

impl Hash for Address {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.memory_reference.get().hash(state)
    }
}

impl Eq for Address {}
impl PartialEq for Address {
    fn eq(&self, other: &Self) -> bool {
        self.memory_reference.get() == other.memory_reference.get()
    }
}

impl PartialEq<MemoryReference> for Address {
    fn eq(&self, other: &MemoryReference) -> bool {
        self.memory_reference.get() == other
    }
}

impl From<Address> for Expression {
    fn from(val: Address) -> Self {
        Expression::Address(val.memory_reference.get().clone())
    }
}

impl AsView for Address {
    fn as_view(&self) -> ExpressionView<'_> {
        ExpressionView::Address(self.memory_reference.get())
    }
}

impl AsView for Bound<'_, Address> {
    fn as_view(&self) -> ExpressionView<'_> {
        ExpressionView::Address(self.get().memory_reference.get())
    }
}

impl_match_args!(Address("memory_reference"));

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Address {
    #[new]
    fn __new__<'py>(py: Python<'py>, memory_reference: PyMemRef<'py>) -> PyResult<Self> {
        let memref = memory_reference
            .try_borrow(py)
            .map(|memref| Ok(memref.to_owned().unbind()))
            .unwrap_or_else(|| Py::new(py, MemoryReference::from(memory_reference)))?;

        Ok(Self {
            memory_reference: memref,
        })
    }

    fn __getnewargs__<'py>(&self, py: Python<'py>) -> (&Bound<'py, MemoryReference>,) {
        (self.memory_reference.bind(py),)
    }

    #[gen_stub(skip)]
    fn __traverse__(&self, visit: PyVisit<'_>) -> Result<(), PyTraverseError> {
        visit.call(&self.memory_reference)?;
        Ok(())
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

pickleable_new! {
    impl Number {
        fn __new__(value: Complex64);
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Number {
    fn __str__(&self) -> String {
        format!("{}", self.value)
    }

    fn __repr__(&self) -> String {
        format!(r#"Number("{}")"#, self.value)
    }
}

impl_match_args!(InfixExpression("left", "operator", "right"));

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

impl_match_args!(PrefixExpression("operator", "expression"));

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

impl_match_args!(FunctionCallExpression("function", "expression"));

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

pickleable_enum! {
    ExpressionFunction => { Cis("CIS"), Cosine("COSINE"), Exponent("EXPONENT"), Sine("SINE"), SquareRoot("SQUARE_ROOT") },
    InfixOperator => { Caret("CARET"), Plus("PLUS"), Minus("MINUS"), Slash("SLASH"), Star("STAR") },
    PrefixOperator => { Plus("PLUS"), Minus("MINUS") }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl ExpressionFunction {
    #[new]
    #[pyo3(signature = (value, /))]
    fn __new__(mut value: String) -> PyResult<Self> {
        value.make_ascii_uppercase();
        Self::from_str(&value)
            .map_err(|_| ValueError::new_err("not an ExpressionFunction: {value}"))
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PrefixOperator {
    #[new]
    #[pyo3(signature = (value, /))]
    fn __new__(mut value: String) -> PyResult<Self> {
        value.make_ascii_uppercase();
        Self::from_str(&value).map_err(|_| ValueError::new_err("not a PrefixOperator: {value}"))
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl InfixOperator {
    #[new]
    #[pyo3(signature = (value, /))]
    fn __new__(mut value: String) -> PyResult<Self> {
        value.make_ascii_uppercase();
        Self::from_str(&value).map_err(|_| ValueError::new_err("not an InfixOperator: {value}"))
    }
}

// TODO(migration-guide): In PyQuil v4, substitution maps were keyed on a combination of types,
// including `MemoryReference`s; the latter type implemented its own `__hash__` method
// as a combination of the name _and_ memory offset.
// Substitution maps required that each individual memory offset was provided
// as a distinct memory reference (with different offsets, that is).
//

/// Used to store memory reference values in a parameter substitution map.
///
/// In PyQuil v4, users could substitute arbitrary portions of an expression's memory references
/// via individual `MemoryReference`s.
/// Using that method now now will raise deprecation warnings, but it will still work for now.
pub(crate) enum MemoryMap {
    /// A contiguous slice of memory values, starting at offset 0.
    /// We assume users will provide all values, and start with this initially.
    Contiguous(Vec<f64>),
    /// If, when processing their substitutions, we encounter a gap in memory,
    /// we switch to this representation, which allows for arbitrary offsets.
    Disjoint(HashMap<usize, f64>),
}

impl MemoryMapper for MemoryMap {
    fn get(&self, index: usize) -> Option<&f64> {
        match self {
            MemoryMap::Contiguous(values) => values.get(index),
            MemoryMap::Disjoint(map) => map.get(&index),
        }
    }
}

/// A MemoryReference key in a parameter substitution map.
///
/// This allows Python users to provide a bona fide [`MemoryReference`] object
/// (and many things that can be converted into one, like a `MemoryReference`)
/// _or_ just the name of a declared memory reference when calling `Expression.evaluate`.
///
/// In order to be useful in substitution maps, it has to borrow as `str`,
/// which additionally requires it implements Hash and PartialEq/Eq in the same way.
/// In particular, two [`MemRefKey`]s are considered equal
/// exactly when the name they reference is equal,
/// even if there are multiple distinct [`MemoryReference`] objects with the same name.
#[derive(FromPyObject, Debug)]
pub(crate) enum MemRefKey<'py> {
    Str(String),
    MemRef(PyMemRef<'py>),
}

impl std::borrow::Borrow<str> for MemRefKey<'_> {
    fn borrow(&self) -> &str {
        match self {
            Self::Str(var) => var,
            Self::MemRef(memref) => memref.name(),
        }
    }
}

impl std::hash::Hash for MemRefKey<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let borrowed: &str = self.borrow();
        borrowed.hash(state)
    }
}

impl Eq for MemRefKey<'_> {}
impl PartialEq for MemRefKey<'_> {
    fn eq(&self, other: &Self) -> bool {
        let x: &str = self.borrow();
        let y: &str = other.borrow();
        x == y
    }
}

/// A key in a parameter substitution map.
///
/// This allows Python users to provide a single dictionary when calling `Expression.substitute`,
/// rather than needing to provide separate dictionaries for variables versus memory references.
///
/// It is distinct from [`MemRefKey`] because when given a bare string,
/// we need to know whether the value is a single complex number (for a variable)
/// or a list of floats (for a memory reference).
#[derive(FromPyObject, Debug)]
pub(crate) enum SubstitutionKey<'py> {
    Str(String),
    Parameter(Parameter),
    MemoryReference(PyMemRef<'py>),
}

impl std::borrow::Borrow<str> for SubstitutionKey<'_> {
    fn borrow(&self) -> &str {
        match self {
            SubstitutionKey::Str(var) => var,
            SubstitutionKey::Parameter(param) => param.borrow(),
            SubstitutionKey::MemoryReference(memref) => memref.name(),
        }
    }
}

impl std::hash::Hash for SubstitutionKey<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let borrowed: &str = self.borrow();
        borrowed.hash(state)
    }
}

impl Eq for SubstitutionKey<'_> {}
impl PartialEq for SubstitutionKey<'_> {
    fn eq(&self, other: &Self) -> bool {
        let x: &str = self.borrow();
        let y: &str = other.borrow();
        x == y
    }
}

#[derive(FromPyObject, Debug, Clone)]
pub(crate) enum SubstitutionValue {
    Variable(Complex64),
    Memory(Vec<f64>),
}

impl From<SubstitutionKey<'_>> for Expression {
    fn from(key: SubstitutionKey) -> Self {
        match key {
            // This assumes bare strings are meant to be variable names, not memory references.
            SubstitutionKey::Str(var) => Expression::Variable(var),
            SubstitutionKey::Parameter(param) => Expression::Variable(param.name),
            SubstitutionKey::MemoryReference(memref) => Expression::Address(memref.into()),
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

// Used to require that T extends PyExpression and provides an `AsView` implementation.
struct PyExpr<T>(pub T);
impl<T> From<PyExpr<T>> for pyo3::PyClassInitializer<T>
where
    T: pyo3::PyClass<BaseType = PyExpression> + AsView,
{
    fn from(value: PyExpr<T>) -> Self {
        pyo3::PyClassInitializer::from(super::PyExpression).add_subclass(value.0)
    }
}

mod macros {
    /// Implement [`IntoPyObject`] for a `#[pyclass(extends = PyExpression)]`.
    macro_rules! py_expression {
        ($T:ty) => {
            impl<'py> pyo3::IntoPyObject<'py> for $T {
                type Target = Self;
                type Output = Bound<'py, Self::Target>;
                type Error = PyErr;

                fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
                    Ok(Py::new(py, PyClassInitializer::from(PyExpr(self)))?.into_bound(py))
                }
            }
        };
    }

    pub(super) use py_expression;
}

#[cfg(feature = "stubs")]
pub(crate) mod stubs {
    use pyo3_stub_gen::{impl_stub_type, runtime::PyRuntimeType, type_alias, PyStubType, TypeInfo};

    use std::collections::HashSet;

    #[allow(clippy::wildcard_imports)]
    use super::*;
    use crate::quilpy::union;

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

    // Note: In stubs, these expand to the (potential expansion of the) types on the right,
    // so when sensible, prefer using a type alias, as it keeps things tidier.
    // That said, only use a type alias if the type is semantically equivalent;
    // otherwise, the longer type string is likely more appropriate and helpful.
    impl_stub_type!(Expression = PyExpression);
    impl_stub_type!(ExpressionLike = ExpressionDesignator | MemoryReference | String);
    impl_stub_type!(MemRefKey<'_> = PyMemRef<'_> | String);
    impl_stub_type!(SubstitutionKey<'_> = Parameter | PyMemRef<'_> | String);
    impl_stub_type!(SubstitutionValue = Complex64 | Vec<Complex64>);
    impl_stub_type!(Evaluated = Expression | Complex64);

    // Note: Proper aliases MUST be added to the module in `post_init`!
    type_alias!(
        "quil._quil.expression",
        ExpressionValueDesignator = i64 | f64 | Complex64
    );
    type_alias!(
        "quil._quil.expression",
        ExpressionDesignator = Expression | ExpressionValueDesignator
    );
    type_alias!(
        "quil._quil.expression",
        ParameterDesignator = ExpressionDesignator | MemoryReference
    );

    impl PyRuntimeType for ExpressionValueDesignator {
        fn runtime_type_object(py: Python<'_>) -> PyResult<Bound<'_, PyAny>> {
            union!(py, PyInt, PyFloat, PyComplex)
        }
    }

    impl PyRuntimeType for ExpressionDesignator {
        fn runtime_type_object(py: Python<'_>) -> PyResult<Bound<'_, PyAny>> {
            union!(py, PyExpression, PyInt, PyFloat, PyComplex)
        }
    }
}
