//! Python bindings for Pauli-related types ([`PauliGate`], [`PauliTerm`], and [`PauliSum`]).

use std::{
    collections::{HashMap, HashSet},
    mem,
    sync::atomic::{AtomicUsize, Ordering},
};

use itertools::Itertools as _;
use num_complex::Complex64;
use pyo3::{
    exceptions::{
        PyDeprecationWarning, PyIndexError, PyNotImplementedError, PyTypeError, PyUserWarning,
        PyValueError,
    },
    prelude::*,
    types::{IntoPyDict as _, PyComplex, PyDict, PyFloat, PyFrozenSet, PyInt, PyString},
    CastError, IntoPyObjectExt, PyTypeCheck,
};

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::{gen_methods_from_python, gen_stub_pyclass, gen_stub_pymethods};

use crate::{
    expression::{
        consts::{IMAGINARY_UNIT, ONE, ZERO},
        quilpy::{quil_exp, ExpressionLike, PyExpression, QualifiedExpression},
        Expression,
    },
    instruction::{Gate, Instruction, ParseInstructionError, PauliGate, PauliSum, PauliTerm, Qubit},
    quil::Quil,
    quilpy::{errors, py_deprecated, Migrate},
    Program,
};

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PauliGate {
    #[new]
    fn __new__(input: &str) -> Result<Self, ParseInstructionError> {
        Self::parse(input)
    }

    /// Parse a ``PauliGate`` from a string.
    ///
    /// Raises a ``ParseExpressionError`` error if the string isn't a valid Quil expression.
    #[staticmethod]
    fn parse(input: &str) -> Result<Self, ParseInstructionError> {
        <Self as std::str::FromStr>::from_str(input)
            .map_err(|err| ParseInstructionError::Parse(err.to_string()))
    }
}

impl PauliGate {
    /// Return the product and a complex phase result of multiplying two gates.
    fn product(self, other: PauliGate) -> (PauliGate, Complex64) {
        match (self, other) {
            (PauliGate::I, g) | (g, PauliGate::I) => (g, Complex64::new(1.0, 0.0)),
            (PauliGate::X, PauliGate::X)
            | (PauliGate::Y, PauliGate::Y)
            | (PauliGate::Z, PauliGate::Z) => (PauliGate::I, Complex64::new(1.0, 0.0)),
            (PauliGate::X, PauliGate::Y) => (PauliGate::Z, Complex64::new(0.0, 1.0)),
            (PauliGate::X, PauliGate::Z) => (PauliGate::Y, Complex64::new(0.0, -1.0)),
            (PauliGate::Y, PauliGate::X) => (PauliGate::Z, Complex64::new(0.0, -1.0)),
            (PauliGate::Y, PauliGate::Z) => (PauliGate::X, Complex64::new(0.0, 1.0)),
            (PauliGate::Z, PauliGate::X) => (PauliGate::Y, Complex64::new(0.0, 1.0)),
            (PauliGate::Z, PauliGate::Y) => (PauliGate::X, Complex64::new(0.0, -1.0)),
        }
    }
}

impl<'a, 'py> FromPyObject<'a, 'py> for PauliGate {
    type Error = PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        if let Ok(gate) = obj.cast::<PauliGate>() {
            Ok(*gate.get())
        } else if let Ok(s) = obj.extract::<String>() {
            Ok(PauliGate::parse(&s)?)
        } else {
            Err(CastError::new(obj, PauliGate::classinfo_object(obj.py())))?
        }
    }
}

/// Argument type when constructing a `PauliTerm` from Python.
///
/// Technically, we only accept `Qubit::Variable` and `Qubit::Fixed` (or `str` and `int`),
/// but this wraps `Qubit` and raises a type error if the input is `Qubit::Placeholder`.
// TODO: Now that there are PyQubit subclasses, limit this to the fixed and variable subclasses.
#[derive(FromPyObject)]
pub(crate) struct PauliArg(Qubit);

impl TryFrom<PauliArg> for String {
    type Error = PyErr;

    fn try_from(value: PauliArg) -> Result<Self, Self::Error> {
        match value.0 {
            Qubit::Variable(v) => Ok(v),
            Qubit::Fixed(v) => Ok(format!("q{v}")),
            Qubit::Placeholder(_) => Err(PyTypeError::new_err(
                "cannot use Qubit::Placeholder as a PauliTerm argument",
            )),
        }
    }
}

fn convert_pauli_targets(values: Vec<PauliArg>) -> PyResult<Vec<String>> {
    let mut result = Vec::with_capacity(values.len());
    for value in values {
        result.push(String::try_from(value)?);
    }
    Ok(result)
}

#[cfg(feature = "stubs")]
pyo3_stub_gen::inventory::submit! {
    gen_methods_from_python! {
        r#"
        import builtins
        import typing
        import typing_extensions

        from quil._quil.expression import ExpressionDesignator

        class PauliTerm:
            @typing.overload
            def __new__(
                cls,
                op: typing.Literal[PauliGate.I] | typing.Literal["I"],
                index: PauliTargetDesignator | None,
                coefficient: _quil.expression.ExpressionDesignator = 1.0,
            ) -> PauliTerm:
                """Construct a `PauliTerm` for a single Identity operator."""

            @typing.overload
            def __new__(
                cls,
                op: PauliGate | str,
                index: PauliTargetDesignator | None,
                coefficient: _quil.expression.ExpressionDesignator = 1.0,
            ) -> PauliTerm:
                """Construct a `PauliTerm` for a single operator and argument."""

            @typing.overload
            @typing_extensions.deprecated("this constructor is deprecated; use `PauliTerm.from_list` instead")
            def __new__(
                cls,
                arguments: collections.abc.Sequence[tuple[PauliGate | str, PauliTargetDesignator]],
                expression: _quil.expression.ExpressionDesignator = 1.0,
            ) -> PauliTerm:
                """Construct a `PauliTerm` from a sequence of arguments."""

            @typing.overload
            def __mul__(self, other: PauliTerm | _quil.expression.ExpressionDesignator) -> PauliTerm: ...
            @typing.overload
            def __mul__(self, other: PauliSum) -> PauliSum: ...

            @typing.overload
            def __add__(self, other: _quil.expression.ExpressionDesignator) -> PauliTerm: ...
            @typing.overload
            def __add__(self, other: PauliTerm | PauliSum) -> PauliSum: ...

            @typing.overload
            def __radd__(self, other: _quil.expression.ExpressionDesignator) -> PauliTerm: ...
            @typing.overload
            def __radd__(self, other: PauliTerm | PauliSum) -> PauliSum: ...

            @typing.overload
            def __pow__(self, exponent: builtins.int, modulo: None=None) -> PauliTerm:
                r"""Compute an integer power of this `PauliTerm`.

                Integer exponents can be computed efficiently into a single `PauliTerm`.
                See the documentation on [`PauliTerm::__pow__`] for more information.
                """
            # Note: Do **NOT** specify `float` or `complex` in these annotations!
            @typing.overload
            def __pow__(self, exponent: _quil.expression.Expression | builtins.str, modulo: None=None) -> PauliSum:
                r"""Compute the power of this `PauliTerm`.

                Note: This method additionally supports `float`, but cannot be typed as such
                due a limitation within [the Python type system specification][type_spec]
                concerning the [special case for `float` and `complex`][special_case].
                Type checkers like `mypy` and Pyright implement that special case unsoundly
                by treating `float := int | float` and `complex := int | float | complex`,
                causing errors when diverging overloads dependent on `float` or `complex`.
                In other words, if this specified the `exponent` could be a `float | complex`,
                then those type checkers would report that they overlap with the `int` case.

                If we don't `@overload` the integer case, when a user writes `term ** some_int`,
                the type checker will say the result is a `PauliSum`,
                even though it will actually be a single `PauliTerm`.
                Because (non-Expression) integers are the more likely value to be used as an exponent,
                and because users should (correctly) assume the result can be a single `PauliTerm`,
                we choose to `@overload` the integer case and report that the result is a `PauliTerm`.

                As a consequence, users who do write `term ** some_float` will get a type error.
                They can ignore the type error by appending `# pyright:ignore[reportOperatorIssue]`,
                but if they know the `float` can be represented as an `int`, they should cast it,
                or wrap the `float` in an `Expression` to get a `PauliSum` result.

                For more discussion, see:
                https://discuss.python.org/t/clarifying-the-float-int-complex-special-case/54018/71

                [type_spec]: https://typing.python.org/en/latest/spec/type-system.html
                [special_case]: https://typing.python.org/en/latest/spec/special-types.html#special-cases-for-float-and-complex
                """
            @typing.overload
            def __pow__(self, exponent: typing.Any, modulo: typing.Any) -> typing.NoReturn:
                """Raising a `PauliTerm` with a `modulo` argument is not supported."""
        "#
    }
}

/// Used to extract the `exponent` in [`PauliTerm::__pow__`].
///
/// Integer arguments are more likely, and they have a more efficient implementation,
/// so we handle them separately from other things we can convert into an `Expression`.
/// See the documentation on [`PauliTerm::__pow__`] for more information.
enum PauliExponent {
    Expression(ExpressionLike),
    Int(i64),
    Complex(Complex64),
    Variable(String),
}

// Don't derive FromPyObject because we need to detect Python numbers before ExpressionLike,
// but both will extract it, since Expression implements __complex__.
impl<'a, 'py> FromPyObject<'a, 'py> for PauliExponent {
    type Error = PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        // Check for numbers first, since `ExpressionLike` would extract them.
        if let Ok(value) = obj.cast::<PyInt>() {
            return Ok(Self::Int(value.extract()?));
        }
        if let Ok(value) = obj.cast::<PyFloat>() {
            return Ok(Self::Complex(Complex64::from(value.extract::<f64>()?)));
        }
        if let Ok(value) = obj.cast::<PyComplex>() {
            return Ok(Self::Complex(value.extract()?));
        }

        // Capture `str`s into `Variable`s.
        if let Ok(value) = obj.cast::<PyString>() {
            return Ok(Self::Variable(value.extract()?));
        }

        if let Ok(expr_obj) = obj.cast::<PyExpression>() {
            if let Ok(expr) = Expression::try_from(std::ops::Deref::deref(&expr_obj)) {
                return Ok(Self::Expression(expr.into()));
            }
        }

        Err(PyTypeError::new_err(format!(
            "could not convert {} to an Expression",
            obj.get_type().name()?
        )))
    }
}

// Do not specify `float` or `complex` in this annotation.
// See notes in `PauliTerm::__pow__` for an explanation.
#[cfg(feature = "stubs")]
pyo3_stub_gen::impl_stub_type!(PauliExponent = PyInt | PyString | QualifiedExpression);

/// Apply a binary operation to two [`Expression`]s,
/// with special handling to leave the result as a `Number` when possible.
///
/// Specifically, if both operands are `Number`s,
/// the result is a `Number` with the result of the operation applied.
/// Otherwise, if the operation would mathematically be a no-op (e.g., adding 0 or dividing by 1),
/// the result is just a clone of the other operand.
/// Finally, if neither condition is true, the result is the result of `<a> <op> <b>`.
///
/// # Example Usage
///
/// ```rust,ignore
/// let (a, b) = (Complex64::new(1.0, 2.0), Complex64::new(3.0, 4.0));
/// assert_eq!(simple!(Expression::Number(a) * Expression::Number(b)), Expression::Number(a * b));
/// assert_matches!(simple!(Expression::PiConstant * Expression::Number(b)), Expression::InfixExpression(_));
/// ```
macro_rules! simple {
    // Division is a bit special compared to other operators, so we handle it separately.
    (($a:expr) / ($b:expr)) => {
        match ($a, $b) {
            (_, Expression::Number(Complex64::ZERO)) => $a / $b,
            (Expression::Number(a), Expression::Number(b)) => Expression::Number(*a / *b),
            (a, Expression::Number(Complex64::ONE)) a,
            (a, b) => a / b,
        }
    };

    (($a:expr) ^ ($b:expr)) => {
        match ($a, $b) {
            (_, Expression::Number(Complex64::ZERO)) => Expression::Number(Complex64::ONE),
            (Expression::Number(Complex64::ONE), _) => Expression::Number(Complex64::ONE),
            (a, Expression::Number(Complex64::ONE)) => a,
            (Expression::Number(a), Expression::Number(b)) => {
                // Keep it as an expression if the result is not finite.
                let n = a.powc(b);
                if n.is_finite() {
                    Expression::Number(n)
                } else {
                    Expression::Number(a) ^ Expression::Number(b)
                }
            }
            (a, b) => a ^ b,
        }
    };

    // Handle addition, subtraction, and multiplication by comparing against the group identity.
    (($a:expr) $op:tt ($b:expr), $unit:pat) => {
        match ($a, $b) {
            (Expression::Number(a), Expression::Number(b)) => Expression::Number(a $op b),
            (a, $unit) => a,
            ($unit, b) => b,
            (a, b) => a $op b,
        }
    };

    // Look for an operator token at the head of the left list.
    (@search [* $b:expr] [$a:expr]) => { simple!(($a) * ($b), Expression::Number(Complex64::ONE)) };
    (@search [+ $b:expr] [$a:expr]) => { simple!(($a) + ($b), Expression::Number(Complex64::ZERO)) };
    (@search [- $b:expr] [$a:expr]) => { simple!(($a) - ($b), Expression::Number(Complex64::ZERO)) };
    (@search [/ $b:expr] [$a:expr]) => { simple!(($a) / ($b)) };
    (@search [^ $b:expr] [$a:expr]) => { simple!(($a) ^ ($b)) };

    // Put all the tokens in a left list, then shuffle them to the right until we find an operator.
    (@search [$head:tt $($rest:tt)*] [$($a:tt)+]) => { simple!(@search [$($rest)*] [$($a)+ $head]) };
    (@search [] [$($a:tt)*]) => { compile_error!(
        concat!("expected `<expr> <op> <expr>`; got: ", stringify!($($a),*)))
    };
    ($head:tt $($rest:tt)+) => { simple!(@search [$($rest)+] [$head]) };
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PauliTerm {
    // TODO(migration-guide):
    // - Rust users making use of the `python` feature need to update usage of `__new__`.
    // - Python users of `quil` should be aware of the (temporary) combined API.
    // - In constrast to the behavior of earlier versions of `quil-rs`,
    //   the current version may rearrange and/or simplify arguments here and in `PauliSum`s.
    //   The result is always logically equivalent, but may lead to different Quil output,
    //   and consequently, different compilation and hence observable differences.
    //   Given that a PauliSum is already a pretty high-level abstraction of a gate,
    //   users should expect that compiler optimizations and physical realizations
    //   will impact the observable behavior of a program, in any case.
    // - The PyQuil v4 methods accepted `Qubit::Placeholder` instances
    //   and just converted them into a `str`, leading to invalid Quil identifiers.
    //   Since that was a bug, `Placeholder`s now explicitly raise an error if used.
    // - Relatedly, the PyQuil v4 methods accepted integers and `Qubit::Fixed` instances
    //   and converted them into `str`s directly (e.g., `Qubit::Fixed(0)` became `"0"`),
    //   which again are not a valid Quil identifiers.
    //   This now prefixes them with a `q` to make them valid identifiers,
    //   but that could cause conflicts if a user gave a mix of strings and integers.
    //   Since that's a pretty unlikely thing to see in the wild,
    //   we just document that it's their responsibility to avoid such conflicts.
    //   And as always, nothing prevents them from giving an invalid identifier
    //   directly as a string/`Qubit::Variable`.
    /// Construct a new `PauliTerm` from a single operator and qubit index.
    ///
    /// To construct a `PauliTerm`, provide a `PauliGate` operator and an argument string.
    /// As a special case, if `op` is the identity operator, the argument may be `None`.
    /// Additionally, the argument parameter can be derived automatically
    /// from a non-placeholder `Qubit` instance or from a non-negative integer;
    /// in the latter case, the argument will be formatted as ``"q{index}"``
    /// to generate a valid Quil argument string.
    /// Optionally, you can provide a `coefficient`,
    /// either directly as an `Expression` or as a numeric literal.
    ///
    /// ```python
    /// from quil.instructions import PauliTerm, PauliGate
    /// term = PauliTerm("X", 0, 1.5) * PauliTerm(PauliGate.Y, "q") * PauliTerm("I", None)
    /// ```
    ///
    /// At present and for compatibility purposes,
    /// you can construct a `PauliTerm` using a sequence of `(operator, qubit)` pairs,
    /// but doing so is deprecated in favor of the `PauliTerm.from_list` static method.
    ///
    /// ```python
    /// # This will raise a deprecation warning in PyQuil v5.
    /// term = PauliTerm([(PauliGate.X, 0), (PauliGate.Y, "q"), ("I", None)], 1.5)
    /// # Prefer this form:
    /// term = PauliTerm.from_list([(PauliGate.X, 0), (PauliGate.Y, "q"), ("I", None)], 1.5)
    /// ```
    ///
    /// Note that to be valid `quil`, the  `coefficient` must be real-valued
    /// and only reference real numeric literals or parameters.
    // Developer note:
    // The stubs for the documented constructors are added manually above.
    // The reason for two constructors here is backwards compatibility:
    // PyQuil v4 used the first form, while `quil` had used the second.
    // The second form is deprecated in favor of `PauliTerm.from_list`,
    // and at some point, we should remove it from `quil` and simplify this constructor.
    #[new]
    #[pyo3(signature = (
            op=None,
            index=None,
            coefficient=None,
            arguments=None,
            expression=None,
    ))]
    fn __new__(
        py: Python<'_>,
        op: Option<Migrate<PauliGate, Vec<(PauliGate, PauliArg)>>>,
        index: Option<Migrate<PauliArg, ExpressionLike>>,
        coefficient: Option<ExpressionLike>,
        arguments: Option<Vec<(PauliGate, PauliArg)>>,
        expression: Option<ExpressionLike>,
    ) -> PyResult<Self> {
        match (op, index, arguments, expression) {
            // Allow `index` to be `None` if `op` is `I`.
            (Some(Migrate::New(PauliGate::I)), None, None, None) => {
                let expression = coefficient.map(Into::into).unwrap_or(ONE);
                Ok(Self::new(Vec::new(), expression))
            }

            // Otherwise, given an `op`, we require an `index`.
            (Some(Migrate::New(op)), Some(Migrate::New(index)), None, None) => {
                let expression = coefficient.map(Into::into).unwrap_or(ONE);
                Ok(Self::new(vec![(op, index.try_into()?)], expression))
            }

            // Second constructor, account for positional vs keyword arguments.
            (
                // all positional
                Some(Migrate::Old(arguments)),
                Some(Migrate::Old(expression)),
                None,
                None,
            )
            | (
                // positional arguments, keyword expression
                Some(Migrate::Old(arguments)),
                None,
                None,
                Some(expression),
            )
            | (
                // all keywords
                None,
                None,
                Some(arguments),
                Some(expression),
            ) => {
                py_deprecated!(
                    py,
                    c"PauliTerm constructor with a sequence of arguments is deprecated; use `PauliTerm.from_list` instead"
                )?;
                Self::from_list(arguments, expression)
            }

            // Second constructor, with default expression of 1.0.
            (Some(Migrate::Old(arguments)), None, None, None)
            | (None, None, Some(arguments), None) => {
                py_deprecated!(
                    py,
                    c"PauliTerm constructor with a sequence of arguments is deprecated; use `PauliTerm.from_list` instead"
                )?;
                Self::from_list(arguments, ExpressionLike::from(ONE))
            }

            (Some(Migrate::New(_)), None, None, None) => Err(PyValueError::new_err(
                "PauliTerm with non-identity operator must have an `index` qubit",
            )),

            _ => Err(PyValueError::new_err(
                "invalid combination of arguments for PauliTerm constructor",
            )),
        }
    }

    fn __getnewargs__(&self) -> (Vec<(PauliGate, String)>, QualifiedExpression) {
        (self.arguments.clone(), self.expression.clone().into())
    }

    /// Construct a new `PauliTerm` from a list of operators and an optional coefficient.
    ///
    /// If the given arguments are disjoint (as required by a well-formed Quil `PauliTerm`),
    /// the resulting `PauliTerm` will preserve that order and use the given coefficient as-is.
    ///
    /// Otherwise, operations on the same argument are combined via Pauli algebra rules,
    /// and the coefficient is multiplied by any resulting complex phase,
    /// and the resulting `PauliTerm` may have a different order of arguments than the input list.
    #[pyo3(signature = (terms_list, coefficient=ExpressionLike::from(ONE)))]
    #[staticmethod]
    fn from_list(
        terms_list: Vec<(PauliGate, PauliArg)>,
        coefficient: ExpressionLike,
    ) -> PyResult<Self> {
        let mut result = PauliTerm::new(Vec::new(), ONE);
        for term in terms_list {
            let term = (term.0, term.1.try_into()?);
            result *= term;
        }
        result.expression = simple!(coefficient.into() * result.expression);
        Ok(result)
    }

    /// Length of the PauliTerm is the number of Pauli operators in the term.
    ///
    /// A term that consists of only a scalar has a length of zero.
    fn __len__(&self) -> usize {
        self.arguments.len()
    }

    /// Create a new copy of this [`PauliTerm`].
    #[pyo3(warn(
        message = "`copy` is deprecated; use `copy.copy(term)` instead.",
        category = PyDeprecationWarning
    ))]
    fn copy(&self) -> Self {
        self.clone()
    }

    /// Create [`Program`] from the [`PauliTerm`].
    #[getter]
    fn program(&self) -> PyResult<Program> {
        let mut program = Program::new();
        for (op, qubit) in &self.arguments {
            let g = Gate::new(
                op.to_string(),
                vec![],
                vec![Qubit::Variable(qubit.clone())],
                vec![],
            )?;
            program.add_instruction(Instruction::Gate(g));
        }

        Ok(program)
    }

    /// Get the arguments of the [`PauliTerm`] as [`Qubit`]s.
    fn get_qubits(&self) -> Vec<Qubit> {
        self.arguments
            .iter()
            .map(|(_, q)| Qubit::Variable(q.clone()))
            .collect()
    }

    /// Get the [`PauliGate`] matching the argument in the [`PauliTerm`],
    /// or [`PauliGate::I`] if the argument is not present in the term.
    ///
    /// As a special case for backwards compatibility, this accepts integer arguments
    /// and `Qubit::Fixed` instances, which are converted into a string of the form ``"q{index}"``.
    fn __getitem__(&self, argument: PauliArg) -> PyResult<PauliGate> {
        let argument = match argument.0 {
            Qubit::Variable(v) => v,
            Qubit::Fixed(v) => format!("q{v}"),
            Qubit::Placeholder(_) => {
                return Err(errors::ValueError::new_err(
                    "QubitPlaceholders are not supported",
                ));
            }
        };

        Ok(self
            .arguments
            .iter()
            .find_map(|(gate, qubit)| {
                if *qubit == argument {
                    Some(*gate)
                } else {
                    None
                }
            })
            .unwrap_or(PauliGate::I))
    }

    /// Iterate over the arguments in this [`PauliTerm`].
    fn __iter__(slf: Bound<'_, Self>) -> PauliArgIter {
        PauliArgIter::new(slf.unbind())
    }

    /// Return the product of this [`PauliTerm`] with another `PauliTerm`,
    /// [`PauliSum`], or number according to the Pauli algebra rules.
    fn __mul__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        if let Ok(other) = other.cast::<PauliTerm>() {
            (self.clone() * other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<PauliSum>() {
            (self.clone() * other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.extract::<ExpressionLike>() {
            (self.clone() * Expression::from(other)).into_bound_py_any(py)
        } else {
            other.py().NotImplemented().into_bound_py_any(py)
        }
    }

    fn __rmul__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        self.__mul__(py, other)
    }

    fn __add__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        if let Ok(other) = other.cast::<PauliTerm>() {
            (self.clone() + other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<PauliSum>() {
            (self.clone() + other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.extract::<ExpressionLike>() {
            (self.clone() + Expression::from(other)).into_bound_py_any(py)
        } else {
            other.py().NotImplemented().into_bound_py_any(py)
        }
    }

    fn __radd__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        self.__add__(py, other)
    }

    fn __sub__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        if let Ok(other) = other.cast::<PauliTerm>() {
            (self.clone() - other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<PauliSum>() {
            (self.clone() - other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.extract::<ExpressionLike>() {
            (self.clone() - Expression::from(other)).into_bound_py_any(py)
        } else {
            other.py().NotImplemented().into_bound_py_any(py)
        }
    }

    fn __rsub__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        if let Ok(other) = other.cast::<PauliTerm>() {
            (other.get().clone() - self.clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<PauliSum>() {
            (other.get().clone() - self.clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.extract::<ExpressionLike>() {
            (Expression::from(other) - self.clone()).into_bound_py_any(py)
        } else {
            other.py().NotImplemented().into_bound_py_any(py)
        }
    }

    /// Compute the power of this [`PauliTerm`].
    ///
    /// This returns a new [`PauliTerm`] or [`PauliSum`]
    /// representing the result of raising this term to the given exponent.
    ///
    /// As with other operations on this type,
    /// the arguments and coefficient may be rearranged and/or simplified,
    /// so logically equivalent results may not be identical one another.
    /// The simplifications and rearrangements are not guaranteed to be stable across versions,
    /// and may change the observable behavior of a program when executed on a QPU.
    ///
    /// In the common case that the exponent is a small integer, this returns a [`PauliTerm`].
    /// To handle more general exponents, this returns a two-term [`PauliSum`].
    ///
    /// # General Mathematical Explanation
    ///
    /// To be specific, for a scaled Pauli operator `T = cP` and complex `k`, `T^k = c^k * P^k`.
    /// Note that for Pauli operators, `P^2 = I` with eigenvalues `+1` and `-1`, so we can write
    /// `P^k = (1/2)(I+P)(1^k) + (1/2)(I-P)((-1)^k) = (1/2)(1+(-1)^k)*I + (1/2)(1-(-1)^k)*P`.
    /// Define `a = (1/2)(1+(-1)^k)` and `b = (1/2)(1-(-1)^k)`, so that `P^k = aI + bP`,
    /// and we can write `T^k = c^k * P^k = c^k * (aI + bP) = (c^k * a)I + (c^k * b)P`.
    /// Thus, the result is a two-term `PauliSum` with coefficients `c^k * a` and `c^k * b`.
    ///
    /// # Special Cases for Integer Exponents
    ///
    /// When `k` is an even integer, `(-1)^k = 1`, so `a = 1`, `b = 0`, and `T^k = c^k * I`.
    /// When `k` is an odd integer,  `(-1)^k = 0`, so `a = 0`, `b = 1`, and `T^k = c^k * P`.
    /// Thus, we can simplify the result to a single `PauliTerm` when given an integer exponent.
    /// If the existing term's coefficient is numeric, we simplify the result to its numeric form.
    ///
    /// Note that this only applies to exponents given as integers that fit in an `i32`;
    /// integers with larger magnitudes are converted to [`Expression::Number`]s,
    /// and [`Expression`]-based exponents are treated as described below.
    ///
    /// # Large Integer, Complex Numbers, and General Expressions
    ///
    /// Large integers, floats, and complex numbers are converted to [`Expression::Number`]s,
    /// and `str`s are converted to [`Expression::Variable`]s.
    /// and general [`Expression`]s are used directly.
    ///
    /// General [`Expression`]s are supported via the above substitutions, processed symbolically.
    /// The exact [`Expression`]s are not formally part of the API and may change between versions.
    /// At present, we evaluate `(-1)^k` with the principal branch `exp(i * pi * k)`,
    /// resulting in `a = (1 + exp(i * pi * k)) / 2` and `b = (1 - exp(i * pi * k)) / 2`;
    /// as with the numeric case, the exact [`Expression`]s produced may be simplified
    /// when the existing term's coefficient is numeric.
    ///
    /// For non-integer exponents, we
    ///
    /// evaluated via the principal branch
    /// which in general results in complex coefficients,
    /// and hence the two-term `PauliSum` result.
    ///
    /// Note that a valid Quil ``DEFGATE ... AS PAULI-SUM`` requires coefficient expressions
    /// to be real-valued and reference only real numeric literals or gate-defined parameters.
    ///
    /// If called with Python's 3-argument `pow` function, the `modulo` argument is not supported,
    /// and will raise a `NotImplementedError` if provided.
    ///
    /// Note: As explained above, this method returns `PauliTerm` for (most) integer exponents,
    /// and a `PauliSum` for (most) non-integer exponents, and accepts `float` and `complex`.
    /// Despite this, due to the way Python and type-checkers special-case them,
    /// it is not possible to express this accurately as a type annotation overload.
    /// Consequently, this is typed with an expectation that integers are the most common numbers,
    /// and also are likely to be in the range of a 32-bit signed integer,
    /// in which case the result is a `PauliTerm`.
    /// Although it cannot be annotated as such, it is valid to pass a `float` or `complex`,
    /// and the result will be a `PauliSum` (as will be true for `Expression`s and large ints).
    /// If you find yourself running into type-checking errors when passing a `float` or `complex`,
    /// you can work around it by wrapping the argument in an `Expression`.
    ///
    /// This arises because Python specifies that `int` is assignable to `float` and `complex`
    /// and that `float` is assignable to `complex`, and from the definition of `assignable`
    /// (see: https://typing.python.org/en/latest/spec/glossary.html#term-assignable),
    /// the consequence is that `int` is treated as a subtype of `float` and `complex`,
    /// leading to all kinds of unsoundness in type-checking vs actual runtime behavior.
    /// For more information, see this discussion:
    /// https://discuss.python.org/t/clarifying-the-float-int-complex-special-case/54018/71
    #[gen_stub(override_return_type(type_repr = "PauliTerm | PauliSum"))]
    fn __pow__<'py>(
        &self,
        py: Python<'py>,
        exponent: PauliExponent,
        modulo: Option<Bound<'py, PyAny>>,
    ) -> PyResult<Bound<'py, PyAny>> {
        if modulo.is_some() {
            return Err(PyNotImplementedError::new_err(
                "`modulo` is not supported for `PauliTerm.__pow__`",
            ));
        }

        fn powi(term: &PauliTerm, exponent: i64) -> PauliTerm {
            // If `k=2x`, so `P^(2x) == (P^2)^x == I^x == I`;
            // else, `k=2x+1`, so `P^(2x+1) == P^(2x) * P == I * P == P`.
            let args = if exponent % 2 == 0 {
                Vec::new()
            } else {
                term.arguments.clone()
            };

            let scalar =
                simple!(term.expression.clone() ^ Expression::Number((exponent as f64).into()));
            PauliTerm::new(args, scalar)
        }

        let expr = match exponent {
            PauliExponent::Int(exponent) => {
                return powi(self, exponent).into_bound_py_any(py);
            }

            // If the complex number is small integer, use the shortcut.
            PauliExponent::Complex(n)
                if n.im == 0.0
                    && n.re.fract() == 0.0
                    && (i64::MIN as f64) <= n.re
                    && n.re <= (i64::MAX as f64) =>
            {
                return powi(self, n.re as i64).into_bound_py_any(py);
            }

            PauliExponent::Complex(n) => Expression::Number(n),
            PauliExponent::Variable(v) => Expression::Variable(v),
            PauliExponent::Expression(expr) => expr.into(),
        };

        // (-1)^k = exp(i * pi * k)
        let exp_i_pi_k = quil_exp((IMAGINARY_UNIT * Expression::PiConstant * expr.clone()).into());

        let a = (ONE + exp_i_pi_k.clone()) / Expression::Number(Complex64::from(2.0));
        let b = (ONE - exp_i_pi_k) / Expression::Number(Complex64::from(2.0));

        let c_pow_k = self.expression.clone() ^ expr;

        let term_ident = PauliTerm {
            arguments: Vec::new(),
            expression: c_pow_k.clone() * a,
        };

        let term_pauli = PauliTerm {
            arguments: self.arguments.clone(),
            expression: c_pow_k * b,
        };

        PauliSum {
            arguments: self.arguments().cloned().collect(),
            terms: vec![term_ident, term_pauli],
        }
        .into_bound_py_any(py)
    }

    // TODO(migration-guide):
    // - At one time, this method returned an empty string if arguments was empty,
    // but later the expected logic changed to return `"I"` in that case.
    // That happened at the same time a `sort_ops=True` argument was added,
    // which came with an explanation that both would change in the future.
    // This makes those changes: the default is now `False`, and both empty cases return `"I"`.
    //
    // - This adds a new keyword-only `delimiter` argument which is used to join the
    // operator-argument pair. By default (and for backwards compatibility), it's the empty string.
    // The `__str__` method below uses `*` to match the original output of the `__repr__` method.
    // The `__repr__` method itself is replaced with the Rust-derived default,
    // matching that of all the other classes in this crate.
    //
    // - When the afforementioned `delimiter` argument is empty (matching the original behavior),
    // if an argument contains 'X', 'Y', 'Z', or 'I', then the output would be produced ambiguous.
    // This works around it by wrapping such arguments in parentheses,
    // so the output no longer matches the original behavior.
    //
    // - The original class primarily focused on integer qubit indicies,
    // but those are not valid Quil identifiers; the constructor for this class
    // generally forces those into valid identifier by prefixing them with `q`,
    // which is to say the original output would have only had output like `X0Y1` etc,
    // but this will produce `Xq0Yq1` instead.
    //
    // - The companion method below to parses these strings is updated with these changes in mind,
    // so it can still handle unambiguous output from the original class,
    // but will also handle the more general from this updated method produces.
    //
    /// Return an identifier string for the PauliTerm (ignoring the coefficient).
    ///
    /// For example, ``PauliTerm.from_list([("X", 0), ("Y", "q")]).id() == "Xq0Yq"``.
    ///
    /// If an argument contains a character that would otherwise be interpreted as a Pauli operator
    /// (i.e., `X`, `Y`, or `Z`), its wrapped in parentheses to avoid ambiguity. For example,
    /// ``PauliTerm.from_list([("X", "X0")]).id() == "X(X0)"``.
    ///
    /// Don't use this to compare terms (use ``pt0 == pt1`` or ``hash(pt0)`` for that).
    /// You can pass ``sort_ops=True`` to sort arguments by qubit.
    ///
    /// Note that if the term has no operators, this function will return ``"I"``.
    /// If you need to check for identity, use ``term.is_identity()`` instead.
    #[pyo3(signature = (sort_ops=false, /, delimiter=""))]
    fn id(&self, sort_ops: bool, delimiter: &str) -> String {
        if self.arguments.is_empty() {
            return "I".to_string();
        }

        fn format_arg(op: &PauliGate, q: &str, delimiter: &str) -> String {
            if delimiter.is_empty() && q.contains(['X', 'Y', 'Z', 'I']) {
                format!("{op}({q})")
            } else {
                format!("{op}{q}")
            }
        }

        if sort_ops {
            self.arguments
                .iter()
                .sorted_by(|(_, a), (_, b)| a.cmp(b))
                .map(|(op, q)| format_arg(op, q, delimiter))
                .join(delimiter)
        } else {
            self.arguments
                .iter()
                .map(|(op, q)| format_arg(op, q, delimiter))
                .join(delimiter)
        }
    }

    /// Return a compact string representation of the PauliTerm.
    fn compact_str(&self) -> PyResult<String> {
        Ok(format!(
            "({})*{}",
            self.expression.to_quil()?,
            self.id(false, "")
        ))
    }

    /// Return a string representation of the PauliTerm.
    fn __str__(&self) -> PyResult<String> {
        Ok(format!(
            "{}*{}",
            self.expression.to_quil()?,
            self.id(false, "*")
        ))
    }

    #[staticmethod]
    fn from_compact_str(str_pauli_term: &str) -> PyResult<Self> {
        let mut arguments = Vec::new();
        // A valid string consists of an expression followed by a sequence terms,
        // which are each a Pauli operator and a (possibly parenthesized) qubit identifier.
        // The operators act as delimiters, as "<expr>*<op><qubit><op><qubit>...".
        let mut parts = str_pauli_term.match_indices(&['X', 'Y', 'Z', 'I']);

        fn err(msg: &str) -> PyErr {
            PyValueError::new_err(format!("Invalid compact string representation: {msg}"))
        }

        // The first part is the expression, which should have the form `(<coefficient>)*`.
        let (mut last_idx, expr_str) = parts.next().ok_or_else(|| err("no operators found"))?;
        let expression = expr_str
            .strip_prefix("(")
            .and_then(|s| s.strip_suffix(")*"))
            .ok_or_else(|| err("expected (<coefficient>)*<terms>"))?
            .trim()
            .parse::<Expression>()?;

        for (idx, qubit) in parts {
            let op_str = &str_pauli_term[last_idx..idx];
            let op = PauliGate::parse(op_str).map_err(|_| {
                PyValueError::new_err(format!("unknown operator at {idx}: {op_str}"))
            })?;
            last_idx = idx + op_str.len() + qubit.len();

            let qubit = qubit
                .trim()
                .strip_prefix("(")
                .and_then(|s| s.strip_suffix(")"))
                .unwrap_or(qubit)
                .trim();
            if qubit.is_empty() {
                return Err(err("missing qubit identifier after final operator"));
            }

            // Special-case for backwards compatbility: accept a numeric qubit identifier,
            // even though it isn't a valid Quil identifier. Prefix it with `q` to make it valid.
            let qubit = if qubit.matches(char::is_alphabetic).next().is_some() {
                qubit.to_string()
            } else {
                format!("q{qubit}")
            };

            arguments.push((op, qubit));
        }

        if !str_pauli_term[last_idx..].trim().is_empty() {
            return Err(err("trailing characters after last Pauli operator"));
        }

        Ok(PauliTerm::new(arguments, expression))
    }

    /// Return a string representation of the PauliTerm without its coefficient
    /// and with implicit qubit indices.
    ///
    /// If an iterable of qubits is provided, each character in the resulting string
    /// represents the Pauli operator acting on the corresponding qubit.
    ///
    /// ```python
    /// >>> p = PauliTerm("X", 0) * PauliTerm("Y", 1, 1.0j)
    /// >>> p.pauli_string()
    /// 'XY'
    /// >>> p.pauli_string(qubits=[0])
    /// 'X'
    /// >>> p.pauli_string(qubits=[0, 2])
    /// 'XI'
    /// ```
    #[pyo3(signature = (qubits=None))]
    fn pauli_string(&self, qubits: Option<Vec<u64>>) -> String {
        match qubits {
            None => self
                .arguments
                .iter()
                .map(|(gate, _)| gate.to_string())
                .collect::<Vec<_>>()
                .join(""),

            Some(qubits) => {
                // TODO: We should really likely have a Python-specific version of PauliTerm
                // using a HashMap<Qubit, PauliGate> rather than the Quil-specific representation.
                let mut qubit_map: HashMap<u64, PauliGate> = HashMap::new();
                for (gate, qubit) in &self.arguments {
                    qubit
                        .strip_prefix('q')
                        .and_then(|q| q.parse::<u64>().ok())
                        .or_else(|| qubit.parse::<u64>().ok())
                        .map(|index| qubit_map.insert(index, *gate));
                }
                qubits
                    .iter()
                    .map(|q| {
                        qubit_map
                            .get(q)
                            .copied()
                            .unwrap_or(PauliGate::I)
                            .to_string()
                    })
                    .collect::<Vec<_>>()
                    .join("")
            }
        }
    }

    /// Return a frozenset of operations in this term.
    ///
    /// Use this in place of `id` if the order of operations in the term does not matter.
    #[gen_stub(override_return_type(type_repr = "builtins.frozenset[builtins.tuple[PauliGate, builtins.str]]", imports = ("builtins")))]
    fn operations_as_set<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyFrozenSet>> {
        PyFrozenSet::new(py, self.arguments.clone())
    }

    /// Return `True` if and only if all operators are identity,
    /// including the case in which the list of operators is empty.
    fn is_identity(&self) -> bool {
        self.arguments.iter().all(|(gate, _)| *gate == PauliGate::I)
    }

    // This is the name that PyQuil v4 used for this property;
    // it's probably a better name anyway, so there's no immediate plan to deprecate it.
    /// Return a copy of the coefficient [`Expression`] of the [`PauliTerm`],
    /// aka its `expression` property.
    #[getter]
    fn coefficient(&self) -> Expression {
        self.expression.clone()
    }

    /// Supported for backwards compatibility, though only for use within PyQuil itself.
    /// This is not part of the public API and may be removed or changed at any time.
    #[pyo3(warn(message = "`_ops` is not part of the public API", category = PyUserWarning))]
    #[getter]
    fn _ops<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyDict>> {
        self.arguments.iter().cloned().into_py_dict(py)
    }
}

#[expect(non_snake_case)]
impl PauliTerm {
    #[expect(unused)]
    fn ID() -> Self {
        PauliTerm::new(Vec::new(), ONE)
    }

    #[expect(unused)]
    fn ZERO() -> Self {
        PauliTerm::new(Vec::new(), ZERO)
    }

    fn IDx(x: Expression) -> Self {
        PauliTerm::new(Vec::new(), x)
    }
}

impl std::ops::Add<Expression> for PauliTerm {
    type Output = PauliSum;

    fn add(self, rhs: Expression) -> Self::Output {
        self + PauliTerm::IDx(rhs)
    }
}

impl std::ops::Add<PauliTerm> for Expression {
    type Output = PauliSum;

    fn add(self, rhs: PauliTerm) -> Self::Output {
        PauliTerm::IDx(self) + rhs
    }
}

impl std::ops::Sub<Expression> for PauliTerm {
    type Output = PauliSum;

    fn sub(self, rhs: Expression) -> Self::Output {
        self - PauliTerm::IDx(rhs)
    }
}

impl std::ops::Sub<PauliTerm> for Expression {
    type Output = PauliSum;

    fn sub(self, rhs: PauliTerm) -> Self::Output {
        PauliTerm::IDx(self) - rhs
    }
}

impl std::ops::Add<PauliTerm> for PauliTerm {
    type Output = PauliSum;

    fn add(self, rhs: PauliTerm) -> Self::Output {
        let terms = vec![self, rhs];
        let arguments = PauliSum::into_args(&terms);
        PauliSum { arguments, terms }
    }
}

impl std::ops::Sub<PauliTerm> for PauliTerm {
    type Output = PauliSum;

    fn sub(self, rhs: PauliTerm) -> Self::Output {
        self + (Expression::Number(-Complex64::ONE) * rhs)
    }
}

impl std::ops::Add<PauliSum> for PauliTerm {
    type Output = PauliSum;

    fn add(self, rhs: PauliSum) -> Self::Output {
        // Technically, this puts `self` at the end of the `PauliTerm` list,
        rhs + self
    }
}

impl std::ops::Sub<PauliSum> for PauliTerm {
    type Output = PauliSum;

    fn sub(self, rhs: PauliSum) -> Self::Output {
        self + (Expression::Number(-Complex64::ONE) * rhs)
    }
}

impl std::ops::Mul<Expression> for PauliTerm {
    type Output = PauliTerm;

    fn mul(self, rhs: Expression) -> Self::Output {
        PauliTerm {
            arguments: self.arguments,
            expression: simple!(self.expression * rhs),
        }
    }
}

impl std::ops::Mul<PauliTerm> for Expression {
    type Output = PauliTerm;

    fn mul(self, rhs: PauliTerm) -> Self::Output {
        PauliTerm {
            arguments: rhs.arguments,
            expression: simple!(self * rhs.expression),
        }
    }
}

impl std::ops::Mul<PauliTerm> for PauliTerm {
    type Output = PauliTerm;

    fn mul(self, rhs: PauliTerm) -> Self::Output {
        let mut result = self;
        for pair in rhs.arguments.into_iter() {
            result *= pair;
        }
        result * rhs.expression
    }
}

impl std::ops::Mul<PauliSum> for PauliTerm {
    type Output = PauliSum;

    fn mul(self, rhs: PauliSum) -> Self::Output {
        PauliSum::mul(rhs, self)
    }
}

impl std::ops::Add<Expression> for PauliSum {
    type Output = PauliSum;

    fn add(self, rhs: Expression) -> Self::Output {
        let PauliSum {
            arguments,
            mut terms,
        } = self;
        terms.push(PauliTerm::IDx(rhs));
        PauliSum { arguments, terms }
    }
}

impl std::ops::Add<PauliTerm> for PauliSum {
    type Output = PauliSum;

    fn add(self, rhs: PauliTerm) -> Self::Output {
        let PauliSum {
            mut arguments,
            mut terms,
        } = self;
        for (_, qubit) in rhs.arguments.iter() {
            if !arguments.contains(qubit) {
                arguments.push(qubit.clone());
            }
        }
        terms.push(rhs);
        PauliSum { arguments, terms }
    }
}

impl std::ops::Add<PauliSum> for PauliSum {
    type Output = PauliSum;

    fn add(self, rhs: PauliSum) -> Self::Output {
        let PauliSum {
            mut arguments,
            mut terms,
        } = self;

        for arg in rhs.arguments {
            if !arguments.contains(&arg) {
                arguments.push(arg);
            }
        }

        terms.extend(rhs.terms);
        PauliSum { arguments, terms }
    }
}

impl std::ops::Sub<Expression> for PauliSum {
    type Output = PauliSum;

    fn sub(self, rhs: Expression) -> Self::Output {
        let PauliSum {
            arguments,
            mut terms,
        } = self;
        terms.push(PauliTerm::IDx(simple!(
            Expression::Number(-Complex64::ONE) * rhs
        )));
        PauliSum { arguments, terms }
    }
}

impl std::ops::Sub<PauliSum> for Expression {
    type Output = PauliSum;

    fn sub(self, rhs: PauliSum) -> Self::Output {
        (Expression::Number(-Complex64::ONE) * rhs) + self
    }
}

impl std::ops::Sub<PauliTerm> for PauliSum {
    type Output = PauliSum;

    fn sub(self, rhs: PauliTerm) -> Self::Output {
        self + (Expression::Number(-Complex64::ONE) * rhs)
    }
}

impl std::ops::Sub<PauliSum> for PauliSum {
    type Output = PauliSum;

    fn sub(self, rhs: PauliSum) -> Self::Output {
        self + (Expression::Number(-Complex64::ONE) * rhs)
    }
}

impl std::ops::Mul<Expression> for PauliSum {
    type Output = PauliSum;

    fn mul(self, rhs: Expression) -> Self::Output {
        if rhs == ONE {
            self
        } else if rhs == ZERO {
            PauliSum {
                arguments: self.arguments,
                terms: Vec::new(),
            }
        } else {
            let PauliSum { arguments, terms } = self;
            let terms = terms.into_iter().map(|term| term * rhs.clone()).collect();
            PauliSum { arguments, terms }
        }
    }
}

impl std::ops::Mul<PauliSum> for Expression {
    type Output = PauliSum;

    fn mul(self, rhs: PauliSum) -> Self::Output {
        PauliSum::mul(rhs, self)
    }
}

impl std::ops::Mul<PauliTerm> for PauliSum {
    type Output = PauliSum;

    fn mul(self, rhs: PauliTerm) -> Self::Output {
        let mut terms = self.terms;
        let mut unique_args = HashSet::new();

        for term in terms.iter_mut() {
            let mut new_term = rhs.clone();
            for pair in term.arguments.iter() {
                new_term *= pair.clone();
            }
            *term = new_term;

            for (_, qubit) in term.arguments.iter() {
                unique_args.insert(qubit);
            }
        }

        let arguments = unique_args.into_iter().cloned().collect();
        PauliSum { arguments, terms }
    }
}

impl std::ops::Mul<PauliSum> for PauliSum {
    type Output = PauliSum;

    fn mul(self, rhs: PauliSum) -> Self::Output {
        let mut terms = Vec::new();
        let mut unique_args = HashSet::new();

        for term1 in self.terms.iter() {
            for term2 in rhs.terms.iter() {
                let new_term = term1.clone() * term2.clone();
                for (_, qubit) in new_term.arguments.iter() {
                    if !unique_args.contains(qubit) {
                        unique_args.insert(qubit.clone());
                    }
                }
                terms.push(new_term);
            }
        }

        let arguments = unique_args.into_iter().collect();
        PauliSum { arguments, terms }
    }
}

impl std::ops::MulAssign<(PauliGate, String)> for PauliTerm {
    /// Return the product of this [`PauliTerm`] with a single `(PauliGate, String)` pair;
    /// no guarantees are made about the order of the resulting arguments.
    fn mul_assign(&mut self, rhs: (PauliGate, String)) {
        let (op, qubit) = rhs;

        // Check if this qubit is already present in this term.
        // Assume we're not likely to have many arguments; if benchmarking proves this to be slow,
        // we can introduce a Python-specific version of `PauliTerm` that uses a `HashMap`,
        // or we can just enforce that `arguments` is always sorted, then use binary search.
        match self.arguments.iter().position(|(_, q)| q == &qubit) {
            Some(idx) => {
                let (new_op, phase) = self.arguments[idx].0.product(op);
                if new_op == PauliGate::I {
                    self.arguments.swap_remove(idx);
                } else {
                    self.arguments[idx] = (new_op, qubit);
                    // Take the expression out of self so we expand simple! without cloning.
                    let expr = mem::replace(&mut self.expression, Expression::PiConstant);
                    self.expression = simple!(expr * Expression::Number(phase))
                }
            }
            None => {
                self.arguments.push((op, qubit));
            }
        };
    }
}

/// An iterator over the qubit indices and Pauli operators in a [`PauliTerm`].
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(module = "quil._quil.instructions", frozen)]
pub(crate) struct PauliArgIter {
    // Using `Py<_>` avoids cloning the entire `PauliTerm`,
    // and since that class is frozen, we can skip all the Python reference counting
    // by using an atomic index.
    // Note that individual `(PauliGate, String)` pairs still must be cloned when iterated.
    inner: Py<PauliTerm>,
    index: AtomicUsize,
}

impl PauliArgIter {
    fn new(inner: Py<PauliTerm>) -> Self {
        Self {
            inner,
            index: AtomicUsize::new(0),
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PauliArgIter {
    fn __iter__(slf: PyRef<'_, Self>) -> PyRef<'_, Self> {
        slf
    }

    #[gen_stub(override_return_type(type_repr = "builtins.tuple[PauliGate, builtins.str]", imports = ("builtins")))]
    fn __next__(slf: PyRef<'_, Self>) -> Option<(PauliGate, String)> {
        slf.inner
            .get()
            .arguments
            .get(slf.index.fetch_add(1, Ordering::Relaxed))
            .cloned()
    }

    #[gen_stub(skip)]
    fn __traverse__(&self, visit: pyo3::PyVisit) -> Result<(), pyo3::PyTraverseError> {
        visit.call(&self.inner)
    }
}

/// An iterator over the [`PauliTerm`]s of a [`PauliSum`].
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(module = "quil._quil.instructions", frozen)]
pub(crate) struct PauliTermIter {
    inner: Py<PauliSum>,
    index: AtomicUsize,
}

impl PauliTermIter {
    fn new(inner: Py<PauliSum>) -> Self {
        Self {
            inner,
            index: AtomicUsize::new(0),
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PauliTermIter {
    fn __iter__(slf: PyRef<'_, Self>) -> PyRef<'_, Self> {
        slf
    }

    #[gen_stub(override_return_type(type_repr = "PauliTerm"))]
    fn __next__(slf: PyRef<'_, Self>) -> Option<PauliTerm> {
        slf.inner
            .get()
            .terms
            .get(slf.index.fetch_add(1, Ordering::Relaxed))
            .cloned()
    }

    #[gen_stub(skip)]
    fn __traverse__(&self, visit: pyo3::PyVisit) -> Result<(), pyo3::PyTraverseError> {
        visit.call(&self.inner)
    }
}

// PauliSum constructor stub overloads:
// - the first is the new (PyQuil) preferred order `(terms, arguments=None)`
// - the second is the old (`quil`) deprecated order `(arguments, terms)`
// The former is preferred because `arguments` can (and should?) be inferred from `terms`.
#[cfg(feature = "stubs")]
pyo3_stub_gen::inventory::submit! {
    gen_methods_from_python! {
        r#"
        import collections.abc
        import typing
        import typing_extensions
        from quil import _quil

        class PauliSum:
            @typing.overload
            def __new__(
                cls,
                terms: collections.abc.Sequence[PauliTerm],
                arguments: collections.abc.Sequence[PauliTargetDesignator] | None = None,
            ) -> PauliSum:
                """Construct a new `PauliSum` from a list of `PauliTerm`s
                and an optional list of arguments.
                """

            @typing.overload
            @typing_extensions.deprecated("This parameter order is deprecated; use `(terms, arguments)` instead.")
            def __new__(
                cls,
                arguments: collections.abc.Sequence[PauliTargetDesignator],
                terms: collections.abc.Sequence[PauliTerm],
            ) -> PauliSum:
                """Construct a new `PauliSum` from arguments and `PauliTerm`s.

                This constructor is deprecated; use `(terms, arguments)` instead.
                """
        "#
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PauliSum {
    // TODO(migration-guide):
    //
    // - Rust users of `quil-rs` with the `python` feature
    //   should be aware that `PauliSum::new` is no longer shared directly with Python.
    // - Python users of `quil` will receive deprecation warnings
    //   for existing usage of the `PauliSum.__new__` constructor
    //   and should update usage from `PauliSum(arguments, terms)` to `PauliSum(terms)`.
    /// Construct a new `PauliSum` from a list of `PauliTerm`s and an optional list of arguments.
    ///
    /// For backwards compatibility, this constructor supports `(arguments, terms)`,
    /// but if `arguments` are given, the preferred order is `(terms, arguments)`,
    /// and the other order will issue a deprecation warning.
    /// If not given, `arguments` are inferred from the `PauliTerm`s.
    // Developer Note: The stubs for the documented constructors are added manually above.
    // The two signatures are united in a backwards-compatible way
    // by inserting a positional-only parameter that can work as `terms` or `arguments`;
    // the second argument is still `terms` to support both positional and keyword usage,
    // but likewise accepts either `terms` or `arguments`.
    // Finally, the `arguments` parameter has been moved to the end
    // to maintain compatibility with keyword-only usage.
    //
    // At some point, we'll act on the deprecation notice
    // and stop accepting the `(arguments, terms)` order,
    // and this whole constructor can be greatly simplified.
    #[new]
    #[pyo3(signature = (terms_or_args=None, /, terms=None, arguments=None))]
    fn __new__(
        py: Python<'_>,
        terms_or_args: Option<Migrate<Vec<PauliTerm>, Vec<PauliArg>>>,
        terms: Option<Migrate<Vec<PauliTerm>, Vec<PauliArg>>>,
        arguments: Option<Vec<PauliArg>>,
    ) -> PyResult<PauliSum> {
        match (terms_or_args, terms, arguments) {
            // Single-parameter `terms` as positional or keyword parameters.
            (Some(Migrate::New(terms)), None, None) | (None, Some(Migrate::New(terms)), None) => {
                let arguments = PauliSum::into_args(&terms);
                Ok(PauliSum { arguments, terms })
            }

            // New-style two-parameter new-style constructor `(terms, arguments)`,
            // as positional, mixed, and keyword-only versions.
            (Some(Migrate::New(terms)), Some(Migrate::Old(arguments)), None)
            | (Some(Migrate::New(terms)), None, Some(arguments))
            | (None, Some(Migrate::New(terms)), Some(arguments)) => {
                let arguments = convert_pauli_targets(arguments)?;
                // Let the existing constructor check for valid parameters.
                Ok(PauliSum::new(arguments, terms)?)
            }

            // Old-style two-parameter constructor `(arguments, terms)`;
            // the first covers both positional and mixed, the second keyword-only versions.
            (Some(Migrate::Old(arguments)), Some(Migrate::New(terms)), None) => {
                py_deprecated!(
                    py,
                    c"`PauliSum(arguments, terms)` is deprecated; use `PauliSum(terms, arguments)` instead."
                )?;

                let arguments = convert_pauli_targets(arguments)?;
                // Let the existing constructor check for valid parameters.
                Ok(PauliSum::new(arguments, terms)?)
            }

            // Only given `arguments`, by position or keyword,
            (Some(Migrate::Old(_)), None, None) | (None, None, _) => {
                Err(PyValueError::new_err("missing argument `terms`"))
            }

            // Given `terms=<list of strings>` or two lists of strings positionally.
            (Some(Migrate::Old(_)), None, Some(_)) | (_, Some(Migrate::Old(_)), _) => Err(
                PyTypeError::new_err("`terms` must be a list of `PauliTerm`s"),
            ),

            // Given two lists of `PauliTerm`s, but one should be `arguments`.
            (Some(Migrate::New(_)), Some(Migrate::New(_)), None) => {
                Err(PyTypeError::new_err("`arguments` must be a list of `str`s"))
            }

            (Some(_), Some(_), Some(_)) => Err(PyValueError::new_err(
                "too many arguments; use `PauliSum(terms, arguments)`",
            )),
        }
    }

    // TODO: Ideally, we'd store `Qubit`s instead. We could use a Python-specific class,
    // which has some advantages, or update the Rust version (as a breaking change).
    fn __getnewargs__(&self) -> (Vec<PauliTerm>, Vec<String>) {
        (self.terms.clone(), self.arguments.clone())
    }

    // TODO(migration-guide): The `__repr__` method is replaced with the Rust-derived default,
    // which is more consistent with the other classes in this crate.
    // This __str__ method replaces the original `__repr__`.
    /// Return a string representation of the PauliSum.
    fn __str__(&self) -> PyResult<String> {
        let terms_str = self
            .terms
            .iter()
            .map(|term| term.compact_str())
            .collect::<Result<Vec<_>, _>>()?
            .join(" + ");
        Ok(format!("PauliSum({})", terms_str))
    }

    /// Return the number of terms in this [`PauliSum`].
    fn __len__(&self) -> usize {
        self.terms.len()
    }

    fn __getitem__<'py>(&self, py: Python<'py>, index: isize) -> PyResult<Bound<'py, PyAny>> {
        let len = self.terms.len() as isize;
        let index = if index < 0 { len + index } else { index };
        if index < 0 || index >= len {
            Err(PyIndexError::new_err("index out of range"))
        } else {
            self.terms[index as usize].clone().into_bound_py_any(py)
        }
    }

    // TODO: is this just going to clone the PauliSum anyway?
    /// Iterate over the [`PauliTerm`]s in this [`PauliSum`].
    fn __iter__(slf: Bound<'_, Self>) -> PauliTermIter {
        PauliTermIter::new(slf.unbind())
    }

    fn __add__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        if let Ok(other) = other.cast::<PauliTerm>() {
            (self.clone() + other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<PauliSum>() {
            (self.clone() + other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.extract::<ExpressionLike>() {
            (self.clone() + Expression::from(other)).into_bound_py_any(py)
        } else {
            other.py().NotImplemented().into_bound_py_any(py)
        }
    }

    fn __radd__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        self.__add__(py, other)
    }

    fn __mul__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        if let Ok(other) = other.cast::<PauliTerm>() {
            (self.clone() * other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<PauliSum>() {
            (self.clone() * other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.extract::<ExpressionLike>() {
            (self.clone() * Expression::from(other)).into_bound_py_any(py)
        } else {
            other.py().NotImplemented().into_bound_py_any(py)
        }
    }

    fn __rmul__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        self.__mul__(py, other)
    }

    fn __pow__(&self, exponent: u64, modulo: Option<Bound<'_, PyAny>>) -> PyResult<Self> {
        if modulo.is_some() {
            return Err(PyNotImplementedError::new_err(
                "`modulo` is not supported for `PauliSum.__pow__`",
            ));
        }

        if exponent == 0 {
            return Ok(PauliSum {
                arguments: Vec::new(),
                terms: Vec::new(),
            });
        }

        let mut result = self.clone();
        for _ in 1..exponent {
            result = result * self.clone();
        }
        Ok(result)
    }

    fn __sub__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        if let Ok(other) = other.cast::<PauliTerm>() {
            (self.clone() - other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<PauliSum>() {
            (self.clone() - other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.extract::<ExpressionLike>() {
            (self.clone() - Expression::from(other)).into_bound_py_any(py)
        } else {
            other.py().NotImplemented().into_bound_py_any(py)
        }
    }

    fn __rsub__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        if let Ok(other) = other.cast::<PauliTerm>() {
            (other.get().clone() - self.clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<PauliSum>() {
            (other.get().clone() - self.clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.extract::<ExpressionLike>() {
            (Expression::from(other) - self.clone()).into_bound_py_any(py)
        } else {
            other.py().NotImplemented().into_bound_py_any(py)
        }
    }

    // TODO(migration-guide): This used to return `Qubit`s.
    /// Get a list of all the qubits in the sum of the terms.
    fn get_qubits(&self) -> Vec<String> {
        self.arguments.clone()
    }
}
