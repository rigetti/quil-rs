//! This module defines exceptions used (or catchable) on the Python-side.
//!
//! We define an exception hierarchy, with `QuilError` as the base,
//! and typically if we return an error, we should return a type that derives from `QuilError`
//! (see the note below about the... exception to this rule).
//! Doing so is particularly useful in unit tests, where tests that are expected to raise
//! can use `pytest.raises(QuilError)` to be certain that the error caught came from our code.
//! Generally, such tests should be as specific as needed to avoid a spurious match,
//! but general enough to being brittle to changes in the specific error returned.
//!
//! In any case, when returning an error from Python-exposed code,
//! you can directly return a Rust error type that converts into one of these exceptions,
//! or you can construct a [`PyErr`][pyo3::PyErr] directly using `{Exception}::new_err("message")`.
//! In the former case, the return type will be `Result<T, {RustErrorType}>`,
//! while the latter must be `Result<T, PyErr>` or more simply [`PyResult<T>`][pyo3::PyResult].
//! You can also use the `?` operator to convert to a `PyErr` from these error types.
//!
//! Here are some concrete examples:
//!
//! ```rust
//! use crate::quilpy::errors::ToQuilStringError;
//!
//! fn foo() -> pyo3::PyResult<()> {
//!     Err(ToQuilStringError::new_err("Label has not yet been resolved"))
//! }
//! fn foo2() -> Result<(), pyo3::PyErr> {
//!     Err(ToQuilStringError::new_err("Label has not yet been resolved"))
//! }
//! fn foo3() -> Result<(), crate::quil::ToQuilError> {
//!     Err(crate::quil::ToQuilError::UnresolvedLabelPlaceholder)
//! }
//! ```
//!
//! If you find yourself using `new_err` with the same message more than once,
//! it may be worth defining a new error type here and using it instead.
//! This will ensure a consistent message that we can easily change in one place,
//! and gives the option to avoid constructing the `PyErr` if the error can be handled in Rust.
//!
//! # Defining New Exceptions
//!
//! There are two macros to define exceptions here:
//!
//! - `create_exception!` defines a new exception type that is not backed by a Rust error type.
//!   - This is the right choice for "base" exception types used to group other exceptions.
//!   - Use this for error types that will always be returned to Python and with a custom message.
//! - `exception!` defines a new exception type backed by a Rust error type.
//!   - Obviously, you'll need a Rust error type as well.
//!     If that type already exists, great, otherwise (if it's Python-API-only),
//!     you can define that type in this module.
//!   - You'll need to specify the Python exception it derives from,
//!     which should be `QuilError` or another type that derives from it.
//!
//! ## Naming
//!
//! Note that "custom" error types that are exposed to the Python API need two names:
//! one is the Rust-specific type name to which you'll reference in Rust code,
//! and the other is the Python name that will be used in the Python API.
//!
//! Except for the base `QuilError`, avoid starting the Python name with `Quil`
//! so that using the type doesn't stutter: `quil.ValueError` is better than `quil.QuilValueError`.
//!
//! When the Rust error type is used outside of the Python API,
//! it must be defined in a module outside of this one,
//! so it is possible to use the same name for both by using the full path in the macro.
//!
//! For error type defined directly in this module, the Rust type name must be unique,
//! and in any case, we can adopt a convention here to make it clear a type is Python-specific.
//! Consequently, we use the same convention as `PyO3` and prefix the Rust type name with `Py`.
//! This sometimes leads a "conflict" the in names between those defined within `PyO3`
//! and the names we define here, but that is intentional, as in those cases,
//! we want to use the error type derived from `QuilError` rather than the standard Python type.
//!
//! ## When NOT to Define a New Exception
//!
//! As mentioned above, we generally want to return errors that derive from `QuilError`,
//! as it makes it easier to validate we're our code is the source of an error in unit tests.
//! However, there are cases where users reasonably expect a standard Python exception type,
//! such as `KeyError` or `TypeError`. In an ideal world, we'd define a type that derives
//! from both our `QuilError` and the standard Python exception type,
//! but Python explicitly discourages inheriting from multiple exception types.
//!
//! So, if you expect the standard Python exception type to be more useful to users,
//! then you should return that type instead of a `QuilError`-derived type.
//! A good candidate is returning the standard `TypeError` if you must manually process a `PyAny`
//! (although consider [`pyo3::CastError`], which will be more informative).
//!
//! On the other hand, avoid using the standard Python `ValueError` type
//! (used to indicate the argument has the right type, but an inappropriate value)
//! in favor of defining a new `QuilValueError` type that derives from `QuilError`:
//! these are almost certainly worth unit testing, and likely aren't valuable for users to catch,
//! or even if they are, they probably _shouldn't_ be doing so by catching _all_ `ValueError`s,
//! and even if they really do want to do that, Python has mechanisms to make that work
//! (but it's slightly more verbose -- a good hint that they probably shouldn't be doing it).
//!
use pyo3::exceptions::PyException;
use rigetti_pyo3::{create_exception, exception};

/// Errors that can occur when calling a `quil` function.
///
/// These errors only apply to Python users and are not reachable directly from the Rust APIs.
#[derive(Debug, thiserror::Error, PartialEq)]
#[non_exhaustive]
pub(crate) enum PyValueError {
    #[error("`{0}` and `{1}` are mutually exclusive")]
    MutuallyExclusive(String, String),
    #[error("missing required parameter `{0}`")]
    MissingRequired(String),
}

/// Errors that occur when an array argument has an inappropriate value.
///
/// These errors only apply to Python users and are not reachable directly from the Rust APIs.
#[derive(Debug, thiserror::Error, PartialEq)]
#[non_exhaustive]
pub(crate) enum PyArrayError {
    #[error("not a square matrix: {0}")]
    NonSquare(ndarray::ShapeError),
    #[error("matrix dimensions must be a power of a prime qudit dimension (e.g., 2, 3, 4, 5, 7, 8, 9, 11, ...), but got {0}")]
    DimensionError(u64),
}

// All exceptions defined here should use this as their base,
// or something that derives from it, so that all exceptions are grouped under it.
create_exception!(
    quil._quil,
    QuilError,
    PyException,
    "Base exception type for errors raised by this package."
);

exception!(
    PyValueError,
    quil._quil,
    ValueError,
    QuilError,
    "Raised when an argument to a quil function has an inappropriate value."
);

exception!(
    PyArrayError,
    quil._quil,
    ArrayError,
    ValueError,
    "Raised when an array argument has an inappropriate value."
);

create_exception!(
    quil._quil,
    PickleError,
    QuilError,
    "Errors when trying to pickle or deepcopy."
);

exception!(
    crate::quil::ToQuilError,
    quil._quil,
    ToQuilStringError,
    QuilError,
    "Errors which can occur when converting a Quil item to a string."
);

// expression errors
exception!(
    crate::expression::EvaluationError,
    quil._quil.expression,
    EvaluationError,
    QuilError,
    "Errors that may occur while evaluation an ``Expression``."
);

exception!(
    crate::program::ParseProgramError<crate::expression::Expression>,
    quil._quil.expression,
    ParseExpressionError,
    QuilError,
    "Errors that may occur while parsing an ``Expression``."
);

// instruction errors
create_exception!(
    quil._quil.instructions,
    InstructionError,
    QuilError,
    "Base error type for errors related to ``Instruction`` processing."
);

exception!(
    crate::program::SyntaxError<crate::instruction::MemoryReference>,
    quil._quil.instructions,
    ParseMemoryReferenceError,
    QuilError,
    "Errors that may occur while parsing a ``MemoryReference``."
);

exception!(
    crate::instruction::CallError,
    quil._quil.instructions,
    CallError,
    QuilError,
    "Errors that may occur when initializing a ``Call``."
);

exception!(
    crate::instruction::ExternError,
    quil._quil.instructions,
    ExternError,
    QuilError,
    "Errors that may occur when initializing or validating a ``PRAGMA EXTERN`` instruction."
);

exception!(
    crate::instruction::GateError,
    quil._quil.instructions,
    GateError,
    QuilError,
    "Errors that may occur when performing operations on a ``Gate``."
);

exception!(
    crate::instruction::DefGateSequenceError,
    quil._quil.instructions,
    DefGateSequenceError,
    InstructionError,
    "Errors that can occur when initializing a sequence gate definition."
);

exception!(
    crate::instruction::ParseInstructionError,
    quil._quil.instructions,
    ParseInstructionError,
    InstructionError,
    "Errors that may occur while parsing an ``Instruction``."
);

// validation.identifier errors
exception!(
    crate::validation::identifier::IdentifierValidationError,
    quil._quil.validation.identifier,
    IdentifierValidationError,
    QuilError,
    "Errors that may occur when validating a Quil identifier."
);

// waveform errors
create_exception!(
    quil._quil.waveform,
    WaveformError,
    QuilError,
    "Base error type for errors related to waveform processing."
);

exception!(
    crate::waveform::WaveformParameterError,
    quil._quil.waveform,
    WaveformParameterError,
    WaveformError,
    "Errors that may occur when parsing waveform parameters."
);

exception!(
    crate::waveform::WaveformInvocationError,
    quil._quil.waveform,
    WaveformInvocationError,
    WaveformError,
    "Errors that may occur when parsing a waveform."
);

exception!(
    crate::waveform::sampling::SamplingError,
    quil._quil.waveform.sampling,
    SamplingError,
    WaveformError,
    "Errors that may occur when sampling waveforms."
);

// program errors
exception!(
    crate::program::ProgramError,
    quil._quil.program,
    ProgramError,
    QuilError,
    "Errors encountered related to a Program."
);

exception!(
    crate::program::scheduling::ComputedScheduleError,
    quil._quil.program,
    ComputedScheduleError,
    ProgramError,
    "Error raised if the computed schedule is invalid."
);

exception!(
    crate::program::analysis::BasicBlockScheduleError,
    quil._quil.program,
    BasicBlockScheduleError,
    ProgramError
);

exception!(
    crate::program::analysis::QubitGraphError,
    quil._quil.program,
    QubitGraphError,
    ProgramError
);
