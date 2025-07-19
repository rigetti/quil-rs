//! This module defines exceptions used (or catchable) on the Python-side,
//! along with conversions from their Rust error counterparts within this crate.
use pyo3::exceptions::PyException;

/// Create a Python exception and a conversion from its Rust type.
/// Note that the exception class must still be added to the module.
macro_rules! exception {
    ( $rust_err: ty, $module:expr, $py_err: ident, $base: ty $(, $doc: expr)? ) => {
        pyo3::create_exception!( $module, $py_err, $base $(, $doc)? );

        #[doc = concat!(
            "Convert a Rust ",
            "`", stringify!($rust_err), "`",
            " into a Python ",
            "`", stringify!($py_err), "`."
        )]
        impl std::convert::From<$rust_err> for pyo3::PyErr {
            fn from(err: $rust_err) -> Self {
                $py_err::new_err(err.to_string())
            }
        }
    };
}

// TODO: Create a unified error hierarchy: https://github.com/rigetti/quil-rs/issues/461
// Base exception type for this package.
pyo3::create_exception!(
    quil.program,
    QuilError,
    PyException,
    "Base exception type for errors raised by this package."
);

// expression errors
exception!(
    crate::expression::EvaluationError,
    quil.expression,
    EvaluationError,
    QuilError,
    "Errors that may occur while evaluation an ``Expression``."
);

exception!(
    crate::program::ParseProgramError<crate::expression::Expression>,
    quil.expression,
    ParseExpressionError,
    QuilError,
    "Errors that may occur while parsing an ``Expression``."
);

// instruction errors
pyo3::create_exception!(
    quil.instructions,
    InstructionError,
    QuilError,
    "Base error type for errors related to ``Instruction`` processing."
);

exception!(
    crate::program::SyntaxError<crate::instruction::MemoryReference>,
    quil.instructions,
    ParseMemoryReferenceError,
    QuilError,
    "Errors that may occur while parsing a ``MemoryReference``."
);

exception!(
    crate::instruction::CallError,
    quil.instruction,
    CallError,
    QuilError,
    "Errors that may occur when initializing a ``Call``."
);

exception!(
    crate::instruction::ExternError,
    quil.instruction,
    ExternError,
    QuilError,
    "Errors that may occur when initializing or validating a ``PRAGMA EXTERN`` instruction."
);

exception!(
    crate::instruction::GateError,
    quil.instruction,
    GateError,
    QuilError,
    "Errors that may occur when performing operations on a ``Gate``."
);

exception!(
    crate::instruction::ParseInstructionError,
    quil.instructions,
    ParseInstructionError,
    InstructionError,
    "Errors that may occur while parsing an ``Instruction``."
);

// validation.identifier errors
exception!(
    crate::validation::identifier::IdentifierValidationError,
    quil.validation.identifier,
    IdentifierValidationError,
    QuilError,
    "Errors that may occur when validating a Quil identifier."
);

// program errors
exception!(
    crate::program::ProgramError,
    quil.program,
    ProgramError,
    QuilError,
    "Errors encountered related to a Program."
);

exception!(
    crate::program::scheduling::ComputedScheduleError,
    quil.program,
    ComputedScheduleError,
    ProgramError,
    "Error raised if the computed schedule is invalid."
);

exception!(
    crate::program::analysis::BasicBlockScheduleError,
    quil.program,
    BasicBlockScheduleError,
    ProgramError
);

exception!(
    crate::program::analysis::QubitGraphError,
    quil.program,
    QubitGraphError,
    ProgramError
);

// quil errors
exception!(
    crate::quil::ToQuilError,
    quil.program,
    ToQuilError,
    QuilError,
    "Errors which can occur when converting a Quil item to a string."
);
