//! This module defines exceptions used (or catchable) on the Python-side.
use pyo3::exceptions::PyException;
use rigetti_pyo3::{create_exception, exception};

create_exception!(
    quil,
    QuilError,
    PyException,
    "Base exception type for errors raised by this package."
);

create_exception!(
    quil,
    ValueError,
    QuilError,
    "Raised when an argument to a quil function has an inappropriate value."
);

create_exception!(
    quil,
    PickleError,
    QuilError,
    "Errors when trying to pickle or deepcopy."
);

exception!(
    crate::quil::ToQuilError,
    quil,
    ToQuilStringError,
    QuilError,
    "Errors which can occur when converting a Quil item to a string."
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
create_exception!(
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
    quil.instructions,
    CallError,
    QuilError,
    "Errors that may occur when initializing a ``Call``."
);

exception!(
    crate::instruction::ExternError,
    quil.instructions,
    ExternError,
    QuilError,
    "Errors that may occur when initializing or validating a ``PRAGMA EXTERN`` instruction."
);

exception!(
    crate::instruction::GateError,
    quil.instructions,
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
