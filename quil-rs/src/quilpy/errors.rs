//! This module defines exceptions used (or catchable) on the Python-side.
use pyo3::exceptions::PyException;
use rigetti_pyo3::{create_exception, exception};

create_exception!(
    quil._quil,
    QuilError,
    PyException,
    "Base exception type for errors raised by this package."
);

create_exception!(
    quil._quil,
    ValueError,
    QuilError,
    "Raised when an argument to a quil function has an inappropriate value."
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
