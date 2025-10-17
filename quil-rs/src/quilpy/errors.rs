//! This module defines exceptions used (or catchable) on the Python-side,
//! along with conversions from their Rust error counterparts within this crate.
use pyo3::exceptions::PyException;

/// Create a new Python exception using the correct macro
/// based on whether the "stubs" features is active.
macro_rules! create_exception {
    ( $module:expr, $py_err: ident, $base: ty ) => {
        create_exception!($module, $py_err, $base, "");
    };
    ( $module:expr, $py_err: ident, $base: ty, $doc: expr ) => {
        ::pyo3::create_exception!($module, $py_err, $base, $doc);

        #[cfg(feature = "stubs")]
        impl ::pyo3_stub_gen::PyStubType for $py_err {
            fn type_output() -> ::pyo3_stub_gen::TypeInfo {
                ::pyo3_stub_gen::TypeInfo::locally_defined(
                    stringify!($py_err),
                    stringify!($module).into(),
                )
            }
        }

        #[cfg(feature = "stubs")]
        pyo3_stub_gen::inventory::submit! {
            pyo3_stub_gen::type_info::PyClassInfo {
                pyclass_name: stringify!($py_err),
                struct_id: std::any::TypeId::of::<$py_err>,
                getters: &[],
                setters: &[],
                module: Some(stringify!($module)),
                doc: $doc,
                bases: &[|| <$base as pyo3_stub_gen::PyStubType>::type_output()],
                has_eq: false,
                has_ord: false,
                has_hash: false,
                has_str: false,
            }
        }
    };
}

/// Create a Python exception and a conversion from its Rust type.
/// Note that the exception class must still be added to the module.
macro_rules! exception {
    ( $rust_err: ty, $module:expr, $py_err: ident, $base: ty $(, $doc: expr)? ) => {
        create_exception!( $module, $py_err, $base $(, $doc)? );

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
    crate::instruction::DefGateSequenceError,
    quil.instructions,
    DefGateSequenceError,
    InstructionError,
    "Errors that can occur when initializing a sequence gate definition."
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
