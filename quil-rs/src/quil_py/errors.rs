//! This module defines exceptions used (or catchable) on the Python-side,
//! along with conversions from their Rust error counterparts within this crate.
use pyo3::exceptions::PyValueError;

/// Create a Python exception and a conversion from its Rust type.
/// Note that the exception class must still be added to the module.
macro_rules! exception {
    ( $rust_err: ty, $module:expr, $py_err: ident, $base: ty, $doc: expr ) => {
        pyo3::create_exception!( $module, $py_err, $base, $doc );
        
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

use crate::{instruction::MemoryReference, program::SyntaxError};

exception!(
    SyntaxError<MemoryReference>,
    quil.instructions,
    ParseMemoryReferenceError,
    PyValueError,
    "Errors that may occur while parsing a ``MemoryReference``."
);

exception!(
    crate::expression::EvaluationError,
    quil.expression,
    EvaluationError,
    PyValueError,
    "Error that may occur while evaluation an ``Expression``."
);

exception!(
    crate::program::ParseProgramError<crate::expression::Expression>,
    quil.expression,
    ParseExpressionError,
    PyValueError,
    "Error that may occur while parsing an ``Expression``."
);

