use rigetti_pyo3::{py_wrap_error, pyo3::exceptions::PyValueError, wrap_error};

wrap_error!(IdentifierValidationError(
    quil_rs::validation::identifier::IdentifierValidationError
));

py_wrap_error!(
    quil,
    IdentifierValidationError,
    PyIdentifierValidationError,
    PyValueError
);
