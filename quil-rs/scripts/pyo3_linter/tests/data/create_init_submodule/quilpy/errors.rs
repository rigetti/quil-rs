use pyo3::create_exception;
use rigetti_pyo3::exception;

create_exception!(quil, QuilError, pyo3::exceptions::PyException);

exception!(
    crate::SomeRustError,
    quil,
    ValueError,
    QuilError,
    "A value error."
);
