use quil_rs::validation::identifier::{validate_identifier, validate_user_identifier};
use rigetti_pyo3::{
    create_init_submodule, py_wrap_error,
    pyo3::{exceptions::PyValueError, pyfunction, PyResult},
    wrap_error, ToPythonError,
};

wrap_error!(IdentifierValidationError(
    quil_rs::validation::identifier::IdentifierValidationError
));

py_wrap_error!(
    quil,
    IdentifierValidationError,
    PyIdentifierValidationError,
    PyValueError
);

#[pyfunction]
#[pyo3(name = "validate_identifier")]
pub fn py_validate_identifier(ident: &str) -> PyResult<()> {
    validate_identifier(ident)
        .map_err(IdentifierValidationError::from)
        .map_err(IdentifierValidationError::to_py_err)
}

#[pyfunction]
#[pyo3(name = "validate_identifier")]
pub fn py_validate_user_identifier(ident: &str) -> PyResult<()> {
    validate_user_identifier(ident)
        .map_err(IdentifierValidationError::from)
        .map_err(IdentifierValidationError::to_py_err)
}

create_init_submodule! {
    errors: [PyIdentifierValidationError],
    funcs: [py_validate_identifier, py_validate_user_identifier],
}
