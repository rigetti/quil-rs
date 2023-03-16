use quil_rs::validation::identifier::{validate_identifier, validate_user_identifier};
use rigetti_pyo3::{
    create_init_submodule, py_wrap_error,
    pyo3::{exceptions::PyValueError, pyfunction, PyResult},
    wrap_error, ToPythonError,
};

wrap_error!(RustIdentifierValidationError(
    quil_rs::validation::identifier::IdentifierValidationError
));

py_wrap_error!(
    quil,
    RustIdentifierValidationError,
    IdentifierValidationError,
    PyValueError
);

#[pyfunction]
#[pyo3(name = "validate_identifier")]
pub fn py_validate_identifier(ident: &str) -> PyResult<()> {
    validate_identifier(ident)
        .map_err(RustIdentifierValidationError::from)
        .map_err(RustIdentifierValidationError::to_py_err)
}

#[pyfunction]
#[pyo3(name = "validate_user_identifier")]
pub fn py_validate_user_identifier(ident: &str) -> PyResult<()> {
    validate_user_identifier(ident)
        .map_err(RustIdentifierValidationError::from)
        .map_err(RustIdentifierValidationError::to_py_err)
}

create_init_submodule! {
    errors: [IdentifierValidationError],
    funcs: [py_validate_identifier, py_validate_user_identifier],
}
