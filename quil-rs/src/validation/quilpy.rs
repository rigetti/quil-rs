use pyo3::{prelude::*, wrap_pymodule};

use super::identifier::{validate_identifier, validate_user_identifier};

#[pymodule]
#[pyo3(name = "validation", module = "quil", submodule)]
pub(crate) fn init_submodule(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_wrapped(wrap_pymodule!(init_subsubmodule))?;
    Ok(())
}

#[pymodule]
#[pyo3(name = "identifier", module = "quil.validation", submodule)]
fn init_subsubmodule(m: &Bound<'_, PyModule>) -> PyResult<()> {
    use crate::quilpy::errors;

    let py = m.py();
    m.add("IdentifierValidationError", py.get_type::<errors::IdentifierValidationError>())?;
    m.add_function(wrap_pyfunction!(validate_identifier, m)?)?;
    m.add_function(wrap_pyfunction!(validate_user_identifier, m)?)?;
    Ok(())
}
