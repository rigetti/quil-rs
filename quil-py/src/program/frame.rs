use pyo3::{pymethods, PyResult, Python};
use std::collections::HashMap;

use quil_rs::program::FrameSet;
use rigetti_pyo3::{impl_repr, py_wrap_type, PyWrapper, ToPython};

use crate::instruction::{PyFrameAttributes, PyFrameIdentifier};

py_wrap_type! {
    PyFrameSet(FrameSet) as "FrameSet"
}
impl_repr!(PyFrameSet);

#[pymethods]
impl PyFrameSet {
    fn get_all_frames(
        &self,
        py: Python<'_>,
    ) -> PyResult<HashMap<PyFrameIdentifier, PyFrameAttributes>> {
        self.as_inner()
            .iter()
            .map(|(ident, attribs)| Ok((ident.to_python(py)?, attribs.to_python(py)?)))
            .collect()
    }
}
