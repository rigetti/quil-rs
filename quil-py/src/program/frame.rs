use std::collections::HashMap;

use quil_rs::program::FrameSet;
use rigetti_pyo3::{
    impl_repr, py_wrap_type,
    pyo3::{pyclass::CompareOp, pymethods, IntoPy, PyObject, PyResult, Python},
    PyWrapper, ToPython,
};

use crate::instruction::{PyFrameAttributes, PyFrameIdentifier};

py_wrap_type! {
    #[derive(Debug, PartialEq, Eq)]
    PyFrameSet(FrameSet) as "FrameSet"
}
impl_repr!(PyFrameSet);

#[pymethods]
impl PyFrameSet {
    #[new]
    pub fn new() -> Self {
        Self(FrameSet::new())
    }

    pub fn get_all_frames(
        &self,
        py: Python<'_>,
    ) -> PyResult<HashMap<PyFrameIdentifier, PyFrameAttributes>> {
        self.as_inner()
            .iter()
            .map(|(ident, attribs)| Ok((ident.to_python(py)?, attribs.to_python(py)?)))
            .collect()
    }

    pub fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self.as_inner() == other.as_inner()).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}

impl Default for PyFrameSet {
    fn default() -> Self {
        Self::new()
    }
}
