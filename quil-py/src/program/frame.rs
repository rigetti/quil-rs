use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::convert::AsRef;

use quil_rs::{
    instruction::{FrameAttributes, FrameIdentifier},
    program::FrameSet,
};
use rigetti_pyo3::{
    impl_as_mut_for_wrapper, impl_repr, py_wrap_type,
    pyo3::{pymethods, PyResult, Python},
    PyTryFrom, PyWrapper, PyWrapperMut, ToPython,
};

use crate::{
    impl_eq,
    instruction::{PyFrameAttributes, PyFrameIdentifier, PyInstruction},
};

py_wrap_type! {
    #[derive(Debug, PartialEq, Eq)]
    PyFrameSet(FrameSet) as "FrameSet"
}
impl_repr!(PyFrameSet);
impl_as_mut_for_wrapper!(PyFrameSet);
impl_eq!(PyFrameSet);

#[pymethods]
impl PyFrameSet {
    #[new]
    pub fn new() -> Self {
        Self(FrameSet::new())
    }

    pub fn get(
        &self,
        py: Python<'_>,
        identifier: PyFrameIdentifier,
    ) -> PyResult<Option<PyFrameAttributes>> {
        self.as_inner()
            .get(&FrameIdentifier::py_try_from(py, &identifier)?)
            .to_python(py)
    }

    pub fn get_keys(&self, py: Python<'_>) -> PyResult<Vec<PyFrameIdentifier>> {
        self.as_inner().get_keys().to_python(py)
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

    pub fn insert(
        &mut self,
        py: Python<'_>,
        identifier: PyFrameIdentifier,
        attributes: PyFrameAttributes,
    ) -> PyResult<()> {
        self.as_inner_mut().insert(
            FrameIdentifier::py_try_from(py, &identifier)?,
            FrameAttributes::py_try_from(py, &attributes)?,
        );
        Ok(())
    }

    pub fn merge(&mut self, py: Python<'_>, other: PyFrameSet) -> PyResult<()> {
        self.as_inner_mut()
            .merge(FrameSet::py_try_from(py, &other)?);
        Ok(())
    }

    pub fn intersection(
        &self,
        py: Python<'_>,
        identifiers: HashSet<PyFrameIdentifier>,
    ) -> PyResult<Self> {
        Ok(Self(
            self.as_inner().intersection(
                &HashSet::<FrameIdentifier>::py_try_from(py, &identifiers)?
                    .iter()
                    .map(Borrow::borrow)
                    .collect(),
            ),
        ))
    }

    pub fn is_empty(&self) -> bool {
        self.as_inner().is_empty()
    }

    pub fn to_instructions(&self, py: Python<'_>) -> PyResult<Vec<PyInstruction>> {
        self.as_inner().to_instructions().to_python(py)
    }

    pub fn __len__(&self) -> usize {
        self.as_inner().len()
    }
}

impl Default for PyFrameSet {
    fn default() -> Self {
        Self::new()
    }
}
