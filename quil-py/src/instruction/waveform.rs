use std::collections::HashMap;

use quil_rs::{
    expression::Expression,
    instruction::{Waveform, WaveformDefinition, WaveformInvocation},
};

use rigetti_pyo3::{
    impl_hash, impl_repr, impl_str, py_wrap_data_struct,
    pyo3::{
        pyclass::CompareOp, pymethods, types::PyString, IntoPy, Py, PyObject, PyResult, Python,
    },
    PyTryFrom, PyWrapper,
};

use crate::expression::PyExpression;

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyWaveform(Waveform) as "Waveform" {
        matrix: Vec<Expression> => Vec<PyExpression>,
        parameters: Vec<String> => Vec<Py<PyString>>
    }
}
impl_repr!(PyWaveform);
impl_hash!(PyWaveform);

#[pymethods]
impl PyWaveform {
    #[new]
    pub fn new(
        py: Python<'_>,
        matrix: Vec<PyExpression>,
        parameters: Vec<String>,
    ) -> PyResult<Self> {
        Ok(Self(Waveform::new(
            Vec::<Expression>::py_try_from(py, &matrix)?,
            parameters,
        )))
    }

    pub fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self.as_inner() == other.as_inner()).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyWaveformDefinition(WaveformDefinition) as "WaveformDefinition" {
        name: String => Py<PyString>,
        definition: Waveform => PyWaveform
    }
}
impl_repr!(PyWaveformDefinition);
impl_str!(PyWaveformDefinition);
impl_hash!(PyWaveformDefinition);

#[pymethods]
impl PyWaveformDefinition {
    #[new]
    pub fn new(py: Python<'_>, name: String, definition: PyWaveform) -> PyResult<Self> {
        Ok(Self(WaveformDefinition::new(
            name,
            Waveform::py_try_from(py, &definition)?,
        )))
    }

    pub fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self.as_inner() == other.as_inner()).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyWaveformInvocation(WaveformInvocation) as "WaveformInvocation" {
        name: String => Py<PyString>,
        parameters: HashMap<String, Expression> => HashMap<String, PyExpression>
    }
}
impl_repr!(PyWaveformInvocation);
impl_str!(PyWaveformInvocation);
impl_hash!(PyWaveformInvocation);

#[pymethods]
impl PyWaveformInvocation {
    #[new]
    pub fn new(
        py: Python<'_>,
        name: String,
        parameters: HashMap<String, PyExpression>,
    ) -> PyResult<Self> {
        Ok(Self(WaveformInvocation::new(
            name,
            HashMap::<String, Expression>::py_try_from(py, &parameters)?,
        )))
    }

    pub fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self.as_inner() == other.as_inner()).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}
