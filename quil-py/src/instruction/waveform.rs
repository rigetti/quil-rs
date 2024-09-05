use indexmap::IndexMap;

use quil_rs::{
    expression::Expression,
    instruction::{Waveform, WaveformDefinition, WaveformInvocation, WaveformParameters},
};

use rigetti_pyo3::{
    impl_hash, impl_repr, py_wrap_data_struct,
    pyo3::{pymethods, types::PyString, Py, PyResult, Python},
    PyTryFrom,
};

use crate::{
    expression::PyExpression, impl_copy_for_instruction, impl_eq, impl_pickle_for_instruction,
    impl_to_quil,
};

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
impl_eq!(PyWaveform);

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
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass, module = "quil.instructions")]
    PyWaveformDefinition(WaveformDefinition) as "WaveformDefinition" {
        name: String => Py<PyString>,
        definition: Waveform => PyWaveform
    }
}
impl_repr!(PyWaveformDefinition);
impl_to_quil!(PyWaveformDefinition);
impl_copy_for_instruction!(PyWaveformDefinition);
impl_hash!(PyWaveformDefinition);
impl_eq!(PyWaveformDefinition);
impl_pickle_for_instruction!(PyWaveformDefinition);

#[pymethods]
impl PyWaveformDefinition {
    #[new]
    pub fn new(py: Python<'_>, name: String, definition: PyWaveform) -> PyResult<Self> {
        Ok(Self(WaveformDefinition::new(
            name,
            Waveform::py_try_from(py, &definition)?,
        )))
    }
}

pub type PyWaveformParameters = IndexMap<String, PyExpression>;

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyWaveformInvocation(WaveformInvocation) as "WaveformInvocation" {
        name: String => Py<PyString>,
        parameters: WaveformParameters => PyWaveformParameters
    }
}
impl_repr!(PyWaveformInvocation);
impl_to_quil!(PyWaveformInvocation);
impl_eq!(PyWaveformInvocation);

#[pymethods]
impl PyWaveformInvocation {
    #[new]
    pub fn new(py: Python<'_>, name: String, parameters: PyWaveformParameters) -> PyResult<Self> {
        Ok(Self(WaveformInvocation::new(
            name,
            WaveformParameters::py_try_from(py, &parameters)?,
        )))
    }
}
