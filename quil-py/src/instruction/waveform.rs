use pyo3::{types::PyString, Py};
use quil_rs::{
    expression::Expression,
    instruction::{Waveform, WaveformDefinition},
};
use rigetti_pyo3::{impl_repr, impl_str, py_wrap_data_struct};

use crate::expression::PyExpression;

py_wrap_data_struct! {
    PyWaveform(Waveform) as "Waveform" {
        matrix: Vec<Expression> => Vec<PyExpression>,
        parameters: Vec<String> => Vec<Py<PyString>>
    }
}
impl_repr!(PyWaveform);

py_wrap_data_struct! {
    PyWaveformDefinition(WaveformDefinition) as "WaveformDef" {
        name: String => Py<PyString>,
        definition: Waveform => PyWaveform
    }
}
impl_repr!(PyWaveformDefinition);
impl_str!(PyWaveformDefinition);
