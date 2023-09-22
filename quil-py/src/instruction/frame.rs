use std::collections::HashMap;
use std::hash::Hash;

use quil_rs::{
    expression::Expression,
    instruction::{
        AttributeValue, Capture, FrameAttributes, FrameDefinition, FrameIdentifier,
        MemoryReference, Pulse, Qubit, RawCapture, SetFrequency, SetPhase, SetScale,
        ShiftFrequency, ShiftPhase, SwapPhases, WaveformInvocation,
    },
};
use rigetti_pyo3::{
    impl_hash, impl_repr, py_wrap_data_struct, py_wrap_union_enum,
    pyo3::{
        pymethods,
        types::{PyBool, PyString},
        Py, PyResult, Python,
    },
    PyTryFrom,
};

use super::PyQubit;
use crate::{
    expression::PyExpression,
    impl_copy_for_instruction, impl_eq, impl_to_quil,
    instruction::{PyMemoryReference, PyWaveformInvocation},
};

py_wrap_union_enum! {
    #[derive(Debug, PartialEq, Eq)]
    PyAttributeValue(AttributeValue) as "AttributeValue" {
        string: String => Py<PyString>,
        expression: Expression => PyExpression
    }
}
impl_repr!(PyAttributeValue);
impl_to_quil!(PyAttributeValue);
impl_hash!(PyAttributeValue);
impl_eq!(PyAttributeValue);

pub type PyFrameAttributes = HashMap<String, PyAttributeValue>;

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyFrameDefinition(FrameDefinition) as "FrameDefinition" {
        identifier: FrameIdentifier => PyFrameIdentifier,
        attributes: HashMap<String, AttributeValue> => PyFrameAttributes
    }
}
impl_repr!(PyFrameDefinition);
impl_to_quil!(PyFrameDefinition);
impl_copy_for_instruction!(PyFrameDefinition);
impl_eq!(PyFrameDefinition);

#[pymethods]
impl PyFrameDefinition {
    #[new]
    pub fn new(
        py: Python<'_>,
        identifier: PyFrameIdentifier,
        attributes: PyFrameAttributes,
    ) -> PyResult<Self> {
        Ok(Self(FrameDefinition::new(
            FrameIdentifier::py_try_from(py, &identifier)?,
            FrameAttributes::py_try_from(py, &attributes)?,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq, Hash)]
    #[pyo3(subclass)]
    PyFrameIdentifier(FrameIdentifier) as "FrameIdentifier" {
        name: String => Py<PyString>,
        qubits: Vec<Qubit> => Vec<PyQubit>
    }
}
impl_repr!(PyFrameIdentifier);
impl_to_quil!(PyFrameIdentifier);
impl_hash!(PyFrameIdentifier);
impl_eq!(PyFrameIdentifier);

#[pymethods]
impl PyFrameIdentifier {
    #[new]
    pub fn new(py: Python<'_>, name: String, qubits: Vec<PyQubit>) -> PyResult<Self> {
        Ok(Self(FrameIdentifier::new(
            name,
            Vec::<Qubit>::py_try_from(py, &qubits)?,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyCapture(Capture) as "Capture" {
        blocking: bool => Py<PyBool>,
        frame: FrameIdentifier => PyFrameIdentifier,
        memory_reference: MemoryReference => PyMemoryReference,
        waveform: WaveformInvocation => PyWaveformInvocation
    }
}
impl_repr!(PyCapture);
impl_to_quil!(PyCapture);
impl_copy_for_instruction!(PyCapture);
impl_eq!(PyCapture);

#[pymethods]
impl PyCapture {
    #[new]
    pub fn new(
        py: Python<'_>,
        blocking: bool,
        frame: PyFrameIdentifier,
        memory_reference: PyMemoryReference,
        waveform: PyWaveformInvocation,
    ) -> PyResult<Self> {
        Ok(Self(Capture::new(
            blocking,
            FrameIdentifier::py_try_from(py, &frame)?,
            MemoryReference::py_try_from(py, &memory_reference)?,
            WaveformInvocation::py_try_from(py, &waveform)?,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyPulse(Pulse) as "Pulse" {
        blocking: bool => Py<PyBool>,
        frame: FrameIdentifier => PyFrameIdentifier,
        waveform: WaveformInvocation => PyWaveformInvocation
    }
}
impl_repr!(PyPulse);
impl_to_quil!(PyPulse);
impl_copy_for_instruction!(PyPulse);
impl_eq!(PyPulse);

#[pymethods]
impl PyPulse {
    #[new]
    pub fn new(
        py: Python<'_>,
        blocking: bool,
        frame: PyFrameIdentifier,
        waveform: PyWaveformInvocation,
    ) -> PyResult<Self> {
        Ok(Self(Pulse::new(
            blocking,
            FrameIdentifier::py_try_from(py, &frame)?,
            WaveformInvocation::py_try_from(py, &waveform)?,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyRawCapture(RawCapture) as "RawCapture" {
        blocking: bool => Py<PyBool>,
        frame: FrameIdentifier => PyFrameIdentifier,
        duration: Expression => PyExpression,
        memory_reference: MemoryReference => PyMemoryReference
    }
}

impl_repr!(PyRawCapture);
impl_to_quil!(PyRawCapture);
impl_copy_for_instruction!(PyRawCapture);
impl_hash!(PyRawCapture);
impl_eq!(PyRawCapture);

#[pymethods]
impl PyRawCapture {
    #[new]
    pub fn new(
        py: Python<'_>,
        blocking: bool,
        frame: PyFrameIdentifier,
        duration: PyExpression,
        memory_reference: PyMemoryReference,
    ) -> PyResult<Self> {
        Ok(Self(RawCapture::new(
            blocking,
            FrameIdentifier::py_try_from(py, &frame)?,
            Expression::py_try_from(py, &duration)?,
            MemoryReference::py_try_from(py, &memory_reference)?,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PySetFrequency(SetFrequency) as "SetFrequency" {
        frame: FrameIdentifier => PyFrameIdentifier,
        frequency: Expression => PyExpression
    }
}
impl_repr!(PySetFrequency);
impl_to_quil!(PySetFrequency);
impl_copy_for_instruction!(PySetFrequency);
impl_hash!(PySetFrequency);
impl_eq!(PySetFrequency);

#[pymethods]
impl PySetFrequency {
    #[new]
    pub fn new(
        py: Python<'_>,
        frame: PyFrameIdentifier,
        frequency: PyExpression,
    ) -> PyResult<Self> {
        Ok(Self(SetFrequency::new(
            FrameIdentifier::py_try_from(py, &frame)?,
            Expression::py_try_from(py, &frequency)?,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PySetPhase(SetPhase) as "SetPhase" {
        frame: FrameIdentifier => PyFrameIdentifier,
        phase: Expression => PyExpression
    }
}
impl_repr!(PySetPhase);
impl_to_quil!(PySetPhase);
impl_copy_for_instruction!(PySetPhase);
impl_hash!(PySetPhase);
impl_eq!(PySetPhase);

#[pymethods]
impl PySetPhase {
    #[new]
    pub fn new(py: Python<'_>, frame: PyFrameIdentifier, phase: PyExpression) -> PyResult<Self> {
        Ok(Self(SetPhase::new(
            FrameIdentifier::py_try_from(py, &frame)?,
            Expression::py_try_from(py, &phase)?,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PySetScale(SetScale) as "SetScale" {
        frame: FrameIdentifier => PyFrameIdentifier,
        scale: Expression => PyExpression
    }
}
impl_repr!(PySetScale);
impl_to_quil!(PySetScale);
impl_copy_for_instruction!(PySetScale);
impl_hash!(PySetScale);
impl_eq!(PySetScale);

#[pymethods]
impl PySetScale {
    #[new]
    pub fn new(py: Python<'_>, frame: PyFrameIdentifier, scale: PyExpression) -> PyResult<Self> {
        Ok(Self(SetScale::new(
            FrameIdentifier::py_try_from(py, &frame)?,
            Expression::py_try_from(py, &scale)?,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyShiftFrequency(ShiftFrequency) as "ShiftFrequency" {
        frame: FrameIdentifier => PyFrameIdentifier,
        frequency: Expression => PyExpression
    }
}
impl_repr!(PyShiftFrequency);
impl_to_quil!(PyShiftFrequency);
impl_copy_for_instruction!(PyShiftFrequency);
impl_hash!(PyShiftFrequency);
impl_eq!(PyShiftFrequency);

#[pymethods]
impl PyShiftFrequency {
    #[new]
    pub fn new(
        py: Python<'_>,
        frame: PyFrameIdentifier,
        frequency: PyExpression,
    ) -> PyResult<Self> {
        Ok(Self(ShiftFrequency::new(
            FrameIdentifier::py_try_from(py, &frame)?,
            Expression::py_try_from(py, &frequency)?,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyShiftPhase(ShiftPhase) as "ShiftPhase" {
        frame: FrameIdentifier => PyFrameIdentifier,
        phase: Expression => PyExpression
    }
}
impl_repr!(PyShiftPhase);
impl_to_quil!(PyShiftPhase);
impl_copy_for_instruction!(PyShiftPhase);
impl_hash!(PyShiftPhase);
impl_eq!(PyShiftPhase);

#[pymethods]
impl PyShiftPhase {
    #[new]
    pub fn new(py: Python<'_>, frame: PyFrameIdentifier, phase: PyExpression) -> PyResult<Self> {
        Ok(Self(ShiftPhase::new(
            FrameIdentifier::py_try_from(py, &frame)?,
            Expression::py_try_from(py, &phase)?,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PySwapPhases(SwapPhases) as "SwapPhases" {
        frame_1: FrameIdentifier => PyFrameIdentifier,
        frame_2: FrameIdentifier => PyFrameIdentifier
    }
}
impl_repr!(PySwapPhases);
impl_to_quil!(PySwapPhases);
impl_copy_for_instruction!(PySwapPhases);
impl_hash!(PySwapPhases);
impl_eq!(PySwapPhases);

#[pymethods]
impl PySwapPhases {
    #[new]
    pub fn new(
        py: Python<'_>,
        frame_1: PyFrameIdentifier,
        frame_2: PyFrameIdentifier,
    ) -> PyResult<Self> {
        Ok(Self(SwapPhases::new(
            FrameIdentifier::py_try_from(py, &frame_1)?,
            FrameIdentifier::py_try_from(py, &frame_2)?,
        )))
    }
}
