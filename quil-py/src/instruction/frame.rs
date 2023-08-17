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
        pyclass::CompareOp,
        pymethods,
        types::{PyBool, PyString},
        IntoPy, Py, PyObject, PyResult, Python,
    },
    PyTryFrom, PyWrapper,
};

use super::PyQubit;
use crate::{
    expression::PyExpression,
    impl_to_quil,
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

#[pymethods]
impl PyAttributeValue {
    pub fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self.as_inner() == other.as_inner()).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}

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

    pub fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self.as_inner() == other.as_inner()).into_py(py),
            _ => py.NotImplemented(),
        }
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

#[pymethods]
impl PyFrameIdentifier {
    #[new]
    pub fn new(py: Python<'_>, name: String, qubits: Vec<PyQubit>) -> PyResult<Self> {
        Ok(Self(FrameIdentifier::new(
            name,
            Vec::<Qubit>::py_try_from(py, &qubits)?,
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
    PyCapture(Capture) as "Capture" {
        blocking: bool => Py<PyBool>,
        frame: FrameIdentifier => PyFrameIdentifier,
        memory_reference: MemoryReference => PyMemoryReference,
        waveform: WaveformInvocation => PyWaveformInvocation
    }
}
impl_repr!(PyCapture);
impl_to_quil!(PyCapture);

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
    PyPulse(Pulse) as "Pulse" {
        blocking: bool => Py<PyBool>,
        frame: FrameIdentifier => PyFrameIdentifier,
        waveform: WaveformInvocation => PyWaveformInvocation
    }
}

impl_repr!(PyPulse);
impl_to_quil!(PyPulse);

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
    PyRawCapture(RawCapture) as "RawCapture" {
        blocking: bool => Py<PyBool>,
        frame: FrameIdentifier => PyFrameIdentifier,
        duration: Expression => PyExpression,
        memory_reference: MemoryReference => PyMemoryReference
    }
}

impl_repr!(PyRawCapture);
impl_to_quil!(PyRawCapture);
impl_hash!(PyRawCapture);

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
    PySetFrequency(SetFrequency) as "SetFrequency" {
        frame: FrameIdentifier => PyFrameIdentifier,
        frequency: Expression => PyExpression
    }
}
impl_repr!(PySetFrequency);
impl_to_quil!(PySetFrequency);
impl_hash!(PySetFrequency);

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
    PySetPhase(SetPhase) as "SetPhase" {
        frame: FrameIdentifier => PyFrameIdentifier,
        phase: Expression => PyExpression
    }
}
impl_repr!(PySetPhase);
impl_to_quil!(PySetPhase);
impl_hash!(PySetPhase);

#[pymethods]
impl PySetPhase {
    #[new]
    pub fn new(py: Python<'_>, frame: PyFrameIdentifier, phase: PyExpression) -> PyResult<Self> {
        Ok(Self(SetPhase::new(
            FrameIdentifier::py_try_from(py, &frame)?,
            Expression::py_try_from(py, &phase)?,
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
    PySetScale(SetScale) as "SetScale" {
        frame: FrameIdentifier => PyFrameIdentifier,
        scale: Expression => PyExpression
    }
}
impl_repr!(PySetScale);
impl_to_quil!(PySetScale);
impl_hash!(PySetScale);

#[pymethods]
impl PySetScale {
    #[new]
    pub fn new(py: Python<'_>, frame: PyFrameIdentifier, scale: PyExpression) -> PyResult<Self> {
        Ok(Self(SetScale::new(
            FrameIdentifier::py_try_from(py, &frame)?,
            Expression::py_try_from(py, &scale)?,
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
    PyShiftFrequency(ShiftFrequency) as "ShiftFrequency" {
        frame: FrameIdentifier => PyFrameIdentifier,
        frequency: Expression => PyExpression
    }
}
impl_repr!(PyShiftFrequency);
impl_to_quil!(PyShiftFrequency);
impl_hash!(PyShiftFrequency);

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
    PyShiftPhase(ShiftPhase) as "ShiftPhase" {
        frame: FrameIdentifier => PyFrameIdentifier,
        phase: Expression => PyExpression
    }
}
impl_repr!(PyShiftPhase);
impl_to_quil!(PyShiftPhase);
impl_hash!(PyShiftPhase);

#[pymethods]
impl PyShiftPhase {
    #[new]
    pub fn new(py: Python<'_>, frame: PyFrameIdentifier, phase: PyExpression) -> PyResult<Self> {
        Ok(Self(ShiftPhase::new(
            FrameIdentifier::py_try_from(py, &frame)?,
            Expression::py_try_from(py, &phase)?,
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
    PySwapPhases(SwapPhases) as "SwapPhases" {
        frame_1: FrameIdentifier => PyFrameIdentifier,
        frame_2: FrameIdentifier => PyFrameIdentifier
    }
}
impl_repr!(PySwapPhases);
impl_to_quil!(PySwapPhases);
impl_hash!(PySwapPhases);

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

    pub fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self.as_inner() == other.as_inner()).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}
