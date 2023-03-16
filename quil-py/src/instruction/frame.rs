use std::collections::HashMap;
use std::hash::Hash;

use quil_rs::instruction::{
    AttributeValue, FrameAttributes, FrameDefinition, FrameIdentifier, Qubit,
};
use rigetti_pyo3::{
    impl_hash, impl_repr, impl_str, py_wrap_data_struct, py_wrap_union_enum,
    pyo3::{
        pyclass::CompareOp, pymethods, types::PyString, IntoPy, Py, PyObject, PyResult, Python,
    },
    PyTryFrom, PyWrapper,
};

use super::PyQubit;
use crate::expression::PyExpression;

py_wrap_union_enum! {
    #[derive(Debug, PartialEq, Eq)]
    PyAttributeValue(AttributeValue) as "AttributeValue" {
        string: String => Py<PyString>,
        expression: Expression => PyExpression
    }
}
impl_repr!(PyAttributeValue);
impl_str!(PyAttributeValue);

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
impl_str!(PyFrameDefinition);

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
impl_str!(PyFrameIdentifier);
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
