use std::collections::HashMap;
use std::hash::Hash;

use quil_rs::instruction::{AttributeValue, FrameDefinition, FrameIdentifier, Qubit};
use rigetti_pyo3::{
    impl_hash, impl_repr, impl_str, py_wrap_data_struct, py_wrap_union_enum,
    pyo3::{types::PyString, Py},
};

use super::{PyExpression, PyQubit};

py_wrap_data_struct! {
    #[derive(PartialEq, Eq, Hash)]
    PyFrameIdentifier(FrameIdentifier) as "FrameIdentifier" {
        name: String => Py<PyString>,
        qubits: Vec<Qubit> => Vec<PyQubit>
    }
}
impl_repr!(PyFrameIdentifier);
impl_str!(PyFrameIdentifier);
impl_hash!(PyFrameIdentifier);

py_wrap_union_enum! {
    PyAttributeValue(AttributeValue) as "AttributeValue" {
        string: String => Py<PyString>,
        expression: Expression => PyExpression
    }
}

pub type PyFrameAttributes = HashMap<String, PyAttributeValue>;

py_wrap_data_struct! {
    PyFrameDefinition(FrameDefinition) as "FrameDefinition" {
        identifier: FrameIdentifier => PyFrameIdentifier,
        attributes: HashMap<String, AttributeValue> => PyFrameAttributes
    }
}
impl_repr!(PyFrameDefinition);
