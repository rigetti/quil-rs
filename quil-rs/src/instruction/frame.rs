use std::collections::HashMap;
use std::fmt;

use serde::{Deserialize, Serialize};

use super::{format_qubits, Qubit};
use crate::expression::Expression;

#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum AttributeValue {
    String(String),
    Expression(Expression),
}

impl fmt::Display for AttributeValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use AttributeValue::*;
        match self {
            String(value) => write!(f, "\"{value}\""),
            Expression(value) => write!(f, "{value}"),
        }
    }
}

pub type FrameAttributes = HashMap<String, AttributeValue>;

#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub struct FrameDefinition {
    pub identifier: FrameIdentifier,
    pub attributes: HashMap<String, AttributeValue>,
}

impl FrameDefinition {
    pub fn new(identifier: FrameIdentifier, attributes: FrameAttributes) -> Self {
        Self {
            identifier,
            attributes,
        }
    }
}

impl fmt::Display for FrameDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "DEFFRAME {}:{}",
            self.identifier,
            self.attributes
                .iter()
                .map(|(k, v)| format!("\n\t{k}: {v}"))
                .collect::<String>()
        )
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct FrameIdentifier {
    pub name: String,
    pub qubits: Vec<Qubit>,
}

impl FrameIdentifier {
    pub fn new(name: String, qubits: Vec<Qubit>) -> Self {
        Self { name, qubits }
    }
}

impl fmt::Display for FrameIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} \"{}\"", format_qubits(&self.qubits), self.name)
    }
}
