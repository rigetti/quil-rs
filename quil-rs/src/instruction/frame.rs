use std::collections::HashMap;
use std::fmt;

use serde::{Deserialize, Serialize};

use super::{format_qubits, MemoryReference, Qubit, WaveformInvocation};
use crate::expression::Expression;

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Capture {
    pub blocking: bool,
    pub frame: FrameIdentifier,
    pub memory_reference: MemoryReference,
    pub waveform: WaveformInvocation,
}

impl Capture {
    pub fn new(
        blocking: bool,
        frame: FrameIdentifier,
        memory_reference: MemoryReference,
        waveform: WaveformInvocation,
    ) -> Self {
        Self {
            blocking,
            frame,
            memory_reference,
            waveform,
        }
    }
}

impl fmt::Display for Capture {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.blocking {
            write!(f, "NONBLOCKING ")?;
        }
        write!(
            f,
            "CAPTURE {} {} {}",
            self.frame, self.waveform, self.memory_reference
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Pulse {
    pub blocking: bool,
    pub frame: FrameIdentifier,
    pub waveform: WaveformInvocation,
}

impl Pulse {
    pub fn new(blocking: bool, frame: FrameIdentifier, waveform: WaveformInvocation) -> Self {
        Self {
            blocking,
            frame,
            waveform,
        }
    }
}

impl fmt::Display for Pulse {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.blocking {
            write!(f, "NONBLOCKING ")?
        }
        write!(f, "PULSE {} {}", self.frame, self.waveform)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RawCapture {
    pub blocking: bool,
    pub frame: FrameIdentifier,
    pub duration: Expression,
    pub memory_reference: MemoryReference,
}

impl RawCapture {
    pub fn new(
        blocking: bool,
        frame: FrameIdentifier,
        duration: Expression,
        memory_reference: MemoryReference,
    ) -> Self {
        Self {
            blocking,
            frame,
            duration,
            memory_reference,
        }
    }
}

impl fmt::Display for RawCapture {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.blocking {
            write!(f, "NONBLOCKING ")?
        }
        write!(
            f,
            "RAW-CAPTURE {} {} {}",
            self.frame, self.duration, self.memory_reference
        )
    }
}
