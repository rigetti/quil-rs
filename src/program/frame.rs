// Copyright 2021 Rigetti Computing
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

use std::collections::{HashMap, HashSet};

use crate::instruction::{FrameAttributes, FrameDefinition, FrameIdentifier, Instruction, Qubit};

/// A collection of Quil frames (`DEFFRAME` instructions) with utility methods.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct FrameSet {
    frames: HashMap<FrameIdentifier, FrameAttributes>,
}

impl FrameSet {
    pub fn new() -> Self {
        FrameSet {
            frames: HashMap::new(),
        }
    }

    /// Return a list of all frame IDs described by this FrameSet.
    pub fn get_keys(&self) -> Vec<&FrameIdentifier> {
        self.frames.keys().collect()
    }

    /// Return all contained FrameIdentifiers which include **exactly** these qubits (in any order)
    /// and - if names are provided - match one of the names.
    pub fn get_matching_keys(&self, qubits: &[Qubit], names: &[String]) -> Vec<&FrameIdentifier> {
        let qubit_set: HashSet<&Qubit> = qubits.iter().collect();
        self.frames
            .iter()
            .filter(|(identifier, _)| {
                (names.is_empty() || names.contains(&identifier.name))
                    && qubit_set == identifier.qubits.iter().collect()
            })
            .map(|(id, _)| id)
            .collect::<Vec<_>>()
    }

    /// Retrieve the attributes of a frame by its identifier.
    pub fn get(&self, identifier: &FrameIdentifier) -> Option<&FrameAttributes> {
        self.frames.get(identifier)
    }

    /// Insert a new frame by ID, overwriting any existing one.
    pub fn insert(&mut self, identifier: FrameIdentifier, attributes: FrameAttributes) {
        self.frames.insert(identifier, attributes);
    }

    /// Iterate through the contained frames.
    pub fn iter(&self) -> std::collections::hash_map::Iter<'_, FrameIdentifier, FrameAttributes> {
        self.frames.iter()
    }

    /// Return the number of frames described within.
    pub fn len(&self) -> usize {
        self.frames.len()
    }

    /// Return true if this describes no frames.
    pub fn is_empty(&self) -> bool {
        self.frames.is_empty()
    }

    /// Return the Quil instructions which describe the contained frames.
    pub fn to_instructions(&self) -> Vec<Instruction> {
        self.frames
            .iter()
            .map(|(identifier, attributes)| {
                Instruction::FrameDefinition(FrameDefinition {
                    identifier: identifier.clone(),
                    attributes: attributes.clone(),
                })
            })
            .collect()
    }
}
