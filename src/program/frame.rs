/**
 * Copyright 2021 Rigetti Computing
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 **/
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

    /// Return all frames in the set which match all of these conditions. If a frame _would_ match, but is
    /// not present in this [FrameSet], then it is not returned (notably, the [FrameMatchCondition::Specific]
    /// match condition).
    pub(crate) fn get_matching_keys<'s, 'a>(
        &'s self,
        condition: FrameMatchCondition<'a>,
    ) -> HashSet<&'s FrameIdentifier> {
        let keys = self.frames.keys();

        match condition {
            FrameMatchCondition::All => keys.collect(),
            FrameMatchCondition::AnyOfNames(names) => {
                keys.filter(|&f| names.contains(&f.name)).collect()
            }
            FrameMatchCondition::AnyOfQubits(qubits) => {
                let any_of_set: HashSet<_> = qubits.iter().collect();

                keys.filter(|&f| f.qubits.iter().any(|q| any_of_set.contains(q)))
                    .collect()
            }
            FrameMatchCondition::ExactQubits(qubits) => {
                let all_of_set: HashSet<_> = qubits.iter().collect();

                keys.filter(|&f| f.qubits.iter().all(|q| all_of_set.contains(q)))
                    .collect()
            }
            FrameMatchCondition::Specific(frame) => {
                if let Some((frame, _)) = self.frames.get_key_value(&frame) {
                    vec![frame].into_iter().collect()
                } else {
                    HashSet::new()
                }
            }
            FrameMatchCondition::And(conditions) => {
                let individual_sets: Vec<HashSet<&FrameIdentifier>> = conditions
                    .into_iter()
                    .map(|c| self.get_matching_keys(c))
                    .collect();
                individual_sets
                    .into_iter()
                    .reduce(|acc, el| acc.into_iter().filter(|&v| el.contains(v)).collect())
                    .unwrap_or_default()
            }
            FrameMatchCondition::Or(conditions) => {
                let individual_sets: Vec<HashSet<&FrameIdentifier>> = conditions
                    .into_iter()
                    .map(|c| self.get_matching_keys(c))
                    .collect();
                individual_sets
                    .into_iter()
                    .reduce(|mut acc, el| {
                        el.into_iter().for_each(|v| {
                            acc.insert(v);
                        });
                        acc
                    })
                    .unwrap_or_default()
            }
        }
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

pub(crate) enum FrameMatchCondition<'a> {
    /// Match all frames in the set
    All,

    /// Match all frames which shares any one of these names
    AnyOfNames(&'a [String]),

    /// Match all frames which contain any of these qubits
    AnyOfQubits(&'a [Qubit]),

    /// Match all frames which contain exactly these qubits
    ExactQubits(&'a [Qubit]),

    /// Return these specific frames, if present in the set
    Specific(&'a FrameIdentifier),

    /// Return all frames which match all of these conditions
    And(Vec<FrameMatchCondition<'a>>),

    /// Return all frames which match any of these conditions
    #[allow(dead_code)]
    Or(Vec<FrameMatchCondition<'a>>),
}
