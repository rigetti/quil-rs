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
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct FrameSet {
    frames: HashMap<FrameIdentifier, FrameAttributes>,
}

impl FrameSet {
    pub fn new() -> Self {
        FrameSet {
            frames: HashMap::new(),
        }
    }

    /// Retrieve the attributes of a frame by its identifier.
    pub fn get(&self, identifier: &FrameIdentifier) -> Option<&FrameAttributes> {
        self.frames.get(identifier)
    }

    /// Return a list of all frame IDs described by this FrameSet.
    pub fn get_keys(&self) -> Vec<&FrameIdentifier> {
        self.frames.keys().collect()
    }

    pub(crate) fn get_matching_keys_for_conditions<'s>(
        &'s self,
        condition: FrameMatchConditions,
    ) -> MatchedFrames<'s> {
        let used = condition
            .used
            .map_or_else(HashSet::new, |c| self.get_matching_keys_for_condition(c));

        let blocked = condition.blocked.map_or_else(HashSet::new, |c| {
            let mut blocked = self.get_matching_keys_for_condition(c);

            if !used.is_empty() {
                blocked.retain(|&f| !used.contains(&f));
            }

            blocked
        });

        MatchedFrames { used, blocked }
    }

    /// Return all frames in the set which match all of these conditions. If a frame _would_ match, but is
    /// not present in this [FrameSet], then it is not returned (notably, the [FrameMatchCondition::Specific]
    /// match condition).
    pub(crate) fn get_matching_keys_for_condition<'s>(
        &'s self,
        condition: FrameMatchCondition,
    ) -> HashSet<&'s FrameIdentifier> {
        let keys = self.frames.keys();

        match condition {
            FrameMatchCondition::All => keys.collect(),
            FrameMatchCondition::AnyOfNames(names) => {
                keys.filter(|&f| names.contains(f.name.as_str())).collect()
            }
            FrameMatchCondition::AnyOfQubits(qubits) => keys
                .filter(|&f| f.qubits.iter().any(|q| qubits.contains(&q)))
                .collect(),
            FrameMatchCondition::ExactQubits(qubits) => keys
                .filter(|&f| f.qubits.iter().all(|q| qubits.contains(q)))
                .collect(),
            FrameMatchCondition::Specific(frame) => {
                // This unusual pattern (fetch key & value by key, discard value) allows us to return
                // a reference to `self` rather than `condition`, keeping lifetimes simpler.
                if let Some((frame, _)) = self.frames.get_key_value(frame) {
                    HashSet::from([frame])
                } else {
                    HashSet::new()
                }
            }
            FrameMatchCondition::And(conditions) => conditions
                .into_iter()
                .map(|c| self.get_matching_keys_for_condition(c))
                .reduce(|acc, el| acc.into_iter().filter(|&v| el.contains(v)).collect())
                .unwrap_or_default(),
            FrameMatchCondition::Or(conditions) => conditions
                .into_iter()
                .flat_map(|c| self.get_matching_keys_for_condition(c))
                .collect(),
        }
    }

    /// Insert a new frame by ID, overwriting any existing one.
    pub fn insert(&mut self, identifier: FrameIdentifier, attributes: FrameAttributes) {
        self.frames.insert(identifier, attributes);
    }

    /// Merge another [FrameSet] with this one, overwriting any existing keys
    pub fn merge(&mut self, other: FrameSet) {
        self.frames.extend(other.frames);
    }

    /// Return a new [FrameSet] which describes only the given [FrameIdentifier]s.
    pub fn intersection(&self, identifiers: &HashSet<&FrameIdentifier>) -> Self {
        let mut new_frameset = Self::new();

        for (identifier, definition) in &self.frames {
            if identifiers.contains(&identifier) {
                new_frameset.insert(identifier.clone(), definition.clone())
            }
        }

        new_frameset
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

    /// Return the Quil instructions which describe the contained frames, consuming the [`FrameSet`].
    pub fn into_instructions(self) -> Vec<Instruction> {
        self.frames
            .into_iter()
            .map(|(identifier, attributes)| {
                Instruction::FrameDefinition(FrameDefinition {
                    identifier,
                    attributes,
                })
            })
            .collect()
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

#[derive(Debug)]
pub(crate) enum FrameMatchCondition<'a> {
    /// Match all frames in the set
    All,

    /// Match all frames which share any one of these names
    AnyOfNames(HashSet<&'a str>),

    /// Match all frames which contain any of these qubits
    AnyOfQubits(HashSet<&'a Qubit>),

    /// Match all frames which contain exactly these qubits
    ExactQubits(HashSet<&'a Qubit>),

    /// Return this specific frame, if present in the set
    Specific(&'a FrameIdentifier),

    /// Return all frames which match all of these conditions
    And(Vec<FrameMatchCondition<'a>>),

    /// Return all frames which match any of these conditions
    Or(Vec<FrameMatchCondition<'a>>),
}

/// A pair of conditions to match frames within a [`crate::Program`] (or another scope).
///
/// This allows for deferred evaluation of matching an instruction against available frames.
pub(crate) struct FrameMatchConditions<'a> {
    /// A condition to identify which frames within a [`crate::Program`] (or another scope)
    /// are actively used by an [`Instruction`].
    ///
    /// If `None`, then this [`Instruction`] does not use any frames, regardless of which are available.
    pub used: Option<FrameMatchCondition<'a>>,

    /// A condition to identify which frames within a [`crate::Program`] (or another scope)
    /// are blocked by an [`Instruction`]. A "blocked" frame is one which is not used by the
    /// `Instruction` but is not available for use by other instructions while this one executes.
    ///
    /// **Note**: for efficiency in computation, this may match frames also matched by `used`.
    /// In order to query which frames are _blocked but not used_, both conditions must first
    /// be evaluated in the scope of the available frames.
    pub blocked: Option<FrameMatchCondition<'a>>,
}

/// The product of evaluating  [`FrameMatchConditions`] in the scope of available frames (such as within a [`crate::Program`]).
#[derive(Debug)]
pub struct MatchedFrames<'a> {
    /// Which concrete frames are blocked and not used.
    /// This set is mutually exclusive with `used`.
    pub blocked: HashSet<&'a FrameIdentifier>,

    /// Which concrete frames are used by the [`Instruction`]
    pub used: HashSet<&'a FrameIdentifier>,
}

impl<'a> MatchedFrames<'a> {
    /// Which concrete frames are blocked and not used.
    /// This set is mutually exclusive with `used`.
    pub fn blocked(&self) -> &HashSet<&'a FrameIdentifier> {
        &self.blocked
    }

    /// Which concrete frames are used by the [`Instruction`]
    pub fn used(&self) -> &HashSet<&'a FrameIdentifier> {
        &self.used
    }
}
