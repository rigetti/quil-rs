use std::fmt::Debug;

use crate::instruction::GateSignature;

use super::{CalibrationExpansion, CalibrationSource, DefGateSequenceExpansion, InstructionIndex};

/// A SourceMap provides information necessary to understand which parts of a target
/// were derived from which parts of a source artifact, in such a way that they can be
/// mapped in either direction.
///
/// The behavior of such mappings depends on the implementations of the generic `Index` types,
/// but this may be a many-to-many mapping, where one element of the source is mapped (contributes)
/// to zero or many elements of the target, and vice versa.
///
/// This is also intended to be mergeable in a chain, such that the combined result of a series
/// of transformations can be expressed within a single source mapping.
#[derive(Clone, Debug, PartialEq)]
pub struct SourceMap<SourceIndex, TargetIndex> {
    pub(crate) entries: Vec<SourceMapEntry<SourceIndex, TargetIndex>>,
}

impl<SourceIndex, TargetIndex> SourceMap<SourceIndex, TargetIndex> {
    /// Return all source ranges in the source map which were used to generate the target range.
    ///
    /// This is `O(n)` where `n` is the number of entries in the map.
    pub fn list_sources<QueryIndex>(&self, target_index: &QueryIndex) -> Vec<&SourceIndex>
    where
        TargetIndex: SourceMapIndexable<QueryIndex>,
    {
        self.entries
            .iter()
            .filter_map(|entry| {
                if entry.target_location().intersects(target_index) {
                    Some(entry.source_location())
                } else {
                    None
                }
            })
            .collect()
    }

    /// Return all target ranges in the source map which were used to generate the source range.
    ///
    /// This is `O(n)` where `n` is the number of entries in the map.
    pub fn list_targets<QueryIndex>(&self, source_index: &QueryIndex) -> Vec<&TargetIndex>
    where
        SourceIndex: SourceMapIndexable<QueryIndex>,
    {
        self.entries
            .iter()
            .filter_map(|entry| {
                if entry.source_location().intersects(source_index) {
                    Some(entry.target_location())
                } else {
                    None
                }
            })
            .collect()
    }
}

impl<SourceIndex, TargetIndex> Default for SourceMap<SourceIndex, TargetIndex> {
    fn default() -> Self {
        Self {
            entries: Vec::new(),
        }
    }
}

impl<SourceIndex, TargetIndex> SourceMap<SourceIndex, TargetIndex> {
    pub fn entries(&self) -> &[SourceMapEntry<SourceIndex, TargetIndex>] {
        &self.entries
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SourceMapEntry<SourceIndex, TargetIndex> {
    /// The locator within the source artifact
    pub(crate) source_location: SourceIndex,

    /// The locator within the target artifact
    pub(crate) target_location: TargetIndex,
}

impl<SourceIndex, TargetIndex> SourceMapEntry<SourceIndex, TargetIndex> {
    pub fn source_location(&self) -> &SourceIndex {
        &self.source_location
    }

    pub fn target_location(&self) -> &TargetIndex {
        &self.target_location
    }
}

/// A trait for types which can be used as lookup indices in a `SourceMap.`
pub trait SourceMapIndexable<Index> {
    /// Return `true` if a source or target index intersects `other`.
    fn intersects(&self, other: &Index) -> bool;
}

impl SourceMapIndexable<InstructionIndex> for InstructionIndex {
    fn intersects(&self, other: &InstructionIndex) -> bool {
        self == other
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct InstructionSource(std::ops::Range<InstructionIndex>);

impl InstructionSource {
    pub fn start(&self) -> InstructionIndex {
        self.0.start
    }

    pub fn end(&self) -> InstructionIndex {
        self.0.end
    }

    pub fn contains(&self, index: &InstructionIndex) -> bool {
        self.0.contains(index)
    }
}

impl From<InstructionIndex> for InstructionSource {
    fn from(value: InstructionIndex) -> Self {
        Self(value..InstructionIndex(value.0 + 1))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum InstructionTarget<R> {
    Copied(InstructionIndex),
    Rewrite(R),
}

#[derive(Clone, Debug, PartialEq)]
pub enum InstructionTargetRewrite {
    Calibration(CalibrationExpansion),
    DefGateSequence(DefGateSequenceExpansion),
}

impl From<CalibrationExpansion> for InstructionTargetRewrite {
    fn from(value: CalibrationExpansion) -> Self {
        Self::Calibration(value)
    }
}

impl From<DefGateSequenceExpansion> for InstructionTargetRewrite {
    fn from(value: DefGateSequenceExpansion) -> Self {
        Self::DefGateSequence(value)
    }
}

pub type InstructionSourceMap =
    SourceMap<InstructionSource, InstructionTarget<InstructionTargetRewrite>>;

impl<R: Into<InstructionTargetRewrite>> From<SourceMap<InstructionIndex, InstructionTarget<R>>>
    for InstructionSourceMap
{
    fn from(value: SourceMap<InstructionIndex, InstructionTarget<R>>) -> Self {
        let entries = value
            .entries
            .into_iter()
            .map(|entry| SourceMapEntry {
                source_location: InstructionSource::from(entry.source_location),
                target_location: match entry.target_location {
                    InstructionTarget::Copied(index) => InstructionTarget::Copied(index),
                    InstructionTarget::Rewrite(rewrite) => {
                        InstructionTarget::Rewrite(rewrite.into())
                    }
                },
            })
            .collect();
        Self { entries }
    }
}

impl SourceMapIndexable<InstructionIndex> for InstructionTarget<InstructionTargetRewrite> {
    fn intersects(&self, other: &InstructionIndex) -> bool {
        match self {
            Self::Copied(index) => index == other,
            Self::Rewrite(InstructionTargetRewrite::Calibration(expansion)) => {
                expansion.intersects(other)
            }
            Self::Rewrite(InstructionTargetRewrite::DefGateSequence(expansion)) => {
                expansion.intersects(other)
            }
        }
    }
}

impl SourceMapIndexable<InstructionIndex> for InstructionSource {
    fn intersects(&self, other: &InstructionIndex) -> bool {
        self.contains(other)
    }
}

impl SourceMapIndexable<CalibrationSource> for InstructionTarget<InstructionTargetRewrite> {
    fn intersects(&self, other: &CalibrationSource) -> bool {
        if let Self::Rewrite(InstructionTargetRewrite::Calibration(expansion)) = self {
            expansion.intersects(other)
        } else {
            false
        }
    }
}

impl SourceMapIndexable<GateSignature> for InstructionTarget<InstructionTargetRewrite> {
    fn intersects(&self, other: &GateSignature) -> bool {
        if let Self::Rewrite(InstructionTargetRewrite::DefGateSequence(expansion)) = self {
            expansion.intersects(other)
        } else {
            false
        }
    }
}
