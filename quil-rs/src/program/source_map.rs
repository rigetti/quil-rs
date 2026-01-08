use std::fmt::Debug;

use crate::instruction::GateSignature;

use super::{CalibrationSource, InstructionIndex};

/// A `SourceMap` provides information necessary to understand which parts of a target
/// were derived from which parts of a source artifact, in such a way that they can be
/// mapped in either direction.
///
/// The behavior of such mappings depends on the implementations of the generic `Index` types,
/// but this may be a many-to-many mapping, where one element of the source is mapped (contributes)
/// to zero or many elements of the target, and vice versa.
///
/// This is also intended to be mergeable in a chain, such that the combined result of a series
/// of transformations can be expressed within a single source mapping.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourceMap<SourceIndex, TargetIndex> {
    pub(crate) entries: Vec<SourceMapEntry<SourceIndex, TargetIndex>>,
}

impl<SourceIndex, TargetIndex> SourceMap<SourceIndex, TargetIndex> {
    pub fn new(entries: Vec<SourceMapEntry<SourceIndex, TargetIndex>>) -> Self {
        Self { entries }
    }

    /// Return all source ranges in the source map which were used to generate the target range.
    ///
    /// This is `O(n)` where `n` is the number of entries in the map.
    pub fn list_sources<QueryIndex>(&self, target_index: &QueryIndex) -> Vec<&SourceIndex>
    where
        TargetIndex: SourceMapIndexable<QueryIndex>,
    {
        self.entries
            .iter()
            .filter(|&entry| entry.target_location().contains(target_index))
            .map(SourceMapEntry::source_location)
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
            .filter(|entry| entry.source_location().contains(source_index))
            .map(SourceMapEntry::target_location)
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourceMapEntry<SourceIndex, TargetIndex> {
    /// The locator within the source artifact
    pub(crate) source_location: SourceIndex,

    /// The locator within the target artifact
    pub(crate) target_location: TargetIndex,
}

impl<SourceIndex, TargetIndex> SourceMapEntry<SourceIndex, TargetIndex> {
    pub fn new(source_location: SourceIndex, target_location: TargetIndex) -> Self {
        Self {
            source_location,
            target_location,
        }
    }

    pub fn source_location(&self) -> &SourceIndex {
        &self.source_location
    }

    pub fn target_location(&self) -> &TargetIndex {
        &self.target_location
    }
}

/// A trait for types which can be used as lookup indices in a `SourceMap.`
pub trait SourceMapIndexable<Index> {
    /// Return `true` if `self` contains or is equal to `other`.
    fn contains(&self, other: &Index) -> bool;
}

impl SourceMapIndexable<InstructionIndex> for InstructionIndex {
    fn contains(&self, other: &InstructionIndex) -> bool {
        self == other
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExpansionResult<R> {
    Unmodified(InstructionIndex),
    Rewritten(R),
}

impl<R> SourceMapIndexable<InstructionIndex> for ExpansionResult<R>
where
    R: SourceMapIndexable<InstructionIndex>,
{
    fn contains(&self, other: &InstructionIndex) -> bool {
        match self {
            Self::Unmodified(index) => index == other,
            Self::Rewritten(rewrite) => rewrite.contains(other),
        }
    }
}

impl<R> SourceMapIndexable<CalibrationSource> for ExpansionResult<R>
where
    R: SourceMapIndexable<CalibrationSource>,
{
    fn contains(&self, other: &CalibrationSource) -> bool {
        if let Self::Rewritten(rewrite) = self {
            rewrite.contains(other)
        } else {
            false
        }
    }
}

impl<'a, R> SourceMapIndexable<GateSignature<'a>> for ExpansionResult<R>
where
    R: SourceMapIndexable<GateSignature<'a>>,
{
    fn contains(&self, other: &GateSignature<'a>) -> bool {
        if let Self::Rewritten(rewrite) = self {
            rewrite.contains(other)
        } else {
            false
        }
    }
}
