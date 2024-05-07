use std::ops::Range;

/// A SourceMap provides information necessary to understand which parts of a target
/// were derived from which parts of a source artifact, in such a way that they can be
/// mapped in either direction.
///
/// The behavior of such mappings depends on the implementations of the generic `Index` types,
/// but this may be a many-to-many mapping, where one element of the source is mapped (contributed)
/// to zero or many elements of the target, and vice versa.
///
/// This is also intended to be mergeable in a chain, such that the combined result of a series
/// of transformations can be expressed within a single source mapping.
#[derive(Clone, Debug, PartialEq)]
pub struct SourceMap<SourceIndex: SourceMapRange, TargetIndex: SourceMapRange> {
    pub(crate) entries: Vec<SourceMapEntry<SourceIndex, TargetIndex>>,
}

impl<SourceIndex: SourceMapRange, TargetIndex: SourceMapRange> Default
    for SourceMap<SourceIndex, TargetIndex>
{
    fn default() -> Self {
        Self {
            entries: Vec::new(),
        }
    }
}

impl<SourceIndex: SourceMapRange, TargetIndex: SourceMapRange> SourceMap<SourceIndex, TargetIndex> {
    pub fn entries(&self) -> &[SourceMapEntry<SourceIndex, TargetIndex>] {
        &self.entries
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SourceMapEntry<SourceIndex: SourceMapRange, TargetIndex: SourceMapRange> {
    /// The locator within the source artifact
    pub(crate) source_location: SourceIndex,

    /// The locator within the target artifact
    pub(crate) target_location: TargetIndex,
}

impl<SourceIndex: SourceMapRange, TargetIndex: SourceMapRange>
    SourceMapEntry<SourceIndex, TargetIndex>
{
    pub fn source_location(&self) -> &SourceIndex {
        &self.source_location
    }

    pub fn target_location(&self) -> &TargetIndex {
        &self.target_location
    }
}

/// A `SourceMapRange` is a section of the source or target artifact that can be mapped to.
///
/// This may be a single line, a range of lines, or a more complex data structure.
pub trait SourceMapRange {
    type Value: PartialOrd;

    /// Whether the given value is contained (even partially) within this range.
    ///
    /// This is used to determine overlap between source and target ranges.
    fn contains(&self, value: &Self::Value) -> bool;

    /// The starting value of this range.
    fn start(&self) -> &Self::Value;
}

impl SourceMapRange for usize {
    type Value = usize;

    fn contains(&self, value: &Self::Value) -> bool {
        self == value
    }

    fn start(&self) -> &Self::Value {
        self
    }
}

impl SourceMapRange for Range<usize> {
    type Value = usize;

    fn contains(&self, value: &Self::Value) -> bool {
        self.contains(value)
    }

    fn start(&self) -> &Self::Value {
        &self.start
    }
}
