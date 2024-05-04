use std::ops::Range;

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
    pub(crate) source_location: SourceIndex,
    pub(crate) target_location: TargetIndex,
}

pub trait SourceMapRange {
    type Value: PartialOrd;

    fn contains(&self, value: &Self::Value) -> bool;

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
