use std::ops::Range;

use pyo3::{
    types::{PyModule, PyTuple},
    PyAny, PyResult, Python,
};
use quil_rs::{
    instruction::GateSignature,
    program::{
        CalibrationExpansion, CalibrationSource, DefGateSequenceExpansion, InstructionIndex,
        InstructionTarget, SourceMap, SourceMapEntry, SourceMapIndexable,
    },
};
use rigetti_pyo3::{impl_repr, py_wrap_type, py_wrap_union_enum, pyo3::pymethods, PyWrapper};

use crate::{
    impl_eq,
    instruction::{PyCalibrationIdentifier, PyGateSignature, PyMeasureCalibrationIdentifier},
};

/// A concrete type for [`InstructionTarget::Rewrite`] variant. This is used to present
/// a single type to Python that can represent calibration expansions, gate sequence expansions,
/// and other rewrites we may want to perform in the future.
///
/// Note, should we want to support rewrites from Python, we can expose an additional
/// pyo3 type that wraps classes implementing the required Python interface. See the pyo3
/// [trait bound](https://pyo3.rs/main/trait-bounds.html) documentation.
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

impl SourceMapIndexable<InstructionIndex> for InstructionTargetRewrite {
    fn intersects(&self, other: &InstructionIndex) -> bool {
        match self {
            Self::Calibration(expansion) => expansion.intersects(other),
            Self::DefGateSequence(expansion) => expansion.intersects(other),
        }
    }
}

impl SourceMapIndexable<CalibrationSource> for InstructionTargetRewrite {
    fn intersects(&self, other: &CalibrationSource) -> bool {
        if let Self::Calibration(expansion) = self {
            expansion.intersects(other)
        } else {
            false
        }
    }
}

impl SourceMapIndexable<GateSignature> for InstructionTargetRewrite {
    fn intersects(&self, other: &GateSignature) -> bool {
        if let Self::DefGateSequence(expansion) = self {
            expansion.intersects(other)
        } else {
            false
        }
    }
}

/// In its most general form, a source within a source map can correspond to
/// a range of instructions, rather than just a single instruction. We use
/// this type as the [`SourceMap`] `SourceIndex` for forward compatibility.
#[derive(Clone, Debug, PartialEq)]
pub struct InstructionSource(std::ops::Range<InstructionIndex>);

impl InstructionSource {
    fn start(&self) -> InstructionIndex {
        self.0.start
    }

    fn end(&self) -> InstructionIndex {
        self.0.end
    }

    fn contains(&self, index: &InstructionIndex) -> bool {
        self.0.contains(index)
    }
}

impl From<&InstructionIndex> for InstructionSource {
    fn from(value: &InstructionIndex) -> Self {
        Self(*value..InstructionIndex(value.0 + 1))
    }
}

impl SourceMapIndexable<InstructionIndex> for InstructionSource {
    fn intersects(&self, other: &InstructionIndex) -> bool {
        self.contains(other)
    }
}

/// A single concrete type for [`SourceMap`] using instruction indices
/// that we will expose to Python.
type InstructionSourceMap =
    SourceMap<InstructionSource, InstructionTarget<InstructionTargetRewrite>>;

py_wrap_type! {
    #[derive(Debug, PartialEq)]
    PyCalibrationExpansion(CalibrationExpansion) as "CalibrationExpansion"
}

impl_repr!(PyCalibrationExpansion);
impl_eq!(PyCalibrationExpansion);

#[pymethods]
impl PyCalibrationExpansion {
    pub fn calibration_used(&self) -> PyCalibrationSource {
        self.as_inner().calibration_used().into()
    }

    pub fn range<'py>(&self, py: Python<'py>) -> PyResult<&'py PyAny> {
        let range = PyModule::import(py, "builtins")?.getattr("range")?;
        let Range { start, end } = self.as_inner().range();
        let tuple = PyTuple::new(py, [start.0, end.0]);
        range.call1(tuple)?.extract()
    }

    pub fn expansions(&self) -> PyInstructionSourceMap {
        let source_map = self.as_inner().expansions().clone();
        to_instruction_source_map(source_map).into()
    }
}

py_wrap_type! {
    #[derive(Debug, PartialEq)]
    PyDefGateSequenceExpansion(DefGateSequenceExpansion) as "DefGateSequenceExpansion"
}

impl_repr!(PyDefGateSequenceExpansion);
impl_eq!(PyDefGateSequenceExpansion);

#[pymethods]
impl PyDefGateSequenceExpansion {
    pub fn calibration_used(&self) -> PyGateSignature {
        self.as_inner().defgate_sequence_source().into()
    }

    pub fn range<'py>(&self, py: Python<'py>) -> PyResult<&'py PyAny> {
        let range = PyModule::import(py, "builtins")?.getattr("range")?;
        let Range { start, end } = self.as_inner().range();
        let tuple = PyTuple::new(py, [start.0, end.0]);
        range.call1(tuple)?.extract()
    }

    pub fn expansions(&self) -> PyInstructionSourceMap {
        to_instruction_source_map(self.as_inner().nested_expansions().clone()).into()
    }
}

py_wrap_union_enum! {
    #[derive(Debug, PartialEq)]
    PyInstructionTargetRewrite(InstructionTargetRewrite) as "InstructionTargetRewrite" {
        calibration: Calibration => PyCalibrationExpansion,
        defgate_sequence: DefGateSequence => PyDefGateSequenceExpansion
    }
}

py_wrap_type! {
    #[derive(Debug, PartialEq)]
    PyInstructionSourceMap(InstructionSourceMap) as "InstructionSourceMap"
}

impl_repr!(PyInstructionSourceMap);
impl_eq!(PyInstructionSourceMap);

#[pymethods]
impl PyInstructionSourceMap {
    pub fn entries(&self) -> Vec<PyInstructionSourceMapEntry> {
        self.as_inner()
            .entries()
            .iter()
            .map(|entry| entry.into())
            .collect()
    }

    /// Given an instruction index within the resulting expansion, return the locations in the source
    /// which were expanded to generate that instruction.
    ///
    /// This is `O(n)` where `n` is the number of first-level calibration expansions performed.
    pub fn list_sources_for_target_index(&self, target_index: usize) -> Vec<(usize, usize)> {
        self.as_inner()
            .list_sources(&InstructionIndex(target_index))
            .into_iter()
            .map(|index| (index.start().0, index.end().0))
            .collect()
    }

    /// Given a particular calibration (`DEFCAL` or `DEFCAL MEASURE`), return the locations in the source
    /// program which were expanded using that calibration.
    ///
    /// This is `O(n)` where `n` is the number of first-level calibration expansions performed.
    pub fn list_sources_for_calibration_used(
        &self,
        calibration_used: PyCalibrationSource,
    ) -> Vec<usize> {
        self.as_inner()
            .list_sources(calibration_used.as_inner())
            .into_iter()
            .map(|index| index.start().0)
            .collect()
    }

    /// Given a gate signature, return the locations in the source program which were
    /// expanded using that gate signature.
    ///
    /// This is `O(n)` where `n` is the number of first-level sequence gate definition
    /// expansions performed.
    pub fn list_sources_for_gate_expansion(&self, gate_signature: PyGateSignature) -> Vec<usize> {
        self.as_inner()
            .list_sources(gate_signature.as_inner())
            .into_iter()
            .map(|index| index.start().0)
            .collect()
    }

    /// Given a source index, return information about its expansion.
    ///
    /// This is `O(n)` where `n` is the number of first-level calibration expansions performed.
    pub fn list_targets_for_source_index(&self, source_index: usize) -> Vec<PyInstructionTarget> {
        self.as_inner()
            .list_targets(&InstructionIndex(source_index))
            .into_iter()
            .map(|expansion| InstructionTargetShim::from(expansion.clone()).into())
            .collect()
    }
}

impl<R: Into<InstructionTargetRewrite> + Clone>
    From<SourceMap<InstructionIndex, InstructionTarget<R>>> for PyInstructionSourceMap
{
    fn from(value: SourceMap<InstructionIndex, InstructionTarget<R>>) -> Self {
        to_instruction_source_map(value).into()
    }
}

fn to_instruction_source_map<R: Into<InstructionTargetRewrite> + Clone>(
    value: SourceMap<InstructionIndex, InstructionTarget<R>>,
) -> InstructionSourceMap {
    let entries = value
        .entries()
        .iter()
        .cloned()
        .map(|entry| {
            SourceMapEntry::new(
                InstructionSource::from(entry.source_location()),
                match entry.target_location().clone() {
                    InstructionTarget::Copied(index) => InstructionTarget::Copied(index),
                    InstructionTarget::Rewrite(rewrite) => {
                        InstructionTarget::Rewrite(rewrite.into())
                    }
                },
            )
        })
        .collect();
    InstructionSourceMap::new(entries)
}

type InstructionSourceMapEntry =
    SourceMapEntry<InstructionSource, InstructionTarget<InstructionTargetRewrite>>;

py_wrap_type! {
    #[derive(Debug, PartialEq)]
    PyInstructionSourceMapEntry(InstructionSourceMapEntry) as "InstructionSourceMapEntry"
}

impl_repr!(PyInstructionSourceMapEntry);
impl_eq!(PyInstructionSourceMapEntry);

#[pymethods]
impl PyInstructionSourceMapEntry {
    pub fn source_location(&self) -> (usize, usize) {
        let source_location = self.as_inner().source_location();
        (source_location.start().0, source_location.end().0)
    }

    pub fn target_location(&self) -> PyInstructionTarget {
        InstructionTargetShim::from(self.as_inner().target_location().clone()).into()
    }
}

py_wrap_union_enum! {
    #[derive(Debug, PartialEq)]
    PyCalibrationSource(CalibrationSource) as "CalibrationSource" {
        calibration: Calibration => PyCalibrationIdentifier,
        measure_calibration: MeasureCalibration => PyMeasureCalibrationIdentifier
    }
}

impl_repr!(PyCalibrationSource);
impl_eq!(PyCalibrationSource);

/// Because we expose [`usize`] to Python, rather than [`InstructionIndex`], to Python,
/// we need a shim to use the [`rigetti_pyo3::py_wrap_union_enum!`] macro.
#[derive(Debug, PartialEq, Clone)]
pub enum InstructionTargetShim {
    Copied(usize),
    Rewrite(InstructionTargetRewrite),
}

impl From<InstructionTargetShim> for InstructionTarget<InstructionTargetRewrite> {
    fn from(value: InstructionTargetShim) -> Self {
        match value {
            InstructionTargetShim::Copied(index) => {
                InstructionTarget::Copied(InstructionIndex(index))
            }
            InstructionTargetShim::Rewrite(rewrite) => InstructionTarget::Rewrite(rewrite),
        }
    }
}

impl From<InstructionTarget<InstructionTargetRewrite>> for InstructionTargetShim {
    fn from(value: InstructionTarget<InstructionTargetRewrite>) -> Self {
        match value {
            InstructionTarget::Copied(index) => InstructionTargetShim::Copied(index.0),
            InstructionTarget::Rewrite(rewrite) => InstructionTargetShim::Rewrite(rewrite),
        }
    }
}

py_wrap_union_enum! {
    #[derive(Debug, PartialEq)]
    PyInstructionTarget(InstructionTargetShim) as "InstructionTarget" {
        copied: Copied => usize,
        rewrite: Rewrite => PyInstructionTargetRewrite
    }
}

impl_repr!(PyInstructionTarget);
impl_eq!(PyInstructionTarget);
