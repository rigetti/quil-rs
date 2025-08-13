use std::ops::Range;

use pyo3::{
    types::{PyModule, PyTuple},
    PyAny, PyResult, Python,
};
use quil_rs::{
    instruction::GateSignature,
    program::{
        CalibrationExpansion, CalibrationSource, DefGateSequenceExpansion, ExpansionResult,
        InstructionIndex, SourceMap, SourceMapEntry, SourceMapIndexable,
    },
};
use rigetti_pyo3::{impl_repr, py_wrap_type, py_wrap_union_enum, pyo3::pymethods, PyWrapper};

use crate::{
    impl_eq,
    instruction::{PyCalibrationIdentifier, PyGateSignature, PyMeasureCalibrationIdentifier},
};

py_wrap_type! {
    #[derive(Debug, PartialEq)]
    PyInstructionIndex(InstructionIndex) as "InstructionIndex"
}

impl_repr!(PyInstructionIndex);
impl_eq!(PyInstructionIndex);

#[pymethods]
impl PyInstructionIndex {
    #[new]
    pub fn new(index: usize) -> Self {
        Self(InstructionIndex(index))
    }

    #[getter]
    pub fn index(&self) -> usize {
        self.0 .0
    }
}

/// A single type for the `TargetIndex` on a [`SourceMap`] that we expose to Python. This
/// can represent calibration expansions, gate sequence expansions, and other rewrites
/// we may want to perform in the future.
///
/// Note, should we want to support rewrites from Python, we can expose an additional
/// pyo3 type that wraps classes implementing the required Python interface. See the pyo3
/// [trait bound](https://pyo3.rs/main/trait-bounds.html) documentation.
//
// Additionally note, we expose [`usize`] to Python, rather than [`InstructionIndex`],
// to Python, facilitating usage of the [`rigetti_pyo3::py_wrap_union_enum!`] macro.
#[derive(Clone, Debug, PartialEq)]
pub enum ExpansionResultShim {
    Unmodified(InstructionIndex),
    Calibration(CalibrationExpansion),
    DefGateSequence(DefGateSequenceExpansion),
}

impl From<CalibrationExpansion> for ExpansionResultShim {
    fn from(value: CalibrationExpansion) -> Self {
        Self::Calibration(value)
    }
}

impl From<DefGateSequenceExpansion> for ExpansionResultShim {
    fn from(value: DefGateSequenceExpansion) -> Self {
        Self::DefGateSequence(value)
    }
}

impl<R: Into<ExpansionResultShim> + Clone> From<ExpansionResult<R>> for ExpansionResultShim {
    fn from(value: ExpansionResult<R>) -> Self {
        match value {
            ExpansionResult::Unmodified(index) => Self::Unmodified(index),
            ExpansionResult::Rewritten(rewrite) => rewrite.into(),
        }
    }
}

py_wrap_union_enum! {
    #[derive(Debug, PartialEq)]
    PyInstructionTarget(ExpansionResultShim) as "InstructionTarget" {
        unmodified: Unmodified => PyInstructionIndex,
        calibration: Calibration => PyCalibrationExpansion,
        defgate_sequence: DefGateSequence => PyDefGateSequenceExpansion
    }
}

impl_repr!(PyInstructionTarget);
impl_eq!(PyInstructionTarget);

impl SourceMapIndexable<InstructionIndex> for ExpansionResultShim {
    fn contains(&self, other: &InstructionIndex) -> bool {
        match self {
            Self::Unmodified(index) => index == other,
            Self::Calibration(expansion) => expansion.contains(other),
            Self::DefGateSequence(expansion) => expansion.contains(other),
        }
    }
}

impl SourceMapIndexable<CalibrationSource> for ExpansionResultShim {
    fn contains(&self, other: &CalibrationSource) -> bool {
        if let Self::Calibration(expansion) = self {
            expansion.contains(other)
        } else {
            false
        }
    }
}

impl SourceMapIndexable<GateSignature> for ExpansionResultShim {
    fn contains(&self, other: &GateSignature) -> bool {
        if let Self::DefGateSequence(expansion) = self {
            expansion.contains(other)
        } else {
            false
        }
    }
}

/// A single concrete type for [`SourceMap`] using instruction indices
/// that we will expose to Python.
type InstructionSourceMap = SourceMap<InstructionIndex, ExpansionResultShim>;

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

/// Because [`SourceMap`] is defined outside of this crate, we define this
/// utility function rather than implementing `From<SourceMap<...>>`.
fn to_instruction_source_map<R: Into<ExpansionResultShim> + Clone>(
    value: SourceMap<InstructionIndex, ExpansionResult<R>>,
) -> InstructionSourceMap {
    let entries = value
        .entries()
        .iter()
        .cloned()
        .map(|entry| {
            SourceMapEntry::new(
                *entry.source_location(),
                ExpansionResultShim::from(entry.target_location().clone()),
            )
        })
        .collect();
    InstructionSourceMap::new(entries)
}

py_wrap_type! {
    #[derive(Debug, PartialEq)]
    PyDefGateSequenceExpansion(DefGateSequenceExpansion) as "DefGateSequenceExpansion"
}

impl_repr!(PyDefGateSequenceExpansion);
impl_eq!(PyDefGateSequenceExpansion);

#[pymethods]
impl PyDefGateSequenceExpansion {
    pub fn defgate_sequence_source(&self) -> PyGateSignature {
        self.as_inner().source_signature().into()
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
            .map(|entry| entry.clone().into())
            .collect()
    }

    /// Given an instruction index within the resulting expansion, return the locations in the source
    /// which were expanded to generate that instruction.
    ///
    /// This is `O(n)` where `n` is the number of first-level expansions performed,
    /// which is at worst `O(i)` where `i` is the number of source instructions.
    pub fn list_sources_for_target_index(
        &self,
        target_index: PyInstructionIndex,
    ) -> Vec<PyInstructionIndex> {
        self.as_inner()
            .list_sources(target_index.as_inner())
            .into_iter()
            .map(PyInstructionIndex::from)
            .collect()
    }

    /// Given a particular calibration (`DEFCAL` or `DEFCAL MEASURE`), return the locations in the source
    /// program which were expanded using that calibration.
    ///
    /// This is `O(n)` where `n` is the number of first-level calibration expansions performed,
    /// which is at worst `O(i)` where `i` is the number of source instructions.
    pub fn list_sources_for_calibration_used(
        &self,
        calibration_used: PyCalibrationSource,
    ) -> Vec<PyInstructionIndex> {
        self.as_inner()
            .list_sources(calibration_used.as_inner())
            .into_iter()
            .map(PyInstructionIndex::from)
            .collect()
    }

    /// Given a gate signature, return the locations in the source program which were
    /// expanded using that gate signature.
    ///
    /// This is `O(n)` where `n` is the number of first-level sequence gate expansions performed,
    /// which is at worst `O(i)` where `i` is the number of source instructions.
    pub fn list_sources_for_gate_expansion(
        &self,
        gate_signature: PyGateSignature,
    ) -> Vec<PyInstructionIndex> {
        self.as_inner()
            .list_sources(gate_signature.as_inner())
            .into_iter()
            .map(PyInstructionIndex::from)
            .collect()
    }

    /// Given a source index, return information about its expansion.
    ///
    /// This is `O(n)` where `n` is the number of first-level expansions performed,
    /// which is at worst `O(i)` where `i` is the number of source instructions.
    pub fn list_targets_for_source_index(
        &self,
        source_index: PyInstructionIndex,
    ) -> Vec<PyInstructionTarget> {
        let inner = self.as_inner();

        inner
            .list_targets(&InstructionIndex::from(source_index))
            .into_iter()
            .map(PyInstructionTarget::from)
            .collect()
    }
}

impl<R: Into<ExpansionResultShim> + Clone> From<SourceMap<InstructionIndex, ExpansionResult<R>>>
    for PyInstructionSourceMap
{
    fn from(value: SourceMap<InstructionIndex, ExpansionResult<R>>) -> Self {
        to_instruction_source_map(value).into()
    }
}

type InstructionSourceMapEntry = SourceMapEntry<InstructionIndex, ExpansionResultShim>;

py_wrap_type! {
    #[derive(Debug, PartialEq)]
    PyInstructionSourceMapEntry(InstructionSourceMapEntry) as "InstructionSourceMapEntry"
}

impl_repr!(PyInstructionSourceMapEntry);
impl_eq!(PyInstructionSourceMapEntry);

#[pymethods]
impl PyInstructionSourceMapEntry {
    pub fn source_location(&self) -> PyInstructionIndex {
        PyInstructionIndex::from(self.as_inner().source_location())
    }

    pub fn target_location(&self) -> PyInstructionTarget {
        self.as_inner().target_location().clone().into()
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
