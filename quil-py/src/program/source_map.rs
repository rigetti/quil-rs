use std::ops::Range;

use pyo3::{
    conversion, exceptions,
    types::{PyModule, PyTuple},
    IntoPy, Py, PyAny, PyResult, Python,
};
use quil_rs::program::{
    CalibrationExpansion, CalibrationSource, DefGateSequenceExpansion, InstructionIndex,
    InstructionSource, InstructionSourceMap, InstructionTarget, InstructionTargetRewrite,
    SourceMap, SourceMapEntry,
};
use rigetti_pyo3::{
    impl_as_mut_for_wrapper, impl_repr, py_wrap_type, py_wrap_union_enum, pyo3::pymethods,
    PyTryFrom, PyWrapper, ToPython,
};

use crate::{
    impl_eq,
    instruction::{PyCalibrationIdentifier, PyGateSignature, PyMeasureCalibrationIdentifier},
};

use super::PyProgram;

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
        InstructionSourceMap::from(self.as_inner().expansions().clone()).into()
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
        InstructionSourceMap::from(self.as_inner().nested_expansions().clone()).into()
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

    /// Given a source index, return information about its expansion.
    ///
    /// This is `O(n)` where `n` is the number of first-level calibration expansions performed.
    pub fn list_targets_for_source_index(
        &self,
        source_index: usize,
    ) -> Vec<PyInstructionTarget> {
        self.as_inner()
            .list_targets(&InstructionIndex(source_index))
            .into_iter()
            .map(|expansion| InstructionTargetShim::from(expansion.clone()).into())
            .collect()
    }
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

#[derive(Debug, PartialEq, Clone)]
enum InstructionTargetShim {
    Copied(usize),
    Rewrite(InstructionTargetRewrite)
}

impl From<InstructionTargetShim> for InstructionTarget<InstructionTargetRewrite> {
   fn from(value: InstructionTargetShim) -> Self {
        match value {
            InstructionTargetShim::Copied(index) => InstructionTarget::Copied(InstructionIndex(index)),
            InstructionTargetShim::Rewrite(rewrite) => InstructionTarget::Rewrite(rewrite)
        }
   } 
}

impl From<InstructionTarget<InstructionTargetRewrite>> for InstructionTargetShim {
    fn from(value: InstructionTarget<InstructionTargetRewrite>) -> Self {
        match value {
            InstructionTarget::Copied(index) => InstructionTargetShim::Copied(index.0),
            InstructionTarget::Rewrite(rewrite) => InstructionTargetShim::Rewrite(rewrite)
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