use std::ops::Range;

use pyo3::{
    types::{PyInt, PyModule, PyTuple},
    Py, PyAny, PyResult, Python,
};
use quil_rs::program::{
    CalibrationExpansion, CalibrationSource, MaybeCalibrationExpansion,
    ProgramCalibrationExpansion, ProgramCalibrationExpansionSourceMap, SourceMap, SourceMapEntry,
};
use rigetti_pyo3::{impl_repr, py_wrap_type, py_wrap_union_enum, pyo3::pymethods, PyWrapper};

use crate::{
    impl_eq,
    instruction::{PyCalibrationIdentifier, PyMeasureCalibrationIdentifier},
};

use super::PyProgram;

type CalibrationExpansionSourceMap = SourceMap<usize, CalibrationExpansion>;
type CalibrationExpansionSourceMapEntry = SourceMapEntry<usize, CalibrationExpansion>;
type ProgramCalibrationExpansionSourceMapEntry = SourceMapEntry<usize, MaybeCalibrationExpansion>;

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
        let tuple = PyTuple::new(py, [start, end]);
        range.call1(tuple)?.extract()
    }

    pub fn expansions(&self) -> PyCalibrationExpansionSourceMap {
        self.as_inner().expansions().into()
    }
}

py_wrap_type! {
    #[derive(Debug, PartialEq)]
    PyCalibrationExpansionSourceMap(CalibrationExpansionSourceMap) as "CalibrationExpansionSourceMap"
}

impl_repr!(PyCalibrationExpansionSourceMap);
impl_eq!(PyCalibrationExpansionSourceMap);

#[pymethods]
impl PyCalibrationExpansionSourceMap {
    pub fn entries(&self) -> Vec<PyCalibrationExpansionSourceMapEntry> {
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
    pub fn list_sources_for_target_index(&self, target_index: usize) -> Vec<usize> {
        self.as_inner()
            .list_sources(&target_index)
            .into_iter()
            .copied()
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
            .copied()
            .collect()
    }

    /// Given a source index, return information about its expansion.
    ///
    /// This is `O(n)` where `n` is the number of source instructions.
    pub fn list_targets_for_source_index(
        &self,
        source_index: usize,
    ) -> Vec<PyCalibrationExpansion> {
        self.as_inner()
            .list_targets(&source_index)
            .into_iter()
            .map(|expansion| expansion.into())
            .collect()
    }
}

py_wrap_type! {
    #[derive(Debug, PartialEq)]
    PyCalibrationExpansionSourceMapEntry(CalibrationExpansionSourceMapEntry) as "CalibrationExpansionSourceMapEntry"
}

impl_repr!(PyCalibrationExpansionSourceMapEntry);
impl_eq!(PyCalibrationExpansionSourceMapEntry);

#[pymethods]
impl PyCalibrationExpansionSourceMapEntry {
    pub fn source_location(&self) -> usize {
        *self.as_inner().source_location()
    }

    pub fn target_location(&self) -> PyCalibrationExpansion {
        self.as_inner().target_location().into()
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

py_wrap_union_enum! {
    #[derive(Debug, PartialEq)]
    PyMaybeCalibrationExpansion(MaybeCalibrationExpansion) as "MaybeCalibrationExpansion" {
        expanded: Expanded => PyCalibrationExpansion,
        unexpanded: Unexpanded => usize => Py<PyInt>
    }
}

impl_repr!(PyMaybeCalibrationExpansion);
impl_eq!(PyMaybeCalibrationExpansion);

py_wrap_type! {
    #[derive(Debug, PartialEq)]
    PyProgramCalibrationExpansion(ProgramCalibrationExpansion) as "ProgramCalibrationExpansion"
}

impl_repr!(PyProgramCalibrationExpansion);
impl_eq!(PyProgramCalibrationExpansion);

#[pymethods]
impl PyProgramCalibrationExpansion {
    pub fn program(&self) -> PyProgram {
        self.as_inner().program().into()
    }

    pub fn source_map(&self) -> PyProgramCalibrationExpansionSourceMap {
        self.as_inner().source_map().clone().into()
    }
}

py_wrap_type! {
    #[derive(Debug, PartialEq)]
    PyProgramCalibrationExpansionSourceMap(ProgramCalibrationExpansionSourceMap) as "ProgramCalibrationExpansionSourceMap"
}

impl_repr!(PyProgramCalibrationExpansionSourceMap);
impl_eq!(PyProgramCalibrationExpansionSourceMap);

#[pymethods]
impl PyProgramCalibrationExpansionSourceMap {
    pub fn entries(&self) -> Vec<PyProgramCalibrationExpansionSourceMapEntry> {
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
    pub fn list_sources_for_target_index(&self, target_index: usize) -> Vec<usize> {
        self.as_inner()
            .list_sources(&target_index)
            .into_iter()
            .copied()
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
            .copied()
            .collect()
    }

    /// Given a source index, return information about its expansion.
    ///
    /// This is `O(n)` where `n` is the number of source instructions.
    pub fn list_targets_for_source_index(
        &self,
        source_index: usize,
    ) -> Vec<PyMaybeCalibrationExpansion> {
        self.as_inner()
            .list_targets(&source_index)
            .into_iter()
            .map(|expansion| expansion.into())
            .collect()
    }
}

py_wrap_type! {
    #[derive(Debug, PartialEq)]
    PyProgramCalibrationExpansionSourceMapEntry(ProgramCalibrationExpansionSourceMapEntry) as "ProgramCalibrationExpansionSourceMapEntry"
}

impl_repr!(PyProgramCalibrationExpansionSourceMapEntry);
impl_eq!(PyProgramCalibrationExpansionSourceMapEntry);

#[pymethods]
impl PyProgramCalibrationExpansionSourceMapEntry {
    pub fn source_location(&self) -> usize {
        *self.as_inner().source_location()
    }

    pub fn target_location(&self) -> PyMaybeCalibrationExpansion {
        self.as_inner().target_location().clone().into()
    }
}
