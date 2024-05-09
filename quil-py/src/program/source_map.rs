use pyo3::{types::PyInt, Py};
use quil_rs::program::{
    CalibrationExpansion, CalibrationSource, MaybeCalibrationExpansion,
    ProgramCalibrationExpansion, ProgramCalibrationExpansionSourceMap, SourceMap, SourceMapEntry,
};
use rigetti_pyo3::{impl_repr, py_wrap_type, py_wrap_union_enum, pyo3::pymethods, PyWrapper};

use crate::instruction::{PyCalibrationIdentifier, PyMeasureCalibrationIdentifier};

use super::PyProgram;

type CalibrationExpansionSourceMap = SourceMap<usize, CalibrationExpansion>;
type CalibrationExpansionSourceMapEntry = SourceMapEntry<usize, CalibrationExpansion>;
type ProgramCalibrationExpansionSourceMapEntry = SourceMapEntry<usize, MaybeCalibrationExpansion>;

py_wrap_type! {
    #[derive(Debug, PartialEq)]
    PyCalibrationExpansion(CalibrationExpansion) as "CalibrationExpansion"
}

impl_repr!(PyCalibrationExpansion);

#[pymethods]
impl PyCalibrationExpansion {
    pub fn calibration_used(&self) -> PyCalibrationSource {
        self.as_inner().calibration_used().into()
    }

    // Reviewer: is there a better return type?
    pub fn range(&self) -> (usize, usize) {
        (self.as_inner().range().start, self.as_inner().range().end)
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

#[pymethods]
impl PyCalibrationExpansionSourceMap {
    pub fn entries(&self) -> Vec<PyCalibrationExpansionSourceMapEntry> {
        self.as_inner()
            .entries()
            .iter()
            .map(|entry| entry.into())
            .collect()
    }
}

py_wrap_type! {
    #[derive(Debug, PartialEq)]
    PyCalibrationExpansionSourceMapEntry(CalibrationExpansionSourceMapEntry) as "CalibrationExpansionSourceMapEntry"
}

impl_repr!(PyCalibrationExpansionSourceMapEntry);

#[pymethods]
impl PyCalibrationExpansionSourceMapEntry {
    pub fn source_location(&self) -> usize {
        self.as_inner().source_location().clone().into()
    }

    pub fn target_location(&self) -> PyCalibrationExpansion {
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

py_wrap_union_enum! {
    #[derive(Debug, PartialEq)]
    PyMaybeCalibrationExpansion(MaybeCalibrationExpansion) as "MaybeCalibrationExpansion" {
        expanded: Expanded => PyCalibrationExpansion,
        unexpanded: Unexpanded => usize => Py<PyInt>
    }
}

impl_repr!(PyMaybeCalibrationExpansion);

py_wrap_type! {
    #[derive(Debug, PartialEq)]
    PyProgramCalibrationExpansion(ProgramCalibrationExpansion) as "ProgramCalibrationExpansion"
}

impl_repr!(PyProgramCalibrationExpansion);

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

#[pymethods]
impl PyProgramCalibrationExpansionSourceMap {
    pub fn entries(&self) -> Vec<PyProgramCalibrationExpansionSourceMapEntry> {
        self.as_inner()
            .entries()
            .iter()
            .map(|entry| entry.into())
            .collect()
    }
}

py_wrap_type! {
    #[derive(Debug, PartialEq)]
    PyProgramCalibrationExpansionSourceMapEntry(ProgramCalibrationExpansionSourceMapEntry) as "ProgramCalibrationExpansionSourceMapEntry"
}

impl_repr!(PyProgramCalibrationExpansionSourceMapEntry);

#[pymethods]
impl PyProgramCalibrationExpansionSourceMapEntry {
    pub fn source_location(&self) -> usize {
        self.as_inner().source_location().clone().into()
    }

    pub fn target_location(&self) -> PyMaybeCalibrationExpansion {
        self.as_inner().target_location().clone().into()
    }
}
