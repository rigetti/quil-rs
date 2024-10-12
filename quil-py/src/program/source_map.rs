use std::ops::Range;

use pyo3::{
    conversion, exceptions,
    types::{PyModule, PyTuple},
    IntoPy, Py, PyAny, PyResult, Python,
};
use quil_rs::program::{
    CalibrationExpansion, CalibrationSource, InstructionIndex, MaybeCalibrationExpansion,
    ProgramCalibrationExpansion, ProgramCalibrationExpansionSourceMap, SourceMap, SourceMapEntry,
};
use rigetti_pyo3::{
    impl_as_mut_for_wrapper, impl_repr, py_wrap_type, py_wrap_union_enum, pyo3::pymethods,
    PyTryFrom, PyWrapper, ToPython,
};

use crate::{
    impl_eq,
    instruction::{PyCalibrationIdentifier, PyMeasureCalibrationIdentifier},
};

use super::PyProgram;

type CalibrationExpansionSourceMap = SourceMap<InstructionIndex, CalibrationExpansion>;
type CalibrationExpansionSourceMapEntry = SourceMapEntry<InstructionIndex, CalibrationExpansion>;
type ProgramCalibrationExpansionSourceMapEntry =
    SourceMapEntry<InstructionIndex, MaybeCalibrationExpansion>;

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
            .list_sources(&InstructionIndex(target_index))
            .into_iter()
            .map(|index| index.0)
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
            .map(|index| index.0)
            .collect()
    }

    /// Given a source index, return information about its expansion.
    ///
    /// This is `O(n)` where `n` is the number of first-level calibration expansions performed.
    pub fn list_targets_for_source_index(
        &self,
        source_index: usize,
    ) -> Vec<PyCalibrationExpansion> {
        self.as_inner()
            .list_targets(&InstructionIndex(source_index))
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
        self.as_inner().source_location().0
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

// Note: this type is manually implemented below because there is no `Into` conversion from `InstructionIndex` to `usize`
// This manual implementation follows the same API as this invocation otherwise would:
// ```
// py_wrap_union_enum! {
//     #[derive(Debug, PartialEq)]
//     PyMaybeCalibrationExpansion(MaybeCalibrationExpansion) as "MaybeCalibrationExpansion" {
//         expanded: Expanded => PyCalibrationExpansion,
//         unexpanded: Unexpanded => InstructionIndex => usize
//     }
// }
// ```
py_wrap_type! {
    #[derive(Debug, PartialEq)]
    PyMaybeCalibrationExpansion(MaybeCalibrationExpansion) as "MaybeCalibrationExpansion"
}

impl_as_mut_for_wrapper!(PyMaybeCalibrationExpansion);

#[pymethods]
impl PyMaybeCalibrationExpansion {
    #[new]
    pub fn new(py: Python, input: &PyAny) -> PyResult<Self> {
        if let Ok(inner) = <_ as PyTryFrom<PyAny>>::py_try_from(py, input) {
            let inner = &inner;
            if let Ok(item) = PyTryFrom::py_try_from(py, inner) {
                return Ok(Self::from(MaybeCalibrationExpansion::Expanded(item)));
            }
        }

        if let Ok(inner) = <_ as PyTryFrom<PyAny>>::py_try_from(py, input) {
            if let Ok(item) = PyTryFrom::<usize>::py_try_from(py, &inner) {
                return Ok(Self::from(MaybeCalibrationExpansion::Unexpanded(
                    InstructionIndex(item),
                )));
            }
        }

        Err(exceptions::PyValueError::new_err(format!(
            "could not create {} from {}",
            stringify!($name),
            input.repr()?
        )))
    }

    #[allow(unreachable_code, unreachable_patterns)]
    pub fn inner(&self, py: Python) -> PyResult<Py<PyAny>> {
        match &self.0 {
            MaybeCalibrationExpansion::Expanded(inner) => Ok(
                conversion::IntoPy::<Py<PyAny>>::into_py(ToPython::to_python(&inner, py)?, py),
            ),
            MaybeCalibrationExpansion::Unexpanded(inner) => Ok(inner.0.into_py(py)),
            _ => {
                use exceptions::PyRuntimeError;
                Err(PyRuntimeError::new_err(
                    "Enum variant has no inner data or is unimplemented",
                ))
            }
        }
    }

    pub fn as_expanded(&self) -> Option<PyCalibrationExpansion> {
        match &self.0 {
            MaybeCalibrationExpansion::Expanded(inner) => {
                Some(PyCalibrationExpansion(inner.clone()))
            }
            _ => None,
        }
    }

    pub fn as_unexpanded(&self) -> Option<usize> {
        match &self.0 {
            MaybeCalibrationExpansion::Unexpanded(inner) => Some(inner.0),
            _ => None,
        }
    }

    #[staticmethod]
    pub fn from_expanded(inner: PyCalibrationExpansion) -> Self {
        Self(MaybeCalibrationExpansion::Expanded(inner.into_inner()))
    }

    #[staticmethod]
    pub fn from_unexpanded(inner: usize) -> Self {
        Self(MaybeCalibrationExpansion::Unexpanded(InstructionIndex(
            inner,
        )))
    }

    pub fn is_expanded(&self) -> bool {
        matches!(self.0, MaybeCalibrationExpansion::Expanded(_))
    }

    pub fn is_unexpanded(&self) -> bool {
        matches!(self.0, MaybeCalibrationExpansion::Unexpanded(_))
    }

    pub fn to_expanded(&self) -> PyResult<PyCalibrationExpansion> {
        match &self.0 {
            MaybeCalibrationExpansion::Expanded(inner) => Ok(PyCalibrationExpansion(inner.clone())),
            _ => Err(pyo3::exceptions::PyValueError::new_err(
                "expected self to be an Expanded variant",
            )),
        }
    }

    pub fn to_unexpanded(&self) -> PyResult<usize> {
        match &self.0 {
            MaybeCalibrationExpansion::Unexpanded(inner) => Ok(inner.0),
            _ => Err(pyo3::exceptions::PyValueError::new_err(
                "expected self to be an Unexpanded variant",
            )),
        }
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
    /// This is `O(n)` where `n` is the number of source instructions.
    pub fn list_sources_for_target_index(&self, target_index: usize) -> Vec<usize> {
        self.as_inner()
            .list_sources(&InstructionIndex(target_index))
            .into_iter()
            .map(|index| index.0)
            .collect()
    }

    /// Given a particular calibration (`DEFCAL` or `DEFCAL MEASURE`), return the locations in the source
    /// program which were expanded using that calibration.
    ///
    /// This is `O(n)` where `n` is the number of source instructions.
    pub fn list_sources_for_calibration_used(
        &self,
        calibration_used: PyCalibrationSource,
    ) -> Vec<usize> {
        self.as_inner()
            .list_sources(calibration_used.as_inner())
            .into_iter()
            .map(|index| index.0)
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
            .list_targets(&InstructionIndex(source_index))
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
        self.as_inner().source_location().0
    }

    pub fn target_location(&self) -> PyMaybeCalibrationExpansion {
        self.as_inner().target_location().clone().into()
    }
}
