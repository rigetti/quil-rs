use std::collections::{BTreeMap, HashMap, HashSet};

use quil_rs::{instruction::Instruction, Program};

use rigetti_pyo3::{
    create_init_submodule, impl_as_mut_for_wrapper, impl_repr, py_wrap_type,
    pyo3::{
        create_exception, exceptions::PyValueError, prelude::*, pyclass::CompareOp, types::PyList,
    },
    PyWrapper, PyWrapperMut, ToPython,
};

use crate::instruction::{
    PyDeclaration, PyGateDefinition, PyInstruction, PyMemoryRegion, PyQubit, PyWaveform,
};

pub use self::{calibration_set::PyCalibrationSet, frame::PyFrameSet};

mod calibration_set;
mod frame;

// The quil-rs counterparts to these errors use generics which cause complications trying to map
// them directly to an error type here.
create_exception!(quil, ProgramError, PyValueError);
create_exception!(quil, ParseError, PyValueError);

py_wrap_type! {
    #[derive(Debug, PartialEq)]
    PyProgram(Program) as "Program"
}
impl_as_mut_for_wrapper!(PyProgram);
impl_repr!(PyProgram);

impl Default for PyProgram {
    fn default() -> Self {
        Self::new()
    }
}

#[pymethods]
impl PyProgram {
    #[new]
    pub fn new() -> Self {
        PyProgram(Program::default())
    }

    #[getter]
    pub fn instructions<'a>(&self, py: Python<'a>) -> PyResult<&'a PyList> {
        Ok(PyList::new(
            py,
            self.as_inner()
                .instructions
                .iter()
                .map(|i| i.to_python(py))
                .collect::<PyResult<Vec<PyInstruction>>>()?,
        ))
    }

    #[getter]
    pub fn calibrations(&self, py: Python<'_>) -> PyResult<PyCalibrationSet> {
        self.as_inner().calibrations.to_python(py)
    }

    #[getter]
    pub fn waveforms(&self, py: Python<'_>) -> PyResult<BTreeMap<String, PyWaveform>> {
        self.as_inner().waveforms.to_python(py)
    }

    #[getter]
    pub fn frames(&self, py: Python<'_>) -> PyResult<PyFrameSet> {
        self.as_inner().frames.to_python(py)
    }

    #[getter]
    pub fn memory_regions(&self, py: Python<'_>) -> PyResult<BTreeMap<String, PyMemoryRegion>> {
        self.as_inner()
            .memory_regions
            .iter()
            .map(|(name, memory_region)| Ok((name.to_python(py)?, memory_region.to_python(py)?)))
            .collect()
    }

    #[getter]
    // TODO: Should this filtering move to Program? Should we assume memory_regions will always make up all
    // declarations and simplify this?
    pub fn declarations(&self, py: Python<'_>) -> PyResult<HashMap<String, PyDeclaration>> {
        self.as_inner()
            .to_instructions(true)
            .iter()
            .filter_map(|inst| match inst {
                Instruction::Declaration(declaration) => Some(declaration),
                _ => None,
            })
            .map(|declaration| Ok((declaration.name.clone(), declaration.to_python(py)?)))
            .collect()
    }

    #[getter]
    pub fn defined_gates(&self, py: Python<'_>) -> PyResult<Vec<PyGateDefinition>> {
        self.as_inner()
            .to_instructions(true)
            .iter()
            .filter_map(|inst| match inst {
                Instruction::GateDefinition(gate_def) => Some(gate_def.to_python(py)),
                _ => None,
            })
            .collect()
    }

    fn dagger(&self) -> PyResult<Self> {
        self.as_inner()
            .dagger()
            .map_err(|e| ProgramError::new_err(e.to_string()))
            .map(PyProgram::from)
    }

    pub fn expand_calibrations(&self) -> PyResult<Self> {
        self.as_inner()
            .expand_calibrations()
            .map_err(|e| ParseError::new_err(e.to_string()))
            .map(PyProgram::from)
    }

    pub fn into_simplified(&self) -> PyResult<Self> {
        self.as_inner()
            .into_simplified()
            .map_err(|e| ParseError::new_err(e.to_string()))
            .map(PyProgram::from)
    }

    pub fn get_used_qubits(&self, py: Python<'_>) -> PyResult<HashSet<PyQubit>> {
        self.as_inner()
            .get_used_qubits()
            .iter()
            .map(|q| q.to_python(py))
            .collect()
    }

    pub fn add_instruction(&mut self, instruction: PyInstruction) {
        self.as_inner_mut().add_instruction(instruction.into())
    }

    pub fn to_instructions(
        &self,
        include_headers: bool,
        py: Python<'_>,
    ) -> PyResult<Vec<PyInstruction>> {
        self.as_inner()
            .to_instructions(include_headers)
            .iter()
            .map(|i| i.to_python(py))
            .collect()
    }

    pub fn to_headers(&self, py: Python<'_>) -> PyResult<Vec<PyInstruction>> {
        self.as_inner()
            .to_headers()
            .iter()
            .map(|h| h.to_python(py))
            .collect()
    }

    pub fn validate_protoquil(&self, _py: Python<'_>) -> PyResult<()> {
        self.as_inner()
            .is_protoquil()
            .map_err(|e| ProgramError::new_err(e.to_string()))
    }

    pub fn validate_quilt(&self, _py: Python<'_>) -> PyResult<()> {
        self.as_inner()
            .is_quilt()
            .map_err(|e| ProgramError::new_err(e.to_string()))
    }

    pub fn __str__(&self) -> String {
        self.as_inner().to_string(true)
    }

    pub fn __add__(&self, py: Python<'_>, rhs: Self) -> PyResult<Self> {
        let new = self.as_inner() + rhs.as_inner();
        new.to_python(py)
    }

    fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self.as_inner() == other.as_inner()).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}

create_init_submodule! {
    classes: [ PyFrameSet, PyProgram, PyCalibrationSet ],
}
