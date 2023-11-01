use std::{
    collections::{BTreeMap, HashMap, HashSet},
    str::FromStr,
};

use numpy::{PyArray2, ToPyArray};
use quil_rs::{
    instruction::{Instruction, QubitPlaceholder, TargetPlaceholder, Waveform},
    program::{CalibrationSet, FrameSet, MemoryRegion},
    Program,
};
use rigetti_pyo3::{
    create_init_submodule, impl_as_mut_for_wrapper, impl_from_str, impl_parse, impl_repr,
    num_complex::Complex64,
    py_wrap_error, py_wrap_type,
    pyo3::{
        exceptions::PyValueError,
        prelude::*,
        types::{PyBytes, PyFunction, PyList},
        IntoPy,
    },
    wrap_error, PyTryFrom, PyWrapper, PyWrapperMut, ToPython, ToPythonError,
};

use crate::{
    impl_eq, impl_to_quil,
    instruction::{PyDeclaration, PyGateDefinition, PyInstruction, PyQubit, PyWaveform},
};

pub use self::{calibration::PyCalibrationSet, frame::PyFrameSet, memory::PyMemoryRegion};

mod calibration;
mod frame;
mod memory;

wrap_error!(ProgramError(quil_rs::program::ProgramError));
py_wrap_error!(quil, ProgramError, PyProgramError, PyValueError);

py_wrap_type! {
    #[derive(Debug, PartialEq)]
    // If unset, the module defaults to builtin, which can't be pickled
    #[pyo3(module = "quil.program")]
    PyProgram(Program) as "Program"
}
impl_as_mut_for_wrapper!(PyProgram);
impl_repr!(PyProgram);
impl_from_str!(PyProgram, ProgramError);
impl_parse!(PyProgram);
impl_to_quil!(PyProgram);
impl_eq!(PyProgram);

impl Default for PyProgram {
    fn default() -> Self {
        Self::new()
    }
}

#[pymethods]
impl PyProgram {
    #[new]
    pub fn new() -> Self {
        Self(Program::default())
    }

    pub fn clone_without_body_instructions(&self) -> Self {
        Self(self.as_inner().clone_without_body_instructions())
    }

    pub fn copy(&self) -> Self {
        Self(self.as_inner().clone())
    }

    #[getter]
    pub fn body_instructions<'a>(&self, py: Python<'a>) -> PyResult<&'a PyList> {
        Ok(PyList::new(
            py,
            self.as_inner()
                .body_instructions()
                .map(|i| i.to_python(py))
                .collect::<PyResult<Vec<PyInstruction>>>()?,
        ))
    }

    #[getter]
    pub fn calibrations(&self, py: Python<'_>) -> PyResult<PyCalibrationSet> {
        self.as_inner().calibrations.to_python(py)
    }

    #[setter]
    pub fn set_calibrations(
        &mut self,
        py: Python<'_>,
        calibrations: PyCalibrationSet,
    ) -> PyResult<()> {
        let program = self.as_inner_mut();
        program.calibrations = CalibrationSet::py_try_from(py, &calibrations)?;
        Ok(())
    }

    #[getter]
    pub fn waveforms(&self, py: Python<'_>) -> PyResult<BTreeMap<String, PyWaveform>> {
        self.as_inner().waveforms.to_python(py)
    }

    #[setter]
    pub fn set_waveforms(
        &mut self,
        py: Python<'_>,
        waveforms: BTreeMap<String, PyWaveform>,
    ) -> PyResult<()> {
        self.as_inner_mut().waveforms = BTreeMap::<String, Waveform>::py_try_from(py, &waveforms)?;
        Ok(())
    }

    #[getter]
    pub fn frames(&self, py: Python<'_>) -> PyResult<PyFrameSet> {
        self.as_inner().frames.to_python(py)
    }

    #[setter]
    pub fn set_frames(&mut self, py: Python<'_>, frames: PyFrameSet) -> PyResult<()> {
        self.as_inner_mut().frames = FrameSet::py_try_from(py, &frames)?;
        Ok(())
    }

    #[getter]
    pub fn memory_regions(&self, py: Python<'_>) -> PyResult<BTreeMap<String, PyMemoryRegion>> {
        self.as_inner()
            .memory_regions
            .iter()
            .map(|(name, memory_region)| Ok((name.to_python(py)?, memory_region.to_python(py)?)))
            .collect()
    }

    #[setter]
    pub fn set_memory_regions(
        &mut self,
        py: Python<'_>,
        memory_regions: BTreeMap<String, PyMemoryRegion>,
    ) -> PyResult<()> {
        self.as_inner_mut().memory_regions =
            BTreeMap::<String, MemoryRegion>::py_try_from(py, &memory_regions)?;
        Ok(())
    }

    #[getter]
    // TODO: Should this filtering move to Program? Should we assume memory_regions will always make up all
    // declarations and simplify this?
    pub fn declarations(&self, py: Python<'_>) -> PyResult<HashMap<String, PyDeclaration>> {
        self.as_inner()
            .to_instructions()
            .iter()
            .filter_map(|inst| match inst {
                Instruction::Declaration(declaration) => Some(declaration),
                _ => None,
            })
            .map(|declaration| Ok((declaration.name.clone(), declaration.to_python(py)?)))
            .collect()
    }

    #[getter]
    pub fn gate_definitions(&self, py: Python<'_>) -> PyResult<BTreeMap<String, PyGateDefinition>> {
        self.as_inner()
            .gate_definitions
            .iter()
            .map(|(name, gate_def)| Ok((name.to_python(py)?, gate_def.to_python(py)?)))
            .collect()
    }

    #[setter]
    pub fn set_gate_definitions(&mut self, definitions: BTreeMap<String, PyGateDefinition>) {
        self.as_inner_mut().gate_definitions = definitions
            .into_iter()
            .map(|(name, gate_def)| (name, gate_def.into_inner()))
            .collect();
    }

    pub fn dagger(&self) -> PyResult<Self> {
        self.as_inner()
            .dagger()
            .map(PyProgram::from)
            .map_err(ProgramError::from)
            .map_err(ProgramError::to_py_err)
    }

    pub fn expand_calibrations(&self) -> PyResult<Self> {
        self.as_inner()
            .expand_calibrations()
            .map(PyProgram::from)
            .map_err(ProgramError::from)
            .map_err(ProgramError::to_py_err)
    }

    pub fn into_simplified(&self) -> PyResult<Self> {
        self.as_inner()
            .into_simplified()
            .map(PyProgram::from)
            .map_err(ProgramError::from)
            .map_err(ProgramError::to_py_err)
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

    pub fn add_instructions(&mut self, instructions: Vec<PyInstruction>) {
        self.as_inner_mut()
            .add_instructions(instructions.into_iter().map(Into::into))
    }

    pub fn to_instructions(&self, py: Python<'_>) -> PyResult<Vec<PyInstruction>> {
        self.as_inner()
            .to_instructions()
            .iter()
            .map(|i| i.to_python(py))
            .collect()
    }

    pub fn to_unitary(&self, py: Python<'_>, n_qubits: u64) -> PyResult<Py<PyArray2<Complex64>>> {
        Ok(self
            .as_inner()
            .to_unitary(n_qubits)
            .map_err(ProgramError::from)
            .map_err(ProgramError::to_py_err)?
            .to_pyarray(py)
            .to_owned())
    }

    pub fn resolve_placeholders(&mut self) {
        self.as_inner_mut().resolve_placeholders();
    }

    // Because we can't bubble up an error from inside the closures, they panic when the given
    // Python functions return an error or an unexpected type. This is unusual, but in a Python
    // program, this function will only raise because [`pyo3`] wraps Rust panics in a
    // `PanicException`.
    #[pyo3(signature = (*, target_resolver = None, qubit_resolver = None))]
    pub fn resolve_placeholders_with_custom_resolvers(
        &mut self,
        target_resolver: Option<Py<PyFunction>>,
        qubit_resolver: Option<Py<PyFunction>>,
    ) {
        #[allow(clippy::type_complexity)]
        let rs_qubit_resolver: Box<dyn Fn(&QubitPlaceholder) -> Option<u64>> =
            if let Some(resolver) = qubit_resolver {
                Box::new(move |placeholder: &QubitPlaceholder| -> Option<u64> {
                    Python::with_gil(|py| {
                        let resolved_qubit = resolver
                            .call1(
                                py,
                                (placeholder
                                    .to_python(py)
                                    .expect("QubitPlaceholder.to_python() should be infallible"),),
                            )
                            .unwrap_or_else(|err| {
                                panic!("qubit_resolver returned an error: {err}")
                            });

                        resolved_qubit.extract(py).unwrap_or_else(|err| {
                            panic!("qubit_resolver must return None or int: {err}")
                        })
                    })
                })
            } else {
                self.as_inner().default_qubit_resolver()
            };

        #[allow(clippy::type_complexity)]
        let rs_target_resolver: Box<dyn Fn(&TargetPlaceholder) -> Option<String>> =
            if let Some(resolver) = target_resolver {
                Box::new(move |placeholder: &TargetPlaceholder| -> Option<String> {
                    Python::with_gil(|py| {
                        let resolved_label = resolver
                            .call1(
                                py,
                                (placeholder
                                    .to_python(py)
                                    .expect("TargetPlaceholder.to_python() should be infallibe"),),
                            )
                            .unwrap_or_else(|err| {
                                panic!("label_resolver returned an error: {err}")
                            });

                        resolved_label.extract(py).unwrap_or_else(|err| {
                            panic!("label_resolver must return None or str: {err}")
                        })
                    })
                })
            } else {
                self.as_inner().default_target_resolver()
            };

        self.as_inner_mut()
            .resolve_placeholders_with_custom_resolvers(rs_target_resolver, rs_qubit_resolver);
    }

    pub fn __add__(&self, py: Python<'_>, rhs: Self) -> PyResult<Self> {
        let new = self.as_inner().clone() + rhs.as_inner().clone();
        new.to_python(py)
    }

    pub fn __iadd__(&mut self, rhs: Self) {
        *self.as_inner_mut() += rhs.as_inner().clone()
    }

    // This will raise an error if the program contains any unresolved
    // placeholders. This is because they can't be converted to valid quil,
    // nor can they be serialized and deserialized in a consistent
    // way.
    pub fn __getstate__(&self, py: Python<'_>) -> PyResult<Py<PyBytes>> {
        Ok(PyBytes::new(py, self.to_quil()?.as_bytes()).into_py(py))
    }

    pub fn __setstate__(&mut self, py: Python<'_>, state: &PyBytes) -> PyResult<()> {
        *self = Program::from_str(std::str::from_utf8(state.as_bytes())?)
            .map_err(ProgramError::from)
            .map_err(ProgramError::to_py_err)?
            .to_python(py)?;
        Ok(())
    }
}

create_init_submodule! {
    classes: [ PyFrameSet, PyProgram, PyCalibrationSet, PyMemoryRegion ],
}
