use pyo3::{exceptions::PyValueError, types::PyType};
use quil_rs::program::analysis::{
    BasicBlock, BasicBlockOwned, BasicBlockScheduleError, ControlFlowGraph, ControlFlowGraphOwned,
    QubitGraph, QubitGraphError,
};
use rigetti_pyo3::{
    impl_repr, py_wrap_error, py_wrap_type, pyo3::prelude::*, wrap_error, PyWrapper, ToPythonError,
};

use crate::instruction::{PyInstruction, PyTarget};

use super::{scheduling::PyScheduleSeconds, PyProgram};

py_wrap_type! {
    #[pyo3(subclass)]
    PyControlFlowGraph(ControlFlowGraphOwned) as "ControlFlowGraph"
}

impl_repr!(PyControlFlowGraph);

#[pymethods]
impl PyControlFlowGraph {
    #[new]
    #[classmethod]
    pub fn new(_: Py<PyType>, instance: Self) -> Self {
        instance
    }

    pub fn has_dynamic_control_flow(&self) -> bool {
        ControlFlowGraph::from(self.as_inner()).has_dynamic_control_flow()
    }

    pub fn basic_blocks(&self) -> Vec<PyBasicBlock> {
        ControlFlowGraph::from(self.as_inner())
            .into_blocks()
            .into_iter()
            .map(BasicBlockOwned::from)
            .map(PyBasicBlock::from)
            .collect()
    }
}

py_wrap_type! {
    #[pyo3(subclass)]
    PyBasicBlock(BasicBlockOwned) as "BasicBlock"
}
impl_repr!(PyBasicBlock);

wrap_error!(RustBasicBlockScheduleError(BasicBlockScheduleError));
py_wrap_error!(
    quil,
    RustBasicBlockScheduleError,
    PyBasicBlockScheduleError,
    PyValueError
);

wrap_error!(RustQubitGraphError(QubitGraphError));
py_wrap_error!(quil, RustQubitGraphError, PyQubitGraphError, PyValueError);

#[pymethods]
impl PyBasicBlock {
    #[new]
    #[classmethod]
    pub fn new(_: Py<PyType>, instance: Self) -> Self {
        instance
    }

    pub fn as_schedule_seconds(&self, program: &PyProgram) -> PyResult<PyScheduleSeconds> {
        BasicBlock::from(self.as_inner())
            .as_schedule_seconds(program.as_inner())
            .map(|v| v.into())
            .map_err(RustBasicBlockScheduleError::from)
            .map_err(RustBasicBlockScheduleError::to_py_err)
    }

    pub fn gate_depth(&self, gate_minimum_qubit_count: usize) -> PyResult<usize> {
        let block = BasicBlock::from(self.as_inner());
        QubitGraph::try_from(&block)
            .map(|graph| graph.gate_depth(gate_minimum_qubit_count))
            .map_err(RustQubitGraphError::from)
            .map_err(RustQubitGraphError::to_py_err)
    }

    pub fn instructions(&self) -> Vec<PyInstruction> {
        BasicBlock::from(self.as_inner())
            .instructions()
            .iter()
            .copied()
            .map(PyInstruction::from)
            .collect()
    }

    pub fn label(&self) -> Option<PyTarget> {
        BasicBlock::from(self.as_inner()).label().map(|l| l.into())
    }

    pub fn terminator(&self) -> Option<PyInstruction> {
        BasicBlock::from(self.as_inner())
            .terminator()
            .clone()
            .into_instruction()
            .map(PyInstruction::from)
    }
}
