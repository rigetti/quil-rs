use pyo3::exceptions::PyValueError;
use quil_rs::program::analysis::{
    BasicBlock, BasicBlockOwned, BasicBlockScheduleError, ControlFlowGraph, ControlFlowGraphOwned,
    QubitGraph, QubitGraphError,
};
use rigetti_pyo3::{
    impl_repr, py_wrap_error, py_wrap_type, pyo3::prelude::*, wrap_error, ToPythonError,
};

use crate::instruction::{PyInstruction, PyTarget};

use super::{scheduling::PyFixedSchedule, PyProgram};

py_wrap_type! {
    PyControlFlowGraph(ControlFlowGraphOwned) as "ControlFlowGraph"
}

impl_repr!(PyControlFlowGraph);

#[pymethods]
impl PyControlFlowGraph {
    pub fn has_dynamic_control_flow(&self) -> bool {
        ControlFlowGraph::from(&self.0).has_dynamic_control_flow()
    }

    pub fn basic_blocks(&self) -> Vec<PyBasicBlock> {
        ControlFlowGraph::from(&self.0)
            .into_blocks()
            .into_iter()
            .map(BasicBlockOwned::from)
            .map(PyBasicBlock::from)
            .collect()
    }
}

py_wrap_type! {
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
    pub fn as_fixed_schedule(
        &self,
        program: &PyProgram,
        include_zero_duration_instructions: bool,
    ) -> PyResult<PyFixedSchedule> {
        BasicBlock::from(&self.0)
            .as_fixed_schedule(&program.0, include_zero_duration_instructions)
            .map(|v| v.into())
            .map_err(RustBasicBlockScheduleError::from)
            .map_err(RustBasicBlockScheduleError::to_py_err)
    }

    pub fn gate_depth(&self, gate_minimum_qubit_count: usize) -> PyResult<usize> {
        let block = BasicBlock::from(&self.0);
        QubitGraph::try_from(&block)
            .map(|graph| graph.gate_depth(gate_minimum_qubit_count))
            .map_err(RustQubitGraphError::from)
            .map_err(RustQubitGraphError::to_py_err)
    }

    pub fn gate_volume(&self) -> usize {
        BasicBlock::from(&self.0).gate_volume()
    }

    pub fn instructions(&self) -> Vec<PyInstruction> {
        BasicBlock::from(&self.0)
            .instructions()
            .iter()
            .copied()
            .map(PyInstruction::from)
            .collect()
    }

    pub fn label(&self) -> Option<PyTarget> {
        BasicBlock::from(&self.0).label().map(|l| l.into())
    }

    pub fn terminator(&self) -> Option<PyInstruction> {
        BasicBlock::from(&self.0)
            .terminator()
            .clone()
            .into_instruction()
            .map(PyInstruction::from)
    }

    pub fn topological_swap_count(&self) -> usize {
        BasicBlock::from(&self.0).topological_swap_count()
    }
}
