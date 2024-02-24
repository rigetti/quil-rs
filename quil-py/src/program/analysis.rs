use pyo3::exceptions::PyValueError;
use quil_rs::program::analysis::{
    BasicBlock, BasicBlockOwned, ControlFlowGraph, ControlFlowGraphOwned,
};
use rigetti_pyo3::{py_wrap_type, pyo3::prelude::*};

use crate::instruction::{PyInstruction, PyTarget};

use super::{scheduling::PyFixedSchedule, PyProgram};

py_wrap_type! {
    PyControlFlowGraph(ControlFlowGraphOwned) as "ControlFlowGraph"
}

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

#[pymethods]
impl PyBasicBlock {
    pub fn label(&self) -> Option<PyTarget> {
        BasicBlock::from(&self.0).label().map(|l| l.into())
    }

    pub fn instructions(&self) -> Vec<PyInstruction> {
        BasicBlock::from(&self.0)
            .instructions()
            .iter()
            .copied()
            .map(PyInstruction::from)
            .collect()
    }

    pub fn terminator(&self) -> Option<PyInstruction> {
        BasicBlock::from(&self.0)
            .terminator()
            .clone()
            .into_instruction()
            .map(PyInstruction::from)
    }

    pub fn as_fixed_schedule(&self, program: &PyProgram) -> PyResult<PyFixedSchedule> {
        BasicBlock::from(&self.0)
            .as_fixed_schedule(&program.0)
            .map(|v| v.into())
            .map_err(|e| PyValueError::new_err(e.to_string()).into())
    }
}
