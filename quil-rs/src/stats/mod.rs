mod execution_graph;

// Use quil_rs::program::graph for pulse-level programs, but not qubit programs.

use crate::{
    instruction::{Instruction, Qubit},
    Program,
};
use execution_graph::{Error as ExecutionGraphError, ExecutionGraph};

pub struct ProgramStats {
    program: Program,
    execution_graph: ExecutionGraph,
}

fn make_execution_graph(program: &Program) -> Result<ExecutionGraph, ExecutionGraphError> {
    ExecutionGraph::new(program.body_instructions().cloned())
}

impl ProgramStats {
    pub fn new(program: Program) -> Result<Self, ExecutionGraphError> {
        let execution_graph = make_execution_graph(&program)?;

        Ok(Self {
            program,
            execution_graph,
        })
    }

    fn execution_graph(&self) -> &ExecutionGraph {
        &self.execution_graph
    }

    /// The total number of instructions in the program *body*.
    ///
    /// This does not include:
    /// - memory region definitions
    /// - frame definitions
    /// - waveform definitions
    /// - gate definitions
    pub fn body_instruction_count(&self) -> usize {
        self.program.body_instructions().count()
    }

    /// The total number of instructions in the program.
    ///
    /// This includes all definitions excluded by [`Program::instruction_count`].
    pub fn instruction_count(&self) -> usize {
        self.program.to_instructions().len()
    }

    /// The maximum number of *successive* gates in the native Quil program.
    pub fn gate_depth(&self) -> Option<usize> {
        self.execution_graph().gate_depth().ok()
    }

    /// The total number of gates in the program. Also called the "gate volume".
    pub fn gate_volume(&self) -> usize {
        self.program
            .body_instructions()
            .filter(|i| matches!(i, Instruction::Gate(_)))
            .count()
    }

    /// The maximum number of two-qubit gates in the native Quil program.
    pub fn multiqubit_gate_depth(&self) -> Option<usize> {
        self.execution_graph().multi_qubit_gate_depth().ok()
    }

    /// A list of all qubits used in the program.
    pub fn qubits_used(&self) -> Vec<Qubit> {
        // TODO: return a set instead?
        self.program.get_used_qubits().iter().cloned().collect()
    }

    /// Rough estimate of fidelity of the native Quil program.
    pub fn fidelity_estimate(&self) -> Option<f64> {
        todo!()
    }

    /// The total number of swaps (i.e. `SWAP-PHASES`) in the native Quil program.
    pub fn topological_swap_count(&self) -> usize {
        // TODO: gate named swap
        self.program
            .body_instructions()
            .filter(|i| matches!(i, Instruction::SwapPhases(_)))
            .count()
    }

    /// Output qubit index relabeling due to SWAP insertion.
    // fn final_rewriting(&self) -> Vec<u64>;
    /// Rough estimate of native quil program length in seconds.
    // fn program_duration_seconds(&self) -> Option<f64>;
    /// The estimated runtime of the program on a Rigetti QPU, in milliseconds. Available only for
    /// protoquil compliant programs.
    // fn qpu_runtime_estimation(&self) -> Option<f64>;

    /// Whether the program uses dynamic control flow.
    pub fn has_dynamic_control_flow(&self) -> bool {
        false // TODO
    }
}
