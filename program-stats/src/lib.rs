#![warn(clippy::all)]

mod execution_graph;

// Use quil_rs::program::graph for pulse-level programs, but not qubit programs.

use execution_graph::{Error as ExecutionGraphError, ExecutionGraph};
use quil_rs::{
    instruction::{Instruction, Qubit},
    Program,
};

pub trait QuilProgramStats {
    /// The total number of instructions in the program *body*.
    ///
    /// This does not include:
    /// - memory region definitions
    /// - frame definitions
    /// - waveform definitions
    /// - gate definitions
    fn body_instruction_count(&self) -> usize;

    /// The total number of instructions in the program.
    ///
    /// This includes all definitions excluded by [`Program::instruction_count`].
    fn instruction_count(&self) -> usize;

    /// The maximum number of *successive* gates in the native Quil program.
    fn gate_depth(&self) -> Option<usize>;

    /// The total number of gates in the program. Also called the "gate volume".
    fn gate_volume(&self) -> usize;

    /// The maximum number of two-qubit gates in the native Quil program.
    fn multiqubit_gate_depth(&self) -> Option<usize>;

    /// A list of all qubits used in the program.
    fn qubits_used(&self) -> Vec<Qubit>; // Hash or BTreeSet?

    /// Rough estimate of fidelity of the native Quil program.
    fn fidelity_estimate(&self) -> Option<f64>;

    /// The total number of swaps (i.e. `SWAP-PHASES`) in the native Quil program.
    fn topological_swap_count(&self) -> usize;

    /// Output qubit index relabeling due to SWAP insertion.
    // fn final_rewriting(&self) -> Vec<u64>;
    /// Rough estimate of native quil program length in seconds.
    // fn program_duration_seconds(&self) -> Option<f64>;
    /// The estimated runtime of the program on a Rigetti QPU, in milliseconds. Available only for
    /// protoquil compliant programs.
    // fn qpu_runtime_estimation(&self) -> Option<f64>;
    fn has_dynamic_control_flow(&self) -> bool;
}

fn make_execution_graph(program: &Program) -> Result<ExecutionGraph, ExecutionGraphError> {
    ExecutionGraph::new(program.body_instructions().cloned())
}

impl QuilProgramStats for Program {
    fn body_instruction_count(&self) -> usize {
        self.body_instructions().count()
    }

    fn instruction_count(&self) -> usize {
        self.to_instructions().len()
    }

    fn gate_depth(&self) -> Option<usize> {
        make_execution_graph(self).ok()?.gate_depth().ok()
    }

    fn gate_volume(&self) -> usize {
        self.body_instructions()
            .filter(|i| matches!(i, Instruction::Gate(_)))
            .count()
    }

    fn multiqubit_gate_depth(&self) -> Option<usize> {
        make_execution_graph(self)
            .ok()?
            .multi_qubit_gate_depth()
            .ok()
    }

    fn qubits_used(&self) -> Vec<Qubit> {
        self.get_used_qubits().iter().cloned().collect()
    }

    fn fidelity_estimate(&self) -> Option<f64> {
        todo!()
    }

    fn topological_swap_count(&self) -> usize {
        // TODO: gate named swap
        self.body_instructions()
            .filter(|i| matches!(i, Instruction::SwapPhases(_)))
            .count()
    }

    fn has_dynamic_control_flow(&self) -> bool {
        false // TODO
    }
}
