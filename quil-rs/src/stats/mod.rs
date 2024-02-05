mod execution_graph;

#[cfg(test)]
pub(crate) mod test_programs;

// Use quil_rs::program::graph for pulse-level programs, but not qubit programs.

use std::collections::HashSet;
use std::ops::Neg;

use crate::{
    instruction::{Instruction, Qubit},
    Program,
};
use execution_graph::{Error as ExecutionGraphError, ExecutionGraph};

pub trait InstructionsSource {
    fn len(&self) -> usize;
    fn body_instructions(&self) -> impl Iterator<Item = &Instruction> + '_;
    fn get_used_qubits(&self) -> impl IntoIterator<Item = &Qubit>;
}

impl InstructionsSource for Program {
    fn len(&self) -> usize {
        Program::len(self)
    }

    fn body_instructions(&self) -> impl Iterator<Item = &Instruction> + '_ {
        Program::body_instructions(self)
    }

    // There's no generic trait for "set-like" types, so we return a concrete
    // type here (as an API convenience) but do not require this type in the
    // trait definition.
    #[allow(refining_impl_trait)]
    fn get_used_qubits(&self) -> &HashSet<Qubit> {
        Program::get_used_qubits(self)
    }
}

impl InstructionsSource for &Program {
    fn len(&self) -> usize {
        (*self).len()
    }

    fn body_instructions(&self) -> impl Iterator<Item = &Instruction> + '_ {
        (*self).body_instructions()
    }

    // See note on `Program` impl above.
    #[allow(refining_impl_trait)]
    fn get_used_qubits(&self) -> &HashSet<Qubit> {
        (*self).get_used_qubits()
    }
}

pub struct ProgramStats<S: InstructionsSource> {
    source: S,
    // Programs with dynamic control flow, pragmas, or RF controls are not supported for execution-graph operations.
    execution_graph: Result<ExecutionGraph, ExecutionGraphError>,
}

fn make_execution_graph<S: InstructionsSource>(
    source: S,
) -> Result<ExecutionGraph, ExecutionGraphError> {
    ExecutionGraph::new(source.body_instructions().cloned())
}

impl<'a> ProgramStats<&'a Program> {
    pub fn from_program(program: &'a Program) -> Self {
        let execution_graph = make_execution_graph(program);

        Self {
            source: program,
            execution_graph,
        }
    }

    // Provided because Program specifically returns a hash-set, but in general
    // `InstructionsSource` may not provide that feature.
    pub fn set_of_qubits_used(&self) -> &HashSet<Qubit> {
        self.source.get_used_qubits()
    }
}

impl<S: InstructionsSource> ProgramStats<S> {
    fn execution_graph(&self) -> Result<&ExecutionGraph, &ExecutionGraphError> {
        self.execution_graph.as_ref()
    }

    /// The total number of instructions in the program.
    ///
    /// This includes all definitions excluded by [`Program::body_instruction_count`].
    pub fn len(&self) -> usize {
        self.source.len()
    }

    /// The total number of instructions in the program *body*.
    ///
    /// This does not include:
    /// - memory region definitions
    /// - frame definitions
    /// - waveform definitions
    /// - gate definitions
    pub fn body_instruction_count(&self) -> usize {
        self.source.body_instructions().count()
    }

    /// The maximum number of *successive* gates in the native Quil program.
    /// If the program does not have a valid ExecutionGraph, this will return
    /// `None`; use `execution_graph()` to see why the graph is invalid.
    pub fn gate_depth(&self) -> Option<usize> {
        self.execution_graph().map(ExecutionGraph::gate_depth).ok()
    }

    /// The total number of gates in the program. Also called the "gate volume".
    pub fn gate_volume(&self) -> usize {
        self.source
            .body_instructions()
            .filter(|i| matches!(i, Instruction::Gate(_)))
            .count()
    }

    /// The maximum number of two-qubit gates in the native Quil program.
    /// If the program does not have a valid ExecutionGraph, this will return
    /// `None`; use `execution_graph()` to see why the graph is invalid.
    pub fn multiqubit_gate_depth(&self) -> Option<usize> {
        self.execution_graph().map(ExecutionGraph::multi_qubit_gate_depth).ok()
    }

    /// A list of all qubits used in the program.
    pub fn qubits_used(&self) -> impl IntoIterator<Item = &Qubit> {
        self.source.get_used_qubits()
    }

    /// Rough estimate of fidelity of the native Quil program.
    pub fn fidelity_estimate<F>(&self, get_fidelity: F) -> f64
    where
        F: Fn(&Instruction) -> Option<f64>,
    {
        // TODO: double check implementation (#335)
        self.source
            .body_instructions()
            .filter_map(get_fidelity)
            .map(|f: f64| f.ln().powi(2))
            .sum::<f64>()
            .sqrt()
            .neg()
            .exp()
    }

    /// The total number of `SWAP` gates in the native Quil program.
    pub fn topological_swap_count(&self) -> usize {
        self.source
            .body_instructions()
            .filter_map(|i| match i {
                Instruction::Gate(gate) => Some(gate),
                _ => None,
            })
            .filter(|gate| gate.name.eq_ignore_ascii_case("SWAP"))
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
        self.source.body_instructions().any(|i| {
            matches!(
                i,
                Instruction::Jump(_) | Instruction::JumpWhen(_) | Instruction::JumpUnless(_)
            )
        })
    }
}

#[cfg(test)]
mod tests {
    use std::f64::consts;

    use crate::Program;
    use rstest::rstest;

    use super::test_programs::*;
    use super::*;

    #[rstest]
    #[case(QUIL_AS_TREE, 3)]
    #[case(QUIL_AS_INVERSE_TREE, 3)]
    #[case(QUIL_AS_LINEAR, 4)]
    #[case(QUIL_WITH_DIAMOND, 7)]
    #[case(QUIL_WITH_SWAP, 5)]
    #[case(KITCHEN_SINK_QUIL, 2)]
    fn gate_volume(#[case] input: &str, #[case] expected: usize) {
        let program: Program = input.parse().unwrap();
        let stats = ProgramStats::from_program(&program);
        let volume = stats.gate_volume();
        assert_eq!(expected, volume);
    }

    #[rstest]
    #[case(QUIL_AS_TREE)]
    #[case(QUIL_AS_INVERSE_TREE)]
    #[case(QUIL_AS_LINEAR)]
    #[case(QUIL_WITH_DIAMOND)]
    #[case(QUIL_WITH_SWAP)]
    #[case(KITCHEN_SINK_QUIL)]
    fn fidelity_estimate_all100percent(#[case] input: &str) {
        let program: Program = input.parse().unwrap();
        let stats = ProgramStats::from_program(&program);
        let all_100p = |_: &Instruction| Some(1.0);
        let fidelity = stats.fidelity_estimate(all_100p);
        assert_eq!(1.0, fidelity);
    }

    #[rstest]
    #[case(QUIL_AS_TREE)]
    #[case(QUIL_AS_INVERSE_TREE)]
    #[case(QUIL_AS_LINEAR)]
    #[case(QUIL_WITH_DIAMOND)]
    #[case(QUIL_WITH_SWAP)]
    #[case(KITCHEN_SINK_QUIL)]
    fn fidelity_estimate_all0percent(#[case] input: &str) {
        let program: Program = input.parse().unwrap();
        let stats = ProgramStats::from_program(&program);
        let all_0p = |_: &Instruction| Some(0.0);
        let fidelity = stats.fidelity_estimate(all_0p);
        assert_eq!(0.0, fidelity);
    }

    #[rstest]
    #[case(QUIL_AS_TREE)]
    #[case(QUIL_AS_INVERSE_TREE)]
    #[case(QUIL_AS_LINEAR)]
    #[case(QUIL_WITH_DIAMOND)]
    #[case(QUIL_WITH_SWAP)]
    #[case(KITCHEN_SINK_QUIL)]
    fn fidelity_estimate_all90percent(#[case] input: &str) {
        let program: Program = input.parse().unwrap();
        let stats = ProgramStats::from_program(&program);
        let all_90p = |_: &Instruction| Some(0.9);
        let fidelity = stats.fidelity_estimate(all_90p);
        let fidelity_sum = stats.body_instruction_count() as f64 * 0.9_f64.ln().powi(2);
        let expected = consts::E.powf(fidelity_sum.sqrt().neg());
        assert_eq!(expected, fidelity);
    }

    #[rstest]
    #[case(QUIL_AS_TREE, 0)]
    #[case(QUIL_AS_INVERSE_TREE, 0)]
    #[case(QUIL_AS_LINEAR, 0)]
    #[case(QUIL_WITH_DIAMOND, 0)]
    #[case(QUIL_WITH_SWAP, 1)]
    #[case(KITCHEN_SINK_QUIL, 0)]
    fn topological_swap_count(#[case] input: &str, #[case] expected: usize) {
        let program: Program = input.parse().unwrap();
        let stats = ProgramStats::from_program(&program);
        let count = stats.topological_swap_count();
        assert_eq!(expected, count);
    }

    #[rstest]
    #[case(QUIL_AS_TREE, false)]
    #[case(QUIL_AS_INVERSE_TREE, false)]
    #[case(QUIL_AS_LINEAR, false)]
    #[case(QUIL_WITH_DIAMOND, false)]
    #[case(KITCHEN_SINK_QUIL, false)]
    #[case(QUIL_WITH_JUMP, true)]
    #[case(QUIL_WITH_JUMP_WHEN, true)]
    #[case(QUIL_WITH_JUMP_UNLESS, true)]
    fn has_dynamic_control_flow(#[case] input: &str, #[case] expected: bool) {
        let program: Program = input.parse().unwrap();
        let stats = ProgramStats::from_program(&program);
        let dynamic = stats.has_dynamic_control_flow();
        assert_eq!(expected, dynamic);
    }

    #[rstest]
    #[case(QUIL_AS_TREE, Some(2))]
    #[case(QUIL_AS_INVERSE_TREE, Some(2))]
    #[case(QUIL_AS_LINEAR, Some(4))]
    #[case(QUIL_WITH_DIAMOND, Some(6))]
    #[case(QUIL_WITH_SWAP, Some(3))]
    #[case(KITCHEN_SINK_QUIL, Some(2))]
    #[case(QUIL_WITH_JUMP, None)]
    #[case(QUIL_WITH_JUMP_WHEN, None)]
    #[case(QUIL_WITH_JUMP_UNLESS, None)]
    fn gate_depth_conditional(#[case] input: &str, #[case] expected: Option<usize>) {
        let program: Program = input.parse().unwrap();
        let stats = ProgramStats::from_program(&program);
        let depth = stats.gate_depth();
        assert_eq!(expected, depth);
    }
}
