use std::collections::HashSet;
use std::ops::Neg;

use super::BasicBlock;
use crate::instruction::{Instruction, Qubit};

impl<'a> BasicBlock<'a> {
    /// The total number of gates in the program.
    pub fn gate_volume(&self) -> usize {
        self.instructions()
            .iter()
            .filter(|i| matches!(i, Instruction::Gate(_)))
            .count()
    }

    /// Qubits used in gates and measurements in the program.
    pub fn qubits_used(&self) -> HashSet<&Qubit> {
        self.instructions()
            .iter()
            .filter_map(|i| match i {
                Instruction::Gate(_) | Instruction::Measurement(_) => Some(i.get_qubits()),
                _ => None,
            })
            .flatten()
            .collect()
    }

    /// Rough estimate of fidelity of the native Quil program. If the provided callback returns `None`, the instruction
    /// will be ignored for the purpose of the estimate.
    pub fn fidelity_estimate<F>(&self, get_fidelity: F) -> f64
    where
        F: Fn(&Instruction) -> Option<f64>,
    {
        // TODO: double check implementation (#335)
        self.instructions()
            .iter()
            .copied()
            .filter_map(get_fidelity)
            .map(|f: f64| f.ln().powi(2))
            .sum::<f64>()
            .sqrt()
            .neg()
            .exp()
    }

    /// The total number of `SWAP` gates in the native Quil program.
    pub fn topological_swap_count(&self) -> usize {
        self.instructions()
            .iter()
            .filter_map(|i| match i {
                Instruction::Gate(gate) => Some(gate),
                _ => None,
            })
            .filter(|gate| gate.name.eq_ignore_ascii_case("SWAP"))
            .count()
    }

    /* TODO #340: additional statistics
    /// Output qubit index relabeling due to SWAP insertion.
    // fn final_rewriting(&self) -> Vec<u64>;

    // TODO #325: duration estimate
    /// The estimated runtime of the program on a Rigetti QPU, in milliseconds. Available only for
    /// protoquil compliant programs.
    // fn qpu_runtime_estimate<F>(&self, get_duration: F) -> Option<f64>
    // where
    //    F: Fn(&Instruction) -> Option<f64>;
    */
}

#[cfg(test)]
mod tests {
    use std::f64::consts;

    use crate::program::analysis::qubit_graph;
    use crate::Program;

    use rstest::rstest;

    use super::super::test_programs::*;
    use super::*;

    #[rstest]
    #[case(KITCHEN_SINK_QUIL, &[Qubit::Fixed(0), Qubit::Fixed(1)])]
    fn block_instructions_from_program(#[case] input: &str, #[case] expected: &[Qubit]) {
        let program: Program = input.parse().unwrap();
        let block: BasicBlock = (&program).try_into().unwrap();
        let qubits = block.qubits_used();
        let expected = expected.iter().collect::<HashSet<_>>();

        assert_eq!(expected, qubits);
    }

    #[rstest]
    #[case(QUIL_AS_TREE, 3)]
    #[case(QUIL_AS_INVERSE_TREE, 3)]
    #[case(QUIL_AS_LINEAR, 4)]
    #[case(QUIL_WITH_DIAMOND, 7)]
    #[case(QUIL_WITH_SWAP, 5)]
    #[case(KITCHEN_SINK_QUIL, 2)]
    fn gate_volume(#[case] input: &str, #[case] expected: usize) {
        let program: Program = input.parse().unwrap();
        let block: BasicBlock = (&program).try_into().unwrap();
        let volume = block.gate_volume();
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
        let block: BasicBlock = (&program).try_into().unwrap();
        let all_100p = |_: &Instruction| Some(1.0);
        let fidelity = block.fidelity_estimate(all_100p);
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
        let block: BasicBlock = (&program).try_into().unwrap();
        let all_0p = |_: &Instruction| Some(0.0);
        let fidelity = block.fidelity_estimate(all_0p);
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
        let block: BasicBlock = (&program).try_into().unwrap();
        let all_90p = |_: &Instruction| Some(0.9);
        let fidelity = block.fidelity_estimate(all_90p);
        let fidelity_sum = block.instructions().len() as f64 * 0.9_f64.ln().powi(2);
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
        let block: BasicBlock = (&program).try_into().unwrap();
        let count = block.topological_swap_count();
        assert_eq!(expected, count);
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
        let block = (&program).try_into();
        let block: BasicBlock = match block {
            Ok(block) => block,
            Err(_) => {
                if expected.is_none() {
                    return;
                } else {
                    panic!("Expected block, got error");
                }
            }
        };

        let maybe_graph: Result<QubitGraph, _> = (&block).try_into();
        match maybe_graph {
            Ok(graph) => {
                let depth = graph.gate_depth();
                assert_eq!(expected, Some(depth));
            }
            Err(_) => {
                assert_eq!(expected, None)
            }
        }
    }
}
