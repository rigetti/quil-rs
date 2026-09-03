//! The `QubitGraph` is a logical execution/dependency graph of
//! instructions with respect to gates on shared qubits.

// Copyright 2024 Rigetti Computing
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
use std::collections::HashMap;

use crate::instruction::{Instruction, InstructionHandler, InstructionRole};
use crate::quil::Quil;
use petgraph::{graph::DiGraph, Direction};

use super::BasicBlock;

#[derive(Debug, thiserror::Error)]
pub enum QubitGraphError {
    #[error("Unsupported instruction: {}", .0.to_quil_or_debug())]
    UnsupportedInstruction(Instruction),
}

/// QubitGraph is a logical execution/dependency graph of instructions.  Pragma, RF Control, and Control Flow instructions
/// are not supported. It is a directed graph *from* the first instructions (the set of instructions that do not depend
/// on prior instructions) *to* the last instructions (the set of instructions that are not prerequisites for any later
/// instructions).
///
/// Nodes are instructions; edges link subsequent instructions which use a shared qubit.
#[derive(Debug)]
pub struct QubitGraph<'a> {
    graph: DiGraph<&'a Instruction, ()>,
}

impl<'a> QubitGraph<'a> {
    pub(crate) fn new<H: InstructionHandler>(
        instructions: impl Iterator<Item = &'a Instruction>,
        handler: &H,
    ) -> Result<Self, QubitGraphError> {
        let mut last_instruction_for_qubit = HashMap::new();
        let mut graph = DiGraph::new();

        for instruction in instructions {
            match handler.role(instruction) {
                InstructionRole::ClassicalCompute => {
                    if let Instruction::Pragma(_) = instruction {
                        return Err(QubitGraphError::UnsupportedInstruction(instruction.clone()));
                    }
                } // Valid, mostly ignored
                InstructionRole::ControlFlow => match &instruction {
                    Instruction::Jump(_)
                    | Instruction::JumpWhen(_)
                    | Instruction::JumpUnless(_) => {
                        return Err(QubitGraphError::UnsupportedInstruction(instruction.clone()))
                    }
                    _ => {}
                },
                InstructionRole::ProgramComposition => {} // Valid, includes Gate, etc.,
                InstructionRole::RFControl => {
                    return Err(QubitGraphError::UnsupportedInstruction(instruction.clone()))
                }
            }

            let qubits: Vec<_> = instruction.get_qubits().into_iter().collect();

            let node = graph.add_node(instruction);

            for qubit in qubits {
                if let Some(last_instruction) = last_instruction_for_qubit.insert(qubit, node) {
                    graph.add_edge(last_instruction, node, ());
                }
            }
        }

        Ok(Self { graph })
    }

    pub fn try_from_basic_block<H: InstructionHandler>(
        block: &BasicBlock<'a>,
        handler: &H,
    ) -> Result<Self, QubitGraphError> {
        QubitGraph::new(block.instructions().iter().copied(), handler)
    }

    /// Returns the length of the longest path from an initial instruction (one with no prerequisite instructions) to a final
    /// instruction (one with no dependent instructions), where the length of a path is the number of gate instructions in the path.
    ///
    /// Implemented as a longest-path dynamic program: nodes are added in
    /// program order and edges always point from an earlier instruction to a
    /// later one, so node indices are already topologically sorted and the
    /// scan is `O(nodes + edges)`. (A previous implementation folded over
    /// every source-to-sink path, which is exponential in the number of
    /// branch-and-reconverge diamonds in the block.)
    ///
    /// # Arguments
    ///
    /// * `gate_minimum_qubit_count` - The minimum number of qubits in a gate for it to be counted in the depth.
    pub fn gate_depth(&self, gate_minimum_qubit_count: usize) -> usize {
        let mut depth_at_entry = vec![0usize; self.graph.node_count()];
        let mut max_depth = 0;
        for node in self.graph.node_indices() {
            let contribution = match self.graph[node] {
                Instruction::Gate(gate) if gate.qubits.len() >= gate_minimum_qubit_count => 1,
                _ => 0,
            };
            let depth = depth_at_entry[node.index()] + contribution;
            max_depth = max_depth.max(depth);
            for next in self.graph.neighbors_directed(node, Direction::Outgoing) {
                depth_at_entry[next.index()] = depth_at_entry[next.index()].max(depth);
            }
        }
        max_depth
    }
}

#[cfg(test)]
mod tests {
    use crate::instruction::DefaultHandler;
    use crate::Program;
    use rstest::rstest;

    use super::*;

    use super::super::test_programs::*;

    #[rstest]
    #[case(QUIL_AS_TREE, 2)]
    #[case(QUIL_AS_INVERSE_TREE, 2)]
    #[case(QUIL_AS_LINEAR, 4)]
    #[case(QUIL_WITH_DIAMOND, 6)]
    #[case(QUIL_WITH_SWAP, 3)]
    #[case(KITCHEN_SINK_QUIL, 2)]
    fn gate_depth(#[case] input: &str, #[case] expected: usize) {
        let program: Program = input.parse().unwrap();
        let block: BasicBlock = (&program).try_into().unwrap();
        let graph = QubitGraph::try_from_basic_block(&block, &DefaultHandler).unwrap();
        let depth = graph.gate_depth(1);
        assert_eq!(expected, depth);
    }

    #[rstest]
    #[case(QUIL_AS_TREE, 1)]
    #[case(QUIL_AS_INVERSE_TREE, 1)]
    #[case(QUIL_AS_LINEAR, 0)]
    #[case(QUIL_WITH_DIAMOND, 2)]
    #[case(QUIL_WITH_SWAP, 1)]
    #[case(KITCHEN_SINK_QUIL, 1)]
    fn multiqubit_gate_depth(#[case] input: &str, #[case] expected: usize) {
        let program: Program = input.parse().unwrap();
        let block: BasicBlock = (&program).try_into().unwrap();
        let graph = QubitGraph::try_from_basic_block(&block, &DefaultHandler).unwrap();
        let depth = graph.gate_depth(2);
        assert_eq!(expected, depth);
    }

    #[test]
    fn gate_depth_is_polynomial_in_diamonds() {
        // 64 branch-and-reconverge diamonds: ~2^64 source-to-sink paths,
        // which must not be enumerated.
        let n = 64;
        let source = "CNOT 0 1\nX 0\nH 1\n".repeat(n);
        let program: Program = source.parse().unwrap();
        let block: BasicBlock = (&program).try_into().unwrap();
        let graph = QubitGraph::try_from_basic_block(&block, &DefaultHandler).unwrap();
        // Longest path alternates CNOT and a 1Q gate: 2 gates per diamond.
        assert_eq!(graph.gate_depth(1), 2 * n);
        assert_eq!(graph.gate_depth(2), n);
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

        let maybe_graph = QubitGraph::try_from_basic_block(&block, &DefaultHandler);
        match maybe_graph {
            Ok(graph) => {
                let depth = graph.gate_depth(1);
                assert_eq!(expected, Some(depth));
            }
            Err(_) => {
                assert_eq!(expected, None)
            }
        }
    }
}
