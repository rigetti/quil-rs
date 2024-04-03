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
    pub(crate) fn new(
        instructions: impl Iterator<Item = &'a Instruction>,
    ) -> Result<Self, QubitGraphError> {
        let mut last_instruction_for_qubit = HashMap::new();
        let mut graph = DiGraph::new();
        let mut handler = InstructionHandler::default();

        for instruction in instructions {
            match handler.role_for_instruction(instruction) {
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

    /// Fold over all paths over the graph, starting from sources (nodes with no incoming edges),
    /// and ending at sinks (nodes with no outgoing edges).
    ///
    /// The `f` function is called for each instruction in each path, with the current accumulator value and the current
    /// instruction.
    ///
    /// # Examples
    ///
    /// ## Tree
    ///
    /// ```text
    /// CNOT 0 1
    /// X 0
    /// H 1
    /// ```
    ///
    /// 1. `CNOT 0 1` is visited with the initial value, and a new accumulator `A` is returned from `f`.
    /// 2. `X 0` is visited with accumulator `A`, and a result value `B` is returned from `f`.
    /// 3. `H 1` is visited with accumulator `A`, and a second result value `C` is returned from `f`.
    /// 4. The result values are collected into a [`Vec`] and returned as `[B, C]`.
    ///
    /// ## Diamond
    ///
    /// If the program graph forms a diamond shape (i.e. multiple paths converge to a single node), the `f` function
    /// will be called multiple times with the same instruction, but with potentially different accumulator values.
    ///
    /// ```text
    /// CNOT 0 1
    /// X 0
    /// H 1
    /// CNOT 1 0
    /// ```
    ///
    /// 1. `CNOT 0 1` is visited with the initial value, and a new accumulator `A` is returned from `f`.
    /// 2. `X 0` is visited with accumulator `A`, and a new accumulator `B` is returned from `f`.
    /// 3. `H 1` is visited with accumulator `A`, and a new accumulator `C` is returned from `f`.
    /// 4. `CNOT 1 0` is visited with accumulator `B`, and a result value `D` is returned from `f`.
    /// 5. `CNOT 1 0` is visited with accumulator `C`, and a result value `E` is returned from `f`.
    /// 5. The result values are collected into a [`Vec`] and returned as `[D, E]`.
    fn path_fold<T, F>(&self, initial_value: T, mut f: F) -> Vec<T>
    where
        T: Clone + std::fmt::Debug,
        F: FnMut(T, &Instruction) -> T,
    {
        let nodes: Vec<_> = self.graph.externals(Direction::Incoming).collect();
        let mut stack = vec![(initial_value, nodes)];
        let mut result = Vec::new();

        while let Some((acc, nodes)) = stack.pop() {
            if nodes.is_empty() {
                result.push(acc);
                continue;
            }

            for node in nodes {
                let instruction = &self.graph[node];
                let value = f(acc.clone(), instruction);
                stack.push((
                    value,
                    self.graph
                        .neighbors_directed(node, Direction::Outgoing)
                        .collect(),
                ));
            }
        }

        result
    }

    /// Returns the length of the longest path from an initial instruction (one with no prerequisite instructions) to a final
    /// instruction (one with no dependent instructions), where the length of a path is the number of gate instructions in the path.
    ///
    /// # Arguments
    ///
    /// * `gate_minimum_qubit_count` - The minimum number of qubits in a gate for it to be counted in the depth.
    pub fn gate_depth(&self, gate_minimum_qubit_count: usize) -> usize {
        let path_lengths = self.path_fold(0, |depth: usize, instruction: &Instruction| -> usize {
            if let Instruction::Gate(gate) = instruction {
                if gate.qubits.len() >= gate_minimum_qubit_count {
                    return depth + 1;
                }
            }
            depth
        });
        path_lengths.into_iter().max().unwrap_or_default()
    }
}

impl<'a> TryFrom<&'_ BasicBlock<'a>> for QubitGraph<'a> {
    type Error = QubitGraphError;

    fn try_from(block: &BasicBlock<'a>) -> Result<Self, Self::Error> {
        QubitGraph::new(block.instructions().iter().copied())
    }
}

#[cfg(test)]
mod tests {
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
        let graph: QubitGraph = (&block).try_into().unwrap();
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
        let graph: QubitGraph = (&block).try_into().unwrap();
        let depth = graph.gate_depth(2);
        assert_eq!(expected, depth);
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
                let depth = graph.gate_depth(1);
                assert_eq!(expected, Some(depth));
            }
            Err(_) => {
                assert_eq!(expected, None)
            }
        }
    }
}
