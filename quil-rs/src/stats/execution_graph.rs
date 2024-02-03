use std::collections::HashMap;

use crate::instruction::{Instruction, InstructionHandler, InstructionRole};
use crate::quil::Quil;
use petgraph::{graph::DiGraph, Direction};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Unsupported instruction: {}", .0.to_quil_or_debug())]
    UnsupportedInstruction(Instruction),
}

/// ExecutionGraph is a logical execution/dependency graph of instructions. (Currently only 'gate' is supported.) It is a directed graph *from* the first instructions (the set of instructions that do not depend on prior instructions) *to* the last instructions (the set of instructions that are not prerequisites for any later instructions).
pub struct ExecutionGraph {
    graph: DiGraph<Instruction, ()>,
}

impl ExecutionGraph {
    pub fn new(instructions: impl IntoIterator<Item = Instruction>) -> Result<Self, Error> {
        let mut last_instruction_for_qubit = HashMap::new();
        let mut graph = DiGraph::new();
        let mut handler = InstructionHandler::default();

        for instruction in instructions.into_iter() {
            match handler.role_for_instruction(&instruction) {
                InstructionRole::ClassicalCompute => {
                    if let Instruction::Pragma(_) = instruction {
                        return Err(Error::UnsupportedInstruction(instruction));
                    }
                } // Valid, mostly ignored
                InstructionRole::ControlFlow => match &instruction {
                    Instruction::Jump(_)
                    | Instruction::JumpWhen(_)
                    | Instruction::JumpUnless(_) => {
                        return Err(Error::UnsupportedInstruction(instruction))
                    }
                    _ => {}
                },
                InstructionRole::ProgramComposition => {} // Valid, includes Gate, etc.,
                InstructionRole::RFControl => {
                    return Err(Error::UnsupportedInstruction(instruction))
                }
            }

            let qubits: Vec<_> = instruction.get_qubits().into_iter().cloned().collect();

            let node = graph.add_node(instruction);

            for qubit in qubits {
                if let Some(last_instruction) = last_instruction_for_qubit.insert(qubit, node) {
                    graph.add_edge(last_instruction, node, ());
                }
            }
        }

        Ok(Self { graph })
    }

    // TODO: TEST THIS WELL
    /// Fold over all paths over the graph, starting from nodes with no incoming edges, and ending at nodes with no
    /// outgoing edges.
    ///
    /// The `f` function is called for each instruction in each path, with the current accumulator value and the
    /// current instruction.
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
    ///
    /// # Errors
    ///
    /// Any error returned from a call to `f` will be returned immediately.
    // TBD: visibility
    fn path_fold<T, F>(&self, initial_value: T, mut f: F) -> Result<Vec<T>, Error>
    where
        T: Clone + std::fmt::Debug,
        F: FnMut(T, &Instruction) -> Result<T, Error>,
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
                let value = f(acc.clone(), instruction)?;
                stack.push((
                    value,
                    self.graph
                        .neighbors_directed(node, Direction::Outgoing)
                        .collect(),
                ));
            }
        }

        Ok(result)
    }

    /// Returns the longest path from an initial instruction (one with no prerequisite instructions) to a final instruction (one with no dependent instructions).
    pub fn gate_depth(&self) -> Result<usize, Error> {
        let path_lengths = self.path_fold(
            0,
            |depth: usize, instruction: &Instruction| -> Result<usize, Error> {
                if let Instruction::Gate(_) = instruction {
                    Ok(depth + 1)
                } else {
                    Ok(depth)
                }
            },
        )?;
        Ok(path_lengths.into_iter().max().unwrap_or_default())
    }

    /// Returns the longest path through the execution graph (like `gate_depth`), only counting instructions corresponding to multi-qubit gates.
    pub fn multi_qubit_gate_depth(&self) -> Result<usize, Error> {
        let path_lengths = self.path_fold(
            0,
            |depth: usize, instruction: &Instruction| -> Result<usize, Error> {
                if let Instruction::Gate(gate) = instruction {
                    if gate.qubits.len() > 1 {
                        return Ok(depth + 1);
                    }
                }
                Ok(depth)
            },
        )?;
        Ok(path_lengths.into_iter().max().unwrap_or_default())
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
    #[case(KITCHEN_SINK_QUIL, 2)]
    fn gate_depth(#[case] input: &str, #[case] expected: usize) {
        let program: Program = input.parse().unwrap();
        let graph = ExecutionGraph::new(program.to_instructions()).unwrap();
        let depth = graph.gate_depth().unwrap();
        assert_eq!(expected, depth);
    }

    #[rstest]
    #[case(QUIL_AS_TREE, 1)]
    #[case(QUIL_AS_INVERSE_TREE, 1)]
    #[case(QUIL_AS_LINEAR, 0)]
    #[case(QUIL_WITH_DIAMOND, 2)]
    #[case(KITCHEN_SINK_QUIL, 1)]
    fn multiqubit_gate_depth(#[case] input: &str, #[case] expected: usize) {
        let program: Program = input.parse().unwrap();
        let graph = ExecutionGraph::new(program.to_instructions()).unwrap();
        let depth = graph.multi_qubit_gate_depth().unwrap();
        assert_eq!(expected, depth);
    }
}
