//! Metrics calculations for a `BasicBlock`.

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
use super::BasicBlock;
use crate::instruction::Instruction;

impl<'a> BasicBlock<'a> {
    /// The total number of gates in the block.
    pub fn gate_volume(&self) -> usize {
        self.instructions()
            .iter()
            .filter(|i| matches!(i, Instruction::Gate(_)))
            .count()
    }

    /// The total number of `SWAP` gates in the block.
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
}

#[cfg(test)]
mod tests {
    use crate::program::analysis::qubit_graph::QubitGraph;
    use crate::Program;

    use rstest::rstest;

    use super::super::test_programs::*;
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
        let block: BasicBlock = (&program).try_into().unwrap();
        let volume = block.gate_volume();
        assert_eq!(expected, volume);
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
                let depth = graph.gate_depth(1);
                assert_eq!(expected, Some(depth));
            }
            Err(_) => {
                assert_eq!(expected, None)
            }
        }
    }
}
