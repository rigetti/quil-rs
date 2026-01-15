// Copyright 2021 Rigetti Computing
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

use std::collections::HashSet;

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::gen_stub_pyclass;

use crate::{
    expression::{Expression, FunctionCallExpression, InfixExpression, PrefixExpression},
    instruction::{CallResolutionError, MemoryReference, Sharing, Vector, WaveformInvocation},
    pickleable_new,
};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.program", eq, frozen, hash, get_all, subclass)
)]
pub struct MemoryRegion {
    pub size: Vector,
    pub sharing: Option<Sharing>,
}

pickleable_new! {
    impl MemoryRegion {
        pub fn new(size: Vector, sharing: Option<Sharing>);
    }
}

/// How an instruction or sequence of instructions can access memory.
///
/// Each access is stored as the name of the region that was accessed.  We do not store the
/// individual indices that were accessed.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct MemoryAccesses {
    /// All memory regions these instructions can read from.
    pub reads: HashSet<String>,

    /// All memory regions these instructions can write to from within the processor.
    ///
    /// The "within the processor" clause indicates that this covers the write to the destination of
    /// a [`MOVE`][Instruction::Move], but not the write to the target of a
    /// [`MEASURE`][Instruction::Measurement].
    pub writes: HashSet<String>,

    /// All memory regions these instructions can write to from outside the processor.
    ///
    /// The "outside the processor" clause indicates that this covers the write to the target of a
    /// [`MEASURE`][Instruction::Measurement], but not the write to the destination of a
    /// [`MOVE`][Instruction::Move].
    pub captures: HashSet<String>,
}

impl MemoryAccesses {
    /// An empty set of memory accesses
    #[inline]
    pub fn none() -> Self {
        Self::default()
    }

    pub fn union(mut self, rhs: Self) -> Self {
        let Self {
            captures,
            reads,
            writes,
        } = rhs;
        self.captures.extend(captures);
        self.reads.extend(reads);
        self.writes.extend(writes);
        self
    }
}

/// Express a mode of memory access.
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum MemoryAccessType {
    /// Read from a memory location
    Read,

    /// Write to a memory location using classical instructions
    Write,

    /// Write to a memory location using readout (`CAPTURE` and `RAW-CAPTURE` instructions)
    Capture,
}

#[derive(Clone, PartialEq, Debug, thiserror::Error)]
pub enum MemoryAccessesError {
    #[error(transparent)]
    CallResolution(#[from] CallResolutionError),

    #[error("Instruction handler reported an error when constructing memory accesses: {0}")]
    InstructionHandlerError(String),
}

pub mod expression {
    // Contains an implementation of an iterator over all [`MemoryReference`]s read from by an
    // expression.  We construct a separate iterator here so we can avoid reifying a vector of
    // [`MemoryReference`]s that we will then immediately consume.

    use super::*;

    /// An iterator over all the memory references contained in an expression.
    #[derive(Clone, Debug)]
    pub struct MemoryReferences<'a> {
        pub(super) stack: Vec<&'a Expression>,
    }

    impl<'a> Iterator for MemoryReferences<'a> {
        type Item = &'a MemoryReference;

        fn next(&mut self) -> Option<Self::Item> {
            // If we imagine collecting into a vector, this function is roughly
            //
            // ```
            // fn collect_into(expr: &Expression, output: &mut Vec<&MemoryReference>) {
            //     match expr {
            //         Expression::NoReferences(_) => (),
            //         Expression::Address(reference) => output.push(reference),
            //         Expression::OneSubexpression(expression) => collect_into(expression, output),
            //         Expression::TwoSubexpressions(left, right) => {
            //             collect_into(left, output);
            //             collect_into(right, output);
            //         }
            //     }
            // }
            // ```
            //
            // In order to implement an iterator without allocating the whole vector, we still have
            // to reify the stack for the two-subexpression case; that's what `self.stack` is.
            //
            // We then implement this function with two loops.  The outer loop, `'stack_search`, is
            // our stack traversal; it finds the nearest stack frame.  The inner loop is effectively
            // our *tail calls*, where we don't need to allocate another stack frame; instead, we
            // assign to the current stack frame and keep going.  We don't have to write back to the
            // vector in the tail call case because no leaf node has more than one reference, so
            // once we've popped a stack frame off it'll fully bottom out.
            //
            // Note also that in the two-subexpression case we actually swap the order from the
            // `collect_into` example.  This is because `collect_into` reuses the same `output`, so
            // the state is preserved and `collect_into(right, output)` appends after `left`.
            // However, when we are emitting our results immediately, the tail call will happen
            // *before* the delayed stack frame.
            //
            // And yes, this would all be simpler with `gen` blocks.

            let Self { stack } = self;

            // Search through all parent expressions
            'stack_search: while let Some(mut expr) = stack.pop() {
                // An optimization for when there's only one child expression
                loop {
                    match expr {
                        // We're done with this expression and didn't find anything; time to walk down
                        // another tree branch, if there are any left.
                        Expression::Number(_)
                        | Expression::PiConstant()
                        | Expression::Variable(_) => continue 'stack_search,

                        // We're done with this expression and it was successful; stop iterating here,
                        // and when we return, walk down another tree branch if there are any left.
                        Expression::Address(reference) => return Some(reference),

                        // This expression only has one subexpression; we can avoid pushing a new
                        // "stack frame" and immediately popping it by overwriting the current stack
                        // frame.
                        Expression::FunctionCall(FunctionCallExpression {
                            expression,
                            function: _,
                        })
                        | Expression::Prefix(PrefixExpression {
                            expression,
                            operator: _,
                        }) => expr = expression,

                        // This expression has two subexpressions; we delay searching through the
                        // right child by pushing it on the stack, and "tail call" to search through
                        // the left child immediately as we did with the single-subexpression case
                        // above.
                        Expression::Infix(InfixExpression {
                            left,
                            right,
                            operator: _,
                        }) => {
                            stack.push(right);
                            expr = left;
                        }
                    }
                }
            }

            // We've finished our traversal; there are no subexpressions left.
            None
        }
    }

    impl std::iter::FusedIterator for MemoryReferences<'_> {}
}

impl Expression {
    /// Return an iterator over all the memory references contained within this expression.
    pub fn memory_references(&self) -> expression::MemoryReferences<'_> {
        expression::MemoryReferences { stack: vec![self] }
    }
}

impl WaveformInvocation {
    /// Return, if any, the memory references contained within this `WaveformInvocation`.
    pub fn memory_references(&self) -> impl std::iter::FusedIterator<Item = &MemoryReference> {
        self.parameters
            .values()
            .flat_map(Expression::memory_references)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use rstest::rstest;

    use crate::{
        expression::Expression,
        instruction::{
            ArithmeticOperand, Convert, DefaultHandler, Exchange, ExternSignatureMap,
            FrameIdentifier, Instruction, InstructionHandler as _, MemoryReference, Qubit,
            SetFrequency, ShiftFrequency, Store,
        },
        program::MemoryAccesses,
    };

    #[rstest]
    #[case(
        r#"
cis(func_ref[0]) ^
cos(func_ref[1]) +
exp(func_ref[2]) -
sin(func_ref[3]) /
sqrt(func_ref[4]) *

(infix_ref[0] ^ infix_ref[0]) ^
(infix_ref[1] + infix_ref[1]) +
(infix_ref[2] - infix_ref[2]) -
(infix_ref[3] / infix_ref[3]) /
(infix_ref[4] * infix_ref[4]) *

1.0 ^

pi +

(-prefix_ref) -

%variable
"#,
        &[
            ("func_ref", 0),
            ("func_ref", 1),
            ("func_ref", 2),
            ("func_ref", 3),
            ("func_ref", 4),
            ("infix_ref", 0),
            ("infix_ref", 0),
            ("infix_ref", 1),
            ("infix_ref", 1),
            ("infix_ref", 2),
            ("infix_ref", 2),
            ("infix_ref", 3),
            ("infix_ref", 3),
            ("infix_ref", 4),
            ("infix_ref", 4),
            ("prefix_ref", 0),
        ]
    )]
    fn expr_references(#[case] expr: &str, #[case] expected_refs: &[(&str, u64)]) {
        let expr = expr.replace('\n', " ").parse::<Expression>().unwrap();

        let computed_refs: Vec<_> = expr.memory_references().cloned().collect();

        let expected_refs: Vec<_> = expected_refs
            .iter()
            .map(|(name, index)| MemoryReference {
                name: (*name).to_owned(),
                index: *index,
            })
            .collect();

        assert_eq!(computed_refs, expected_refs);
    }

    #[rstest]
    #[case(
        Instruction::Store(Store {
            destination: "destination".to_string(),
            offset: MemoryReference {
                name: "offset".to_string(),
                index: Default::default()
            },
            source: ArithmeticOperand::MemoryReference(MemoryReference {
                name: "source".to_string(),
                index: Default::default()
            }),
        }),
        MemoryAccesses {
            captures: HashSet::new(),
            reads: ["source", "offset"].iter().cloned().map(String::from).collect(),
            writes: ["destination"].iter().cloned().map(String::from).collect(),
        }
    )]
    #[case(
        Instruction::Convert(Convert {
            destination: MemoryReference {
                name: "destination".to_string(),
                index: Default::default()
            },
            source: MemoryReference {
                name: "source".to_string(),
                index: Default::default()
            },
        }),
        MemoryAccesses {
            captures: HashSet::new(),
            reads: ["source"].iter().cloned().map(String::from).collect(),
            writes: ["destination"].iter().cloned().map(String::from).collect(),
        }
    )]
    #[case(
        Instruction::Exchange(Exchange {
            left: MemoryReference {
                name: "left".to_string(),
                index: Default::default()
            },
            right: MemoryReference {
                name: "right".to_string(),
                index: Default::default()
            },
        }),
        MemoryAccesses {
            captures: HashSet::new(),
            reads: ["left", "right"].iter().cloned().map(String::from).collect(),
            writes: ["left", "right"].iter().cloned().map(String::from).collect(),
        }
    )]
    #[case(
        Instruction::SetFrequency(SetFrequency {
            frequency: Expression::Address(MemoryReference {
                name: "frequency".to_string(),
                index: Default::default()
            }),
            frame: FrameIdentifier {
                name: "frame".to_string(),
                qubits: vec![Qubit::Fixed(0)]
            }
        }),
        MemoryAccesses {
            captures: HashSet::new(),
            reads: ["frequency"].iter().cloned().map(String::from).collect(),
            writes: HashSet::new(),
        }
    )]
    #[case(
        Instruction::ShiftFrequency(ShiftFrequency {
            frequency: Expression::Address(MemoryReference {
                name: "frequency".to_string(),
                index: Default::default()
            }),
            frame: FrameIdentifier {
                name: "frame".to_string(),
                qubits: vec![Qubit::Fixed(0)]
            }
        }),
        MemoryAccesses {
            captures: HashSet::new(),
            reads: ["frequency"].iter().cloned().map(String::from).collect(),
            writes: HashSet::new(),
        }
    )]
    fn test_instruction_accesses(
        #[case] instruction: Instruction,
        #[case] expected: MemoryAccesses,
    ) {
        let memory_accesses = DefaultHandler
            .memory_accesses(&ExternSignatureMap::default(), &instruction)
            .expect("must be able to get memory accesses");
        assert_eq!(memory_accesses.captures, expected.captures);
        assert_eq!(memory_accesses.reads, expected.reads);
        assert_eq!(memory_accesses.writes, expected.writes);
    }
}
