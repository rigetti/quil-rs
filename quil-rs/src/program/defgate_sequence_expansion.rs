/// This module implements the expansion of sequence gate definitions in a Quil program,
/// as well as the associated source map that tracks how the original
/// instructions map to the expanded instructions.
use std::{collections::HashMap, ops::Range, vec};

use indexmap::{IndexMap, IndexSet};

use crate::{
    expression::Expression,
    instruction::{
        DefGateSequenceExpansionError, GateDefinition, GateSignature, GateSpecification,
        Instruction,
    },
    program::{InstructionIndex, SourceMap, SourceMapEntry},
};

use super::source_map::{ExpansionResult, SourceMapIndexable};

/// Details about the expansion of a sequence gate definition
#[derive(Clone, Debug, PartialEq)]
pub struct DefGateSequenceExpansion<'a> {
    /// The signature of the sequence gate definition that was
    /// used to expand the instruction.
    ///
    /// Note, technically, the gate name itself is sufficient to identify
    /// the sequence gate definition, since gate names are unique within
    /// a program. Nevertheless, we include the full signature here
    /// for later reference within error messages.
    source_signature: crate::instruction::GateSignature<'a>,

    /// The target instruction indices produced by the expansion
    range: Range<InstructionIndex>,

    /// Sequence gate definitions may refer to other sequence gate definitions
    /// per the Quil specification. As such, we need to track how the first-level
    /// sequence instructions map to nested sequence gate definition expansion.
    nested_expansions: SourceMap<InstructionIndex, ExpansionResult<DefGateSequenceExpansion<'a>>>,
}

impl<'a> DefGateSequenceExpansion<'a> {
    /// Borrow the source gate signature of the sequence gate definition
    pub(crate) fn source_signature(&self) -> &GateSignature<'a> {
        &self.source_signature
    }

    /// Returns the range of target instruction indices produced by the expansion
    pub fn range(&self) -> &Range<InstructionIndex> {
        &self.range
    }

    /// Returns the nested expansions of this sequence gate definition
    pub fn nested_expansions(
        &self,
    ) -> &SourceMap<InstructionIndex, ExpansionResult<DefGateSequenceExpansion<'a>>> {
        &self.nested_expansions
    }
}

impl SourceMapIndexable<InstructionIndex> for DefGateSequenceExpansion<'_> {
    fn contains(&self, other: &InstructionIndex) -> bool {
        self.range.contains(other)
    }
}

impl<'a> SourceMapIndexable<GateSignature<'a>> for DefGateSequenceExpansion<'a> {
    fn contains(&self, other: &GateSignature) -> bool {
        &self.source_signature == other
    }
}

type SequenceGateDefinitionSourceMap<'a> =
    SourceMap<InstructionIndex, ExpansionResult<DefGateSequenceExpansion<'a>>>;

/// A utility to expand sequence gate definitions in a Quil program.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct ProgramDefGateSequenceExpander<'a, F> {
    gate_definitions: &'a IndexMap<String, GateDefinition>,
    filter: F,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct ExpandedInstructionsWithSourceMap<'a> {
    pub(crate) instructions: Vec<Instruction>,
    pub(crate) source_map: SequenceGateDefinitionSourceMap<'a>,
}

struct ExpansionStack(IndexSet<String>);

impl ExpansionStack {
    fn new() -> Self {
        Self(IndexSet::new())
    }

    /// Check if the name is in the stack and, if so, return an error.
    fn check(&self, name: impl AsRef<str>) -> Result<(), DefGateSequenceExpansionError> {
        if self.0.contains(name.as_ref()) {
            let cycle = self.0.iter().cloned().collect();
            Err(DefGateSequenceExpansionError::CyclicSequenceGateDefinition(
                cycle,
            ))
        } else {
            Ok(())
        }
    }

    /// Execute a closure with an gate added to the stack.
    fn with_gate_sequence<F, R>(&mut self, name: String, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let must_pop = self.0.insert(name);
        let result = f(self);
        if must_pop {
            self.0.pop();
        }
        result
    }
}

impl<'a, F> ProgramDefGateSequenceExpander<'a, F>
where
    F: Fn(&str) -> bool,
{
    /// Creates a new `ProgramDefGateSequenceExpander`.
    ///
    /// # Arguments
    ///
    /// * `gate_definitions` - A reference to the gate definitions of the program.
    /// * `filter` - A filter to apply to the gate definitions, allowing for selective
    ///   expansion.
    pub(crate) fn new(gate_definitions: &'a IndexMap<String, GateDefinition>, filter: F) -> Self {
        Self {
            gate_definitions,
            filter,
        }
    }

    /// Expands sequence gate definitions in the provided instructions.
    pub(crate) fn expand(
        &self,
        source_instructions: &[Instruction],
    ) -> Result<Vec<Instruction>, DefGateSequenceExpansionError> {
        self.expand_without_source_map_impl(source_instructions, &mut ExpansionStack::new())
    }

    /// Expands sequence gate definitions in the provided instructions and returns a source map
    /// detailing the expansion.
    pub(crate) fn expand_with_source_map(
        &self,
        source_instructions: &'a [Instruction],
    ) -> Result<ExpandedInstructionsWithSourceMap<'a>, DefGateSequenceExpansionError> {
        let mut source_map = SourceMap::default();
        self.expand_with_source_map_impl(
            source_instructions,
            &mut source_map,
            &mut ExpansionStack::new(),
        )
        .map(|instructions| ExpandedInstructionsWithSourceMap {
            instructions,
            source_map,
        })
    }

    fn expand_with_source_map_impl(
        &self,
        source_instructions: &[Instruction],
        source_map: &mut SequenceGateDefinitionSourceMap<'a>,
        stack: &mut ExpansionStack,
    ) -> Result<Vec<Instruction>, DefGateSequenceExpansionError> {
        let mut target_instructions = vec![];
        for (source_instruction_index, source_instruction) in source_instructions.iter().enumerate()
        {
            if let Some((target_gate_instructions, gate_sequence_signature)) =
                self.gate_sequence_from_instruction(source_instruction, stack)?
            {
                // If this instruction is a sequence gate definition, we need to expand it. Before
                // doing so, we add the gate sequence signature to the `gate_expansion_stack`,
                // so all nested expansions within this sequence have access to the stack of
                // already expanded gate definitions.
                let mut nested_expansions = SourceMap::default();
                let recursive_target_gate_instructions = stack.with_gate_sequence(
                    gate_sequence_signature.name().to_string(),
                    |stack| {
                        self.expand_with_source_map_impl(
                            &target_gate_instructions,
                            &mut nested_expansions,
                            stack,
                        )
                    },
                )?;

                let target_instruction_start_index = InstructionIndex(target_instructions.len());
                let target_instruction_end_index = InstructionIndex(
                    target_instruction_start_index.0 + recursive_target_gate_instructions.len(),
                );
                source_map.entries.push(SourceMapEntry {
                    source_location: InstructionIndex(source_instruction_index),
                    target_location: ExpansionResult::Rewritten(DefGateSequenceExpansion {
                        source_signature: gate_sequence_signature,
                        range: target_instruction_start_index..target_instruction_end_index,
                        nested_expansions,
                    }),
                });
                target_instructions.extend(recursive_target_gate_instructions);
            } else {
                target_instructions.push(source_instruction.clone());
                source_map.entries.push(SourceMapEntry {
                    source_location: InstructionIndex(source_instruction_index),
                    target_location: ExpansionResult::Unmodified(InstructionIndex(
                        target_instructions.len() - 1,
                    )),
                });
            }
        }
        Ok(target_instructions)
    }

    fn expand_without_source_map_impl(
        &self,
        source_instructions: &[Instruction],
        stack: &mut ExpansionStack,
    ) -> Result<Vec<Instruction>, DefGateSequenceExpansionError> {
        let mut target_instructions = vec![];
        for source_instruction in source_instructions {
            if let Some((target_gate_instructions, source)) =
                self.gate_sequence_from_instruction(source_instruction, stack)?
            {
                // If this instruction is a sequence gate definition, we need to expand it. Before
                // doing so, we add the gate sequence signature to the the `gate_expansion_stack`,
                // so all nested expansions within this sequence have access to the stack of
                // already expanded gate definitions.
                let recursive_target_gate_instructions = stack
                    .with_gate_sequence(source.name().to_string(), |stack| {
                        self.expand_without_source_map_impl(&target_gate_instructions, stack)
                    })?;
                target_instructions.extend(recursive_target_gate_instructions);
            } else {
                target_instructions.push(source_instruction.clone());
            }
        }
        Ok(target_instructions)
    }

    /// Given an instruction, this function checks if it is a gate instruction that
    /// matches a sequence gate definition. If it does and the gate name is included
    /// by the [`ProgramDefGateSequenceExpander::filter`], it expands the gate
    /// definition into a sequence of instructions and returns them along with the
    /// signature of the gate definition.
    ///
    /// This also checks the `gate_expansion_stack` set to prevent cyclic expansions.
    fn gate_sequence_from_instruction(
        &self,
        instruction: &Instruction,
        stack: &ExpansionStack,
    ) -> Result<Option<(Vec<Instruction>, GateSignature<'a>)>, DefGateSequenceExpansionError> {
        if let Instruction::Gate(gate) = instruction {
            if let Some(gate_definition) = self.gate_definitions.get(&gate.name) {
                if let GateSpecification::Sequence(gate_sequence) = &gate_definition.specification {
                    if (self.filter)(&gate.name) {
                        if gate_definition.parameters.len() != gate.parameters.len() {
                            return Err(DefGateSequenceExpansionError::ParameterCount {
                                expected: gate_definition.parameters.len(),
                                found: gate.parameters.len(),
                            });
                        }
                        let gate_parameter_arguments = gate_definition
                            .parameters
                            .iter()
                            .cloned()
                            .zip(gate.parameters.iter().cloned())
                            .collect::<HashMap<String, Expression>>();

                        if !gate.modifiers.is_empty() {
                            return Err(DefGateSequenceExpansionError::GateModifiersUnsupported(
                                gate.modifiers.clone(),
                            ));
                        }
                        let source = gate_definition.signature();
                        stack.check(source.name())?;

                        let target_gate_instructions = gate_sequence
                            .expand(gate_parameter_arguments, gate.qubits.clone())?
                            .into_iter()
                            .map(Instruction::Gate)
                            .collect::<Vec<_>>();

                        return Ok(Some((target_gate_instructions, source)));
                    }
                }
            }
        }
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use crate::{instruction::GateSignature, Program};

    use super::*;
    use rstest::*;

    /// A test case for the [`ProgramDefGateSequenceExpander`] functionality.
    struct DefGateSequenceExpansionTestCase {
        program: &'static str,
        filter: Box<dyn Fn(&str) -> bool>,
        expected: Result<&'static str, DefGateSequenceExpansionError>,
        source_map_entry_builders:
            Vec<SourceMapEntry<InstructionIndex, ExpansionResult<DefGateSequenceExpansionBuilder>>>,
    }

    /// Below we define a set of test cases for the `DefGateSequenceExpansion` functionality. We
    /// cover valid and invalid expansions.
    ///
    /// Note, the error coverage is comprehensive with the exception of
    /// [`DefGateSequenceExpansionError::UndefinedGateSequenceElementQubit`] and
    /// [`DefGateSequenceExpansionError::InvalidGateSequenceElementQubit`], which
    /// cover Quil errors that are impossible to parse or construct.
    impl DefGateSequenceExpansionTestCase {
        fn to_source_map(
            &self,
        ) -> SourceMap<InstructionIndex, ExpansionResult<DefGateSequenceExpansion<'_>>> {
            SourceMap {
                entries: self
                    .source_map_entry_builders
                    .iter()
                    .map(|builder| SourceMapEntry {
                        source_location: builder.source_location,
                        target_location: match &builder.target_location {
                            ExpansionResult::Rewritten(expansion) => expansion.build(),
                            ExpansionResult::Unmodified(index) => {
                                ExpansionResult::Unmodified(*index)
                            }
                        },
                    })
                    .collect(),
            }
        }

        fn simple_1q_expansions() -> Self {
            const QUIL: &str = r"
DEFGATE seq2(%param1, %param2) a b AS SEQUENCE:
    seq1(%param1) a
    seq1(%param2) b

DEFGATE seq1(%param1) a AS SEQUENCE:
    RZ(%param1) a
    RX(pi/2) a
    RZ(%param1) a

seq2(pi, pi/2) 0 1
";
            const EXPECTED_QUIL: &str = r"
RZ(pi) 0
RX(pi/2) 0
RZ(pi) 0
RZ(pi/2) 1
RX(pi/2) 1
RZ(pi/2) 1
";
            let source_map_entry_builders = vec![build_source_map_entry(
                0,
                DefGateSequenceExpansionBuilder::new(
                    "seq2",
                    &["param1", "param2"],
                    &["a", "b"],
                    0..6,
                    vec![
                        build_source_map_entry(
                            0,
                            DefGateSequenceExpansionBuilder::new(
                                "seq1",
                                &["param1"],
                                &["a"],
                                0..3,
                                vec![
                                    build_source_map_entry_copy(0, 0),
                                    build_source_map_entry_copy(1, 1),
                                    build_source_map_entry_copy(2, 2),
                                ],
                            ),
                        ),
                        build_source_map_entry(
                            1,
                            DefGateSequenceExpansionBuilder::new(
                                "seq1",
                                &["param1"],
                                &["a"],
                                3..6,
                                vec![
                                    build_source_map_entry_copy(0, 0),
                                    build_source_map_entry_copy(1, 1),
                                    build_source_map_entry_copy(2, 2),
                                ],
                            ),
                        ),
                    ],
                ),
            )];
            Self {
                program: QUIL,
                filter: Box::new(|_| true),
                expected: Ok(EXPECTED_QUIL),
                source_map_entry_builders,
            }
        }

        #[expect(clippy::too_many_lines)]
        fn triple_recursize() -> Self {
            const QUIL: &str = r"
DEFGATE some_u2_cycle(%param1, %param2, %param3, %param4, %param5, %param6) a b AS SEQUENCE:
    pmw3(%param1, %param2, %param3) a
    pmw3(%param4, %param5, %param6) b

DEFGATE pmw3(%param1, %param2, %param3) a AS SEQUENCE:
    pmw(%param1) a
    pmw(%param2) a
    pmw(%param3) a

DEFGATE pmw(%param1) a AS SEQUENCE:
    RZ(%param1) a
    RX(pi/2) a
    RZ(-%param1) a

some_u2_cycle(-pi, -pi/2, -pi/4, pi/4, pi/2, pi) 0 1
";
            const EXPECTED_QUIL: &str = r"
RZ(-pi) 0
RX(pi/2) 0
RZ(-(-pi)) 0
RZ(-pi/2) 0
RX(pi/2) 0
RZ(-(-pi/2)) 0
RZ(-pi/4) 0
RX(pi/2) 0
RZ(-(-pi/4)) 0

RZ(pi/4) 1
RX(pi/2) 1
RZ(-(pi/4)) 1
RZ(pi/2) 1
RX(pi/2) 1
RZ(-(pi/2)) 1
RZ(pi) 1
RX(pi/2) 1
RZ(-(pi)) 1
";
            let source_map_entry_builders = vec![build_source_map_entry(
                0,
                DefGateSequenceExpansionBuilder::new(
                    "some_u2_cycle",
                    &["param1", "param2", "param3", "param4", "param5", "param6"],
                    &["a", "b"],
                    0..18,
                    vec![
                        build_source_map_entry(
                            0,
                            DefGateSequenceExpansionBuilder::new(
                                "pmw3",
                                &["param1", "param2", "param3"],
                                &["a"],
                                0..9,
                                vec![
                                    build_source_map_entry(
                                        0,
                                        DefGateSequenceExpansionBuilder::new(
                                            "pmw",
                                            &["param1"],
                                            &["a"],
                                            0..3,
                                            vec![
                                                build_source_map_entry_copy(0, 0),
                                                build_source_map_entry_copy(1, 1),
                                                build_source_map_entry_copy(2, 2),
                                            ],
                                        ),
                                    ),
                                    build_source_map_entry(
                                        1,
                                        DefGateSequenceExpansionBuilder::new(
                                            "pmw",
                                            &["param1"],
                                            &["a"],
                                            3..6,
                                            vec![
                                                build_source_map_entry_copy(0, 0),
                                                build_source_map_entry_copy(1, 1),
                                                build_source_map_entry_copy(2, 2),
                                            ],
                                        ),
                                    ),
                                    build_source_map_entry(
                                        2,
                                        DefGateSequenceExpansionBuilder::new(
                                            "pmw",
                                            &["param1"],
                                            &["a"],
                                            6..9,
                                            vec![
                                                build_source_map_entry_copy(0, 0),
                                                build_source_map_entry_copy(1, 1),
                                                build_source_map_entry_copy(2, 2),
                                            ],
                                        ),
                                    ),
                                ],
                            ),
                        ),
                        build_source_map_entry(
                            1,
                            DefGateSequenceExpansionBuilder::new(
                                "pmw3",
                                &["param1", "param2", "param3"],
                                &["a"],
                                9..18,
                                vec![
                                    build_source_map_entry(
                                        0,
                                        DefGateSequenceExpansionBuilder::new(
                                            "pmw",
                                            &["param1"],
                                            &["a"],
                                            0..3,
                                            vec![
                                                build_source_map_entry_copy(0, 0),
                                                build_source_map_entry_copy(1, 1),
                                                build_source_map_entry_copy(2, 2),
                                            ],
                                        ),
                                    ),
                                    build_source_map_entry(
                                        1,
                                        DefGateSequenceExpansionBuilder::new(
                                            "pmw",
                                            &["param1"],
                                            &["a"],
                                            3..6,
                                            vec![
                                                build_source_map_entry_copy(0, 0),
                                                build_source_map_entry_copy(1, 1),
                                                build_source_map_entry_copy(2, 2),
                                            ],
                                        ),
                                    ),
                                    build_source_map_entry(
                                        2,
                                        DefGateSequenceExpansionBuilder::new(
                                            "pmw",
                                            &["param1"],
                                            &["a"],
                                            6..9,
                                            vec![
                                                build_source_map_entry_copy(0, 0),
                                                build_source_map_entry_copy(1, 1),
                                                build_source_map_entry_copy(2, 2),
                                            ],
                                        ),
                                    ),
                                ],
                            ),
                        ),
                    ],
                ),
            )];
            Self {
                program: QUIL,
                filter: Box::new(|_| true),
                expected: Ok(EXPECTED_QUIL),
                source_map_entry_builders,
            }
        }

        /// Test a program expansion where some instructions are not expanded
        fn unexpanded_instructions() -> Self {
            const QUIL: &str = r"
DEFGATE seq2(%param1, %param2) a b AS SEQUENCE:
    X a
    seq1(%param2) b
    H b
    ISWAP a b

DEFGATE seq1(%param1) a AS SEQUENCE:
    RZ(%param1) a
    RX(pi/2) a
    RZ(%param1) a

ISWAP 0 1
seq2(pi, pi/2) 0 1
MEASURE 0 ro[0]
MEASURE 1 ro[1]
";
            const EXPECTED_QUIL: &str = r"
ISWAP 0 1
X 0
RZ(pi/2) 1
RX(pi/2) 1
RZ(pi/2) 1
H 1
ISWAP 0 1
MEASURE 0 ro[0]
MEASURE 1 ro[1]
";
            let source_map_entry_builders = vec![
                build_source_map_entry(0, ExpansionResult::Unmodified(InstructionIndex(0))),
                build_source_map_entry(
                    1,
                    DefGateSequenceExpansionBuilder::new(
                        "seq2",
                        &["param1", "param2"],
                        &["a", "b"],
                        1..7,
                        vec![
                            build_source_map_entry_copy(0, 0),
                            build_source_map_entry(
                                1,
                                DefGateSequenceExpansionBuilder::new(
                                    "seq1",
                                    &["param1"],
                                    &["a"],
                                    1..4,
                                    vec![
                                        build_source_map_entry_copy(0, 0),
                                        build_source_map_entry_copy(1, 1),
                                        build_source_map_entry_copy(2, 2),
                                    ],
                                ),
                            ),
                            build_source_map_entry_copy(2, 4),
                            build_source_map_entry_copy(3, 5),
                        ],
                    ),
                ),
                build_source_map_entry_copy(2, 7),
                build_source_map_entry_copy(3, 8),
            ];
            Self {
                program: QUIL,
                filter: Box::new(|_| true),
                expected: Ok(EXPECTED_QUIL),
                source_map_entry_builders,
            }
        }

        /// Test that a sequence gate definition works even if one of the qubit parameters
        /// is not used in the expansion.
        ///
        /// This is not expressly forbidden by the Quil specification, so we include
        /// this test to document the behavior.
        fn unused_instruction() -> Self {
            const QUIL: &str = r"
DEFGATE seq1(%param1) a b AS SEQUENCE:
    RZ(%param1) a
    RX(pi/2) a
    RZ(%param1) a

seq1(pi) 0 1
";
            const EXPECTED_QUIL: &str = r"
RZ(pi) 0
RX(pi/2) 0
RZ(pi) 0
";
            let source_map_entry_builders = vec![build_source_map_entry(
                0,
                DefGateSequenceExpansionBuilder::new(
                    "seq1",
                    &["param1"],
                    &["a", "b"],
                    0..3,
                    vec![
                        build_source_map_entry_copy(0, 0),
                        build_source_map_entry_copy(1, 1),
                        build_source_map_entry_copy(2, 2),
                    ],
                ),
            )];
            Self {
                program: QUIL,
                filter: Box::new(|_| true),
                expected: Ok(EXPECTED_QUIL),
                source_map_entry_builders,
            }
        }

        /// Test sequence gate definition expansion within a program, where one or more
        /// sequence gate definitions are not included by the filter.
        fn filtered_sequence() -> Self {
            const QUIL: &str = r"
DEFGATE seq1(%param1) a AS SEQUENCE:
    RZ(%param1) a
    RX(pi/2) a
    RZ(%param1) a

DEFGATE seq2(%param1) a AS SEQUENCE:
    X a

seq1(pi) 0
seq2(pi/2) 0
";
            const EXPECTED_QUIL: &str = r"
RZ(pi) 0
RX(pi/2) 0
RZ(pi) 0
seq2(pi/2) 0
";
            let source_map_entry_builders = vec![
                build_source_map_entry(
                    0,
                    DefGateSequenceExpansionBuilder::new(
                        "seq1",
                        &["param1"],
                        &["a"],
                        0..3,
                        vec![
                            build_source_map_entry_copy(0, 0),
                            build_source_map_entry_copy(1, 1),
                            build_source_map_entry_copy(2, 2),
                        ],
                    ),
                ),
                build_source_map_entry_copy(1, 3),
            ];
            Self {
                program: QUIL,
                filter: Box::new(|k| k == "seq1"),
                expected: Ok(EXPECTED_QUIL),
                source_map_entry_builders,
            }
        }

        fn error_parameter_count() -> Self {
            const QUIL: &str = r"
DEFGATE seq1(%param1) a AS SEQUENCE:
    RZ(%param1) a
seq1() 0
";
            let expected = Err(DefGateSequenceExpansionError::ParameterCount {
                expected: 1,
                found: 0,
            });
            Self {
                program: QUIL,
                filter: Box::new(|_| true),
                expected,
                source_map_entry_builders: vec![],
            }
        }

        fn error_cyclic_sequence_gate_definition() -> Self {
            const QUIL: &str = r"
DEFGATE seq1(%param1) a AS SEQUENCE:
    seq2(%param1) a

DEFGATE seq2(%param1) a AS SEQUENCE:
    seq3(%param1) a

DEFGATE seq3(%param1) a AS SEQUENCE:
    seq1(%param1) a

seq1(pi) 0
";
            let expected = Err(DefGateSequenceExpansionError::CyclicSequenceGateDefinition(
                vec!["seq1".to_string(), "seq2".to_string(), "seq3".to_string()],
            ));
            Self {
                program: QUIL,
                filter: Box::new(|_| true),
                expected,
                source_map_entry_builders: vec![],
            }
        }

        fn error_qubit_count() -> Self {
            const QUIL: &str = r"
DEFGATE seq1(%param1) a AS SEQUENCE:
    RZ(%param1) a

seq1(pi/2) 0 1
";
            let expected = Err(DefGateSequenceExpansionError::QubitCount {
                expected: 1,
                found: 2,
            });
            Self {
                program: QUIL,
                filter: Box::new(|_| true),
                expected,
                source_map_entry_builders: vec![],
            }
        }

        fn error_gate_qubit_argument() -> Self {
            const QUIL: &str = r"
DEFGATE seq1(%param1) a AS SEQUENCE:
    RZ(%param1) a

seq1(pi/2) %q1
";
            let expected = Err(DefGateSequenceExpansionError::NonFixedQubitArgument(
                crate::instruction::Qubit::Variable("q1".to_string()),
            ));
            Self {
                program: QUIL,
                filter: Box::new(|_| true),
                expected,
                source_map_entry_builders: vec![],
            }
        }

        fn error_gate_modifiers_unsupported() -> Self {
            const QUIL: &str = r"
DEFGATE seq1(%param1) a AS SEQUENCE:
    RZ(%param1) a


DAGGER seq1(pi/2) 0
";
            let expected = Err(DefGateSequenceExpansionError::GateModifiersUnsupported(
                vec![crate::instruction::GateModifier::Dagger],
            ));
            Self {
                program: QUIL,
                filter: Box::new(|_| true),
                expected,
                source_map_entry_builders: vec![],
            }
        }
    }

    #[rstest]
    #[case::simple_1q_expansions(DefGateSequenceExpansionTestCase::simple_1q_expansions())]
    #[case::triple_recursize(DefGateSequenceExpansionTestCase::triple_recursize())]
    #[case::unexpanded_instructions(DefGateSequenceExpansionTestCase::unexpanded_instructions())]
    #[case::unused_instruction(DefGateSequenceExpansionTestCase::unused_instruction())]
    #[case::filtered_sequence(DefGateSequenceExpansionTestCase::filtered_sequence())]
    #[case::error_qubit_count(DefGateSequenceExpansionTestCase::error_qubit_count())]
    #[case::error_gate_qubit_argument(DefGateSequenceExpansionTestCase::error_parameter_count())]
    #[case::error_gate_qubit_argument(DefGateSequenceExpansionTestCase::error_gate_qubit_argument())]
    #[case::error_gate_modifiers_unsupported(
        DefGateSequenceExpansionTestCase::error_gate_modifiers_unsupported()
    )]
    #[case::error_cyclic_sequence_gate_definition(
        DefGateSequenceExpansionTestCase::error_cyclic_sequence_gate_definition()
    )]
    fn test_defgate_sequence_expansion(#[case] test_case: DefGateSequenceExpansionTestCase) {
        let program =
            crate::Program::from_str(test_case.program).expect("must be a valid Quil program");
        let program_expansion = ProgramDefGateSequenceExpander {
            gate_definitions: &program.gate_definitions,
            filter: &test_case.filter,
        };
        let result = program_expansion.expand_with_source_map(&program.instructions);

        match (&test_case.expected, result) {
            (Ok(expected), Ok(result)) => {
                let expected_program =
                    Program::from_str(expected).expect("expected program must be valid Quil");
                let mut actual_program = Program::new();
                actual_program.add_instructions(result.instructions);

                pretty_assertions::assert_eq!(expected_program, actual_program);
                pretty_assertions::assert_eq!(test_case.to_source_map(), result.source_map);

                let actual_program_without_source_map = Program::from_instructions(
                    program_expansion
                        .expand(&program.instructions)
                        .expect("expansion without source map should succeed"),
                );
                pretty_assertions::assert_eq!(expected_program, actual_program_without_source_map);
            }
            (Ok(expected), Err(e)) => {
                panic!("Expected instructions:\n\n{expected:?}\n\ngot error:\n\n{e:?}");
            }
            (Err(expected), Ok(result)) => {
                panic!(
                    "Expected error:\n\n{expected:?}\n\ngot:\n\n{:?}",
                    result.instructions
                );
            }
            (Err(expected), Err(found)) => {
                pretty_assertions::assert_eq!(*expected, found);
            }
        }
    }

    struct GateSignatureBuilder {
        gate_name: String,
        gate_parameters: Vec<String>,
        gate_qubits: Vec<String>,
    }

    impl GateSignatureBuilder {
        fn new(
            gate_name: &'static str,
            gate_parameters: &'static [&'static str],
            gate_qubits: &'static [&'static str],
        ) -> Self {
            Self {
                gate_name: gate_name.to_string(),
                gate_parameters: gate_parameters.iter().map(|&s| s.to_string()).collect(),
                gate_qubits: gate_qubits.iter().map(|&s| s.to_string()).collect(),
            }
        }

        fn build(&self) -> GateSignature<'_> {
            GateSignature::try_new(
                &self.gate_name,
                &self.gate_parameters,
                &self.gate_qubits,
                crate::instruction::GateType::Sequence,
            )
            .expect("must be a valid gate signature")
        }
    }

    struct DefGateSequenceExpansionBuilder {
        signature: GateSignatureBuilder,
        range: Range<usize>,
        nested_expansions:
            Vec<SourceMapEntry<InstructionIndex, ExpansionResult<DefGateSequenceExpansionBuilder>>>,
    }

    impl DefGateSequenceExpansionBuilder {
        fn new(
            gate_name: &'static str,
            gate_parameters: &'static [&'static str],
            gate_qubits: &'static [&'static str],
            range: Range<usize>,
            entries: Vec<
                SourceMapEntry<InstructionIndex, ExpansionResult<DefGateSequenceExpansionBuilder>>,
            >,
        ) -> ExpansionResult<Self> {
            ExpansionResult::Rewritten(Self {
                signature: GateSignatureBuilder::new(gate_name, gate_parameters, gate_qubits),
                range,
                nested_expansions: entries,
            })
        }

        fn build(&self) -> ExpansionResult<DefGateSequenceExpansion<'_>> {
            let entries: Vec<_> = self
                .nested_expansions
                .iter()
                .map(|entry| SourceMapEntry {
                    source_location: entry.source_location,
                    target_location: match entry.target_location() {
                        ExpansionResult::Rewritten(expansion) => expansion.build(),
                        ExpansionResult::Unmodified(index) => ExpansionResult::Unmodified(*index),
                    },
                })
                .collect();
            ExpansionResult::Rewritten(DefGateSequenceExpansion {
                source_signature: self.signature.build(),
                range: InstructionIndex(self.range.start)..InstructionIndex(self.range.end),
                nested_expansions: SourceMap { entries },
            })
        }
    }

    fn build_source_map_entry(
        source_location: usize,
        target_location: ExpansionResult<DefGateSequenceExpansionBuilder>,
    ) -> SourceMapEntry<InstructionIndex, ExpansionResult<DefGateSequenceExpansionBuilder>> {
        SourceMapEntry {
            source_location: InstructionIndex(source_location),
            target_location,
        }
    }

    fn build_source_map_entry_copy(
        source_location: usize,
        target_location: usize,
    ) -> SourceMapEntry<InstructionIndex, ExpansionResult<DefGateSequenceExpansionBuilder>> {
        build_source_map_entry(
            source_location,
            ExpansionResult::Unmodified(InstructionIndex(target_location)),
        )
    }
}
