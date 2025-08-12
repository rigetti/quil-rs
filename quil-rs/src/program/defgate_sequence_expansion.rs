/// This module implements the expansion of sequence gate definitions in a Quil program,
/// as well as the associated source map that tracks how the original
/// instructions map to the expanded instructions.
use std::{collections::HashMap, ops::Range};

use indexmap::{IndexMap, IndexSet};

use crate::{
    expression::Expression,
    instruction::{
        DefGateSequenceExpansionError, GateDefinition, GateSignature, GateSpecification,
        Instruction,
    },
    program::{InstructionIndex, SourceMap, SourceMapEntry},
};

use super::source_map::{InstructionTarget, SourceMapIndexable};

/// Details about the expansion of a sequence gate definition
#[derive(Clone, Debug, PartialEq)]
pub struct DefGateSequenceExpansion {
    /// The signature of the sequence gate definition that was
    /// used to expand the instruction.
    ///
    /// Note, technically, the gate name itself is sufficient to identify
    /// the sequence gate definition, since gate names are unique within
    /// a program. Nevertheless, we include the full signature here
    /// for later reference within error messages.
    defgate_sequence_source: crate::instruction::GateSignature,

    /// The target instruction indices produced by the expansion
    range: Range<InstructionIndex>,

    /// Sequence gate definitions may refer to other sequence gate definitions
    /// per the Quil specification. As such, we need to track how the first-level
    /// sequence instructions map to nested sequence gate definition expansion.
    nested_expansions: SourceMap<InstructionIndex, InstructionTarget<DefGateSequenceExpansion>>,
}

impl DefGateSequenceExpansion {
    /// Returns the source gate signature of the sequence gate definition
    pub fn defgate_sequence_source(&self) -> &GateSignature {
        &self.defgate_sequence_source
    }

    /// Returns the range of target instruction indices produced by the expansion
    pub fn range(&self) -> &Range<InstructionIndex> {
        &self.range
    }

    /// Returns the nested expansions of this sequence gate definition
    pub fn nested_expansions(
        &self,
    ) -> &SourceMap<InstructionIndex, InstructionTarget<DefGateSequenceExpansion>> {
        &self.nested_expansions
    }
}

impl SourceMapIndexable<InstructionIndex> for DefGateSequenceExpansion {
    fn intersects(&self, other: &InstructionIndex) -> bool {
        self.range.contains(other)
    }
}

impl SourceMapIndexable<GateSignature> for DefGateSequenceExpansion {
    fn intersects(&self, other: &GateSignature) -> bool {
        &self.defgate_sequence_source == other
    }
}

type SequenceGateDefinitionSourceMap =
    SourceMap<InstructionIndex, InstructionTarget<DefGateSequenceExpansion>>;

/// A utility to expand sequence gate definitions in a Quil program.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct ProgramDefGateSequenceExpander<'program, F> {
    gate_definitions: &'program IndexMap<String, GateDefinition>,
    filter: F,
}

impl<'program, F> ProgramDefGateSequenceExpander<'program, F>
where
    F: Fn(&String) -> bool,
{
    /// Creates a new `ProgramDefGateSequenceExpander`.
    ///
    /// # Arguments
    ///
    /// * `gate_definitions` - A reference to the gate definitions of the program.
    /// * `filter` - A filter to apply to the gate definitions, allowing for selective
    ///   expansion.
    pub(crate) fn new(
        gate_definitions: &'program IndexMap<String, GateDefinition>,
        filter: F,
    ) -> Self {
        Self {
            gate_definitions,
            filter,
        }
    }

    /// Expands sequence gate definitions in the provided instructions.
    pub(crate) fn expand_defgate_sequences(
        &self,
        source_instructions: &[Instruction],
    ) -> Result<Vec<Instruction>, DefGateSequenceExpansionError> {
        self.expand_defgate_sequences_without_source_map_inner(
            source_instructions,
            &mut IndexSet::new(),
        )
    }

    /// Expands sequence gate definitions in the provided instructions and returns a source map
    /// detailing the expansion.
    pub(crate) fn expand_defgate_sequences_with_source_map(
        &self,
        source_instructions: &[Instruction],
    ) -> Result<(Vec<Instruction>, SequenceGateDefinitionSourceMap), DefGateSequenceExpansionError>
    {
        let mut source_map = SourceMap::default();
        self.expand_defgate_sequences_with_source_map_inner(
            source_instructions,
            &mut source_map,
            &mut IndexSet::new(),
        )
        .map(|instructions| (instructions, source_map))
    }

    fn expand_defgate_sequences_with_source_map_inner(
        &self,
        source_instructions: &[Instruction],
        source_map: &mut SequenceGateDefinitionSourceMap,
        seen: &mut IndexSet<String>,
    ) -> Result<Vec<Instruction>, DefGateSequenceExpansionError> {
        let mut target_instructions = vec![];
        for (source_instruction_index, source_instruction) in source_instructions.iter().enumerate()
        {
            if let Some((target_gate_instructions, source)) =
                self.gate_sequence_from_instruction(source_instruction, seen)?
            {
                let mut seen = seen.clone();
                seen.insert(source.name().to_string());

                let mut recursive_source_map = SourceMap::default();
                let recursive_target_gate_instructions = self
                    .expand_defgate_sequences_with_source_map_inner(
                        &target_gate_instructions,
                        &mut recursive_source_map,
                        &mut seen,
                    )?;
                let target_instruction_start_index = InstructionIndex(target_instructions.len());
                let target_instruction_end_index = InstructionIndex(
                    target_instruction_start_index.0 + recursive_target_gate_instructions.len(),
                );
                source_map.entries.push(SourceMapEntry {
                    source_location: InstructionIndex(source_instruction_index),
                    target_location: InstructionTarget::Rewrite(DefGateSequenceExpansion {
                        defgate_sequence_source: source,
                        range: target_instruction_start_index..target_instruction_end_index,
                        nested_expansions: recursive_source_map,
                    }),
                });
                target_instructions.extend(recursive_target_gate_instructions);
            } else {
                target_instructions.push(source_instruction.clone());
                source_map.entries.push(SourceMapEntry {
                    source_location: InstructionIndex(source_instruction_index),
                    target_location: InstructionTarget::Copied(InstructionIndex(
                        target_instructions.len() - 1,
                    )),
                });
            }
        }
        Ok(target_instructions)
    }

    fn expand_defgate_sequences_without_source_map_inner(
        &self,
        source_instructions: &[Instruction],
        seen: &mut IndexSet<String>,
    ) -> Result<Vec<Instruction>, DefGateSequenceExpansionError> {
        let mut target_instructions = vec![];
        for source_instruction in source_instructions.iter() {
            if let Some((target_gate_instructions, source)) =
                self.gate_sequence_from_instruction(source_instruction, seen)?
            {
                let mut seen = seen.clone();
                seen.insert(source.name().to_string());

                let recursive_target_gate_instructions = self
                    .expand_defgate_sequences_without_source_map_inner(
                        &target_gate_instructions,
                        &mut seen,
                    )?;
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
    /// This also checks the `seen` set to prevent cyclic expansions.
    fn gate_sequence_from_instruction(
        &self,
        instruction: &Instruction,
        seen: &mut IndexSet<String>,
    ) -> Result<Option<(Vec<Instruction>, GateSignature)>, DefGateSequenceExpansionError> {
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
                        let source = GateSignature::from(gate_definition);
                        if seen.contains(source.name()) {
                            let cycle = seen.iter().cloned().collect();
                            return Err(
                                DefGateSequenceExpansionError::CyclicSequenceGateDefinition(cycle),
                            );
                        }

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
        filter: Box<dyn Fn(&String) -> bool>,
        expected: Result<&'static str, DefGateSequenceExpansionError>,
        expected_source_map:
            SourceMap<InstructionIndex, InstructionTarget<DefGateSequenceExpansion>>,
    }

    /// Below we define a set of test cases for the `DefGateSequenceExpansion` functionality. We
    /// cover valid and invalid expansions.
    ///
    /// Note, the error coverage is comprehensive with the exception of
    /// [`DefGateSequenceExpansionError::UndefinedGateSequenceElementQubit`] and
    /// [`DefGateSequenceExpansionError::InvalidGateSequenceElementQubit`], which
    /// cover Quil errors that are impossible to parse or construct.
    impl DefGateSequenceExpansionTestCase {
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
RZ(pi) 0 # (0, 0, 0)
RX(pi/2) 0 # (0, 0, 1)
RZ(pi) 0 # (0, 0, 2)
RZ(pi/2) 1 # (0, 1, 0)
RX(pi/2) 1 # (0, 1, 1)
RZ(pi/2) 1 # (0, 1, 2)
";
            let expected_source_map = SourceMap {
                entries: vec![build_source_map_entry(
                    0,
                    build_defgate_sequence_expansion(
                        "seq2",
                        &["param1", "param2"],
                        &["a", "b"],
                        0..6,
                        vec![
                            build_source_map_entry(
                                0,
                                build_defgate_sequence_expansion(
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
                                build_defgate_sequence_expansion(
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
                )],
            };
            Self {
                program: QUIL,
                filter: Box::new(|_| true),
                expected: Ok(EXPECTED_QUIL),
                expected_source_map,
            }
        }

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
            let expected_source_map = SourceMap {
                entries: vec![build_source_map_entry(
                    0,
                    build_defgate_sequence_expansion(
                        "some_u2_cycle",
                        &["param1", "param2", "param3", "param4", "param5", "param6"],
                        &["a", "b"],
                        0..18,
                        vec![
                            build_source_map_entry(
                                0,
                                build_defgate_sequence_expansion(
                                    "pmw3",
                                    &["param1", "param2", "param3"],
                                    &["a"],
                                    0..9,
                                    vec![
                                        build_source_map_entry(
                                            0,
                                            build_defgate_sequence_expansion(
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
                                            build_defgate_sequence_expansion(
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
                                            build_defgate_sequence_expansion(
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
                                build_defgate_sequence_expansion(
                                    "pmw3",
                                    &["param1", "param2", "param3"],
                                    &["a"],
                                    9..18,
                                    vec![
                                        build_source_map_entry(
                                            0,
                                            build_defgate_sequence_expansion(
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
                                            build_defgate_sequence_expansion(
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
                                            build_defgate_sequence_expansion(
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
                )],
            };
            Self {
                program: QUIL,
                filter: Box::new(|_| true),
                expected: Ok(EXPECTED_QUIL),
                expected_source_map,
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
            let expected_source_map = SourceMap {
                entries: vec![
                    build_source_map_entry(0, InstructionTarget::Copied(InstructionIndex(0))),
                    build_source_map_entry(
                        1,
                        build_defgate_sequence_expansion(
                            "seq2",
                            &["param1", "param2"],
                            &["a", "b"],
                            1..7,
                            vec![
                                build_source_map_entry_copy(0, 0),
                                build_source_map_entry(
                                    1,
                                    build_defgate_sequence_expansion(
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
                ],
            };
            Self {
                program: QUIL,
                filter: Box::new(|_| true),
                expected: Ok(EXPECTED_QUIL),
                expected_source_map,
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
            let expected_source_map = SourceMap {
                entries: vec![build_source_map_entry(
                    0,
                    build_defgate_sequence_expansion(
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
                )],
            };
            Self {
                program: QUIL,
                filter: Box::new(|_| true),
                expected: Ok(EXPECTED_QUIL),
                expected_source_map,
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
            let expected_source_map = SourceMap {
                entries: vec![
                    build_source_map_entry(
                        0,
                        build_defgate_sequence_expansion(
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
                ],
            };
            Self {
                program: QUIL,
                filter: Box::new(|k| k == "seq1"),
                expected: Ok(EXPECTED_QUIL),
                expected_source_map,
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
                expected_source_map: SourceMap::default(),
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
                expected_source_map: SourceMap::default(),
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
                expected_source_map: SourceMap::default(),
            }
        }

        fn error_gate_qubit_argument() -> Self {
            const QUIL: &str = r"
DEFGATE seq1(%param1) a AS SEQUENCE:
    RZ(%param1) a

seq1(pi/2) %q1
";
            let expected = Err(DefGateSequenceExpansionError::GateQubitArugment(
                crate::instruction::Qubit::Variable("q1".to_string()),
            ));
            Self {
                program: QUIL,
                filter: Box::new(|_| true),
                expected,
                expected_source_map: SourceMap::default(),
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
                expected_source_map: SourceMap::default(),
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
            filter: test_case.filter,
        };
        let result =
            program_expansion.expand_defgate_sequences_with_source_map(&program.instructions);

        match (test_case.expected, result) {
            (Ok(expected), Ok((actual, source_map))) => {
                let expected_program =
                    Program::from_str(expected).expect("expected program must be valid Quil");
                let mut actual_program = Program::new();
                actual_program.add_instructions(actual);

                pretty_assertions::assert_eq!(expected_program, actual_program);
                pretty_assertions::assert_eq!(test_case.expected_source_map, source_map);

                let actual_program_without_source_map = Program::from_instructions(
                    program_expansion
                        .expand_defgate_sequences(&program.instructions)
                        .expect("expansion without source map should succeed"),
                );
                pretty_assertions::assert_eq!(expected_program, actual_program_without_source_map);
            }
            (Ok(expected), Err(e)) => {
                panic!("Expected instructions:\n\n{expected:?}\n\ngot error:\n\n{e:?}");
            }
            (Err(expected), Ok((actual, _))) => {
                panic!("Expected error:\n\n{expected:?}\n\ngot:\n\n{actual:?}");
            }
            (Err(expected), Err(found)) => {
                pretty_assertions::assert_eq!(expected, found);
            }
        }
    }

    fn build_gate_signature(
        gate_name: &'static str,
        gate_parameters: &'static [&'static str],
        gate_qubits: &'static [&'static str],
    ) -> GateSignature {
        GateSignature::try_new(
            gate_name.to_string(),
            gate_parameters.iter().map(|s| s.to_string()).collect(),
            gate_qubits.iter().map(|s| s.to_string()).collect(),
            crate::instruction::GateType::Sequence,
        )
        .expect("must be a valid gate")
    }

    fn build_defgate_sequence_expansion(
        gate_name: &'static str,
        gate_parameters: &'static [&'static str],
        gate_qubits: &'static [&'static str],
        range: Range<usize>,
        entries: Vec<SourceMapEntry<InstructionIndex, InstructionTarget<DefGateSequenceExpansion>>>,
    ) -> InstructionTarget<DefGateSequenceExpansion> {
        InstructionTarget::Rewrite(DefGateSequenceExpansion {
            defgate_sequence_source: build_gate_signature(gate_name, gate_parameters, gate_qubits),
            range: InstructionIndex(range.start)..InstructionIndex(range.end),
            nested_expansions: SourceMap { entries },
        })
    }

    fn build_source_map_entry(
        source_location: usize,
        target_location: InstructionTarget<DefGateSequenceExpansion>,
    ) -> SourceMapEntry<InstructionIndex, InstructionTarget<DefGateSequenceExpansion>> {
        SourceMapEntry {
            source_location: InstructionIndex(source_location),
            target_location,
        }
    }

    fn build_source_map_entry_copy(
        source_location: usize,
        target_location: usize,
    ) -> SourceMapEntry<InstructionIndex, InstructionTarget<DefGateSequenceExpansion>> {
        build_source_map_entry(
            source_location,
            InstructionTarget::Copied(InstructionIndex(target_location)),
        )
    }
}
