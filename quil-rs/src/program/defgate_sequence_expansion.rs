use std::{collections::HashMap, ops::Range};

use indexmap::{IndexMap, IndexSet};

use crate::{
    expression::Expression,
    filter_set::Filter,
    instruction::{
        DefGateSequenceExpansionError, GateDefinition, GateSignature, GateSpecification,
        Instruction,
    },
    program::{InstructionIndex, SourceMap, SourceMapEntry},
};

use super::source_map::{InstructionSourceMap, InstructionTarget, SourceMapIndexable};

/// Details about the expansion of a calibration
#[derive(Clone, Debug, PartialEq)]
pub struct DefGateSequenceExpansion {
    /// The calibration used to expand the instruction
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

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct ProgramDefGateSequenceExpander<'program> {
    gate_definitions: &'program IndexMap<String, GateDefinition>,
    filter: Filter<String>,
}

impl<'program> ProgramDefGateSequenceExpander<'program> {
    pub(crate) fn new(
        gate_definitions: &'program IndexMap<String, GateDefinition>,
        filter: Filter<String>,
    ) -> Self {
        Self {
            gate_definitions,
            filter,
        }
    }

    pub(crate) fn expand_defgate_sequences(
        &self,
        source_instructions: &[Instruction],
    ) -> Result<Vec<Instruction>, DefGateSequenceExpansionError> {
        self.expand_defgate_sequences_without_source_map_inner(
            source_instructions,
            &mut IndexSet::new(),
        )
    }

    pub(crate) fn expand_defgate_sequences_with_source_map(
        &self,
        source_instructions: &[Instruction],
    ) -> Result<(Vec<Instruction>, InstructionSourceMap), DefGateSequenceExpansionError> {
        let mut source_map = SourceMap::default();
        self.expand_defgate_sequences_with_source_map_inner(
            source_instructions,
            &mut source_map,
            &mut IndexSet::new(),
        )
        .map(|instructions| (instructions, InstructionSourceMap::from(source_map)))
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
                self.gate_sequence_from_instruction(seen, source_instruction)?
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
                self.gate_sequence_from_instruction(seen, source_instruction)?
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

    fn gate_sequence_from_instruction(
        &self,
        seen: &mut IndexSet<String>,
        instruction: &Instruction,
    ) -> Result<Option<(Vec<Instruction>, GateSignature)>, DefGateSequenceExpansionError> {
        if let Instruction::Gate(gate) = instruction {
            if let Some(gate_definition) = self.gate_definitions.get(&gate.name) {
                if let GateSpecification::Sequence(gate_sequence) = &gate_definition.specification {
                    if self.filter.include(&gate.name) {
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

    struct CompactGateSignature(
        &'static str,
        &'static [&'static str],
        &'static [&'static str],
    );

    impl From<CompactGateSignature> for GateSignature {
        fn from(value: CompactGateSignature) -> Self {
            GateSignature::try_new(
                value.0.to_string(),
                value.1.iter().map(|s| s.to_string()).collect(),
                value.2.iter().map(|s| s.to_string()).collect(),
                crate::instruction::GateType::Sequence,
            )
            .expect("must be a valid gate")
        }
    }

    struct CompactDefGateSequenceExpansion {
        defgate_sequence_source: (
            &'static str,
            &'static [&'static str],
            &'static [&'static str],
        ),
        range: Range<usize>,
        expansions: Vec<CompactSourceMapEntry>,
    }

    impl From<CompactDefGateSequenceExpansion> for DefGateSequenceExpansion {
        fn from(value: CompactDefGateSequenceExpansion) -> Self {
            DefGateSequenceExpansion {
                defgate_sequence_source: GateSignature::from(CompactGateSignature(
                    value.defgate_sequence_source.0,
                    value.defgate_sequence_source.1,
                    value.defgate_sequence_source.2,
                )),
                range: InstructionIndex(value.range.start)..InstructionIndex(value.range.end),
                nested_expansions: SourceMap {
                    entries: value
                        .expansions
                        .into_iter()
                        .map(|entry| SourceMapEntry {
                            source_location: InstructionIndex(entry.source_location),
                            target_location: match entry.target_location {
                                InstructionTarget::Copied(index) => {
                                    InstructionTarget::Copied(InstructionIndex(index.0))
                                }
                                InstructionTarget::Rewrite(expansion) => {
                                    InstructionTarget::Rewrite(expansion.into())
                                }
                            },
                        })
                        .collect(),
                },
            }
        }
    }

    struct CompactSourceMapEntry {
        source_location: usize,
        target_location: InstructionTarget<CompactDefGateSequenceExpansion>,
    }

    impl From<CompactSourceMapEntry>
        for SourceMapEntry<InstructionIndex, InstructionTarget<DefGateSequenceExpansion>>
    {
        fn from(value: CompactSourceMapEntry) -> Self {
            SourceMapEntry {
                source_location: InstructionIndex(value.source_location),
                target_location: match value.target_location {
                    InstructionTarget::Copied(index) => {
                        InstructionTarget::Copied(InstructionIndex(index.0))
                    }
                    InstructionTarget::Rewrite(expansion) => {
                        InstructionTarget::Rewrite(expansion.into())
                    }
                },
            }
        }
    }

    struct DefGateSequenceExpansionTestCase {
        program: &'static str,
        filter: Filter<String>,
        expected: Result<&'static str, DefGateSequenceExpansionError>,
        expected_source_map:
            SourceMap<InstructionIndex, InstructionTarget<DefGateSequenceExpansion>>,
    }

    /// Below we define a set of test cases for the `DefGateSequenceExpansion` functionality. We
    /// cover valid and invalid expansions.
    ///
    /// Note, the error coverage is comprehensive with the exception of
    /// [`DefGateSequenceExpansionError::UndefinedGateSequenceElementQubit`] and
    /// [`DefGateSequenceExpansionError::InvalidGateSequenceElementQubit`], which should be
    /// impossible to parse or construct.
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
                entries: vec![CompactSourceMapEntry {
                    source_location: 0,
                    target_location: InstructionTarget::Rewrite(CompactDefGateSequenceExpansion {
                        defgate_sequence_source: ("seq2", &["param1", "param2"], &["a", "b"]),
                        range: 0..6,
                        expansions: vec![
                            CompactSourceMapEntry {
                                source_location: 0,
                                target_location: InstructionTarget::Rewrite(
                                    CompactDefGateSequenceExpansion {
                                        defgate_sequence_source: ("seq1", &["param1"], &["a"]),
                                        range: 0..3,
                                        expansions: vec![
                                            CompactSourceMapEntry {
                                                source_location: 0,
                                                target_location: InstructionTarget::Copied(
                                                    InstructionIndex(0),
                                                ),
                                            },
                                            CompactSourceMapEntry {
                                                source_location: 1,
                                                target_location: InstructionTarget::Copied(
                                                    InstructionIndex(1),
                                                ),
                                            },
                                            CompactSourceMapEntry {
                                                source_location: 2,
                                                target_location: InstructionTarget::Copied(
                                                    InstructionIndex(2),
                                                ),
                                            },
                                        ],
                                    },
                                ),
                            },
                            CompactSourceMapEntry {
                                source_location: 1,
                                target_location: InstructionTarget::Rewrite(
                                    CompactDefGateSequenceExpansion {
                                        defgate_sequence_source: ("seq1", &["param1"], &["a"]),
                                        range: 3..6,
                                        expansions: vec![
                                            CompactSourceMapEntry {
                                                source_location: 0,
                                                target_location: InstructionTarget::Copied(
                                                    InstructionIndex(0),
                                                ),
                                            },
                                            CompactSourceMapEntry {
                                                source_location: 1,
                                                target_location: InstructionTarget::Copied(
                                                    InstructionIndex(1),
                                                ),
                                            },
                                            CompactSourceMapEntry {
                                                source_location: 2,
                                                target_location: InstructionTarget::Copied(
                                                    InstructionIndex(2),
                                                ),
                                            },
                                        ],
                                    },
                                ),
                            },
                        ],
                    }),
                }
                .into()],
            };
            Self {
                program: QUIL,
                filter: Filter::default(),
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
                entries: vec![CompactSourceMapEntry {
                    source_location: 0,
                    target_location: InstructionTarget::Rewrite(CompactDefGateSequenceExpansion {
                        defgate_sequence_source: (
                            "some_u2_cycle",
                            &["param1", "param2", "param3", "param4", "param5", "param6"],
                            &["a", "b"],
                        ),
                        range: 0..18,
                        expansions: vec![
                            CompactSourceMapEntry {
                                source_location: 0,
                                target_location: InstructionTarget::Rewrite(
                                    CompactDefGateSequenceExpansion {
                                        defgate_sequence_source: (
                                            "pmw3",
                                            &["param1", "param2", "param3"],
                                            &["a"],
                                        ),
                                        range: 0..9,
                                        expansions: vec![
                                            CompactSourceMapEntry {
                                                source_location: 0,
                                                target_location: InstructionTarget::Rewrite(
                                                    CompactDefGateSequenceExpansion {
                                                        defgate_sequence_source: (
                                                            "pmw",
                                                            &["param1"],
                                                            &["a"],
                                                        ),
                                                        range: 0..3,
                                                        expansions: vec![
                                                            CompactSourceMapEntry {
                                                                source_location: 0,
                                                                target_location:
                                                                    InstructionTarget::Copied(
                                                                        InstructionIndex(0),
                                                                    ),
                                                            },
                                                            CompactSourceMapEntry {
                                                                source_location: 1,
                                                                target_location:
                                                                    InstructionTarget::Copied(
                                                                        InstructionIndex(1),
                                                                    ),
                                                            },
                                                            CompactSourceMapEntry {
                                                                source_location: 2,
                                                                target_location:
                                                                    InstructionTarget::Copied(
                                                                        InstructionIndex(2),
                                                                    ),
                                                            },
                                                        ],
                                                    },
                                                ),
                                            },
                                            CompactSourceMapEntry {
                                                source_location: 1,
                                                target_location: InstructionTarget::Rewrite(
                                                    CompactDefGateSequenceExpansion {
                                                        defgate_sequence_source: (
                                                            "pmw",
                                                            &["param1"],
                                                            &["a"],
                                                        ),
                                                        range: 3..6,
                                                        expansions: vec![
                                                            CompactSourceMapEntry {
                                                                source_location: 0,
                                                                target_location:
                                                                    InstructionTarget::Copied(
                                                                        InstructionIndex(0),
                                                                    ),
                                                            },
                                                            CompactSourceMapEntry {
                                                                source_location: 1,
                                                                target_location:
                                                                    InstructionTarget::Copied(
                                                                        InstructionIndex(1),
                                                                    ),
                                                            },
                                                            CompactSourceMapEntry {
                                                                source_location: 2,
                                                                target_location:
                                                                    InstructionTarget::Copied(
                                                                        InstructionIndex(2),
                                                                    ),
                                                            },
                                                        ],
                                                    },
                                                ),
                                            },
                                            CompactSourceMapEntry {
                                                source_location: 2,
                                                target_location: InstructionTarget::Rewrite(
                                                    CompactDefGateSequenceExpansion {
                                                        defgate_sequence_source: (
                                                            "pmw",
                                                            &["param1"],
                                                            &["a"],
                                                        ),
                                                        range: 6..9,
                                                        expansions: vec![
                                                            CompactSourceMapEntry {
                                                                source_location: 0,
                                                                target_location:
                                                                    InstructionTarget::Copied(
                                                                        InstructionIndex(0),
                                                                    ),
                                                            },
                                                            CompactSourceMapEntry {
                                                                source_location: 1,
                                                                target_location:
                                                                    InstructionTarget::Copied(
                                                                        InstructionIndex(1),
                                                                    ),
                                                            },
                                                            CompactSourceMapEntry {
                                                                source_location: 2,
                                                                target_location:
                                                                    InstructionTarget::Copied(
                                                                        InstructionIndex(2),
                                                                    ),
                                                            },
                                                        ],
                                                    },
                                                ),
                                            },
                                        ],
                                    },
                                ),
                            },
                            CompactSourceMapEntry {
                                source_location: 1,
                                target_location: InstructionTarget::Rewrite(
                                    CompactDefGateSequenceExpansion {
                                        defgate_sequence_source: (
                                            "pmw3",
                                            &["param1", "param2", "param3"],
                                            &["a"],
                                        ),
                                        range: 9..18,
                                        expansions: vec![
                                            CompactSourceMapEntry {
                                                source_location: 0,
                                                target_location: InstructionTarget::Rewrite(
                                                    CompactDefGateSequenceExpansion {
                                                        defgate_sequence_source: (
                                                            "pmw",
                                                            &["param1"],
                                                            &["a"],
                                                        ),
                                                        range: 0..3,
                                                        expansions: vec![
                                                            CompactSourceMapEntry {
                                                                source_location: 0,
                                                                target_location:
                                                                    InstructionTarget::Copied(
                                                                        InstructionIndex(0),
                                                                    ),
                                                            },
                                                            CompactSourceMapEntry {
                                                                source_location: 1,
                                                                target_location:
                                                                    InstructionTarget::Copied(
                                                                        InstructionIndex(1),
                                                                    ),
                                                            },
                                                            CompactSourceMapEntry {
                                                                source_location: 2,
                                                                target_location:
                                                                    InstructionTarget::Copied(
                                                                        InstructionIndex(2),
                                                                    ),
                                                            },
                                                        ],
                                                    },
                                                ),
                                            },
                                            CompactSourceMapEntry {
                                                source_location: 1,
                                                target_location: InstructionTarget::Rewrite(
                                                    CompactDefGateSequenceExpansion {
                                                        defgate_sequence_source: (
                                                            "pmw",
                                                            &["param1"],
                                                            &["a"],
                                                        ),
                                                        range: 3..6,
                                                        expansions: vec![
                                                            CompactSourceMapEntry {
                                                                source_location: 0,
                                                                target_location:
                                                                    InstructionTarget::Copied(
                                                                        InstructionIndex(0),
                                                                    ),
                                                            },
                                                            CompactSourceMapEntry {
                                                                source_location: 1,
                                                                target_location:
                                                                    InstructionTarget::Copied(
                                                                        InstructionIndex(1),
                                                                    ),
                                                            },
                                                            CompactSourceMapEntry {
                                                                source_location: 2,
                                                                target_location:
                                                                    InstructionTarget::Copied(
                                                                        InstructionIndex(2),
                                                                    ),
                                                            },
                                                        ],
                                                    },
                                                ),
                                            },
                                            CompactSourceMapEntry {
                                                source_location: 2,
                                                target_location: InstructionTarget::Rewrite(
                                                    CompactDefGateSequenceExpansion {
                                                        defgate_sequence_source: (
                                                            "pmw",
                                                            &["param1"],
                                                            &["a"],
                                                        ),
                                                        range: 6..9,
                                                        expansions: vec![
                                                            CompactSourceMapEntry {
                                                                source_location: 0,
                                                                target_location:
                                                                    InstructionTarget::Copied(
                                                                        InstructionIndex(0),
                                                                    ),
                                                            },
                                                            CompactSourceMapEntry {
                                                                source_location: 1,
                                                                target_location:
                                                                    InstructionTarget::Copied(
                                                                        InstructionIndex(1),
                                                                    ),
                                                            },
                                                            CompactSourceMapEntry {
                                                                source_location: 2,
                                                                target_location:
                                                                    InstructionTarget::Copied(
                                                                        InstructionIndex(2),
                                                                    ),
                                                            },
                                                        ],
                                                    },
                                                ),
                                            },
                                        ],
                                    },
                                ),
                            },
                        ],
                    }),
                }
                .into()],
            };
            Self {
                program: QUIL,
                filter: Filter::default(),
                expected: Ok(EXPECTED_QUIL),
                expected_source_map,
            }
        }

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
                    CompactSourceMapEntry {
                        source_location: 0,
                        target_location: InstructionTarget::Copied(InstructionIndex(0)),
                    }
                    .into(),
                    CompactSourceMapEntry {
                        source_location: 1,
                        target_location: InstructionTarget::Rewrite(
                            CompactDefGateSequenceExpansion {
                                defgate_sequence_source: (
                                    "seq2",
                                    &["param1", "param2"],
                                    &["a", "b"],
                                ),
                                range: 1..7,
                                expansions: vec![
                                    CompactSourceMapEntry {
                                        source_location: 0,
                                        target_location: InstructionTarget::Copied(
                                            InstructionIndex(0),
                                        ),
                                    },
                                    CompactSourceMapEntry {
                                        source_location: 1,
                                        target_location: InstructionTarget::Rewrite(
                                            CompactDefGateSequenceExpansion {
                                                defgate_sequence_source: (
                                                    "seq1",
                                                    &["param1"],
                                                    &["a"],
                                                ),
                                                range: 1..4,
                                                expansions: vec![
                                                    CompactSourceMapEntry {
                                                        source_location: 0,
                                                        target_location: InstructionTarget::Copied(
                                                            InstructionIndex(0),
                                                        ),
                                                    },
                                                    CompactSourceMapEntry {
                                                        source_location: 1,
                                                        target_location: InstructionTarget::Copied(
                                                            InstructionIndex(1),
                                                        ),
                                                    },
                                                    CompactSourceMapEntry {
                                                        source_location: 2,
                                                        target_location: InstructionTarget::Copied(
                                                            InstructionIndex(2),
                                                        ),
                                                    },
                                                ],
                                            },
                                        ),
                                    },
                                    CompactSourceMapEntry {
                                        source_location: 2,
                                        target_location: InstructionTarget::Copied(
                                            InstructionIndex(4),
                                        ),
                                    },
                                    CompactSourceMapEntry {
                                        source_location: 3,
                                        target_location: InstructionTarget::Copied(
                                            InstructionIndex(5),
                                        ),
                                    },
                                ],
                            },
                        ),
                    }
                    .into(),
                    CompactSourceMapEntry {
                        source_location: 2,
                        target_location: InstructionTarget::Copied(InstructionIndex(7)),
                    }
                    .into(),
                    CompactSourceMapEntry {
                        source_location: 3,
                        target_location: InstructionTarget::Copied(InstructionIndex(8)),
                    }
                    .into(),
                ],
            };
            Self {
                program: QUIL,
                filter: Filter::default(),
                expected: Ok(EXPECTED_QUIL),
                expected_source_map,
            }
        }

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
                entries: vec![CompactSourceMapEntry {
                    source_location: 0,
                    target_location: InstructionTarget::Rewrite(CompactDefGateSequenceExpansion {
                        defgate_sequence_source: ("seq1", &["param1"], &["a", "b"]),
                        range: 0..3,
                        expansions: vec![
                            CompactSourceMapEntry {
                                source_location: 0,
                                target_location: InstructionTarget::Copied(InstructionIndex(0)),
                            },
                            CompactSourceMapEntry {
                                source_location: 1,
                                target_location: InstructionTarget::Copied(InstructionIndex(1)),
                            },
                            CompactSourceMapEntry {
                                source_location: 2,
                                target_location: InstructionTarget::Copied(InstructionIndex(2)),
                            },
                        ],
                    }),
                }
                .into()],
            };
            Self {
                program: QUIL,
                filter: Filter::default(),
                expected: Ok(EXPECTED_QUIL),
                expected_source_map,
            }
        }

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
                    CompactSourceMapEntry {
                        source_location: 0,
                        target_location: InstructionTarget::Rewrite(
                            CompactDefGateSequenceExpansion {
                                defgate_sequence_source: ("seq1", &["param1"], &["a"]),
                                range: 0..3,
                                expansions: vec![
                                    CompactSourceMapEntry {
                                        source_location: 0,
                                        target_location: InstructionTarget::Copied(
                                            InstructionIndex(0),
                                        ),
                                    },
                                    CompactSourceMapEntry {
                                        source_location: 1,
                                        target_location: InstructionTarget::Copied(
                                            InstructionIndex(1),
                                        ),
                                    },
                                    CompactSourceMapEntry {
                                        source_location: 2,
                                        target_location: InstructionTarget::Copied(
                                            InstructionIndex(2),
                                        ),
                                    },
                                ],
                            },
                        ),
                    }
                    .into(),
                    CompactSourceMapEntry {
                        source_location: 1,
                        target_location: InstructionTarget::Copied(InstructionIndex(3)),
                    }
                    .into(),
                ],
            };
            Self {
                program: QUIL,
                filter: Filter::Include(["seq1".to_string()].into_iter().collect()), // Only include seq2
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
                filter: Filter::default(),
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
                filter: Filter::default(),
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
                filter: Filter::default(),
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
                filter: Filter::default(),
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
                filter: Filter::default(),
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
            filter: test_case.filter.clone(),
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
                pretty_assertions::assert_eq!(
                    InstructionSourceMap::from(test_case.expected_source_map),
                    source_map
                );

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
}
