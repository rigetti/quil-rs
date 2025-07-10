use std::{collections::{HashMap, HashSet}, hash::Hash, ops::Range};

use indexmap::{IndexMap, IndexSet};

use crate::{expression::Expression, filter_set::Filter, instruction::{DefGateSequence, DefGateSequenceExpansionError, Gate, GateDefinition, GateSpecification, Instruction}, program::{InstructionIndex, SourceMap, SourceMapEntry}, quil::Quil};

/// The product of expanding an instruction using a calibration
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct DefGateSequenceExpansionOutput {
    /// The new instructions resulting from the expansion
    pub(crate) new_instructions: Vec<Instruction>,

    /// Details about the expansion process
    pub(crate) detail: DefGateSequenceExpansion,
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
struct DefGateSequenceSource(String);

impl From<&Gate> for DefGateSequenceSource {
    fn from(value: &Gate) -> Self {
        DefGateSequenceSource(value.name.clone())
    }
}

/// Details about the expansion of a calibration
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct DefGateSequenceExpansion {
    /// The calibration used to expand the instruction
    pub(crate) defgate_sequence_source: DefGateSequenceSource,

    /// The target instruction indices produced by the expansion
    pub(crate) range: Range<InstructionIndex>,

    /// A map of source locations to the expansions they produced
    pub(crate) expansions: ProgramDefGateSequenceExpansionSourceMap
}

/// The result of an attempt to expand an instruction within a [`Program`]
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum MaybeDefGateSequenceExpansion {
    /// The instruction was expanded into others
    Expanded(DefGateSequenceExpansion),

    /// The instruction was not expanded, but was simply copied over into the target program at the given instruction index
    Unexpanded(InstructionIndex),
}

pub(crate) type ProgramDefGateSequenceExpansionSourceMap =
    SourceMap<InstructionIndex, MaybeDefGateSequenceExpansion>;

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct ProgramDefGateSequenceExpansion<'program> {
    gate_definitions: &'program IndexMap<String, GateDefinition>,
    filter: Filter<String>,
}

impl<'program> ProgramDefGateSequenceExpansion<'program> {
    pub(crate) fn new(gate_definitions: &'program IndexMap<String, GateDefinition>, filter: Filter<String>) -> Self {
        Self {
            gate_definitions,
            filter,
        }
    }

    pub(crate) fn expand_defgate_sequences(&self, source_instructions: &[Instruction], source_map: Option<&mut ProgramDefGateSequenceExpansionSourceMap>) -> Result<Vec<Instruction>, DefGateSequenceExpansionError> {
        if let Some(source_map) = source_map {
            self.expand_defgate_sequences_inner_with_source_map(source_instructions, source_map, &mut IndexSet::new())
        } else {
            self.expand_defgate_sequences_inner_without_source_map(source_instructions, &mut IndexSet::new())
        }
    }

    fn expand_defgate_sequences_inner_with_source_map(&self, source_instructions: &[Instruction], source_map: &mut ProgramDefGateSequenceExpansionSourceMap, seen: &mut IndexSet<DefGateSequenceSource>) -> Result<Vec<Instruction>, DefGateSequenceExpansionError> {
        let mut target_instructions = vec![];
        for (source_instruction_index, source_instruction) in source_instructions.iter().enumerate() { 
            if let Some((target_gate_instructions, source)) = self.gate_sequence_from_instruction(seen, source_instruction)? {
                let mut seen = seen.clone();
                seen.insert(source.clone());

                let mut recursive_source_map = SourceMap::default();
                let recursive_target_gate_instructions = self.expand_defgate_sequences_inner_with_source_map(&target_gate_instructions, &mut recursive_source_map, &mut seen)?;
                let target_instruction_start_index = InstructionIndex(target_instructions.len());
                let target_instruction_end_index = InstructionIndex(target_instruction_start_index.0 + recursive_target_gate_instructions.len());
                source_map.entries.push(SourceMapEntry {
                    source_location: InstructionIndex(source_instruction_index),
                    target_location: MaybeDefGateSequenceExpansion::Expanded(DefGateSequenceExpansion {
                        defgate_sequence_source: source,
                        range: target_instruction_start_index..target_instruction_end_index,
                        expansions: recursive_source_map
                    })
                });
                target_instructions.extend(recursive_target_gate_instructions);
            } else {
                target_instructions.push(source_instruction.clone());
            }
        }
        Ok(target_instructions)
    }

    fn expand_defgate_sequences_inner_without_source_map(&self, source_instructions: &[Instruction], seen: &mut IndexSet<DefGateSequenceSource>) -> Result<Vec<Instruction>, DefGateSequenceExpansionError> {
        let mut target_instructions = vec![];
        for source_instruction in source_instructions.iter() { 
            if let Some((target_gate_instructions, source)) = self.gate_sequence_from_instruction(seen, source_instruction)? {
                let mut seen = seen.clone();
                seen.insert(source.clone());

                let recursive_target_gate_instructions = self.expand_defgate_sequences_inner_without_source_map(&target_gate_instructions, &mut seen)?;
                target_instructions.extend(recursive_target_gate_instructions);
            } else {
                target_instructions.push(source_instruction.clone());
            }
        }
        Ok(target_instructions)
    }

    fn gate_sequence_from_instruction<'s, 'inst>(&'s self, seen: &mut IndexSet<DefGateSequenceSource>, instruction: &'inst Instruction) -> Result<Option<(Vec<Instruction>, DefGateSequenceSource)>, DefGateSequenceExpansionError> {
        if let Instruction::Gate(gate) = instruction {
            if let Some(gate_definition) = self.gate_definitions.get(&gate.name) {
                if let GateSpecification::Sequence(gate_sequence) = &gate_definition.specification {
                    if self.filter.include(&gate.name) {
                        if gate_definition.parameters.len() != gate.parameters.len() {
                            return Err(DefGateSequenceExpansionError::ParameterCount { expected: gate_definition.parameters.len(), found: gate.parameters.len() });
                        }
                        let gate_parameter_arguments = gate_definition.parameters.iter().cloned().zip(gate.parameters.iter().cloned()).collect::<HashMap<String, Expression>>();

                        if !gate.modifiers.is_empty() {
                            return Err(DefGateSequenceExpansionError::GateModifiersUnsupported(gate.modifiers.clone()));
                        }
                        let source= DefGateSequenceSource::from(gate);
                        if seen.contains(&source) {
                            let cycle = seen.clone().into_iter().map(|source| source.0).collect();
                            return Err(DefGateSequenceExpansionError::CyclicSequenceGateDefinition(cycle));
                        }
        
                        let target_gate_instructions = gate_sequence.expand(gate_parameter_arguments, gate.qubits.clone())?.into_iter().map(Instruction::Gate).collect::<Vec<_>>();

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

    use super::*;
    use rstest::*;

    const TEST1: &'static str = r"
DEFGATE seq2(%param1, %param2) a b AS SEQUENCE:
    seq1(%param1) a
    seq1(%param2) b

DEFGATE seq1(%param1) a AS SEQUENCE:
    RZ(%param1) a
    RX(pi/2) a
    RZ(%param1) a

seq2(pi, pi/2) 0 1
";

    const TEST1_RESULT: &'static str = r"
RZ(pi) 0 # (0, 0, 0)
RX(pi/2) 0 # (0, 0, 1)
RZ(pi) 0 # (0, 0, 2)
RZ(pi/2) 1 # (0, 1, 0)
RX(pi/2) 1 # (0, 1, 1)
RZ(pi/2) 1 # (0, 1, 2)
";

    #[rstest]
    fn test_defgate_sequence_expansion() {
        let program = crate::Program::from_str(TEST1).expect("must be a valid Quil program");
        let program_expansion = ProgramDefGateSequenceExpansion {
            gate_definitions: &program.gate_definitions,
            filter: Filter::default(),
        };
        let mut source_map = ProgramDefGateSequenceExpansionSourceMap::default();
        let expanded = program_expansion.expand_defgate_sequences(&program.instructions, Some(&mut source_map)).expect("must expand defgate sequences");

        let expected_program = crate::Program::from_str(TEST1_RESULT).expect("must be valid Quil program");
        assert_eq!(expanded, expected_program.into_body_instructions().collect::<Vec<_>>());

        let expected_source_map = SourceMap {
            entries: vec![
                SourceMapEntry {
                    source_location: InstructionIndex(0),
                    target_location: MaybeDefGateSequenceExpansion::Expanded(DefGateSequenceExpansion {
                        defgate_sequence_source: DefGateSequenceSource("seq2".to_string()),
                        range: InstructionIndex(0)..InstructionIndex(6),
                        expansions: SourceMap {
                            entries: vec![
                                SourceMapEntry {
                                    source_location: InstructionIndex(0),
                                    target_location: MaybeDefGateSequenceExpansion::Expanded(DefGateSequenceExpansion {
                                        defgate_sequence_source: DefGateSequenceSource("seq1".to_string()),
                                        range: InstructionIndex(0)..InstructionIndex(3),
                                        expansions: SourceMap::default()
                                    })
                                },
                                SourceMapEntry {
                                    source_location: InstructionIndex(1),
                                    target_location: MaybeDefGateSequenceExpansion::Expanded(DefGateSequenceExpansion {
                                        defgate_sequence_source: DefGateSequenceSource("seq1".to_string()),
                                        range: InstructionIndex(3)..InstructionIndex(6),
                                        expansions: SourceMap::default()
                                    })
                                },
                            ]
                        }
                    })
                },
            ]
        };

        println!("{source_map:#?}");
        assert_eq!(expected_source_map, source_map);
    }
}