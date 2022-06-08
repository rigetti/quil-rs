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

use std::collections::{BTreeMap, HashSet};
use std::str::FromStr;

use crate::instruction::{
    Declaration, FrameDefinition, FrameIdentifier, Instruction, Qubit, Waveform, WaveformDefinition,
};
use crate::parser::{lex, parse_instructions};

pub use self::calibration::CalibrationSet;
pub use self::frame::FrameSet;
pub use self::memory::MemoryRegion;

mod calibration;
mod error;
pub(crate) mod frame;
pub mod graph;
mod memory;

#[cfg(feature = "graphviz-dot")]
pub mod graphviz_dot;

/// A Quil Program instance describes a quantum program with metadata used in execution.
///
/// This contains not only instructions which are executed in turn on the quantum processor, but
/// also the "headers" used to describe and manipulate those instructions, such as calibrations
/// and frame definitions.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Program {
    pub calibrations: CalibrationSet,
    pub frames: FrameSet,
    pub memory_regions: BTreeMap<String, MemoryRegion>,
    pub waveforms: BTreeMap<String, Waveform>,
    pub instructions: Vec<Instruction>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            calibrations: CalibrationSet::default(),
            frames: FrameSet::new(),
            memory_regions: BTreeMap::new(),
            waveforms: BTreeMap::new(),
            instructions: vec![],
        }
    }

    /// Add an instruction to the end of the program.
    pub fn add_instruction(&mut self, instruction: Instruction) {
        match instruction {
            Instruction::CalibrationDefinition(calibration) => {
                self.calibrations.push_calibration(calibration);
            }
            Instruction::FrameDefinition(FrameDefinition {
                identifier,
                attributes,
            }) => {
                self.frames.insert(identifier, attributes);
            }
            Instruction::Declaration(Declaration {
                name,
                size,
                sharing,
            }) => {
                self.memory_regions
                    .insert(name, MemoryRegion { size, sharing });
            }
            Instruction::MeasureCalibrationDefinition(calibration) => {
                self.calibrations.push_measurement_calibration(calibration);
            }
            Instruction::WaveformDefinition(WaveformDefinition { name, definition }) => {
                self.waveforms.insert(name, definition);
            }
            other => self.instructions.push(other),
        }
    }

    /// Expand any instructions in the program which have a matching calibration, leaving the others
    /// unchanged. Recurses though each instruction while ensuring there is no cycle in the expansion
    /// graph (i.e. no calibration expands directly or indirectly into itself)
    pub fn expand_calibrations(&self) -> error::ProgramResult<Self> {
        let mut expanded_instructions: Vec<Instruction> = vec![];

        // TODO: Do this more efficiently, possibly with Vec::splice
        for instruction in &self.instructions {
            match self.calibrations.expand(instruction, &[])? {
                Some(expanded) => {
                    expanded_instructions.extend(expanded.into_iter());
                }
                None => {
                    expanded_instructions.push(instruction.clone());
                }
            }
        }

        let mut new_program = self.clone();
        new_program.instructions = vec![];

        for instruction in expanded_instructions {
            new_program.add_instruction(instruction);
        }

        Ok(new_program)
    }

    /// Return the frames which are either "used" or "blocked" by the given instruction.
    ///
    /// An instruction "uses" a frame if it plays on that frame; it "blocks" a frame
    /// if the instruction prevents other instructions from playing on that frame until complete.
    ///
    /// Return `None` if the instruction does not execute in the context of a frame - such
    /// as classical instructions.
    ///
    /// See the [Quil-T spec](https://github.com/quil-lang/quil/blob/master/rfcs/analog/proposal.md)
    /// for more information.
    pub fn get_frames_for_instruction<'a>(
        &'a self,
        instruction: &'a Instruction,
        include_blocked: bool,
    ) -> Option<HashSet<&'a FrameIdentifier>> {
        instruction
            .get_frame_match_condition(include_blocked)
            .map(|condition| self.frames.get_matching_keys(condition))
    }

    /// Returns a HashSet consisting of every Qubit that is used in the program.
    pub fn get_used_qubits(&self) -> HashSet<Qubit> {
        self.instructions
            .iter()
            .flat_map(|i| match i {
                Instruction::Gate(gate) => gate.qubits.clone(),
                Instruction::Measurement(measurement) => vec![measurement.qubit.clone()],
                Instruction::Reset(reset) => match &reset.qubit {
                    Some(qubit) => vec![qubit.to_owned()],
                    None => vec![],
                },
                Instruction::Delay(delay) => delay.qubits.clone(),
                Instruction::Fence(fence) => fence.qubits.clone(),
                _ => vec![],
            })
            .collect::<HashSet<_>>()
    }

    pub fn to_instructions(&self, include_headers: bool) -> Vec<Instruction> {
        let mut result = vec![];

        if include_headers {
            result.extend(self.memory_regions.iter().map(|(name, descriptor)| {
                Instruction::Declaration(Declaration {
                    name: name.clone(),
                    size: descriptor.size.clone(),
                    sharing: descriptor.sharing.clone(),
                })
            }));
            result.extend(self.frames.to_instructions());
            result.extend(self.waveforms.iter().map(|(name, definition)| {
                Instruction::WaveformDefinition(WaveformDefinition {
                    name: name.clone(),
                    definition: definition.clone(),
                })
            }));
            result.extend(self.calibrations.to_instructions());
        }

        result.extend(self.instructions.clone());

        result
    }

    pub fn to_string(&self, include_headers: bool) -> String {
        self.to_instructions(include_headers)
            .iter()
            .map(|inst| format!("{}\n", inst))
            .collect()
    }
}

impl FromStr for Program {
    type Err = nom::Err<String>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lexed = lex(s).map_err(nom::Err::Error)?;
        let (_, instructions) = parse_instructions(&lexed).map_err(|err| match err {
            nom::Err::Incomplete(_) => nom::Err::Error("incomplete".to_owned()),
            nom::Err::Error(error) => nom::Err::Error(format!("{:?}", error)),
            nom::Err::Failure(failure) => nom::Err::Error(format!("{:?}", failure)),
        })?;
        let mut program = Self::new();

        for instruction in instructions {
            program.add_instruction(instruction)
        }

        Ok(program)
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::HashSet, str::FromStr};

    use crate::instruction::Instruction;
    use crate::instruction::Qubit;

    use super::Program;

    #[test]
    fn program_eq() {
        let input = "
DECLARE ro BIT
MEASURE q ro
JUMP-UNLESS @end-reset ro
X q
LABEL @end-reset

DEFCAL I 0:
    DELAY 0 1.0
DEFFRAME 0 \"rx\":
    HARDWARE-OBJECT: \"hardware\"
DEFWAVEFORM custom 6.0:
    1,2
I 0
";
        let a = Program::from_str(input);
        let b = Program::from_str(input);
        assert_eq!(a, b);
    }

    #[test]
    fn program_neq() {
        let input_a = "
DECLARE ro BIT
MEASURE q ro
JUMP-UNLESS @end-reset ro
X q
LABEL @end-reset

DEFCAL I 0:
    DELAY 0 1.0
DEFFRAME 0 \"rx\":
    HARDWARE-OBJECT: \"hardware\"
DEFWAVEFORM custom 6.0:
    1,2
I 0
";
        let input_b = "
DECLARE readout BIT
MEASURE q readout
JUMP-UNLESS @end-reset readout
X q
LABEL @end-reset

DEFCAL I 1:
    DELAY 1 1.0
DEFFRAME 1 \"rx\":
    HARDWARE-OBJECT: \"hardware\"
DEFWAVEFORM custom:
    1,2
1 0
";
        let a = Program::from_str(input_a);
        let b = Program::from_str(input_b);
        assert_ne!(a, b);
    }

    // Assert that headers are correctly parsed from program text, and
    // also exported when the program is exported as a string.
    #[test]
    fn program_headers() {
        let input = "
DECLARE ro BIT[5]
DEFCAL I 0:
    DELAY 0 1.0
DEFFRAME 0 \"rx\":
    HARDWARE-OBJECT: \"hardware\"
DEFWAVEFORM custom:
    1.0, 2.0
I 0
";
        let program = Program::from_str(input).unwrap();
        assert_eq!(program.calibrations.len(), 1);
        assert_eq!(program.memory_regions.len(), 1);
        assert_eq!(program.frames.len(), 1);
        assert_eq!(program.waveforms.len(), 1);
        assert_eq!(program.instructions.len(), 1);

        assert_eq!(program.to_string(false), "I 0\n");

        assert_eq!(
            program.to_string(true),
            "DECLARE ro BIT[5]
DEFFRAME 0 \"rx\":
\tHARDWARE-OBJECT: \"hardware\"
DEFWAVEFORM custom:
\t1.0, 2.0
DEFCAL I 0:
\tDELAY 0 1.0
I 0
"
        );
    }

    #[test]
    fn program_deterministic_ordering() {
        let input = "
DECLARE ro BIT
DECLARE anc BIT
DECLARE ec BIT
";
        let program1 = Program::from_str(input).unwrap().to_string(true);
        let program2 = Program::from_str(input).unwrap().to_string(true);

        // verify that each memory declaration in the program is in the same index as the same
        // program after being re-parsed and serialized.
        assert!(program1.lines().eq(program2.lines()));
    }

    #[test]
    fn frame_blocking() {
        let input = "DEFFRAME 0 \"a\":
\tHARDWARE-OBJECT: \"hardware\"

DEFFRAME 0 \"b\":
\tHARDWARE-OBJECT: \"hardware\"

DEFFRAME 1 \"c\":
\tHARDWARE-OBJECT: \"hardware\"

DEFFRAME 0 1 \"2q\":
\tHARDWARE-OBJECT: \"hardware\"
";

        let program = Program::from_str(input).unwrap();

        for (instruction_string, expected_used_frames, expected_blocked_frames) in vec![
            // Blocking pulses use only the specified frame but block frames intersecting the frame's qubits
            (
                r#"PULSE 0 "a" custom_waveform"#,
                vec![r#"0 "a""#],
                vec![r#"0 "a""#, r#"0 "b""#, r#"0 1 "2q""#],
            ),
            (
                r#"PULSE 1 "c" custom_waveform"#,
                vec![r#"1 "c""#],
                vec![r#"1 "c""#, r#"0 1 "2q""#],
            ),
            // Pulses on non-declared frames and unused qubits do not use or block any frames in the program
            (r#"PULSE 2 "a" custom_waveform"#, vec![], vec![]),
            // Captures work identically to Pulses
            (
                r#"CAPTURE 0 "a" custom_waveform ro[0]"#,
                vec![r#"0 "a""#],
                vec![r#"0 "a""#, r#"0 "b""#, r#"0 1 "2q""#],
            ),
            (
                r#"CAPTURE 1 "c" custom_waveform ro[0]"#,
                vec![r#"1 "c""#],
                vec![r#"1 "c""#, r#"0 1 "2q""#],
            ),
            (r#"CAPTURE 2 "a" custom_waveform ro[0]"#, vec![], vec![]),
            // Raw Captures work identically to Pulses
            (
                r#"RAW-CAPTURE 0 "a" 1e-6 ro[0]"#,
                vec![r#"0 "a""#],
                vec![r#"0 "a""#, r#"0 "b""#, r#"0 1 "2q""#],
            ),
            (
                r#"RAW-CAPTURE 1 "c" 1e-6 ro[0]"#,
                vec![r#"1 "c""#],
                vec![r#"1 "c""#, r#"0 1 "2q""#],
            ),
            (r#"RAW-CAPTURE 2 "a" 1e-6 ro[0]"#, vec![], vec![]),
            // A non-blocking pulse blocks only its precise frame, not other frames on the same qubits
            (
                r#"NONBLOCKING PULSE 0 "a" custom_waveform"#,
                vec![r#"0 "a""#],
                vec![r#"0 "a""#],
            ),
            (
                r#"NONBLOCKING PULSE 1 "c" custom_waveform"#,
                vec![r#"1 "c""#],
                vec![r#"1 "c""#],
            ),
            (
                r#"NONBLOCKING PULSE 0 1 "2q" custom_waveform"#,
                vec![r#"0 1 "2q""#],
                vec![r#"0 1 "2q""#],
            ),
            // A Fence with qubits specified uses and blocks all frames intersecting that qubit
            (
                r#"FENCE 1"#,
                vec![r#"1 "c""#, r#"0 1 "2q""#],
                vec![r#"1 "c""#, r#"0 1 "2q""#],
            ),
            // Fence-all uses and blocks all frames declared in the program
            (
                r#"FENCE"#,
                vec![r#"0 "a""#, r#"0 "b""#, r#"1 "c""#, r#"0 1 "2q""#],
                vec![r#"0 "a""#, r#"0 "b""#, r#"1 "c""#, r#"0 1 "2q""#],
            ),
            // Delay uses and blocks frames on exactly the given qubits and with any of the given names
            (
                r#"DELAY 0 1.0"#,
                vec![r#"0 "a""#, r#"0 "b""#],
                vec![r#"0 "a""#, r#"0 "b""#],
            ),
            (r#"DELAY 1 1.0"#, vec![r#"1 "c""#], vec![r#"1 "c""#]),
            (r#"DELAY 1 "c" 1.0"#, vec![r#"1 "c""#], vec![r#"1 "c""#]),
            (r#"DELAY 0 1 1.0"#, vec![r#"0 1 "2q""#], vec![r#"0 1 "2q""#]),
        ] {
            let instruction = Instruction::parse(instruction_string).unwrap();
            let used_frames: HashSet<String> = program
                .get_frames_for_instruction(&instruction, false)
                .unwrap_or_default()
                .into_iter()
                .map(|f| f.to_string())
                .collect();
            let expected_used_frames: HashSet<String> = expected_used_frames
                .into_iter()
                .map(|el| el.to_owned())
                .collect();
            assert_eq!(
                used_frames, expected_used_frames,
                "Instruction {} *used* frames `{:?}` but we expected `{:?}`",
                instruction, used_frames, expected_used_frames
            );

            let blocked_frames: HashSet<String> = program
                .get_frames_for_instruction(&instruction, true)
                .unwrap()
                .into_iter()
                .map(|f| f.to_string())
                .collect();
            let expected_blocked_frames: HashSet<String> = expected_blocked_frames
                .into_iter()
                .map(|el| el.to_owned())
                .collect();
            assert_eq!(
                blocked_frames, expected_blocked_frames,
                "Instruction {} *blocked* frames `{:?}` but we expected `{:?}`",
                instruction, blocked_frames, expected_blocked_frames
            );
        }
    }

    #[test]
    fn test_get_qubits() {
        let input = "
DECLARE ro BIT
MEASURE q ro
JUMP-UNLESS @end-reset ro
X q
LABEL @end-reset
DEFCAL I 0:
    DELAY 0 1.0
DEFFRAME 0 \"rx\":
    HARDWARE-OBJECT: \"hardware\"
DEFWAVEFORM custom:
    1,2
I 0
";
        let program = Program::from_str(input).unwrap();
        let expected = vec![Qubit::Fixed(0), Qubit::Variable("q".to_string())]
            .into_iter()
            .collect::<HashSet<_>>();
        let actual = program.get_used_qubits();
        assert_eq!(expected, actual);
    }
}
