/**
 * Copyright 2021 Rigetti Computing
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 **/
use std::collections::BTreeMap;
use std::str::FromStr;

use crate::instruction::{
    Capture, Declaration, Delay, Fence, FrameDefinition, FrameIdentifier, Instruction, Pulse,
    RawCapture, SetFrequency, SetPhase, SetScale, ShiftFrequency, ShiftPhase, SwapPhases, Waveform,
    WaveformDefinition,
};
use crate::parser::{lex, parse_instructions};

pub use self::calibration::CalibrationSet;
pub use self::frame::FrameSet;
pub use self::memory::MemoryRegion;

mod calibration;
mod error;
mod frame;
pub mod graph;
mod memory;

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
    pub fn get_frames_for_instruction<'a>(
        &'a self,
        instruction: &'a Instruction,
        include_blocked: bool,
    ) -> Option<Vec<&'a FrameIdentifier>> {
        match &instruction {
            Instruction::Pulse(Pulse {
                blocking, frame, ..
            }) => {
                if *blocking && include_blocked {
                    Some(self.frames.get_keys())
                } else {
                    Some(vec![frame])
                }
            }
            Instruction::Delay(Delay {
                frame_names,
                qubits,
                ..
            }) => {
                let frame_ids = self.frames.get_matching_keys(qubits, frame_names);
                Some(frame_ids)
            }
            Instruction::Fence(Fence { qubits }) => {
                if qubits.is_empty() {
                    Some(self.frames.get_keys())
                } else {
                    Some(self.frames.get_matching_keys(qubits, &[]))
                }
            }
            Instruction::Capture(Capture {
                blocking, frame, ..
            })
            | Instruction::RawCapture(RawCapture {
                blocking, frame, ..
            }) => {
                if *blocking && include_blocked {
                    Some(self.frames.get_keys())
                } else {
                    Some(vec![frame])
                }
            }
            Instruction::SetFrequency(SetFrequency { frame, .. })
            | Instruction::SetPhase(SetPhase { frame, .. })
            | Instruction::SetScale(SetScale { frame, .. })
            | Instruction::ShiftFrequency(ShiftFrequency { frame, .. })
            | Instruction::ShiftPhase(ShiftPhase { frame, .. }) => Some(vec![frame]),
            Instruction::SwapPhases(SwapPhases { frame_1, frame_2 }) => {
                Some(vec![frame_1, frame_2])
            }
            Instruction::Gate(_)
            | Instruction::CircuitDefinition(_)
            | Instruction::GateDefinition(_)
            | Instruction::Declaration(_)
            | Instruction::Measurement(_)
            | Instruction::Reset(_)
            | Instruction::CalibrationDefinition(_)
            | Instruction::FrameDefinition(_)
            | Instruction::MeasureCalibrationDefinition(_)
            | Instruction::Pragma(_)
            | Instruction::WaveformDefinition(_)
            | Instruction::Arithmetic(_)
            | Instruction::Halt
            | Instruction::Label(_)
            | Instruction::Move(_)
            | Instruction::Exchange(_)
            | Instruction::Load(_)
            | Instruction::Store(_)
            | Instruction::Jump(_)
            | Instruction::JumpWhen(_)
            | Instruction::JumpUnless(_)
            | Instruction::GreaterThan(_) => None,
        }
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
    use std::str::FromStr;

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
    1, 2
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
\t1, 2
DEFCAL I 0:
\tDELAY 0 1
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
}
