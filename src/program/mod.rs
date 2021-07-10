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
use std::collections::HashMap;
use std::str::FromStr;

use crate::{
    instruction::{FrameIdentifier, Instruction, Waveform},
    parser::{lex, parse_instructions},
};

mod calibration;
mod frame;
pub mod graph;
mod memory;

pub use self::calibration::CalibrationSet;
pub use self::frame::FrameSet;
pub use self::memory::MemoryRegion;

/// A Quil Program instance describes a quantum program with metadata used in execution.
///
/// This contains not only instructions which are executed in turn on the quantum processor, but
/// also the "headers" used to describe and manipulate those instructions, such as calibrations
/// and frame definitions.
#[derive(Clone, Debug, Default)]
pub struct Program {
    pub calibrations: CalibrationSet,
    pub frames: FrameSet,
    pub memory_regions: HashMap<String, MemoryRegion>,
    pub waveforms: HashMap<String, Waveform>,
    pub instructions: Vec<Instruction>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            calibrations: CalibrationSet::new(),
            frames: FrameSet::new(),
            memory_regions: HashMap::new(),
            waveforms: HashMap::new(),
            instructions: vec![],
        }
    }

    /// Add an instruction to the end of the program.
    pub fn add_instruction(&mut self, instruction: Instruction) {
        use Instruction::*;

        match instruction {
            CalibrationDefinition(calibration) => {
                self.calibrations.push(*calibration);
            }
            FrameDefinition {
                identifier,
                attributes,
            } => {
                self.frames.insert(identifier, attributes);
            }
            Instruction::Declaration {
                name,
                size,
                sharing,
            } => {
                self.memory_regions
                    .insert(name, MemoryRegion { size, sharing });
            }
            WaveformDefinition { name, definition } => {
                self.waveforms.insert(name, definition);
            }
            other => self.instructions.push(other),
        }
    }

    /// Expand any instructions in the program which have a matching calibration, leaving the others
    /// unchanged.
    pub fn expand_calibrations(&mut self) {
        let mut result: Vec<Instruction> = vec![];

        // TODO: Do this more efficiently, possibly with Vec::splice
        for instruction in &self.instructions {
            match self.calibrations.expand(instruction) {
                Some(expanded) => {
                    result.extend(expanded.into_iter());
                }
                None => {
                    result.push(instruction.clone());
                }
            }
        }

        self.instructions = result;
    }

    /// Return the frames which are either used or blocked by the given instruction.
    /// A frame is "blocked" if the instruction prevents other instructions from playing
    /// on that frame until complete; a frame is "used" if the instruction actively
    /// executes on that sequencer.
    pub fn get_blocked_frames<'a>(
        &'a self,
        instruction: &'a Instruction,
        include_blocked: bool,
    ) -> Option<Vec<&'a FrameIdentifier>> {
        use Instruction::*;
        match &instruction {
            Pulse {
                blocking, frame, ..
            } => {
                if *blocking && include_blocked {
                    Some(self.frames.get_keys())
                } else {
                    Some(vec![frame])
                }
            }
            // FIXME: handle blocking
            Delay {
                frame_names,
                qubits,
                ..
            } => {
                let frame_ids = self.frames.get_matching_keys(qubits, frame_names);
                Some(frame_ids)
            }
            Capture { frame, .. }
            | RawCapture { frame, .. }
            | SetFrequency { frame, .. }
            | SetPhase { frame, .. }
            | SetScale { frame, .. }
            | ShiftFrequency { frame, .. }
            | ShiftPhase { frame, .. } => Some(vec![frame]),
            _ => None,
        }
    }

    pub fn to_instructions(&self, include_headers: bool) -> Vec<Instruction> {
        let mut result = vec![];

        if include_headers {
            result.extend(self.frames.to_instructions());
            result.extend(self.calibrations.to_instructions());
        }

        // TODO: other headers/context like frames and waveforms
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
        let lexed = lex(s);
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
    use super::Program;
    use std::str::FromStr;

    #[test]
    fn program_headers() {
        let input = "
DECLARE ro BIT[5]
DEFCAL I 0:
    DELAY 0 1.0
DEFFRAME 0 \"rx\":
    HARDWARE-OBJECT: \"hardware\"
DEFWAVEFORM custom 6.0:
    1,2
I 0
";
        let program = Program::from_str(input).unwrap();
        assert_eq!(program.calibrations.len(), 1);
        assert_eq!(program.memory_regions.len(), 1);
        assert_eq!(program.frames.len(), 1);
        assert_eq!(program.waveforms.len(), 1);
        assert_eq!(program.instructions.len(), 1);
    }
}
