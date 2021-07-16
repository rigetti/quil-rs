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
#[derive(Clone, Debug, Default, PartialEq)]
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

    /// Return the frames which are either "used" or "blocked" by the given instruction.
    ///
    /// An instruction "uses" a frame if it plays on that frame; it "blocks" a frame
    /// if the instruction prevents other instructions from playing on that frame until complete.
    pub fn get_frames_for_instruction<'a>(
        &'a self,
        instruction: &'a Instruction,
        include_blocked: bool,
    ) -> ApplicableFrames<'a> {
        use Instruction::*;
        match &instruction {
            Pulse {
                blocking, frame, ..
            } => {
                if *blocking && include_blocked {
                    ApplicableFrames::AllFrames
                } else {
                    ApplicableFrames::SelectedFrames(vec![frame])
                }
            }
            Delay {
                frame_names,
                qubits,
                ..
            } => {
                let frame_ids = self.frames.get_matching_keys(qubits, frame_names);
                ApplicableFrames::SelectedFrames(frame_ids)
            }
            Fence { qubits } => {
                if qubits.is_empty() {
                    ApplicableFrames::AllFrames
                } else {
                    ApplicableFrames::SelectedFrames(self.frames.get_matching_keys(qubits, &[]))
                }
            }
            Capture {
                blocking, frame, ..
            }
            | RawCapture {
                blocking, frame, ..
            } => {
                if *blocking && include_blocked {
                    ApplicableFrames::AllFrames
                } else {
                    ApplicableFrames::SelectedFrames(vec![frame])
                }
            }
            SetFrequency { frame, .. }
            | SetPhase { frame, .. }
            | SetScale { frame, .. }
            | ShiftFrequency { frame, .. }
            | ShiftPhase { frame, .. } => ApplicableFrames::SelectedFrames(vec![frame]),
            SwapPhases { frame_1, frame_2 } => {
                ApplicableFrames::SelectedFrames(vec![frame_1, frame_2])
            }
            Gate { .. }
            | CircuitDefinition { .. }
            | GateDefinition { .. }
            | Declaration { .. }
            | Measurement { .. }
            | Reset { .. }
            | CalibrationDefinition(_)
            | FrameDefinition { .. }
            | MeasureCalibrationDefinition { .. }
            | Pragma { .. }
            | WaveformDefinition { .. }
            | Arithmetic { .. }
            | Halt
            | Label(_)
            | Move { .. }
            | Exchange { .. }
            | Load { .. }
            | Store { .. }
            | Jump { .. }
            | JumpWhen { .. }
            | JumpUnless { .. } => ApplicableFrames::NoFrames,
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

/// ApplicableFrames describes which frames an instruction executes on.
#[derive(Clone, Debug)]
pub enum ApplicableFrames<'a> {
    /// The instruction does not support execution on particular frames, such as classical control.
    NoFrames,

    /// The instruction executes only on the selected frames.
    SelectedFrames(Vec<&'a FrameIdentifier>),

    /// The instruction executes on all available frames.
    AllFrames,
}

#[cfg(test)]
mod tests {
    use super::Program;
    use std::str::FromStr;

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
DEFWAVEFORM custom 6.0:
    1,2
1 0
";
        let a = Program::from_str(input_a);
        let b = Program::from_str(input_b);
        assert_ne!(a, b);
    }

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
