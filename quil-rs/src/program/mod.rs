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
use std::fmt;
use std::ops;
use std::str::FromStr;

use ndarray::Array2;
use nom_locate::LocatedSpan;

use crate::instruction::{
    Declaration, FrameDefinition, FrameIdentifier, GateError, Instruction, Matrix, Qubit, Waveform,
    WaveformDefinition,
};
use crate::parser::{lex, parse_instructions, ParseError};

pub use self::calibration::CalibrationSet;
pub use self::error::{disallow_leftover, map_parsed, recover, ParseProgramError, SyntaxError};
pub use self::frame::FrameSet;
use self::frame::MatchedFrames;
pub use self::memory::MemoryRegion;

mod calibration;
mod error;
pub(crate) mod frame;
pub mod graph;
mod memory;
pub mod type_check;

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum ProgramError {
    #[error("{0}")]
    ParsingError(#[from] ParseProgramError<Program>),

    #[error("this operation isn't supported on instruction: {0}")]
    UnsupportedOperation(Instruction),

    #[error("instruction {0} expands into itself")]
    RecursiveCalibration(Instruction),

    #[error("{0}")]
    GateError(#[from] GateError),

    #[error("can only compute program unitary for programs composed of `Gate`s; found unsupported instruction: {0}")]
    UnsupportedForUnitary(Instruction),
}

type Result<T> = std::result::Result<T, ProgramError>;

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
    instructions: Vec<Instruction>,
    // private field used for caching operations
    used_qubits: HashSet<Qubit>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            calibrations: CalibrationSet::default(),
            frames: FrameSet::new(),
            memory_regions: BTreeMap::new(),
            waveforms: BTreeMap::new(),
            instructions: vec![],
            used_qubits: HashSet::new(),
        }
    }

    /// Returns an iterator over immutable references to the instructions that make up the body of the program.
    pub fn body_instructions(&self) -> impl Iterator<Item = &Instruction> {
        self.instructions.iter()
    }

    pub fn into_body_instructions(self) -> impl Iterator<Item = Instruction> {
        self.instructions.into_iter()
    }

    /// Returns an iterator over mutable references to the instructions that make up the body of the program.
    #[cfg(test)]
    pub(crate) fn for_each_body_instruction<F>(&mut self, closure: F)
    where
        F: FnMut(&mut Instruction),
    {
        let mut instructions = std::mem::take(&mut self.instructions);
        self.used_qubits.clear();

        instructions.iter_mut().for_each(closure);

        self.add_instructions(instructions);
    }

    /// Like `Clone`, but does not clone the body instructions.
    pub fn clone_without_body_instructions(&self) -> Self {
        Self {
            instructions: Vec::new(),
            calibrations: self.calibrations.clone(),
            frames: self.frames.clone(),
            memory_regions: self.memory_regions.clone(),
            waveforms: self.waveforms.clone(),
            used_qubits: HashSet::new(),
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
            Instruction::Gate(gate) => {
                self.used_qubits.extend(gate.qubits.clone());
                self.instructions.push(Instruction::Gate(gate));
            }
            Instruction::Measurement(measurement) => {
                self.used_qubits.insert(measurement.qubit.clone());
                self.instructions
                    .push(Instruction::Measurement(measurement));
            }
            Instruction::Reset(reset) => {
                if let Some(qubit) = &reset.qubit {
                    self.used_qubits.insert(qubit.clone());
                }
                self.instructions.push(Instruction::Reset(reset));
            }
            Instruction::Delay(delay) => {
                self.used_qubits.extend(delay.qubits.clone());
                self.instructions.push(Instruction::Delay(delay));
            }
            Instruction::Fence(fence) => {
                self.used_qubits.extend(fence.qubits.clone());
                self.instructions.push(Instruction::Fence(fence));
            }
            Instruction::Capture(capture) => {
                self.used_qubits.extend(capture.frame.qubits.clone());
                self.instructions.push(Instruction::Capture(capture));
            }
            Instruction::Pulse(pulse) => {
                self.used_qubits.extend(pulse.frame.qubits.clone());
                self.instructions.push(Instruction::Pulse(pulse));
            }
            Instruction::RawCapture(raw_capture) => {
                self.used_qubits.extend(raw_capture.frame.qubits.clone());
                self.instructions.push(Instruction::RawCapture(raw_capture));
            }
            other => self.instructions.push(other),
        }
    }

    pub fn add_instructions<I>(&mut self, instructions: I)
    where
        I: IntoIterator<Item = Instruction>,
    {
        instructions
            .into_iter()
            .for_each(|i| self.add_instruction(i));
    }

    /// Creates a new conjugate transpose of the [`Program`] by reversing the order of gate
    /// instructions and applying the DAGGER modifier to each.
    ///
    ///
    /// # Errors
    ///
    /// Errors if any of the instructions in the program are not [`Instruction::Gate`]
    pub fn dagger(&self) -> Result<Self> {
        self.to_instructions().into_iter().try_rfold(
            Program::new(),
            |mut new_program, instruction| match instruction {
                Instruction::Gate(gate) => {
                    new_program.add_instruction(Instruction::Gate(gate.dagger()));
                    Ok(new_program)
                }
                _ => Err(ProgramError::UnsupportedOperation(instruction)),
            },
        )
    }

    /// Expand any instructions in the program which have a matching calibration, leaving the others
    /// unchanged. Recurses though each instruction while ensuring there is no cycle in the expansion
    /// graph (i.e. no calibration expands directly or indirectly into itself)
    pub fn expand_calibrations(&self) -> Result<Self> {
        let mut expanded_instructions: Vec<Instruction> = vec![];

        // TODO: Do this more efficiently, possibly with Vec::splice
        for instruction in &self.instructions {
            match self.calibrations.expand(instruction, &[])? {
                Some(expanded) => {
                    expanded_instructions.extend(expanded);
                }
                None => {
                    expanded_instructions.push(instruction.clone());
                }
            }
        }

        let mut new_program = Self {
            calibrations: self.calibrations.clone(),
            frames: self.frames.clone(),
            memory_regions: self.memory_regions.clone(),
            waveforms: self.waveforms.clone(),
            instructions: Vec::new(),
            used_qubits: HashSet::new(),
        };

        new_program.add_instructions(expanded_instructions);

        Ok(new_program)
    }

    /// Build a program from a list of instructions
    pub fn from_instructions(instructions: Vec<Instruction>) -> Self {
        let mut program = Self::default();
        for instruction in instructions {
            program.add_instruction(instruction);
        }
        program
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
    ) -> Option<MatchedFrames<'a>> {
        let qubits_used_by_program = self.get_used_qubits();

        instruction
            .get_frame_match_condition(qubits_used_by_program)
            .map(|condition| self.frames.get_matching_keys_for_conditions(condition))
    }

    /// Returns a HashSet consisting of every Qubit that is used in the program.
    pub fn get_used_qubits(&self) -> &HashSet<Qubit> {
        &self.used_qubits
    }

    /// Simplify this program into a new [`Program`] which contains only instructions
    /// and definitions which are executed; effectively, perform dead code removal.
    ///
    /// Removes:
    /// - All calibrations, following calibration expansion
    /// - Frame definitions which are not used by any instruction such as `PULSE` or `CAPTURE`
    /// - Waveform definitions which are not used by any instruction
    ///
    /// When a valid program is simplified, it remains valid.
    pub fn into_simplified(&self) -> Result<Self> {
        let mut expanded_program = self.expand_calibrations()?;
        // Remove calibrations such that the resulting program contains
        // only instructions. Calibrations have already been expanded, so
        // technically there is no need to keep them around anyway.
        expanded_program.calibrations = CalibrationSet::default();

        let mut frames_used: HashSet<&FrameIdentifier> = HashSet::new();
        let mut waveforms_used: HashSet<&String> = HashSet::new();

        for instruction in &expanded_program.instructions {
            if let Some(matched_frames) = expanded_program.get_frames_for_instruction(instruction) {
                frames_used.extend(matched_frames.used())
            }

            if let Some(waveform) = instruction.get_waveform_invocation() {
                waveforms_used.insert(&waveform.name);
            }
        }

        expanded_program.frames = self.frames.intersection(&frames_used);
        expanded_program
            .waveforms
            .retain(|name, _definition| waveforms_used.contains(name));

        Ok(expanded_program)
    }

    pub fn to_instructions(&self) -> Vec<Instruction> {
        let capacity = self.memory_regions.len()
            + self.frames.len()
            + self.waveforms.len()
            + self.instructions.len();

        let mut instructions: Vec<Instruction> = Vec::with_capacity(capacity);

        instructions.extend(self.memory_regions.iter().map(|(name, descriptor)| {
            Instruction::Declaration(Declaration {
                name: name.clone(),
                size: descriptor.size.clone(),
                sharing: descriptor.sharing.clone(),
            })
        }));
        instructions.extend(self.frames.to_instructions());
        instructions.extend(self.waveforms.iter().map(|(name, definition)| {
            Instruction::WaveformDefinition(WaveformDefinition {
                name: name.clone(),
                definition: definition.clone(),
            })
        }));
        instructions.extend(self.calibrations.to_instructions());
        instructions.extend(self.instructions.clone());
        instructions
    }

    /// Return the unitary of a program.
    ///
    /// # Errors
    ///
    /// Returns an error if the program contains instructions other than `Gate`s.
    pub fn to_unitary(&self, n_qubits: u64) -> Result<Matrix> {
        let mut umat = Array2::eye(2usize.pow(n_qubits as u32));
        for instruction in self.instructions.clone() {
            match instruction {
                Instruction::Halt => {}
                Instruction::Gate(mut gate) => {
                    umat = gate.to_unitary(n_qubits)?.dot(&umat);
                }
                _ => return Err(ProgramError::UnsupportedForUnitary(instruction)),
            }
        }
        Ok(umat)
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for instruction in self.to_instructions() {
            writeln!(f, "{instruction}")?;
        }
        Ok(())
    }
}

impl FromStr for Program {
    type Err = ProgramError;
    fn from_str(s: &str) -> Result<Self> {
        let input = LocatedSpan::new(s);
        let lexed = lex(input).map_err(ParseProgramError::<Self>::from)?;
        map_parsed(
            disallow_leftover(
                parse_instructions(&lexed).map_err(ParseError::from_nom_internal_err),
            ),
            |instructions| {
                let mut program = Self::new();
                program.add_instructions(instructions);
                program
            },
        )
        .map_err(ProgramError::from)
    }
}

impl From<Vec<Instruction>> for Program {
    fn from(instructions: Vec<Instruction>) -> Self {
        let mut p = Program::new();
        p.add_instructions(instructions);
        p
    }
}

impl<'a, 'b> ops::Add<&'b Program> for &'a Program {
    type Output = Program;

    fn add(self, rhs: &'b Program) -> Program {
        let mut new = self.clone();
        new.calibrations.extend(rhs.calibrations.clone());
        new.frames.merge(rhs.frames.clone());
        new.memory_regions.extend(rhs.memory_regions.clone());
        new.waveforms.extend(rhs.waveforms.clone());
        new.instructions.extend(rhs.instructions.clone());
        new
    }
}

#[cfg(test)]
mod tests {
    use super::Program;
    use crate::{
        imag,
        instruction::{Instruction, Matrix, Qubit},
        real,
    };
    use approx::assert_abs_diff_eq;
    use ndarray::{array, linalg::kron, Array2};
    use num_complex::Complex64;
    use once_cell::sync::Lazy;
    use rstest::rstest;
    use std::{collections::HashSet, str::FromStr};

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
DEFWAVEFORM custom:
    1,2
I 0
";
        let a = Program::from_str(input).unwrap();
        let b = Program::from_str(input).unwrap();
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
DEFWAVEFORM custom:
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
I 0
";
        let a = Program::from_str(input_a).unwrap();
        let b = Program::from_str(input_b).unwrap();
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

        assert_eq!(
            program.to_string(),
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
        let program1 = Program::from_str(input).unwrap().to_string();
        let program2 = Program::from_str(input).unwrap().to_string();

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
                vec![r#"0 "b""#, r#"0 1 "2q""#],
            ),
            (
                r#"PULSE 1 "c" custom_waveform"#,
                vec![r#"1 "c""#],
                vec![r#"0 1 "2q""#],
            ),
            // Pulses on non-declared frames and unused qubits do not use or block any frames in the program
            (r#"PULSE 2 "a" custom_waveform"#, vec![], vec![]),
            // Captures work identically to Pulses
            (
                r#"CAPTURE 0 "a" custom_waveform ro[0]"#,
                vec![r#"0 "a""#],
                vec![r#"0 "b""#, r#"0 1 "2q""#],
            ),
            (
                r#"CAPTURE 1 "c" custom_waveform ro[0]"#,
                vec![r#"1 "c""#],
                vec![r#"0 1 "2q""#],
            ),
            (r#"CAPTURE 2 "a" custom_waveform ro[0]"#, vec![], vec![]),
            // Raw Captures work identically to Pulses
            (
                r#"RAW-CAPTURE 0 "a" 1e-6 ro[0]"#,
                vec![r#"0 "a""#],
                vec![r#"0 "b""#, r#"0 1 "2q""#],
            ),
            (
                r#"RAW-CAPTURE 1 "c" 1e-6 ro[0]"#,
                vec![r#"1 "c""#],
                vec![r#"0 1 "2q""#],
            ),
            (r#"RAW-CAPTURE 2 "a" 1e-6 ro[0]"#, vec![], vec![]),
            // A non-blocking pulse blocks only its precise frame, not other frames on the same qubits
            (
                r#"NONBLOCKING PULSE 0 "a" custom_waveform"#,
                vec![r#"0 "a""#],
                vec![],
            ),
            (
                r#"NONBLOCKING PULSE 1 "c" custom_waveform"#,
                vec![r#"1 "c""#],
                vec![],
            ),
            (
                r#"NONBLOCKING PULSE 0 1 "2q" custom_waveform"#,
                vec![r#"0 1 "2q""#],
                vec![],
            ),
            // A Fence with qubits specified uses and blocks all frames intersecting that qubit
            (r#"FENCE 1"#, vec![], vec![r#"1 "c""#, r#"0 1 "2q""#]),
            // Fence-all uses and blocks all frames declared in the program
            (
                r#"FENCE"#,
                vec![],
                vec![r#"0 "a""#, r#"0 "b""#, r#"1 "c""#, r#"0 1 "2q""#],
            ),
            // Delay uses and blocks frames on exactly the given qubits and with any of the given names
            (r#"DELAY 0 1.0"#, vec![r#"0 "a""#, r#"0 "b""#], vec![]),
            (r#"DELAY 1 1.0"#, vec![r#"1 "c""#], vec![]),
            (r#"DELAY 1 "c" 1.0"#, vec![r#"1 "c""#], vec![]),
            (r#"DELAY 0 1 1.0"#, vec![r#"0 1 "2q""#], vec![]),
            (
                r#"SWAP-PHASES 0 "a" 0 "b""#,
                vec![r#"0 "a""#, r#"0 "b""#],
                vec![],
            ),
        ] {
            let instruction = Instruction::parse(instruction_string).unwrap();
            let matched_frames = program.get_frames_for_instruction(&instruction).unwrap();
            let used_frames: HashSet<String> = matched_frames
                .used()
                .iter()
                .map(|f| f.to_string())
                .collect();
            let expected_used_frames: HashSet<String> = expected_used_frames
                .into_iter()
                .map(|el| el.to_owned())
                .collect();
            assert_eq!(
                used_frames, expected_used_frames,
                "Instruction {instruction} *used* frames `{used_frames:?}` but we expected `{expected_used_frames:?}`"
            );

            let blocked_frames: HashSet<String> = matched_frames
                .blocked()
                .iter()
                .map(|f| f.to_string())
                .collect();
            let expected_blocked_frames: HashSet<String> = expected_blocked_frames
                .into_iter()
                .map(|el| el.to_owned())
                .collect();
            assert_eq!(
                blocked_frames, expected_blocked_frames,
                "Instruction {instruction} *blocked* frames `{blocked_frames:?}` but we expected `{expected_blocked_frames:?}`"
            );
        }
    }

    #[test]
    fn into_simplified() {
        let input = "
DEFCAL MEASURE 0 addr:
    CAPTURE 0 \"ro_rx\" custom addr

DEFCAL MEASURE 1 addr:
    CAPTURE 1 \"ro_rx\" custom addr

DEFFRAME 0 \"ro_rx\":
    ATTRIBUTE: \"value\"

DEFFRAME 1 \"ro_rx\":
    ATTRIBUTE: \"other\"

DEFWAVEFORM custom:
    0.0, 1.0

DEFWAVEFORM other_custom:
    2.0, 3.0

DECLARE ro BIT
MEASURE 0 ro
";

        let expected = "
DECLARE ro BIT

DEFFRAME 0 \"ro_rx\":
    ATTRIBUTE: \"value\"

DEFWAVEFORM custom:
    0.0, 1.0

CAPTURE 0 \"ro_rx\" custom ro
";
        let program = Program::from_str(input).map_err(|e| e.to_string()).unwrap();
        let program = program.into_simplified().unwrap();
        assert_eq!(program, Program::from_str(expected).unwrap());
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
        let expected_owned = vec![Qubit::Fixed(0), Qubit::Variable("q".to_string())];
        let expected = expected_owned.iter().collect::<HashSet<_>>();
        let actual = program.get_used_qubits();
        assert_eq!(expected, actual.iter().collect());
    }

    #[test]
    fn test_add_instructions() {
        let mut p = Program::new();
        let instrs = vec![Instruction::Nop, Instruction::Nop];
        p.add_instructions(instrs.clone());
        assert_eq!(p.instructions, instrs);
    }

    #[test]
    fn test_from_vec_instructions() {
        let expected: Program = "NOP\nNOP".parse().expect("Should parse NOPs");
        let p: Program = expected.instructions.clone().into();
        assert_eq!(expected, p);
    }

    #[test]
    fn test_clone_without_body_instructions() {
        let quil = "
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
        // Test is invalid if there are no body instructions
        let original = Program::from_str(quil).unwrap();
        assert!(!original.instructions.is_empty());

        let mut cloned = original.clone_without_body_instructions();
        // Make sure instruction list is empty.
        assert!(cloned.instructions.is_empty());
        assert!(cloned.used_qubits.is_empty());

        // Cloning the instruction list should make the programs equal again.
        // Need to use add_instructions because of the side effects, e.g. setting used_qubits.
        cloned.add_instructions(original.instructions.clone());
        assert_eq!(original, cloned);
    }

    static _0: Complex64 = real!(0.0);
    static _1: Complex64 = real!(1.0);
    static _I: Complex64 = imag!(1.0);
    static _1_SQRT_2: Complex64 = real!(std::f64::consts::FRAC_1_SQRT_2);
    static H: Lazy<Matrix> = Lazy::new(|| array![[_1, _1], [_1, -_1]] * _1_SQRT_2);
    static X: Lazy<Matrix> = Lazy::new(|| array![[_0, _1], [_1, _0]]);
    static Y: Lazy<Matrix> = Lazy::new(|| array![[_0, -_I], [_I, _0]]);
    static Z: Lazy<Matrix> = Lazy::new(|| array![[_1, _0], [_0, -_1]]);
    static CNOT: Lazy<Matrix> = Lazy::new(|| {
        array![
            [_1, _0, _0, _0],
            [_0, _1, _0, _0],
            [_0, _0, _0, _1],
            [_0, _0, _1, _0]
        ]
    });
    static I2: Lazy<Matrix> = Lazy::new(|| Array2::eye(2));
    static I4: Lazy<Matrix> = Lazy::new(|| Array2::eye(4));

    #[rstest]
    #[case("H 0\nH 1\nH 0", 2, &kron(&H, &I2))]
    #[case("H 0\nX 1\nY 2\nZ 3", 4, &kron(&Z, &kron(&Y, &kron(&X, &H))))]
    #[case("X 2\nCNOT 2 1\nCNOT 1 0", 3, &kron(&I2, &CNOT).dot(&kron(&CNOT, &I2)).dot(&kron(&X, &I4)))]
    fn test_to_unitary(#[case] input: &str, #[case] n_qubits: u64, #[case] expected: &Matrix) {
        let program = Program::from_str(input);
        assert!(program.is_ok());
        let matrix = program.unwrap().to_unitary(n_qubits);
        assert!(matrix.is_ok());
        assert_abs_diff_eq!(matrix.as_ref().unwrap(), expected);
    }
}
