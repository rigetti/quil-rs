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

use std::collections::{BTreeMap, HashMap, HashSet};
use std::ops;
use std::str::FromStr;

use indexmap::IndexSet;
use ndarray::Array2;
use nom_locate::LocatedSpan;

use crate::instruction::{
    Declaration, FrameDefinition, FrameIdentifier, GateDefinition, GateError, Instruction, Matrix,
    Qubit, QubitPlaceholder, Target, TargetPlaceholder, Waveform, WaveformDefinition,
};
use crate::parser::{lex, parse_instructions, ParseError};
use crate::quil::Quil;

pub use self::calibration::CalibrationSet;
pub use self::error::{disallow_leftover, map_parsed, recover, ParseProgramError, SyntaxError};
pub use self::frame::FrameSet;
pub use self::frame::MatchedFrames;
pub use self::memory::{MemoryAccesses, MemoryRegion};

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

    #[error("this operation isn't supported on instruction: {}", .0.to_quil_or_debug())]
    UnsupportedOperation(Instruction),

    #[error("instruction {} expands into itself", .0.to_quil_or_debug())]
    RecursiveCalibration(Instruction),

    #[error("{0}")]
    GateError(#[from] GateError),

    #[error("can only compute program unitary for programs composed of `Gate`s; found unsupported instruction: {}", .0.to_quil_or_debug())]
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
    pub gate_definitions: BTreeMap<String, GateDefinition>,
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
            gate_definitions: BTreeMap::new(),
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
            gate_definitions: self.gate_definitions.clone(),
            waveforms: self.waveforms.clone(),
            used_qubits: HashSet::new(),
        }
    }

    /// Add an instruction to the end of the program.
    pub fn add_instruction(&mut self, instruction: Instruction) {
        self.used_qubits
            .extend(instruction.get_qubits().into_iter().cloned());

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
            Instruction::GateDefinition(gate_definition) => {
                self.gate_definitions
                    .insert(gate_definition.name.clone(), gate_definition);
            }
            Instruction::MeasureCalibrationDefinition(calibration) => {
                self.calibrations.push_measurement_calibration(calibration);
            }
            Instruction::WaveformDefinition(WaveformDefinition { name, definition }) => {
                self.waveforms.insert(name, definition);
            }
            Instruction::Gate(gate) => {
                self.instructions.push(Instruction::Gate(gate));
            }
            Instruction::Measurement(measurement) => {
                self.instructions
                    .push(Instruction::Measurement(measurement));
            }
            Instruction::Reset(reset) => {
                self.instructions.push(Instruction::Reset(reset));
            }
            Instruction::Delay(delay) => {
                self.instructions.push(Instruction::Delay(delay));
            }
            Instruction::Fence(fence) => {
                self.instructions.push(Instruction::Fence(fence));
            }
            Instruction::Capture(capture) => {
                self.instructions.push(Instruction::Capture(capture));
            }
            Instruction::Pulse(pulse) => {
                self.instructions.push(Instruction::Pulse(pulse));
            }
            Instruction::RawCapture(raw_capture) => {
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
            gate_definitions: self.gate_definitions.clone(),
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

    /// Return references to all targets used in the program.
    fn get_targets(&self) -> Vec<&Target> {
        self.instructions
            .iter()
            .filter_map(|i| match i {
                Instruction::Label(label) => Some(&label.target),
                Instruction::Jump(jump) => Some(&jump.target),
                Instruction::JumpWhen(jump_when) => Some(&jump_when.target),
                Instruction::JumpUnless(jump_unless) => Some(&jump_unless.target),
                _ => None,
            })
            .collect()
    }

    /// Returns a HashSet consisting of every Qubit that is used in the program.
    pub fn get_used_qubits(&self) -> &HashSet<Qubit> {
        &self.used_qubits
    }

    /// Rebuilds the used_qubits cache from scratch
    fn rebuild_used_qubits(&mut self) {
        self.used_qubits = self
            .to_instructions()
            .iter()
            .flat_map(|instruction| instruction.get_qubits().into_iter().cloned())
            .collect()
    }

    /// Consume the [`Program`] to return all of the instructions which constitute it.
    pub fn into_instructions(self) -> Vec<Instruction> {
        let capacity = self.memory_regions.len()
            + self.frames.len()
            + self.waveforms.len()
            + self.gate_definitions.len()
            + self.instructions.len();

        let mut instructions: Vec<Instruction> = Vec::with_capacity(capacity);

        instructions.extend(self.memory_regions.into_iter().map(|(name, descriptor)| {
            Instruction::Declaration(Declaration {
                name,
                size: descriptor.size,
                sharing: descriptor.sharing,
            })
        }));
        instructions.extend(self.frames.into_instructions());
        instructions.extend(self.waveforms.into_iter().map(|(name, definition)| {
            Instruction::WaveformDefinition(WaveformDefinition { name, definition })
        }));
        instructions.extend(self.calibrations.into_instructions());
        instructions.extend(
            self.gate_definitions
                .into_values()
                .map(Instruction::GateDefinition),
        );
        instructions.extend(self.instructions);
        instructions
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

    /// Resolve [`LabelPlaceholder`]s and [`QubitPlaceholder`]s within the program using default resolvers.
    ///
    /// See [`resolve_placeholders_with_custom_resolvers`](Self::resolve_placeholders_with_custom_resolvers),
    /// [`default_target_resolver`](Self::default_target_resolver),
    /// and [`default_qubit_resolver`](Self::default_qubit_resolver) for more information.
    pub fn resolve_placeholders(&mut self) {
        self.resolve_placeholders_with_custom_resolvers(
            self.default_target_resolver(),
            self.default_qubit_resolver(),
        )
    }

    /// Resolve [`TargetPlaceholder`]s and [`QubitPlaceholder`]s within the program such that the resolved values
    /// will remain unique to that placeholder within the scope of the program.
    ///
    /// The provided `target_resolver` and `qubit_resolver`, will be used to resolve those values respectively.
    /// If your placeholder returns `None` for a particular placeholder, it will not be replaced but will be left as a placeholder.
    ///
    /// If you wish to provide a resolver for either labels or qubits, but want to rely on the
    /// default behavior for the other, considering using either
    /// [`default_qubit_resolver`](Self::default_qubit_resolver) or [`default_target_resolver`](Self::default_target_resolver).
    #[allow(clippy::type_complexity)]
    pub fn resolve_placeholders_with_custom_resolvers(
        &mut self,
        target_resolver: Box<dyn Fn(&TargetPlaceholder) -> Option<String>>,
        qubit_resolver: Box<dyn Fn(&QubitPlaceholder) -> Option<u64>>,
    ) {
        for instruction in &mut self.instructions {
            instruction.resolve_placeholders(&target_resolver, &qubit_resolver);
        }
        self.rebuild_used_qubits()
    }

    /// The default target resolver will resolve each [`TargetPlaceholder`] in the program to a unique target
    /// by applying an auto-incrementing suffix to the base target.
    #[allow(clippy::type_complexity)]
    pub fn default_target_resolver(&self) -> Box<dyn Fn(&TargetPlaceholder) -> Option<String>> {
        let mut fixed_labels = HashSet::new();
        let mut label_placeholders = IndexSet::new();
        for target in self.get_targets() {
            match target {
                Target::Fixed(fixed) => {
                    fixed_labels.insert(fixed.clone());
                }
                Target::Placeholder(placeholder) => {
                    label_placeholders.insert(placeholder.clone());
                }
            }
        }

        let target_resolutions: HashMap<TargetPlaceholder, String> = label_placeholders
            .into_iter()
            .map(|p| {
                let base_label = p.as_inner();
                let mut next_label = format!("{base_label}_0");
                let mut next_suffix = 1;

                while fixed_labels.contains(&next_label) {
                    next_label = format!("{base_label}_{next_suffix}");
                    next_suffix += 1;
                }
                fixed_labels.insert(next_label.clone());

                (p, next_label)
            })
            .collect();

        Box::new(move |key| target_resolutions.get(key).cloned())
    }

    /// The default qubit resolver will resolve each [`QubitPlaceholder`] in the program to
    /// a unique fixed qubit index by incrementing to the next available index.
    #[allow(clippy::type_complexity)]
    pub fn default_qubit_resolver(&self) -> Box<dyn Fn(&QubitPlaceholder) -> Option<u64>> {
        let mut qubits_used: HashSet<u64> = HashSet::new();
        let mut qubit_placeholders: IndexSet<QubitPlaceholder> = IndexSet::new();

        // Stable iteration order makes placeholder resolution deterministic
        for instruction in &self.instructions {
            let qubits = instruction.get_qubits();

            for qubit in qubits {
                match qubit {
                    Qubit::Fixed(index) => {
                        qubits_used.insert(*index);
                    }
                    Qubit::Placeholder(placeholder) => {
                        qubit_placeholders.insert(placeholder.clone());
                    }
                    Qubit::Variable(_) => {}
                }
            }
        }

        let qubit_iterator = (0u64..).filter(|index| !qubits_used.contains(index));
        let qubit_resolutions: HashMap<QubitPlaceholder, u64> =
            qubit_placeholders.into_iter().zip(qubit_iterator).collect();

        Box::new(move |key| qubit_resolutions.get(key).copied())
    }

    /// Return a copy of all of the instructions which constitute this [`Program`].
    pub fn to_instructions(&self) -> Vec<Instruction> {
        let capacity = self.memory_regions.len()
            + self.frames.len()
            + self.waveforms.len()
            + self.gate_definitions.len()
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
        instructions.extend(
            self.gate_definitions
                .values()
                .cloned()
                .map(Instruction::GateDefinition),
        );
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

    /// Get a reference to the [`Instruction`] at the given index, if present.
    pub fn get_instruction(&self, index: usize) -> Option<&Instruction> {
        self.instructions.get(index)
    }
}

impl Quil for Program {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        for instruction in self.to_instructions() {
            instruction.write(writer, fall_back_to_debug)?;
            writeln!(writer)?;
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

impl ops::Add<Program> for Program {
    type Output = Program;

    fn add(self, rhs: Program) -> Program {
        let mut new_program = self;
        new_program.calibrations.extend(rhs.calibrations);
        new_program.memory_regions.extend(rhs.memory_regions);
        new_program.frames.merge(rhs.frames);
        new_program.waveforms.extend(rhs.waveforms);
        new_program.gate_definitions.extend(rhs.gate_definitions);
        new_program.instructions.extend(rhs.instructions);
        new_program.used_qubits.extend(rhs.used_qubits);
        new_program
    }
}

impl ops::AddAssign<Program> for Program {
    fn add_assign(&mut self, rhs: Program) {
        self.calibrations.extend(rhs.calibrations);
        self.memory_regions.extend(rhs.memory_regions);
        self.frames.merge(rhs.frames);
        self.waveforms.extend(rhs.waveforms);
        self.gate_definitions.extend(rhs.gate_definitions);
        self.instructions.extend(rhs.instructions);
        self.used_qubits.extend(rhs.used_qubits);
    }
}

#[cfg(test)]
mod tests {
    use super::Program;
    use crate::{
        imag,
        instruction::{
            Gate, Instruction, Jump, JumpUnless, JumpWhen, Label, Matrix, MemoryReference, Qubit,
            QubitPlaceholder, Target, TargetPlaceholder,
        },
        quil::Quil,
        real,
    };
    use approx::assert_abs_diff_eq;
    use insta::assert_debug_snapshot;
    use ndarray::{array, linalg::kron, Array2};
    use num_complex::Complex64;
    use once_cell::sync::Lazy;
    use rstest::rstest;
    use std::{
        collections::{HashMap, HashSet},
        str::FromStr,
    };

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
            program.to_quil().unwrap(),
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
        let program1 = Program::from_str(input).unwrap().to_quil().unwrap();
        let program2 = Program::from_str(input).unwrap().to_quil().unwrap();

        // verify that each memory declaration in the program is in the same index as the same
        // program after being re-parsed and serialized.
        assert!(program1.lines().eq(program2.lines()));
    }

    #[test]
    fn frame_blocking() {
        let input = r#"DEFFRAME 0 "a":
    HARDWARE-OBJECT: "hardware"

DEFFRAME 0 "b":
    HARDWARE-OBJECT: "hardware"

DEFFRAME 1 "c":
    HARDWARE-OBJECT: "hardware"

DEFFRAME 0 1 "0-1 2q":
    HARDWARE-OBJECT: "hardware"

DEFFRAME 2 "d":
    HARDWARE-OBJECT: "hardware"

DEFFRAME 0 2 "0-2 2q":
    HARDWARE-OBJECT: "hardware"
"#;

        let program = Program::from_str(input).unwrap();

        for (instruction_string, expected_used_frames, expected_blocked_frames) in vec![
            // Blocking pulses use only the specified frame but block frames intersecting the frame's qubits
            (
                r#"PULSE 0 "a" custom_waveform"#,
                vec![r#"0 "a""#],
                vec![r#"0 "b""#, r#"0 1 "0-1 2q""#, r#"0 2 "0-2 2q""#],
            ),
            (
                r#"PULSE 1 "c" custom_waveform"#,
                vec![r#"1 "c""#],
                vec![r#"0 1 "0-1 2q""#],
            ),
            // Pulses on non-declared frames and unused qubits do not use or block any frames in the program
            (r#"PULSE 3 "a" custom_waveform"#, vec![], vec![]),
            // Captures work identically to Pulses
            (
                r#"CAPTURE 0 "a" custom_waveform ro[0]"#,
                vec![r#"0 "a""#],
                vec![r#"0 "b""#, r#"0 1 "0-1 2q""#, r#"0 2 "0-2 2q""#],
            ),
            (
                r#"CAPTURE 1 "c" custom_waveform ro[0]"#,
                vec![r#"1 "c""#],
                vec![r#"0 1 "0-1 2q""#],
            ),
            (r#"CAPTURE 3 "a" custom_waveform ro[0]"#, vec![], vec![]),
            // Raw Captures work identically to Pulses
            (
                r#"RAW-CAPTURE 0 "a" 1e-6 ro[0]"#,
                vec![r#"0 "a""#],
                vec![r#"0 "b""#, r#"0 1 "0-1 2q""#, r#"0 2 "0-2 2q""#],
            ),
            (
                r#"RAW-CAPTURE 1 "c" 1e-6 ro[0]"#,
                vec![r#"1 "c""#],
                vec![r#"0 1 "0-1 2q""#],
            ),
            (r#"RAW-CAPTURE 3 "a" 1e-6 ro[0]"#, vec![], vec![]),
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
                r#"NONBLOCKING PULSE 0 1 "0-1 2q" custom_waveform"#,
                vec![r#"0 1 "0-1 2q""#],
                vec![],
            ),
            // A Fence with qubits specified uses and blocks all frames intersecting that qubit
            (r#"FENCE 1"#, vec![], vec![r#"1 "c""#, r#"0 1 "0-1 2q""#]),
            // Fence-all uses and blocks all frames declared in the program
            (
                r#"FENCE"#,
                vec![],
                vec![
                    r#"0 "a""#,
                    r#"0 "b""#,
                    r#"1 "c""#,
                    r#"0 1 "0-1 2q""#,
                    r#"0 2 "0-2 2q""#,
                    r#"2 "d""#,
                ],
            ),
            // Delay uses and blocks frames on exactly the given qubits and with any of the given names
            (r#"DELAY 0 1.0"#, vec![r#"0 "a""#, r#"0 "b""#], vec![]),
            (r#"DELAY 1 1.0"#, vec![r#"1 "c""#], vec![]),
            (r#"DELAY 1 "c" 1.0"#, vec![r#"1 "c""#], vec![]),
            (r#"DELAY 0 1 "0-1 2q" 1.0"#, vec![r#"0 1 "0-1 2q""#], vec![]),
            (r#"DELAY 0 1.0"#, vec![r#"0 "a""#, r#"0 "b""#], vec![]),
            (
                r#"DELAY 0 1 0.6"#,
                vec![r#"0 "a""#, r#"0 "b""#, r#"1 "c""#, r#"0 1 "0-1 2q""#],
                vec![],
            ),
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
                .map(|f| f.to_quil_or_debug())
                .collect();
            let expected_used_frames: HashSet<String> = expected_used_frames
                .into_iter()
                .map(|el| el.to_owned())
                .collect();
            assert_eq!(
                used_frames, expected_used_frames,
                "Instruction {instruction} *used* frames `{used_frames:?}` but we expected `{expected_used_frames:?}`", instruction=instruction.to_quil_or_debug()
            );

            let blocked_frames: HashSet<String> = matched_frames
                .blocked()
                .iter()
                .map(|f| f.to_quil_or_debug())
                .collect();
            let expected_blocked_frames: HashSet<String> = expected_blocked_frames
                .into_iter()
                .map(|el| el.to_owned())
                .collect();
            assert_eq!(
                blocked_frames, expected_blocked_frames,
                "Instruction {instruction} *blocked* frames `{blocked_frames:?}` but we expected `{expected_blocked_frames:?}`", instruction=instruction.to_quil_or_debug()
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
    fn test_add_programs() {
        let lhs_input = "
DECLARE ro BIT

MEASURE q ro
X q

DEFCAL I 0:
    DELAY 0 1.0
DEFFRAME 0 \"rx\":
    HARDWARE-OBJECT: \"hardware\"
DEFWAVEFORM custom:
    1,2
DEFGATE FOO:
    1, 0
    0, 1
I 0
";
        let rhs_input = "
DECLARE foo REAL
H 1
CNOT 2 3

DEFCAL I 1:
    DELAY 0 1.0
DEFFRAME 1 \"rx\":
    HARDWARE-OBJECT: \"hardware\"
DEFWAVEFORM custom2:
    1,2
DEFGATE BAR:
    0, 1
    1, 0
";
        let lhs = Program::from_str(lhs_input).unwrap();
        let rhs = Program::from_str(rhs_input).unwrap();

        let sum = lhs.clone() + rhs.clone();
        let mut in_place_sum = lhs.clone();
        in_place_sum += rhs;

        let expected_qubits = vec![
            Qubit::Fixed(0),
            Qubit::Fixed(1),
            Qubit::Fixed(2),
            Qubit::Fixed(3),
            Qubit::Variable("q".to_string()),
        ];

        let expected_qubits = expected_qubits.iter().collect::<HashSet<_>>();
        for program in [&sum, &in_place_sum] {
            assert_eq!(program.calibrations.len(), 2);
            assert_eq!(program.memory_regions.len(), 2);
            assert_eq!(program.frames.len(), 2);
            assert_eq!(program.waveforms.len(), 2);
            assert_eq!(program.instructions.len(), 5);
            assert_eq!(expected_qubits, sum.get_used_qubits().iter().collect());
        }
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

    /// Tests that the various methods of getting the instructions from a Program produce
    /// consistent results.
    #[test]
    fn test_to_instructions() {
        let input = "DECLARE foo REAL[1]
DEFFRAME 1 \"rx\":
\tHARDWARE-OBJECT: \"hardware\"
DEFWAVEFORM custom2:
\t1, 2
DEFCAL I 1:
\tDELAY 0 1
DEFGATE BAR AS MATRIX:
\t0, 1
\t1, 0

H 1
CNOT 2 3
";
        let program = Program::from_str(input).unwrap();
        assert_debug_snapshot!(program.to_instructions());
        assert_eq!(program.to_quil().unwrap(), input);
        assert_eq!(program.to_instructions(), program.into_instructions());
    }

    #[test]
    fn placeholder_replacement() {
        let placeholder_1 = QubitPlaceholder::default();
        let placeholder_2 = QubitPlaceholder::default();
        let label_placeholder_1 = TargetPlaceholder::new(String::from("custom_label"));
        let label_placeholder_2 = TargetPlaceholder::new(String::from("custom_label"));

        let mut program = Program::new();

        program.add_instruction(Instruction::Label(Label {
            target: Target::Placeholder(label_placeholder_1.clone()),
        }));

        program.add_instruction(Instruction::Jump(Jump {
            target: Target::Placeholder(label_placeholder_2.clone()),
        }));

        program.add_instruction(Instruction::JumpWhen(JumpWhen {
            target: Target::Placeholder(label_placeholder_2.clone()),
            condition: MemoryReference {
                name: "ro".to_string(),
                index: 0,
            },
        }));

        program.add_instruction(Instruction::JumpUnless(JumpUnless {
            target: Target::Placeholder(label_placeholder_2.clone()),
            condition: MemoryReference {
                name: "ro".to_string(),
                index: 0,
            },
        }));

        program.add_instruction(Instruction::Gate(Gate {
            name: "X".to_string(),
            qubits: vec![Qubit::Placeholder(placeholder_1.clone())],
            parameters: vec![],
            modifiers: vec![],
        }));

        program.add_instruction(Instruction::Gate(Gate {
            name: "Y".to_string(),
            qubits: vec![Qubit::Placeholder(placeholder_2.clone())],
            parameters: vec![],
            modifiers: vec![],
        }));

        let mut auto_increment_resolved = program.clone();
        auto_increment_resolved.resolve_placeholders();
        assert_eq!(
            auto_increment_resolved.instructions,
            vec![
                Instruction::Label(Label {
                    target: Target::Fixed("custom_label_0".to_string())
                }),
                Instruction::Jump(Jump {
                    target: Target::Fixed("custom_label_1".to_string()),
                }),
                Instruction::JumpWhen(JumpWhen {
                    target: Target::Fixed("custom_label_1".to_string()),
                    condition: MemoryReference {
                        name: "ro".to_string(),
                        index: 0,
                    },
                }),
                Instruction::JumpUnless(JumpUnless {
                    target: Target::Fixed("custom_label_1".to_string()),
                    condition: MemoryReference {
                        name: "ro".to_string(),
                        index: 0,
                    },
                }),
                Instruction::Gate(Gate {
                    name: "X".to_string(),
                    qubits: vec![Qubit::Fixed(0)],
                    parameters: vec![],
                    modifiers: vec![],
                }),
                Instruction::Gate(Gate {
                    name: "Y".to_string(),
                    qubits: vec![Qubit::Fixed(1)],
                    parameters: vec![],
                    modifiers: vec![],
                }),
            ]
        );

        let mut custom_resolved = program.clone();
        let custom_target_resolutions = HashMap::from([
            (label_placeholder_1, "new_label".to_string()),
            (label_placeholder_2, "other_new_label".to_string()),
        ]);
        let custom_qubit_resolutions = HashMap::from([(placeholder_1, 42), (placeholder_2, 10000)]);
        custom_resolved.resolve_placeholders_with_custom_resolvers(
            Box::new(move |placeholder| custom_target_resolutions.get(placeholder).cloned()),
            Box::new(move |placeholder| custom_qubit_resolutions.get(placeholder).copied()),
        );
        assert_eq!(
            custom_resolved.instructions,
            vec![
                Instruction::Label(Label {
                    target: Target::Fixed("new_label".to_string())
                }),
                Instruction::Jump(Jump {
                    target: Target::Fixed("other_new_label".to_string()),
                }),
                Instruction::JumpWhen(JumpWhen {
                    target: Target::Fixed("other_new_label".to_string()),
                    condition: MemoryReference {
                        name: "ro".to_string(),
                        index: 0,
                    },
                }),
                Instruction::JumpUnless(JumpUnless {
                    target: Target::Fixed("other_new_label".to_string()),
                    condition: MemoryReference {
                        name: "ro".to_string(),
                        index: 0,
                    },
                }),
                Instruction::Gate(Gate {
                    name: "X".to_string(),
                    qubits: vec![Qubit::Fixed(42)],
                    parameters: vec![],
                    modifiers: vec![],
                }),
                Instruction::Gate(Gate {
                    name: "Y".to_string(),
                    qubits: vec![Qubit::Fixed(10000)],
                    parameters: vec![],
                    modifiers: vec![],
                }),
            ]
        );
    }
}
