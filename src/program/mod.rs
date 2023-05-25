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

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::str::FromStr;

use nom_locate::LocatedSpan;

use crate::instruction::{
    Declaration, FrameDefinition, FrameIdentifier, Instruction, Label, LabelPlaceholder, Qubit,
    QubitPlaceholder, Waveform, WaveformDefinition,
};
use crate::parser::{lex, parse_instructions, ParseError};
use crate::quil::Quil;

pub use self::calibration::CalibrationSet;
pub use self::error::{disallow_leftover, map_parsed, recover, ProgramError, SyntaxError};
pub use self::frame::FrameSet;
pub use self::memory::MemoryRegion;

mod calibration;
mod error;
pub(crate) mod frame;
pub mod graph;
mod memory;
pub mod type_check;

pub type Result<O> = std::result::Result<O, ProgramError<O>>;

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

    pub fn add_instructions(&mut self, instructions: Vec<Instruction>) {
        instructions
            .into_iter()
            .for_each(|i| self.add_instruction(i));
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
        include_blocked: bool,
    ) -> Option<HashSet<&'a FrameIdentifier>> {
        let qubits_used_by_program = self.get_used_qubits();

        instruction
            .get_frame_match_condition(include_blocked, qubits_used_by_program)
            .map(|condition| self.frames.get_matching_keys(condition))
    }

    fn get_labels(&self) -> Vec<&Label> {
        self.instructions
            .iter()
            .filter_map(|i| match i {
                Instruction::Label(label) => Some(label),
                _ => None,
            })
            .collect()
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
                Instruction::Capture(capture) => capture.frame.qubits.clone(),
                Instruction::Pulse(pulse) => pulse.frame.qubits.clone(),
                Instruction::RawCapture(raw_capture) => raw_capture.frame.qubits.clone(),
                _ => vec![],
            })
            .collect::<HashSet<_>>()
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
            if let Some(frames) = expanded_program.get_frames_for_instruction(instruction, false) {
                frames_used.extend(frames)
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

    /// Resolve [`LabelPlaceholder`]s and [`QubitPlaceholder`]s within the program such that the resolved values
    /// will remain unique to that placeholder within the scope of the program.
    ///
    /// If you provide `label_resolver` and/or `qubit_resolver`, those will be used to resolve those values respectively.
    /// If your placeholder returns `None` for a particular placeholder, it will not be replaced but will be left as a placeholder.
    ///
    /// If you do not provide a resolver for a placeholder, a default resolver will be used which will generate a unique value
    /// for that placeholder within the scope of the program using an auto-incrementing value (for qubit) or suffix (for label)
    /// while ensuring that unique value is not already in use within the program.
    #[allow(clippy::type_complexity)]
    pub fn resolve_placeholders(
        &mut self,
        label_resolver: Option<Box<dyn Fn(&LabelPlaceholder) -> Option<String>>>,
        qubit_resolver: Option<Box<dyn Fn(&QubitPlaceholder) -> Option<u64>>>,
    ) {
        let qubit_resolver = qubit_resolver.unwrap_or_else(|| {
            let mut qubits_used: HashSet<u64> = HashSet::new();
            let mut qubit_placeholders: BTreeSet<QubitPlaceholder> = BTreeSet::new();

            for qubit in self.get_used_qubits() {
                match qubit {
                    Qubit::Fixed(index) => {
                        qubits_used.insert(index);
                    }
                    Qubit::Placeholder(placeholder) => {
                        qubit_placeholders.insert(placeholder);
                    }
                    _ => {}
                }
            }

            let mut qubit_iterator = (0u64..).filter(|index| !qubits_used.contains(index));
            let qubit_resolutions: HashMap<QubitPlaceholder, u64> = qubit_placeholders
                .into_iter()
                .map(|p| (p, qubit_iterator.next().unwrap()))
                .collect();

            Box::new(move |key| qubit_resolutions.get(key).copied())
        });

        let label_resolver = label_resolver.unwrap_or_else(|| {
            let mut fixed_labels = HashSet::new();
            let mut label_placeholders = BTreeSet::new();
            for label in self.get_labels() {
                match label {
                    Label::Fixed(fixed) => {
                        fixed_labels.insert(fixed.clone());
                    }
                    Label::Placeholder(placeholder) => {
                        label_placeholders.insert(placeholder.clone());
                    }
                }
            }

            let label_resolutions: HashMap<LabelPlaceholder, String> = label_placeholders
                .into_iter()
                .map(|p| {
                    let base_label = p.as_inner();
                    let mut next_suffix = 0;

                    loop {
                        let next_label = format!("{base_label}_{next_suffix}");

                        if !fixed_labels.contains(&next_label) {
                            fixed_labels.insert(next_label.clone());
                            break (p, next_label);
                        }

                        next_suffix += 1;
                    }
                })
                .collect();

            Box::new(move |key| label_resolutions.get(key).cloned())
        });

        for instruction in &mut self.instructions {
            instruction.resolve_placeholders(&label_resolver, &qubit_resolver);
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
}

impl Quil for Program {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        for instruction in &self.to_instructions(true) {
            instruction.write(writer)?;
            writer.write_char('\n')?;
        }
        Ok(())
    }
}

impl FromStr for Program {
    type Err = ProgramError<Self>;
    fn from_str(s: &str) -> Result<Self> {
        let input = LocatedSpan::new(s);
        let lexed = lex(input).map_err(ProgramError::from)?;
        map_parsed(
            disallow_leftover(
                parse_instructions(&lexed).map_err(ParseError::from_nom_internal_err),
            ),
            |instructions| {
                let mut program = Self::new();
                for instruction in instructions {
                    program.add_instruction(instruction)
                }
                program
            },
        )
    }
}

impl From<Vec<Instruction>> for Program {
    fn from(instructions: Vec<Instruction>) -> Self {
        let mut p = Program::new();
        p.add_instructions(instructions);
        p
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::{collections::HashSet, str::FromStr};

    use crate::instruction::Instruction;
    use crate::instruction::Qubit;
    use crate::instruction::QubitPlaceholder;
    use crate::instruction::{Gate, Label, LabelPlaceholder};
    use crate::quil::Quil;

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
            (r#"FENCE 1"#, vec![], vec![r#"1 "c""#, r#"0 1 "2q""#]),
            // Fence-all uses and blocks all frames declared in the program
            (
                r#"FENCE"#,
                vec![],
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
            (
                r#"SWAP-PHASES 0 "a" 0 "b""#,
                vec![r#"0 "a""#, r#"0 "b""#],
                vec![r#"0 "a""#, r#"0 "b""#],
            ),
        ] {
            let instruction = Instruction::parse(instruction_string).unwrap();
            let used_frames: HashSet<String> = program
                .get_frames_for_instruction(&instruction, false)
                .unwrap_or_default()
                .into_iter()
                .map(|f| f.to_quil_or_debug())
                .collect();
            let expected_used_frames: HashSet<String> = expected_used_frames
                .into_iter()
                .map(|el| el.to_owned())
                .collect();
            assert_eq!(
                used_frames,
                expected_used_frames,
                "Instruction {} *used* frames `{:?}` but we expected `{:?}`",
                instruction.to_quil_or_debug(),
                used_frames,
                expected_used_frames
            );

            let blocked_frames: HashSet<String> = program
                .get_frames_for_instruction(&instruction, true)
                .unwrap()
                .into_iter()
                .map(|f| f.to_quil_or_debug())
                .collect();
            let expected_blocked_frames: HashSet<String> = expected_blocked_frames
                .into_iter()
                .map(|el| el.to_owned())
                .collect();
            assert_eq!(
                blocked_frames,
                expected_blocked_frames,
                "Instruction {} *blocked* frames `{:?}` but we expected `{:?}`",
                instruction.to_quil_or_debug(),
                blocked_frames,
                expected_blocked_frames
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
        let expected = vec![Qubit::Fixed(0), Qubit::Variable("q".to_string())]
            .into_iter()
            .collect::<HashSet<_>>();
        let actual = program.get_used_qubits();
        assert_eq!(expected, actual);
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
    fn placeholder_replacement() {
        let placeholder_1 = QubitPlaceholder::default();
        let placeholder_2 = QubitPlaceholder::default();
        let label_placeholder_1 = LabelPlaceholder::new(String::from("custom_label"));
        let label_placeholder_2 = LabelPlaceholder::new(String::from("custom_label"));

        let mut program = Program::new();

        program.add_instruction(Instruction::Label(Label::Placeholder(
            label_placeholder_1.clone(),
        )));

        program.add_instruction(Instruction::Label(Label::Placeholder(
            label_placeholder_2.clone(),
        )));

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
        auto_increment_resolved.resolve_placeholders(None, None);
        assert_eq!(
            auto_increment_resolved.instructions,
            vec![
                Instruction::Label(Label::Fixed("custom_label_0".to_string())),
                Instruction::Label(Label::Fixed("custom_label_1".to_string())),
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
        let custom_label_resolutions = HashMap::from([
            (label_placeholder_1, "new_label".to_string()),
            (label_placeholder_2, "other_new_label".to_string()),
        ]);
        let custom_qubit_resolutions = HashMap::from([(placeholder_1, 42), (placeholder_2, 10000)]);
        custom_resolved.resolve_placeholders(
            Some(Box::new(move |placeholder| {
                custom_label_resolutions.get(placeholder).cloned()
            })),
            Some(Box::new(move |placeholder| {
                custom_qubit_resolutions.get(placeholder).copied()
            })),
        );
        assert_eq!(
            custom_resolved.instructions,
            vec![
                Instruction::Label(Label::Fixed("new_label".to_string())),
                Instruction::Label(Label::Fixed("other_new_label".to_string())),
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
