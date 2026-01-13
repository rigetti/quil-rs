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
//See the License for the specific language governing permissions and
// limitations under the License.

// TODO (#453): Address large error types.
#![allow(clippy::result_large_err)]

use std::collections::{HashMap, HashSet};
use std::ops::{self};
use std::str::FromStr;

use indexmap::{IndexMap, IndexSet};
use ndarray::Array2;
use nom_locate::LocatedSpan;
use petgraph::algo::DfsSpace;
use petgraph::Graph;

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pymethods};

use crate::instruction::{
    Arithmetic, ArithmeticOperand, ArithmeticOperator, Call, Declaration,
    DefGateSequenceExpansionError, ExternError, ExternPragmaMap, ExternSignatureMap,
    FrameDefinition, FrameIdentifier, GateDefinition, GateError, GateSpecification, Instruction,
    InstructionHandler, Jump, JumpUnless, Label, Matrix, MemoryReference, Move, Pragma, Qubit,
    QubitPlaceholder, ScalarType, Target, TargetPlaceholder, Vector, Waveform, WaveformDefinition,
    RESERVED_PRAGMA_EXTERN,
};
use crate::parser::{lex, parse_instructions, ParseError};
use crate::program::defgate_sequence_expansion::{
    ExpandedInstructionsWithSourceMap, ProgramDefGateSequenceExpander,
};
use crate::quil::Quil;

pub use self::calibration::{
    CalibrationExpansion, CalibrationExpansionOutput, CalibrationSource, Calibrations,
};
pub use self::calibration_set::CalibrationSet;
pub use self::defgate_sequence_expansion::DefGateSequenceExpansion;
pub use self::error::{
    disallow_leftover, map_parsed, recover, LeftoverError, ParseProgramError, SyntaxError,
};
pub use self::frame::FrameSet;
pub use self::frame::MatchedFrames;
pub use self::memory::{MemoryAccesses, MemoryAccessesError, MemoryRegion};
pub use self::source_map::{ExpansionResult, SourceMap, SourceMapEntry, SourceMapIndexable};

pub mod analysis;
mod calibration;
mod calibration_set;
mod defgate_sequence_expansion;
mod error;
pub(crate) mod frame;
mod memory;
pub mod scheduling;
mod source_map;
pub mod type_check;

#[cfg(not(feature = "python"))]
use optipy::strip_pyo3;
#[cfg(feature = "python")]
pub(crate) mod quilpy;

// TODO (#453): Address large error types.
#[allow(clippy::large_enum_variant)]
#[derive(Clone, Debug, PartialEq, thiserror::Error)]
pub enum ProgramError {
    #[error("{0}")]
    ParsingError(#[from] ParseProgramError<Program>),

    #[error("this operation isn't supported on instruction: {}", .0.to_quil_or_debug())]
    UnsupportedOperation(Instruction),

    #[error("instruction {} expands into itself", .0.to_quil_or_debug())]
    RecursiveCalibration(Instruction),

    #[error("{0}")]
    GateError(#[from] GateError),

    #[error(transparent)]
    DefGateSequenceExpansionError(#[from] DefGateSequenceExpansionError),

    #[error("can only compute program unitary for programs composed of `Gate`s; found unsupported instruction: {}", .0.to_quil_or_debug())]
    UnsupportedForUnitary(Instruction),
}

type Result<T> = std::result::Result<T, ProgramError>;

/// A Quil Program instance describes a quantum program with metadata used in execution.
///
/// This contains not only instructions which are executed in turn on the quantum processor, but
/// also the "headers" used to describe and manipulate those instructions, such as calibrations
/// and frame definitions.
#[derive(Clone, Debug, Default, PartialEq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(feature = "python", pyo3::pyclass(module = "quil.program", eq))]
#[cfg_attr(not(feature = "python"), strip_pyo3)]
pub struct Program {
    #[pyo3(get, set)]
    pub calibrations: Calibrations,
    #[pyo3(get, name = "pragma_extern_map")]
    pub extern_pragma_map: ExternPragmaMap,
    #[pyo3(get, set)]
    pub frames: FrameSet,
    #[pyo3(get, set)]
    pub memory_regions: IndexMap<String, MemoryRegion>,
    #[pyo3(get, set)]
    pub waveforms: IndexMap<String, Waveform>,
    #[pyo3(get, set)]
    pub gate_definitions: IndexMap<String, GateDefinition>,
    #[pyo3(get, set)]
    instructions: Vec<Instruction>,
    // private field used for caching operations
    #[pyo3(get)]
    used_qubits: HashSet<Qubit>,
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[cfg_attr(feature = "python", pyo3::pymethods)]
#[cfg_attr(not(feature = "python"), strip_pyo3)]
impl Program {
    #[new]
    pub fn new() -> Self {
        Program::default()
    }

    /// Like `Clone`, but does not clone the body instructions.
    pub fn clone_without_body_instructions(&self) -> Self {
        Self {
            calibrations: self.calibrations.clone(),
            extern_pragma_map: self.extern_pragma_map.clone(),
            frames: self.frames.clone(),
            memory_regions: self.memory_regions.clone(),
            waveforms: self.waveforms.clone(),
            gate_definitions: self.gate_definitions.clone(),
            instructions: Vec::new(),
            used_qubits: HashSet::new(),
        }
    }

    /// Add an instruction to the end of the program.
    ///
    /// Note, parsing extern signatures is deferred here to maintain infallibility
    /// of [`Program::add_instruction`]. This means that invalid `PRAGMA EXTERN`
    /// instructions are still added to the [`Program::extern_pragma_map`];
    /// duplicate `PRAGMA EXTERN` names are overwritten.
    pub fn add_instruction(&mut self, instruction: Instruction) {
        self.used_qubits
            .extend(instruction.get_qubits().into_iter().cloned());

        match instruction {
            Instruction::CalibrationDefinition(calibration) => {
                self.calibrations.insert_calibration(calibration);
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
                self.calibrations
                    .insert_measurement_calibration(calibration);
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
            Instruction::Pragma(pragma) if pragma.name == RESERVED_PRAGMA_EXTERN => {
                self.extern_pragma_map.insert(pragma);
            }
            Instruction::RawCapture(raw_capture) => {
                self.instructions.push(Instruction::RawCapture(raw_capture));
            }
            other => self.instructions.push(other),
        }
    }

    /// Creates a new conjugate transpose of the [`Program`] by reversing the order of gate
    /// instructions and applying the DAGGER modifier to each.
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

    /// Expand any instructions in the program which have a matching calibration,
    /// leaving the others unchanged.
    /// Return the expanded copy of the program.
    ///
    /// Returns an error if any instruction expands into itself.
    ///
    /// See [`Program::expand_calibrations_with_source_map`] for a version that returns a source mapping.
    pub fn expand_calibrations(&self) -> Result<Self> {
        self.expand_calibrations_inner(None)
    }

    /// Return a copy of the [`Program`] wrapped in a loop that repeats `iterations` times.
    ///
    /// The loop is constructed by wrapping the body of the program in classical Quil instructions.
    /// The given `loop_count_reference` must refer to an INTEGER memory region. The value at the
    /// reference given will be set to `iterations` and decremented in the loop. The loop will
    /// terminate when the reference reaches 0. For this reason your program should not itself
    /// modify the value at the reference unless you intend to modify the remaining number of
    /// iterations (i.e. to break the loop).
    ///
    /// The given `start_target` and `end_target` will be used as the entry and exit points for the
    /// loop, respectively. You should provide unique [`Target`]s that won't be used elsewhere in
    /// the program.
    ///
    /// If `iterations` is 0, then a copy of the program is returned without any changes.
    pub fn wrap_in_loop(
        &self,
        loop_count_reference: MemoryReference,
        start_target: Target,
        end_target: Target,
        iterations: u32,
    ) -> Self {
        if iterations == 0 {
            return self.clone();
        }

        let mut looped_program = self.clone_without_body_instructions();

        looped_program.add_instructions(
            vec![
                Instruction::Declaration(Declaration {
                    name: loop_count_reference.name.clone(),
                    size: Vector {
                        data_type: ScalarType::Integer,
                        length: 1,
                    },
                    sharing: None,
                }),
                Instruction::Move(Move {
                    destination: loop_count_reference.clone(),
                    source: ArithmeticOperand::LiteralInteger(iterations.into()),
                }),
                Instruction::Label(Label {
                    target: start_target.clone(),
                }),
            ]
            .into_iter()
            .chain(self.body_instructions().cloned())
            .chain(vec![
                Instruction::Arithmetic(Arithmetic {
                    operator: ArithmeticOperator::Subtract,
                    destination: MemoryReference {
                        name: loop_count_reference.name.clone(),
                        index: 0,
                    },
                    source: ArithmeticOperand::LiteralInteger(1),
                }),
                Instruction::JumpUnless(JumpUnless {
                    target: end_target.clone(),
                    condition: loop_count_reference,
                }),
                Instruction::Jump(Jump {
                    target: start_target,
                }),
                Instruction::Label(Label { target: end_target }),
            ])
            .collect::<Vec<Instruction>>(),
        );

        looped_program
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

    /// Return a copy of all of the instructions which constitute this [`Program`].
    pub fn to_instructions(&self) -> Vec<Instruction> {
        let mut instructions: Vec<Instruction> = Vec::with_capacity(self.len());

        instructions.extend(self.extern_pragma_map.to_instructions());
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
}

impl Program {
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
        F: Fn(&mut Instruction),
    {
        let mut instructions = std::mem::take(&mut self.instructions);
        self.used_qubits.clear();

        instructions.iter_mut().for_each(closure);

        self.add_instructions(instructions);
    }

    pub fn add_instructions<I>(&mut self, instructions: I)
    where
        I: IntoIterator<Item = Instruction>,
    {
        instructions
            .into_iter()
            .for_each(|i| self.add_instruction(i));
    }

    /// Return a new [`Program`] containing only the instructions for which `predicate` returns
    /// true.
    pub fn filter_instructions(&self, predicate: impl FnMut(&Instruction) -> bool) -> Program {
        Program::from_instructions(
            self.to_instructions()
                .into_iter()
                .filter(predicate)
                .collect(),
        )
    }

    /// Expand any instructions in the program which have a matching calibration, leaving the others
    /// unchanged. Return the expanded copy of the program and a source mapping of the expansions made.
    pub fn expand_calibrations_with_source_map(
        &self,
    ) -> Result<(
        Program,
        SourceMap<InstructionIndex, ExpansionResult<CalibrationExpansion>>,
    )> {
        let mut source_mapping = ProgramCalibrationExpansionSourceMap::default();
        let new_program = self.expand_calibrations_inner(Some(&mut source_mapping))?;

        Ok((new_program, source_mapping))
    }

    /// Expand calibrations, writing expansions to a [`SourceMap`] if provided.
    ///
    /// Return an error if any instruction expands into itself.
    ///
    /// Source map may be omitted for faster performance.
    fn expand_calibrations_inner(
        &self,
        mut source_mapping: Option<&mut ProgramCalibrationExpansionSourceMap>,
    ) -> Result<Self> {
        let mut new_program = Self {
            calibrations: self.calibrations.clone(),
            extern_pragma_map: self.extern_pragma_map.clone(),
            frames: self.frames.clone(),
            memory_regions: self.memory_regions.clone(),
            waveforms: self.waveforms.clone(),
            gate_definitions: self.gate_definitions.clone(),
            instructions: Vec::new(),
            used_qubits: HashSet::new(),
        };

        for (index, instruction) in self.instructions.iter().enumerate() {
            let index = InstructionIndex(index);

            match self.calibrations.expand_with_detail(instruction, &[])? {
                Some(expanded) => {
                    new_program.append_calibration_expansion_output_inner(
                        expanded,
                        index,
                        &mut source_mapping,
                    );
                }
                None => {
                    new_program.add_instruction(instruction.clone());
                    if let Some(source_mapping) = source_mapping.as_mut() {
                        source_mapping.entries.push(SourceMapEntry {
                            source_location: index,
                            target_location: ExpansionResult::Unmodified(InstructionIndex(
                                new_program.instructions.len() - 1,
                            )),
                        });
                    }
                }
            }
        }

        Ok(new_program)
    }

    /// Expand any `DefGateSequence` instructions in the program, leaving the others unchanged.
    /// Return the expanded copy of the program. Any sequence gate definitions that are included
    /// by the filter are removed from the program's gate definitions, unless they are referenced
    /// by unexpanded sequence gate definitions.
    ///
    /// The `filter` that determines which sequence gate definitions to keep in the
    /// program. Gates are kept if the filter returns `true` for their name.
    ///
    /// # Example
    ///
    /// Below, we show the results of gate sequence expansion on a program that has two gate
    /// sequence definitions. The first, `seq1`, has a matching calibration and we do not
    /// want to expand it. The second, `seq2`, does not have a matching calibration and
    /// we do want to expand it.
    ///
    //  NOTE: A similar example is documented in the Python documentation for `Program.expand_defgate_sequences`.
    //  These examples should be kept in sync.
    ///
    /// ```rust
    /// # use std::error::Error;
    /// #
    /// # fn main() -> Result<(), Box<dyn Error>> {
    /// use quil_rs::program::Program;
    /// use quil_rs::quil::Quil;
    /// use std::collections::HashSet;
    ///
    /// let quil = r#"
    /// DEFCAL seq1 0 1:
    ///     FENCE 0 1
    ///     NONBLOCKING PULSE 0 "rf" drag_gaussian(duration: 6.000000000000001e-08, fwhm: 1.5000000000000002e-08, t0: 3.0000000000000004e-08, anh: -190000000.0, alpha: -1.6453719598238201, scale: 0.168265925924524, phase: 0.0, detuning: 0)
    ///     NONBLOCKING PULSE 1 "rf" drag_gaussian(duration: 6.000000000000001e-08, fwhm: 1.5000000000000002e-08, t0: 3.0000000000000004e-08, anh: -190000000.0, alpha: -1.6453719598238201, scale: 0.168265925924524, phase: 0.0, detuning: 0)
    ///     FENCE 0 1
    ///
    /// DEFGATE seq1() a b AS SEQUENCE:
    ///     RX(pi/2) a
    ///     RX(pi/2) b
    ///
    /// DEFGATE seq2(%theta, %psi, %phi) a AS SEQUENCE:
    ///     RZ(%theta) a
    ///     RX(pi/2) a
    ///     RZ(%psi) a
    ///     RX(pi/2) a
    ///     RZ(%phi) a
    ///
    ///  seq1 0 1
    ///  seq2(1.5707963267948966, 3.141592653589793, 0) 0
    ///  seq2(3.141592653589793, 0, 1.5707963267948966) 1
    ///  "#;
    ///
    ///  let program: Program = quil.parse().unwrap();
    ///  let calibrated_gate_names = program.calibrations.calibrations.iter().fold(HashSet::new(), |mut acc, calibration| {
    ///     acc.insert(calibration.identifier.name.clone());
    ///     acc
    ///  });
    ///
    ///  let expanded_program = program.expand_defgate_sequences(|name| !calibrated_gate_names.contains(name)).unwrap();
    ///
    ///  let expected_quil = r#"
    /// DEFCAL seq1 0 1:
    ///     FENCE 0 1
    ///     NONBLOCKING PULSE 0 "rf" drag_gaussian(duration: 6.000000000000001e-08, fwhm: 1.5000000000000002e-08, t0: 3.0000000000000004e-08, anh: -190000000.0, alpha: -1.6453719598238201, scale: 0.168265925924524, phase: 0.0, detuning: 0)
    ///     NONBLOCKING PULSE 1 "rf" drag_gaussian(duration: 6.000000000000001e-08, fwhm: 1.5000000000000002e-08, t0: 3.0000000000000004e-08, anh: -190000000.0, alpha: -1.6453719598238201, scale: 0.168265925924524, phase: 0.0, detuning: 0)
    ///     FENCE 0 1
    ///
    /// DEFGATE seq1 a b AS SEQUENCE:
    ///     RX(pi/2) a
    ///     RX(pi/2) b
    ///
    /// seq1 0 1
    ///
    /// RZ(1.5707963267948966) 0
    /// RX(pi/2) 0
    /// RZ(3.141592653589793) 0
    /// RX(pi/2) 0
    /// RZ(0) 0
    ///
    /// RZ(3.141592653589793) 1
    /// RX(pi/2) 1
    /// RZ(0) 1
    /// RX(pi/2) 1
    /// RZ(1.5707963267948966) 1
    ///  "#;
    ///
    ///  let expected_program: Program = expected_quil.parse().unwrap();
    ///
    ///  assert_eq!(expanded_program, expected_program);
    ///  # Ok(())
    ///  # }
    ///  ```
    pub fn expand_defgate_sequences<F>(self, filter: F) -> Result<Self>
    where
        F: Fn(&str) -> bool,
    {
        let (expansion, gate_definitions) = self.initialize_defgate_sequence_expander(filter);
        let new_instructions = expansion.expand(&self.instructions)?;

        let mut new_program = Self {
            calibrations: self.calibrations,
            extern_pragma_map: self.extern_pragma_map,
            frames: self.frames,
            memory_regions: self.memory_regions,
            waveforms: self.waveforms,
            gate_definitions,
            instructions: Vec::new(),
            used_qubits: HashSet::new(),
        };
        new_program.add_instructions(new_instructions);
        Ok(new_program)
    }

    /// Expand any sequence gate definitions in the program, leaving the others unchanged.
    /// Return the expanded copy of the program and a source mapping of the expansions made.
    /// Any sequence gate definitions that are included by the filter are removed from
    /// the program's gate definitions, unless they are referenced by unexpanded
    /// sequence gate definitions.
    ///
    /// # Arguments
    ///
    /// * `filter` - A filter that determines which sequence gate definitions to keep in the
    ///   program. Gates are kept if the filter returns `true` for their name.
    ///
    /// See [`Program::expand_defgate_sequences`](Self::expand_defgate_sequences) for an example.
    pub fn expand_defgate_sequences_with_source_map<F>(
        &self,
        filter: F,
    ) -> Result<(
        Self,
        SourceMap<InstructionIndex, ExpansionResult<DefGateSequenceExpansion<'_>>>,
    )>
    where
        F: Fn(&str) -> bool,
    {
        let (expander, gate_definitions) = self.initialize_defgate_sequence_expander(filter);
        let ExpandedInstructionsWithSourceMap {
            instructions: new_instructions,
            source_map,
        } = expander.expand_with_source_map(&self.instructions)?;

        let mut new_program = Self {
            calibrations: self.calibrations.clone(),
            extern_pragma_map: self.extern_pragma_map.clone(),
            frames: self.frames.clone(),
            memory_regions: self.memory_regions.clone(),
            waveforms: self.waveforms.clone(),
            gate_definitions,
            instructions: Vec::new(),
            used_qubits: HashSet::new(),
        };
        new_program.add_instructions(new_instructions);
        Ok((new_program, source_map))
    }

    fn initialize_defgate_sequence_expander<F>(
        &self,
        filter: F,
    ) -> (
        ProgramDefGateSequenceExpander<'_, F>,
        IndexMap<String, GateDefinition>,
    )
    where
        F: Fn(&str) -> bool,
    {
        let gate_definitions_to_keep =
            filter_sequence_gate_definitions_to_keep(&self.gate_definitions, &filter);
        let expansion = ProgramDefGateSequenceExpander::new(&self.gate_definitions, filter);
        (expansion, gate_definitions_to_keep)
    }

    /// Append the result of a calibration expansion to this program, being aware of which expanded instructions
    /// land in the program body (and thus merit inclusion within a target range) and which do not.
    ///
    /// For example, `DECLARE` instructions are hoisted to a specialized data structure and thus do not appear in
    /// the program body. Thus, they should not be counted in the `target_index` range within a [`SourceMapEntry`].
    fn append_calibration_expansion_output_inner(
        &mut self,
        mut expansion_output: CalibrationExpansionOutput,
        source_index: InstructionIndex,
        source_mapping: &mut Option<&mut ProgramCalibrationExpansionSourceMap>,
    ) {
        if let Some(source_mapping) = source_mapping.as_mut() {
            let previous_program_instruction_body_length = self.instructions.len();

            for instruction in expansion_output.new_instructions {
                let start_length = self.instructions.len();
                self.add_instruction(instruction.clone());
                let end_length = self.instructions.len();

                // If the instruction was not added to the program body, remove its target index from the source map
                // so that the map stays correct.
                if start_length == end_length {
                    let relative_target_index =
                        InstructionIndex(start_length - previous_program_instruction_body_length);
                    expansion_output
                        .detail
                        .remove_target_index(relative_target_index);
                }
            }

            expansion_output.detail.range =
                InstructionIndex(previous_program_instruction_body_length)
                    ..InstructionIndex(self.instructions.len());

            if !expansion_output.detail.range.is_empty() {
                source_mapping.entries.push(SourceMapEntry {
                    source_location: source_index,
                    target_location: ExpansionResult::Rewritten(expansion_output.detail),
                });
            }
        } else {
            self.add_instructions(expansion_output.new_instructions);
        }
    }

    /// Build a program from a list of instructions
    pub fn from_instructions(instructions: Vec<Instruction>) -> Self {
        let mut program = Self::default();
        for instruction in instructions {
            program.add_instruction(instruction);
        }
        program
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
        let mut instructions: Vec<Instruction> = Vec::with_capacity(self.len());

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
        instructions.extend(self.calibrations.to_instructions());
        instructions.extend(
            self.gate_definitions
                .into_values()
                .map(Instruction::GateDefinition),
        );
        instructions.extend(self.extern_pragma_map.into_instructions());
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
    /// - `PRAGMA EXTERN` instructions which are not used by any `CALL` instruction (see
    ///   [`Program::extern_pragma_map`]).
    ///
    /// When a valid program is simplified, it remains valid.
    pub fn simplify<H: InstructionHandler>(&self, handler: &H) -> Result<Self> {
        let mut expanded_program = self.expand_calibrations()?;
        // Remove calibrations such that the resulting program contains
        // only instructions. Calibrations have already been expanded, so
        // technically there is no need to keep them around anyway.
        expanded_program.calibrations = Calibrations::default();

        let mut frames_used: HashSet<&FrameIdentifier> = HashSet::new();
        let mut waveforms_used: HashSet<&String> = HashSet::new();
        let mut extern_signatures_used: HashSet<&String> = HashSet::new();

        for instruction in &expanded_program.instructions {
            if let Some(matched_frames) = handler.matching_frames(&expanded_program, instruction) {
                frames_used.extend(matched_frames.used)
            }

            if let Some(waveform) = instruction.get_waveform_invocation() {
                waveforms_used.insert(&waveform.name);
            }

            if let Instruction::Call(Call { name, .. }) = instruction {
                extern_signatures_used.insert(name);
            }
        }

        expanded_program.frames = self.frames.intersection(&frames_used);
        expanded_program
            .waveforms
            .retain(|name, _definition| waveforms_used.contains(name));
        expanded_program
            .extern_pragma_map
            .retain(|name, _signature| {
                name.as_ref()
                    .map(|name| extern_signatures_used.contains(name))
                    .unwrap_or(false)
            });

        Ok(expanded_program)
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

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        self.memory_regions.len()
            + self.frames.len()
            + self.waveforms.len()
            + self.gate_definitions.len()
            + self.instructions.len()
            + self.extern_pragma_map.len()
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
                Instruction::Halt() => {}
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

    /// Convert the [`Program::extern_pragma_map`] into an [`ExternSignatureMap`].
    ///
    /// This will parse all `PRAGMA EXTERN` instructions in the program. If the
    /// conversion of any [`Pragma`] fails, the [`ExternError`] is returned along
    /// with the offending [`Pragma`].
    pub fn try_extern_signature_map_from_pragma_map(
        &self,
    ) -> std::result::Result<ExternSignatureMap, (Pragma, ExternError)> {
        ExternSignatureMap::try_from(self.extern_pragma_map.clone())
    }
}

/// Filter the sequence gate definitions in the program to keep only those that are
/// excluded by the filter or are referenced by those that are excluded by the filter.
///
/// As with [`Program::expand_defgate_sequences`], gates are kept if the filter
/// returns `true` for their name.
fn filter_sequence_gate_definitions_to_keep<F>(
    gate_definitions: &IndexMap<String, GateDefinition>,
    filter: &F,
) -> IndexMap<String, GateDefinition>
where
    F: Fn(&str) -> bool,
{
    let mut graph: Graph<usize, u8> = Graph::new();
    let gate_sequence_definitions = gate_definitions
        .iter()
        .filter_map(|(gate_name, definition)| {
            if let GateSpecification::Sequence(sequence) = &definition.specification {
                Some((gate_name.clone(), sequence.clone()))
            } else {
                None
            }
        })
        .map(|(gate_name, sequence)| (gate_name, (graph.add_node(1), sequence)))
        .collect::<HashMap<_, _>>();

    gate_sequence_definitions
        .values()
        .flat_map(|(i, sequence)| {
            sequence.gates.iter().filter_map(|gate| {
                if let Some((j, _)) = gate_sequence_definitions.get(&gate.name) {
                    Some((*i, *j))
                } else {
                    None
                }
            })
        })
        .for_each(|edge| {
            graph.add_edge(edge.0, edge.1, 1);
        });

    let mut space = DfsSpace::new(&graph);
    let mut seq_defgates_referenced_by_unfiltered_seq_defgates = HashSet::new();

    for (_, (i, _)) in gate_sequence_definitions
        .iter()
        .filter(|(name, _)| !filter(name))
    {
        for (gate_name, (j, _)) in &gate_sequence_definitions {
            if petgraph::algo::has_path_connecting(&graph, *i, *j, Some(&mut space)) {
                seq_defgates_referenced_by_unfiltered_seq_defgates.insert(gate_name.clone());
            }
        }
    }

    gate_definitions
        .iter()
        .filter(|(gate_name, definition)| {
            if let GateSpecification::Sequence(_) = definition.specification {
                !filter(gate_name)
                    || seq_defgates_referenced_by_unfiltered_seq_defgates.contains(*gate_name)
            } else {
                true
            }
        })
        .map(|(gate_name, definition)| (gate_name.clone(), definition.clone()))
        .collect()
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

    fn add(mut self, rhs: Program) -> Program {
        self += rhs;
        self
    }
}

impl ops::AddAssign<Program> for Program {
    fn add_assign(&mut self, rhs: Program) {
        self.calibrations.extend(rhs.calibrations);
        self.memory_regions.extend(rhs.memory_regions);
        self.frames.merge(rhs.frames);
        self.waveforms.extend(rhs.waveforms);
        self.gate_definitions.extend(rhs.gate_definitions);
        self.extern_pragma_map.extend(rhs.extern_pragma_map);
        self.instructions.extend(rhs.instructions);
        self.used_qubits.extend(rhs.used_qubits);
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(
    feature = "python",
    derive(pyo3::FromPyObject, pyo3::IntoPyObject, pyo3::IntoPyObjectRef)
)]
pub struct InstructionIndex(pub usize);

impl InstructionIndex {
    fn map(self, f: impl FnOnce(usize) -> usize) -> Self {
        Self(f(self.0))
    }
}

type ProgramCalibrationExpansionSourceMap =
    SourceMap<InstructionIndex, ExpansionResult<CalibrationExpansion>>;

#[cfg(test)]
mod tests {
    use super::Program;
    use crate::{
        expression::{
            Expression, InfixExpression, InfixOperator, PrefixExpression, PrefixOperator,
        },
        imag,
        instruction::{
            CalibrationIdentifier, Call, Declaration, DefGateSequence, DefaultHandler,
            ExternSignatureMap, Gate, GateDefinition, GateSpecification, Instruction,
            InstructionHandler, Jump, JumpUnless, JumpWhen, Label, Matrix, MemoryReference, Qubit,
            QubitPlaceholder, ScalarType, Target, TargetPlaceholder, UnresolvedCallArgument,
            Vector, RESERVED_PRAGMA_EXTERN,
        },
        program::{
            calibration::{CalibrationExpansion, CalibrationSource},
            source_map::{ExpansionResult, SourceMap, SourceMapEntry},
            InstructionIndex, MemoryAccesses,
        },
        quil::{Quil, INDENT},
        real,
    };
    use approx::assert_abs_diff_eq;
    use insta::{assert_debug_snapshot, assert_snapshot};
    use internment::ArcIntern;
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
    HARDWARE-OBJECT: \"hardware\"
DEFWAVEFORM custom:
    1, 2
DEFCAL I 0:
    DELAY 0 1
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

    /// Assert that a program's instructions are correctly expanded using its calibrations,
    /// emitting the expected [`SourceMap`] for the expansion.
    #[test]
    fn expand_calibrations() {
        let input = r#"DECLARE ro BIT[1]
DEFFRAME 0 "a":
    HARDWARE-OBJECT: "hardware"

DEFCAL I 0:
    DECLAREMEM
    NOP
    NOP

DEFCAL DECLAREMEM:
    DECLARE mem BIT[1]
    NOP

I 0
PULSE 0 "a" custom_waveform
I 0
"#;

        let expected = "DECLARE ro BIT[1]
DECLARE mem BIT[1]
DEFFRAME 0 \"a\":
    HARDWARE-OBJECT: \"hardware\"
DEFCAL I 0:
    DECLAREMEM
    NOP
    NOP
DEFCAL DECLAREMEM:
    DECLARE mem BIT[1]
    NOP
NOP
NOP
NOP
PULSE 0 \"a\" custom_waveform
NOP
NOP
NOP
";

        let expected_source_map = SourceMap {
            entries: vec![
                SourceMapEntry {
                    source_location: InstructionIndex(0),
                    target_location: ExpansionResult::Rewritten(CalibrationExpansion {
                        calibration_used: CalibrationIdentifier {
                            name: "I".to_string(),
                            qubits: vec![Qubit::Fixed(0)],
                            modifiers: vec![],
                            parameters: vec![],
                        }
                        .into(),
                        range: InstructionIndex(0)..InstructionIndex(3),
                        expansions: SourceMap {
                            entries: vec![
                                SourceMapEntry {
                                    source_location: InstructionIndex(0),
                                    target_location: ExpansionResult::Rewritten(
                                        CalibrationExpansion {
                                            calibration_used: CalibrationSource::Calibration(
                                                CalibrationIdentifier {
                                                    modifiers: vec![],
                                                    name: "DECLAREMEM".to_string(),
                                                    parameters: vec![],
                                                    qubits: vec![],
                                                },
                                            ),
                                            range: InstructionIndex(0)..InstructionIndex(1),
                                            expansions: SourceMap {
                                                entries: vec![
                                                    SourceMapEntry {
                                                        source_location: InstructionIndex(0),
                                                        target_location:
                                                            ExpansionResult::Unmodified(
                                                                InstructionIndex(0),
                                                            ),
                                                    },
                                                    SourceMapEntry {
                                                        source_location: InstructionIndex(1),
                                                        target_location:
                                                            ExpansionResult::Unmodified(
                                                                InstructionIndex(1),
                                                            ),
                                                    },
                                                ],
                                            },
                                        },
                                    ),
                                },
                                SourceMapEntry {
                                    source_location: InstructionIndex(1),
                                    target_location: ExpansionResult::Unmodified(InstructionIndex(
                                        2,
                                    )),
                                },
                                SourceMapEntry {
                                    source_location: InstructionIndex(2),
                                    target_location: ExpansionResult::Unmodified(InstructionIndex(
                                        3,
                                    )),
                                },
                            ],
                        },
                    }),
                },
                SourceMapEntry {
                    source_location: InstructionIndex(1),
                    target_location: ExpansionResult::Unmodified(InstructionIndex(3)),
                },
                SourceMapEntry {
                    source_location: InstructionIndex(2),
                    target_location: ExpansionResult::Rewritten(CalibrationExpansion {
                        calibration_used: CalibrationIdentifier {
                            name: "I".to_string(),
                            qubits: vec![Qubit::Fixed(0)],
                            modifiers: vec![],
                            parameters: vec![],
                        }
                        .into(),
                        range: InstructionIndex(4)..InstructionIndex(7),
                        expansions: SourceMap {
                            entries: vec![
                                SourceMapEntry {
                                    source_location: InstructionIndex(0),
                                    target_location: ExpansionResult::Rewritten(
                                        CalibrationExpansion {
                                            calibration_used: CalibrationSource::Calibration(
                                                CalibrationIdentifier {
                                                    modifiers: vec![],
                                                    name: "DECLAREMEM".to_string(),
                                                    parameters: vec![],
                                                    qubits: vec![],
                                                },
                                            ),
                                            range: InstructionIndex(0)..InstructionIndex(1),
                                            expansions: SourceMap {
                                                entries: vec![
                                                    SourceMapEntry {
                                                        source_location: InstructionIndex(0),
                                                        target_location:
                                                            ExpansionResult::Unmodified(
                                                                InstructionIndex(0),
                                                            ),
                                                    },
                                                    SourceMapEntry {
                                                        source_location: InstructionIndex(1),
                                                        target_location:
                                                            ExpansionResult::Unmodified(
                                                                InstructionIndex(1),
                                                            ),
                                                    },
                                                ],
                                            },
                                        },
                                    ),
                                },
                                SourceMapEntry {
                                    source_location: InstructionIndex(1),
                                    target_location: ExpansionResult::Unmodified(InstructionIndex(
                                        2,
                                    )),
                                },
                                SourceMapEntry {
                                    source_location: InstructionIndex(2),
                                    target_location: ExpansionResult::Unmodified(InstructionIndex(
                                        3,
                                    )),
                                },
                            ],
                        },
                    }),
                },
            ],
        };

        let program = Program::from_str(input).unwrap();
        let (expanded_program, source_map) = program.expand_calibrations_with_source_map().unwrap();
        pretty_assertions::assert_eq!(expanded_program.to_quil().unwrap(), expected);
        pretty_assertions::assert_eq!(source_map, expected_source_map);
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
            let instruction = Instruction::parse_in_test(instruction_string).unwrap();
            let matched_frames = DefaultHandler
                .matching_frames(&program, &instruction)
                .unwrap();
            let used_frames: HashSet<String> = matched_frames
                .used
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
                .blocked
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
        let program = program.simplify(&DefaultHandler).unwrap();
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
        let expected_owned = [Qubit::Fixed(0), Qubit::Variable("q".to_string())];
        let expected = expected_owned.iter().collect::<HashSet<_>>();
        let actual = program.get_used_qubits();
        assert_eq!(expected, actual.iter().collect());
    }

    #[test]
    fn test_add_instructions() {
        let mut p = Program::new();
        let instrs = vec![Instruction::Nop(), Instruction::Nop()];
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

        let expected_qubits = [
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
        let input = format!(
            "DECLARE foo REAL[1]
DEFFRAME 1 \"rx\":
{INDENT}HARDWARE-OBJECT: \"hardware\"
DEFWAVEFORM custom2:
{INDENT}1, 2
DEFCAL I 1:
{INDENT}DELAY 0 1
DEFGATE BAR AS MATRIX:
{INDENT}0, 1
{INDENT}1, 0

H 1
CNOT 2 3
"
        );
        let program = Program::from_str(&input).unwrap();
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

    #[test]
    fn test_filter_instructions() {
        let input = "DECLARE foo REAL[1]
DEFFRAME 1 \"rx\":
\tHARDWARE-OBJECT: \"hardware\"
DEFCAL I 1:
\tDELAY 0 1
DEFGATE BAR AS MATRIX:
\t0, 1
\t1, 0

H 1
CNOT 2 3";

        let program = Program::from_str(input).unwrap();
        let program_without_quil_t =
            program.filter_instructions(|instruction| !instruction.is_quil_t());
        assert_snapshot!(program_without_quil_t.to_quil().unwrap())
    }

    #[test]
    fn test_wrap_in_loop() {
        let input = "DECLARE ro BIT
DECLARE shot_count INTEGER
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
        let program = Program::from_str(input).unwrap().wrap_in_loop(
            MemoryReference {
                name: "shot_count".to_string(),
                index: 0,
            },
            Target::Fixed("loop-start".to_string()),
            Target::Fixed("loop-end".to_string()),
            10,
        );

        assert_snapshot!(program.to_quil().unwrap())
    }

    #[test]
    fn test_equality() {
        let input = "DECLARE foo REAL[1]
DEFFRAME 1 \"rx\":
\tHARDWARE-OBJECT: \"hardware\"
DEFCAL I 0:
\tDELAY 0 1
DEFCAL I 1:
\tDELAY 0 1
DEFCAL I 2:
\tDELAY 0 1
DEFCAL MEASURE 0 addr:
\tCAPTURE 0 \"ro_rx\" custom addr
DEFCAL MEASURE 1 addr:
\tCAPTURE 1 \"ro_rx\" custom addr
DEFWAVEFORM custom:
\t1,2
DEFWAVEFORM custom2:
\t3,4
DEFWAVEFORM another1:
\t4,5
DEFGATE BAR AS MATRIX:
\t0, 1
\t1, 0
DEFGATE FOO AS MATRIX:
\t0, 1
\t1, 0

H 1
CNOT 2 3";

        let program = Program::from_str(input).unwrap();

        // The order of definitions are global in the sense that where they are defined in a
        // program does not matter.
        let is_global_state_instruction = move |i: &Instruction| -> bool {
            matches!(
                i,
                |Instruction::WaveformDefinition(_)| Instruction::GateDefinition(_)
                    | Instruction::FrameDefinition(_)
            )
        };
        // Create a copy of the program, but insert the "global" instructions in reverse order.
        // Since where these instructions are defined doesn't matter, this should be an
        // equivalent program.
        let mut program2 = program.filter_instructions(|i| !is_global_state_instruction(i));
        let global_instructions: Vec<Instruction> = program
            .filter_instructions(is_global_state_instruction)
            .into_instructions()
            .into_iter()
            .rev()
            .collect();
        program2.add_instructions(global_instructions.clone());
        assert_eq!(program, program2);

        // Create another copy of the program with non-global instructions inserted in reverse order.
        // This should not be equal to the original program.
        let mut program3 = Program::from_instructions(
            program
                .filter_instructions(|i| !is_global_state_instruction(i))
                .into_instructions()
                .into_iter()
                .rev()
                .collect(),
        );
        program3.add_instructions(global_instructions);
        assert!(program != program3)
    }

    #[test]
    fn test_deterministic_serialization() {
        let input = "DECLARE foo REAL[1]
DECLARE bar BIT[1]
DECLARE baz BIT[1]
RX(pi) 0
CNOT 0 1
DEFCAL I 0:
\tDELAY 0 1
\tDELAY 1 1
DEFCAL I 1:
\tDELAY 0 1
\tDELAY 1 2
DEFCAL I 2:
\tDELAY 2 1
\tDELAY 2 3
DEFCAL MEASURE 0 addr:
\tRX(pi) 0
\tCAPTURE 0 \"ro_rx\" custom addr
DEFCAL MEASURE 1 addr:
\tRX(pi/2) 1
\tCAPTURE 1 \"ro_rx\" custom addr
DEFCAL MEASURE 2 addr:
\tRX(pi/2) 2
\tCAPTURE 2 \"ro_rx\" custom addr
DEFWAVEFORM custom:
\t1,2
DEFWAVEFORM custom2:
\t3,4
DEFWAVEFORM another1(%a, %b):
\t%a,%b
PULSE 0 \"xy\" flat(duration: 1e-6, iq: 2+3i)
PULSE 0 \"xy\" another1(a: 1e-6, b: 2+3i)
DEFGATE HADAMARD AS MATRIX:
\t(1/sqrt(2)),(1/sqrt(2))
\t(1/sqrt(2)),((-1)/sqrt(2))
DEFGATE RX(%theta) AS MATRIX:
\tcos((%theta/2)),((-1i)*sin((%theta/2)))
\t((-1i)*sin((%theta/2))),cos((%theta/2))
DEFGATE Name AS PERMUTATION:
\t1, 0
DEFCIRCUIT SIMPLE:
\tX 0
\tX 1
DEFGATE BAR AS MATRIX:
\t0, 1
\t1, 0
DEFGATE FOO AS MATRIX:
\t0, 1
\t1, 0
DEFGATE BAZ AS MATRIX:
\t1, 0
\t0, 1
MEASURE 1 bar
MEASURE 0 foo
HALT
DEFCIRCUIT CIRCFOO:
\tLABEL @FOO_A
\tJUMP @FOO_A
DEFFRAME 0 \"xy\":
\tSAMPLE-RATE: 3000
DEFFRAME 0 \"xy\":
\tDIRECTION: \"rx\"
\tCENTER-FREQUENCY: 1000
\tHARDWARE-OBJECT: \"some object\"
\tINITIAL-FREQUENCY: 2000
\tSAMPLE-RATE: 3000";
        let program = Program::from_str(input).unwrap();
        let quil = program.to_quil().unwrap();

        // Asserts that serialization doesn't change on repeated attempts.
        // 100 is chosen because it should be more than sufficient to reveal an
        //     issue and it has a negligible impact on execution speed of the test suite.
        let iterations = 100;
        for _ in 0..iterations {
            let new_program = Program::from_str(input).unwrap();
            assert_eq!(new_program.to_quil().unwrap(), quil);
        }
    }

    /// Test that a program with a `CALL` instruction can be parsed and properly resolved to
    /// the corresponding `EXTERN` instruction. Additionally, test that the memory accesses are
    /// correctly calculated with the resolved `CALL` instruction.
    #[test]
    fn test_extern_call() {
        let input = r#"PRAGMA EXTERN foo "OCTET (params : mut REAL[3])"
DECLARE reals REAL[3]
DECLARE octets OCTET[3]
CALL foo octets[1] reals
"#;
        let program = Program::from_str(input).expect("should be able to parse program");
        let reserialized = program
            .to_quil()
            .expect("should be able to serialize program");
        assert_eq!(input, reserialized);

        let pragma = crate::instruction::Pragma {
            name: RESERVED_PRAGMA_EXTERN.to_string(),
            arguments: vec![crate::instruction::PragmaArgument::Identifier(
                "foo".to_string(),
            )],
            data: Some("OCTET (params : mut REAL[3])".to_string()),
        };
        let call = Call {
            name: "foo".to_string(),
            arguments: vec![
                UnresolvedCallArgument::MemoryReference(MemoryReference {
                    name: "octets".to_string(),
                    index: 1,
                }),
                UnresolvedCallArgument::Identifier("reals".to_string()),
            ],
        };
        let expected_program = Program::from_instructions(vec![
            Instruction::Declaration(Declaration::new(
                "reals".to_string(),
                Vector::new(ScalarType::Real, 3),
                None,
            )),
            Instruction::Declaration(Declaration::new(
                "octets".to_string(),
                Vector::new(ScalarType::Octet, 3),
                None,
            )),
            Instruction::Pragma(pragma.clone()),
            Instruction::Call(call.clone()),
        ]);
        assert_eq!(expected_program, program);

        let extern_signature_map = ExternSignatureMap::try_from(program.extern_pragma_map)
            .expect("should be able parse extern pragmas");
        assert_eq!(extern_signature_map.len(), 1);

        assert_eq!(
            DefaultHandler
                .memory_accesses(&extern_signature_map, &Instruction::Pragma(pragma))
                .expect("should be able to get memory accesses"),
            MemoryAccesses::default()
        );

        assert_eq!(
            DefaultHandler
                .memory_accesses(&extern_signature_map, &Instruction::Call(call))
                .expect("should be able to get memory accesses"),
            MemoryAccesses {
                reads: ["octets", "reals"].into_iter().map(String::from).collect(),
                writes: ["octets", "reals"].into_iter().map(String::from).collect(),
                ..MemoryAccesses::default()
            }
        );
    }

    /// Test that unused `PRAGMA EXTERN` instructions are removed when simplifying a program.
    #[test]
    fn test_extern_call_simplification() {
        let input = r#"PRAGMA EXTERN foo "OCTET (params : mut REAL[3])"
PRAGMA EXTERN bar "OCTET (params : mut REAL[3])"
DECLARE reals REAL[3]
DECLARE octets OCTET[3]
CALL foo octets[1] reals
"#;
        let program = Program::from_str(input).expect("should be able to parse program");

        let expected = r#"PRAGMA EXTERN foo "OCTET (params : mut REAL[3])"
DECLARE reals REAL[3]
DECLARE octets OCTET[3]
CALL foo octets[1] reals
"#;

        let reserialized = program
            .expand_calibrations()
            .expect("should be able to expand calibrations")
            .simplify(&DefaultHandler)
            .expect("should be able to simplify program")
            .to_quil()
            .expect("should be able to serialize program");
        assert_eq!(expected, reserialized);
    }

    /// Test that we can construct a sequence gate definition, add it to a program, and ensure
    /// that the definition is included during [`Program::clone_without_body_instructions`].
    #[test]
    fn test_defgate_as_sequence_mechanics() {
        let pi_divided_by_2 = Expression::Infix(InfixExpression {
            operator: InfixOperator::Slash,
            left: ArcIntern::new(Expression::PiConstant()),
            right: ArcIntern::new(Expression::Number(Complex64 { re: 2.0, im: 0.0 })),
        });
        let negate_variable = |variable: String| {
            Expression::Prefix(PrefixExpression {
                operator: PrefixOperator::Minus,
                expression: ArcIntern::new(Expression::Variable(variable)),
            })
        };
        let new_gate = |gate_name: &str, param: Expression, qubit: String| {
            Gate::new(gate_name, vec![param], vec![Qubit::Variable(qubit)], vec![])
                .expect("must be a valid gate")
        };
        let pmw3 = |param_prefix: String, qubit: String| {
            (0..3)
                .flat_map(|i| {
                    vec![
                        new_gate(
                            "RZ",
                            Expression::Variable(format!("{param_prefix}{i}")),
                            qubit.clone(),
                        ),
                        new_gate("RX", pi_divided_by_2.clone(), qubit.clone()),
                        new_gate(
                            "RZ",
                            negate_variable(format!("{param_prefix}{i}")),
                            qubit.clone(),
                        ),
                    ]
                })
                .collect::<Vec<_>>()
        };
        let gate_sequence = DefGateSequence::try_new(
            ["q0", "q1"].map(String::from).to_vec(),
            pmw3("theta".to_string(), "q0".to_string())
                .into_iter()
                .chain(pmw3("phi".to_string(), "q1".to_string()))
                .collect(),
        )
        .expect("must be valid gate sequence");
        let gate_definition = GateDefinition {
            name: "PMW3".to_string(),
            parameters: vec![
                "theta0".to_string(),
                "theta1".to_string(),
                "theta2".to_string(),
                "phi0".to_string(),
                "phi1".to_string(),
                "phi2".to_string(),
            ],
            specification: GateSpecification::Sequence(gate_sequence),
        };
        let mut program = Program::new();
        program.add_instruction(Instruction::GateDefinition(gate_definition.clone()));
        assert_eq!(program.gate_definitions.len(), 1);
        assert_eq!(program.body_instructions().count(), 0);

        let invocation = Gate::new(
            "PMW3",
            vec![
                Expression::Address(MemoryReference {
                    name: "theta".to_string(),
                    index: 0,
                }),
                Expression::Address(MemoryReference {
                    name: "theta".to_string(),
                    index: 1,
                }),
                Expression::Address(MemoryReference {
                    name: "theta".to_string(),
                    index: 2,
                }),
                Expression::Address(MemoryReference {
                    name: "phi".to_string(),
                    index: 0,
                }),
                Expression::Address(MemoryReference {
                    name: "phi".to_string(),
                    index: 1,
                }),
                Expression::Address(MemoryReference {
                    name: "phi".to_string(),
                    index: 2,
                }),
            ],
            vec![Qubit::Fixed(0), Qubit::Fixed(1)],
            vec![],
        )
        .expect("must be a valid gate");
        program.add_instruction(Instruction::Gate(invocation));
        assert_eq!(program.body_instructions().count(), 1);

        let program_copy = program.clone_without_body_instructions();
        assert_eq!(program_copy.gate_definitions.len(), 1);
        assert_eq!(
            program_copy
                .gate_definitions
                .get("PMW3")
                .expect("must exist"),
            &gate_definition
        );
        assert_eq!(program_copy.body_instructions().count(), 0);
    }

    /// Test that we can expand a gate sequence definition in a program. Note, for more
    /// comprehensive tests on gate sequence expansion and corresponding source maps,
    /// see [`super::defgate_sequence_expansion::ProgramDefGateSequenceExpander`]
    /// tests.
    #[test]
    fn test_gate_sequence_expansion() {
        const QUIL: &str = r"
DECLARE ro BIT[2]

DEFGATE seq1(%param1) a AS SEQUENCE:
    RZ(%param1) a

DEFGATE seq2() a AS SEQUENCE:
    X a

seq1(pi/2) 0
seq2 1

MEASURE 0 ro[0]
MEASURE 1 ro[1]
";
        let program = Program::from_str(QUIL).expect("should parse program");
        let exclude = ["seq1"]
            .into_iter()
            .map(String::from)
            .collect::<HashSet<_>>();
        let filter = |key: &str| !exclude.contains(key);
        let expanded_program = program
            .expand_defgate_sequences(filter)
            .expect("should expand gate sequences");
        const EXPECTED: &str = r"
DECLARE ro BIT[2]
DEFGATE seq1(%param1) a AS SEQUENCE:
    RZ(%param1) a

seq1(pi/2) 0
X 1
MEASURE 0 ro[0]
MEASURE 1 ro[1]
";
        let expected_program = Program::from_str(EXPECTED).expect("should parse expected program");
        pretty_assertions::assert_eq!(expanded_program, expected_program);
    }

    /// Test that gate definitions that are referenced by unexpanded sequence gate definitions
    /// are preserved.
    #[test]
    fn test_gate_sequence_expansion_preserves_referred_gates() {
        const QUIL: &str = r"
DECLARE ro BIT[2]

DEFGATE seq1(%param1) a AS SEQUENCE:
    RZ(%param1) a
    seq2() a

DEFGATE seq2() a AS SEQUENCE:
    H a

DEFGATE seq3() a AS SEQUENCE:
    X a

seq1(pi/2) 0
seq2() 1
seq3 2

MEASURE 0 ro[0]
MEASURE 1 ro[1]
";
        let program = Program::from_str(QUIL).expect("should parse program");
        let exclude = ["seq1"]
            .into_iter()
            .map(String::from)
            .collect::<HashSet<_>>();
        let filter = |key: &str| !exclude.contains(key);
        let expanded_program = program
            .expand_defgate_sequences(filter)
            .expect("should expand gate sequences");
        const EXPECTED: &str = r"
DECLARE ro BIT[2]
DEFGATE seq1(%param1) a AS SEQUENCE:
    RZ(%param1) a
    seq2() a

DEFGATE seq2() a AS SEQUENCE:
    H a

seq1(pi/2) 0
H 1
X 2
MEASURE 0 ro[0]
MEASURE 1 ro[1]
";
        let expected_program = Program::from_str(EXPECTED).expect("should parse expected program");
        pretty_assertions::assert_eq!(expanded_program, expected_program);
    }
}
