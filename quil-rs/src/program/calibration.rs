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

use std::collections::HashMap;
use std::ops::Range;

use itertools::FoldWhile::{Continue, Done};
use itertools::Itertools;

use crate::instruction::{CalibrationIdentifier, MeasureCalibrationIdentifier};
use crate::quil::Quil;
use crate::{
    expression::Expression,
    instruction::{
        Calibration, Capture, Delay, Fence, FrameIdentifier, Gate, Instruction,
        MeasureCalibrationDefinition, Measurement, Pulse, Qubit, RawCapture, SetFrequency,
        SetPhase, SetScale, ShiftFrequency, ShiftPhase,
    },
};

use super::source_map::{SourceMap, SourceMapEntry, SourceMapIndexable};
use super::{CalibrationSet, InstructionIndex, ProgramError};

/// A collection of Quil calibrations (`DEFCAL` instructions) with utility methods.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Calibrations {
    pub calibrations: CalibrationSet<Calibration>,
    pub measure_calibrations: CalibrationSet<MeasureCalibrationDefinition>,
}

struct MatchedCalibration<'a> {
    pub calibration: &'a Calibration,
    pub fixed_qubit_count: usize,
}

impl<'a> MatchedCalibration<'a> {
    pub fn new(calibration: &'a Calibration) -> Self {
        Self {
            calibration,
            fixed_qubit_count: calibration
                .identifier
                .qubits
                .iter()
                .filter(|q| match q {
                    Qubit::Fixed(_) => true,
                    Qubit::Placeholder(_) | Qubit::Variable(_) => false,
                })
                .count(),
        }
    }
}

/// The product of expanding an instruction using a calibration
#[derive(Clone, Debug, PartialEq)]
pub struct CalibrationExpansionOutput {
    /// The new instructions resulting from the expansion
    pub new_instructions: Vec<Instruction>,

    /// Details about the expansion process
    pub detail: CalibrationExpansion,
}

/// Details about the expansion of a calibration
#[derive(Clone, Debug, PartialEq)]
pub struct CalibrationExpansion {
    /// The calibration used to expand the instruction
    pub(crate) calibration_used: CalibrationSource,

    /// The target instruction indices produced by the expansion
    pub(crate) range: Range<InstructionIndex>,

    /// A map of source locations to the expansions they produced
    pub(crate) expansions: SourceMap<InstructionIndex, CalibrationExpansion>,
}

impl CalibrationExpansion {
    /// Remove the given target index from all entries, recursively.
    ///
    /// This is to be used when the given index is removed from the target program
    /// in the process of calibration expansion (for example, a `DECLARE`).
    pub(crate) fn remove_target_index(&mut self, target_index: InstructionIndex) {
        // Adjust the start of the range if the target index is before the range
        if self.range.start >= target_index {
            self.range.start = self.range.start.map(|v| v.saturating_sub(1));
        }

        // Adjust the end of the range if the target index is before the end of the range
        if self.range.end > target_index {
            self.range.end = self.range.end.map(|v| v.saturating_sub(1));
        }

        // Then walk through all entries expanded for this calibration and remove the
        // index as well. This is needed when a recursively-expanded instruction contains
        // an instruction which is excised from the overall calibration.
        if let Some(target_within_expansion) = target_index.0.checked_sub(self.range.start.0) {
            self.expansions.entries.retain_mut(
                |entry: &mut SourceMapEntry<InstructionIndex, CalibrationExpansion>| {
                    entry
                        .target_location
                        .remove_target_index(InstructionIndex(target_within_expansion));

                    !entry.target_location.range.is_empty()
                },
            );
        }
    }

    pub fn calibration_used(&self) -> &CalibrationSource {
        &self.calibration_used
    }

    pub fn range(&self) -> &Range<InstructionIndex> {
        &self.range
    }

    pub fn expansions(&self) -> &SourceMap<InstructionIndex, CalibrationExpansion> {
        &self.expansions
    }
}

impl SourceMapIndexable<InstructionIndex> for CalibrationExpansion {
    fn intersects(&self, other: &InstructionIndex) -> bool {
        self.range.contains(other)
    }
}

impl SourceMapIndexable<CalibrationSource> for CalibrationExpansion {
    fn intersects(&self, other: &CalibrationSource) -> bool {
        self.calibration_used() == other
    }
}

/// The result of an attempt to expand an instruction within a [`Program`]
#[derive(Clone, Debug, PartialEq)]
pub enum MaybeCalibrationExpansion {
    /// The instruction was expanded into others
    Expanded(CalibrationExpansion),

    /// The instruction was not expanded, but was simply copied over into the target program at the given instruction index
    Unexpanded(InstructionIndex),
}

impl SourceMapIndexable<InstructionIndex> for MaybeCalibrationExpansion {
    fn intersects(&self, other: &InstructionIndex) -> bool {
        match self {
            MaybeCalibrationExpansion::Expanded(expansion) => expansion.intersects(other),
            MaybeCalibrationExpansion::Unexpanded(index) => index == other,
        }
    }
}

impl SourceMapIndexable<CalibrationSource> for MaybeCalibrationExpansion {
    fn intersects(&self, other: &CalibrationSource) -> bool {
        match self {
            MaybeCalibrationExpansion::Expanded(expansion) => expansion.intersects(other),
            MaybeCalibrationExpansion::Unexpanded(_) => false,
        }
    }
}

/// A source of a calibration, either a [`Calibration`] or a [`MeasureCalibrationDefinition`]
#[derive(Clone, Debug, PartialEq)]
pub enum CalibrationSource {
    /// Describes a `DEFCAL` instruction
    Calibration(CalibrationIdentifier),

    /// Describes a `DEFCAL MEASURE` instruction
    MeasureCalibration(MeasureCalibrationIdentifier),
}

impl From<CalibrationIdentifier> for CalibrationSource {
    fn from(value: CalibrationIdentifier) -> Self {
        Self::Calibration(value)
    }
}

impl From<MeasureCalibrationIdentifier> for CalibrationSource {
    fn from(value: MeasureCalibrationIdentifier) -> Self {
        Self::MeasureCalibration(value)
    }
}

impl Calibrations {
    /// Return a vector containing a reference to all [`Calibration`]s in the set.
    pub fn calibrations(&self) -> Vec<&Calibration> {
        self.iter_calibrations().collect()
    }

    /// Return a vector containing a reference to all [`MeasureCalibrationDefinition`]s
    /// in the set.
    pub fn measure_calibrations(&self) -> Vec<&MeasureCalibrationDefinition> {
        self.iter_measure_calibrations().collect()
    }

    /// Iterate over all [`Calibration`]s in the set
    pub fn iter_calibrations(&self) -> impl Iterator<Item = &Calibration> {
        self.calibrations.iter()
    }

    /// Iterate over all [`MeasureCalibrationDefinition`]s calibrations in the set
    pub fn iter_measure_calibrations(&self) -> impl Iterator<Item = &MeasureCalibrationDefinition> {
        self.measure_calibrations.iter()
    }

    /// Given an instruction, return the instructions to which it is expanded if there is a match.
    /// Recursively calibrate instructions, returning an error if a calibration directly or indirectly
    /// expands into itself.
    ///
    /// Return only the expanded instructions; for more information about the expansion process,
    /// see [`Self::expand_with_detail`].
    pub fn expand(
        &self,
        instruction: &Instruction,
        previous_calibrations: &[Instruction],
    ) -> Result<Option<Vec<Instruction>>, ProgramError> {
        self.expand_inner(instruction, previous_calibrations, false)
            .map(|expansion| expansion.map(|expansion| expansion.new_instructions))
    }

    /// Given an instruction, return the instructions to which it is expanded if there is a match.
    /// Recursively calibrate instructions, returning an error if a calibration directly or indirectly
    /// expands into itself.
    ///
    /// Also return information about the expansion.
    pub fn expand_with_detail(
        &self,
        instruction: &Instruction,
        previous_calibrations: &[Instruction],
    ) -> Result<Option<CalibrationExpansionOutput>, ProgramError> {
        self.expand_inner(instruction, previous_calibrations, true)
    }

    /// Expand an instruction, returning an error if a calibration directly or indirectly
    /// expands into itself. Return `None` if there are no matching calibrations in `self`.
    ///
    /// # Arguments
    ///
    /// * `instruction` - The instruction to expand.
    /// * `previous_calibrations` - The calibrations that were invoked to yield this current instruction.
    /// * `build_source_map` - Whether to build a source map of the expansion.
    fn expand_inner(
        &self,
        instruction: &Instruction,
        previous_calibrations: &[Instruction],
        build_source_map: bool,
    ) -> Result<Option<CalibrationExpansionOutput>, ProgramError> {
        if previous_calibrations.contains(instruction) {
            return Err(ProgramError::RecursiveCalibration(instruction.clone()));
        }
        let expansion_result = match instruction {
            Instruction::Gate(gate) => {
                let matching_calibration = self.get_match_for_gate(gate);

                match matching_calibration {
                    Some(calibration) => {
                        let mut qubit_expansions: HashMap<&String, Qubit> = HashMap::new();
                        for (index, calibration_qubit) in
                            calibration.identifier.qubits.iter().enumerate()
                        {
                            if let Qubit::Variable(identifier) = calibration_qubit {
                                qubit_expansions.insert(identifier, gate.qubits[index].clone());
                            }
                        }

                        // Variables used within the calibration's definition should be replaced with the actual expressions used by the gate.
                        // That is, `DEFCAL RX(%theta): ...` should have `%theta` replaced by `pi` throughout if it's used to expand `RX(pi)`.
                        let variable_expansions: HashMap<String, Expression> = calibration
                            .identifier
                            .parameters
                            .iter()
                            .zip(gate.parameters.iter())
                            .filter_map(|(calibration_expression, gate_expression)| {
                                if let Expression::Variable(variable_name) = calibration_expression
                                {
                                    Some((variable_name.clone(), gate_expression.clone()))
                                } else {
                                    None
                                }
                            })
                            .collect();

                        let mut instructions = calibration.instructions.clone();

                        for instruction in instructions.iter_mut() {
                            match instruction {
                                Instruction::Gate(Gate { qubits, .. })
                                | Instruction::Delay(Delay { qubits, .. })
                                | Instruction::Capture(Capture {
                                    frame: FrameIdentifier { qubits, .. },
                                    ..
                                })
                                | Instruction::RawCapture(RawCapture {
                                    frame: FrameIdentifier { qubits, .. },
                                    ..
                                })
                                | Instruction::SetFrequency(SetFrequency {
                                    frame: FrameIdentifier { qubits, .. },
                                    ..
                                })
                                | Instruction::SetPhase(SetPhase {
                                    frame: FrameIdentifier { qubits, .. },
                                    ..
                                })
                                | Instruction::SetScale(SetScale {
                                    frame: FrameIdentifier { qubits, .. },
                                    ..
                                })
                                | Instruction::ShiftFrequency(ShiftFrequency {
                                    frame: FrameIdentifier { qubits, .. },
                                    ..
                                })
                                | Instruction::ShiftPhase(ShiftPhase {
                                    frame: FrameIdentifier { qubits, .. },
                                    ..
                                })
                                | Instruction::Pulse(Pulse {
                                    frame: FrameIdentifier { qubits, .. },
                                    ..
                                })
                                | Instruction::Fence(Fence { qubits }) => {
                                    // Swap all qubits for their concrete implementations
                                    for qubit in qubits {
                                        match qubit {
                                            Qubit::Variable(name) => {
                                                if let Some(expansion) = qubit_expansions.get(name)
                                                {
                                                    *qubit = expansion.clone();
                                                }
                                            }
                                            Qubit::Fixed(_) | Qubit::Placeholder(_) => {}
                                        }
                                    }
                                }
                                _ => {}
                            }

                            instruction.apply_to_expressions(|expr| {
                                let previous = std::mem::replace(expr, Expression::PiConstant);
                                *expr = previous.substitute_variables(&variable_expansions);
                            })
                        }

                        Some((
                            instructions,
                            CalibrationSource::Calibration(calibration.identifier.clone()),
                        ))
                    }
                    None => None,
                }
            }
            Instruction::Measurement(measurement) => {
                let matching_calibration = self.get_match_for_measurement(measurement);

                match matching_calibration {
                    Some(calibration) => {
                        let mut instructions = calibration.instructions.clone();
                        for instruction in instructions.iter_mut() {
                            match instruction {
                                Instruction::Pragma(pragma) => {
                                    if pragma.name == "LOAD-MEMORY"
                                        && pragma.data.as_ref()
                                            == Some(&calibration.identifier.parameter)
                                    {
                                        if let Some(target) = &measurement.target {
                                            pragma.data = Some(target.to_quil_or_debug())
                                        }
                                    }
                                }
                                Instruction::Capture(capture) => {
                                    if let Some(target) = &measurement.target {
                                        capture.memory_reference = target.clone()
                                    }
                                }
                                _ => {}
                            }
                        }
                        Some((
                            instructions,
                            CalibrationSource::MeasureCalibration(calibration.identifier.clone()),
                        ))
                    }
                    None => None,
                }
            }
            _ => None,
        };

        // Add this instruction to the breadcrumb trail before recursion
        let mut calibration_path = Vec::with_capacity(previous_calibrations.len() + 1);
        calibration_path.push(instruction.clone());
        calibration_path.extend_from_slice(previous_calibrations);

        self.recursively_expand_inner(expansion_result, &calibration_path, build_source_map)
    }

    fn recursively_expand_inner(
        &self,
        expansion_result: Option<(Vec<Instruction>, CalibrationSource)>,
        calibration_path: &[Instruction],
        build_source_map: bool,
    ) -> Result<Option<CalibrationExpansionOutput>, ProgramError> {
        Ok(match expansion_result {
            Some((instructions, matched_calibration)) => {
                let mut recursively_expanded_instructions = CalibrationExpansionOutput {
                    new_instructions: Vec::new(),
                    detail: CalibrationExpansion {
                        calibration_used: matched_calibration,
                        range: InstructionIndex(0)..InstructionIndex(0),
                        expansions: SourceMap::default(),
                    },
                };

                for (expanded_index, instruction) in instructions.into_iter().enumerate() {
                    let expanded_instructions =
                        self.expand_inner(&instruction, calibration_path, build_source_map)?;
                    match expanded_instructions {
                        Some(mut output) => {
                            if build_source_map {
                                let range_start = InstructionIndex(
                                    recursively_expanded_instructions.new_instructions.len(),
                                );

                                recursively_expanded_instructions
                                    .new_instructions
                                    .extend(output.new_instructions);

                                let range_end = InstructionIndex(
                                    recursively_expanded_instructions.new_instructions.len(),
                                );
                                output.detail.range = range_start..range_end;

                                recursively_expanded_instructions
                                    .detail
                                    .expansions
                                    .entries
                                    .push(SourceMapEntry {
                                        source_location: InstructionIndex(expanded_index),
                                        target_location: output.detail,
                                    });
                            } else {
                                recursively_expanded_instructions
                                    .new_instructions
                                    .extend(output.new_instructions);
                            }
                        }
                        None => {
                            recursively_expanded_instructions
                                .new_instructions
                                .push(instruction);
                        }
                    };
                }

                if build_source_map {
                    // While this appears to be duplicated information at this point, it's useful when multiple
                    // source mappings are merged together.
                    recursively_expanded_instructions.detail.range = InstructionIndex(0)
                        ..InstructionIndex(
                            recursively_expanded_instructions.new_instructions.len(),
                        );
                }

                Some(recursively_expanded_instructions)
            }
            None => None,
        })
    }

    /// Returns the last-specified [`MeasureCalibrationDefinition`] that matches the target
    /// qubit (if any), or otherwise the last-specified one that specified no qubit.
    ///
    /// If multiple calibrations match the measurement, the precedence is as follows:
    ///
    ///   1. Match fixed qubit.
    ///   2. Match variable qubit.
    ///   3. Match no qubit.
    ///
    /// In the case of multiple calibrations with equal precedence, the last one wins.
    pub fn get_match_for_measurement(
        &self,
        measurement: &Measurement,
    ) -> Option<&MeasureCalibrationDefinition> {
        measurement.target.as_ref()?;

        self.measure_calibrations()
            .into_iter()
            .rev()
            .fold_while(None, |best_match, calibration| {
                if let Some(qubit) = &calibration.identifier.qubit {
                    match qubit {
                        Qubit::Fixed(_) if qubit == &measurement.qubit => Done(Some(calibration)),
                        Qubit::Variable(_)
                            if best_match.is_none()
                                || best_match.is_some_and(|c| c.identifier.qubit.is_none()) =>
                        {
                            Continue(Some(calibration))
                        }
                        _ => Continue(best_match),
                    }
                } else if best_match.is_none() {
                    Continue(Some(calibration))
                } else {
                    Continue(best_match)
                }
            })
            .into_inner()
    }

    /// Return the final calibration which matches the gate per the QuilT specification:
    ///
    /// A calibration matches a gate if:
    /// 1. It has the same name
    /// 2. It has the same modifiers
    /// 3. It has the same qubit count (any mix of fixed & variable)
    /// 4. It has the same parameter count (both specified and unspecified)
    /// 5. All fixed qubits in the calibration definition match those in the gate
    /// 6. All specified parameters in the calibration definition match those in the gate
    pub fn get_match_for_gate(&self, gate: &Gate) -> Option<&Calibration> {
        let mut matched_calibration: Option<MatchedCalibration> = None;

        for calibration in self
            .iter_calibrations()
            .filter(|calibration| calibration.identifier.matches(gate))
        {
            matched_calibration = match matched_calibration {
                None => Some(MatchedCalibration::new(calibration)),
                Some(previous_match) => {
                    let potential_match = MatchedCalibration::new(calibration);
                    if potential_match.fixed_qubit_count >= previous_match.fixed_qubit_count {
                        Some(potential_match)
                    } else {
                        Some(previous_match)
                    }
                }
            }
        }

        matched_calibration.map(|m| m.calibration)
    }

    /// Return the count of contained calibrations.
    pub fn len(&self) -> usize {
        self.calibrations.len()
    }

    /// Return true if this contains no data.
    pub fn is_empty(&self) -> bool {
        self.calibrations.is_empty()
    }

    /// Insert a [`Calibration`] into the set.
    ///
    /// If a calibration with the same [`CalibrationSignature`] already exists in the set, it will
    /// be replaced, and the old calibration is returned.
    pub fn insert_calibration(&mut self, calibration: Calibration) -> Option<Calibration> {
        self.calibrations.replace(calibration)
    }

    /// Insert a [`MeasureCalibration`] into the set.
    ///
    /// If a calibration with the same [`CalibrationSignature`] already exists in the set, it will
    /// be replaced, and the old calibration is returned.
    pub fn insert_measurement_calibration(
        &mut self,
        calibration: MeasureCalibrationDefinition,
    ) -> Option<MeasureCalibrationDefinition> {
        self.measure_calibrations.replace(calibration)
    }

    /// Append another [`CalibrationSet`] onto this one.
    ///
    /// Calibrations with conflicting [`CalibrationSignature`]s are overwritten by the ones in the
    /// given set.
    pub fn extend(&mut self, other: Calibrations) {
        self.calibrations.extend(other.calibrations);
        self.measure_calibrations.extend(other.measure_calibrations);
    }

    /// Return the Quil instructions which describe the contained calibrations, consuming the
    /// [`CalibrationSet`]
    pub fn into_instructions(self) -> Vec<Instruction> {
        self.calibrations
            .into_iter()
            .map(Instruction::CalibrationDefinition)
            .chain(
                self.measure_calibrations
                    .into_iter()
                    .map(Instruction::MeasureCalibrationDefinition),
            )
            .collect()
    }

    /// Return the Quil instructions which describe the contained calibrations.
    pub fn to_instructions(&self) -> Vec<Instruction> {
        self.iter_calibrations()
            .cloned()
            .map(Instruction::CalibrationDefinition)
            .chain(
                self.iter_measure_calibrations()
                    .cloned()
                    .map(Instruction::MeasureCalibrationDefinition),
            )
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use crate::program::calibration::{CalibrationSource, MeasureCalibrationIdentifier};
    use crate::program::source_map::{SourceMap, SourceMapEntry};
    use crate::program::{InstructionIndex, Program};
    use crate::quil::Quil;

    use insta::assert_snapshot;
    use rstest::rstest;

    use super::{CalibrationExpansion, CalibrationExpansionOutput, CalibrationIdentifier};

    #[rstest]
    #[case(
        "Calibration-Param-Precedence",
        concat!(
            "DEFCAL RX(%theta) %qubit:\n",
            "    PULSE 1 \"xy\" gaussian(duration: 1, fwhm: 2, t0: 3)\n",
            "DEFCAL RX(%theta) 0:\n",
            "    PULSE 2 \"xy\" gaussian(duration: 1, fwhm: 2, t0: 3)\n",
            "DEFCAL RX(pi/2) 0:\n",
            "    PULSE 3 \"xy\" gaussian(duration: 1, fwhm: 2, t0: 3)\n",
            "RX(pi/2) 1\n",
            "RX(pi) 0\n",
            "RX(pi/2) 0\n"
        ),
    )]
    #[case(
        "Calibration-Simple",
        concat!(
            "DEFCAL X 0:\n",
            "    PULSE 0 \"xy\" gaussian(duration: 1, fwhm: 2, t0: 3)\n",
            "X 0\n",
        ),
    )]
    #[case(
        "Calibration-Literal-Parameter",
        concat!(
            "DEFCAL RX(3.141592653589793) 0:\n",
            "    NOP\n",
            "RX(3.141592653589793) 0\n",
        ),
    )]
    #[case(
        "Calibration-Instruction-Match",
        concat!(
            "DEFCAL X 0:\n",
            "    Y 0\n",
            "DEFCAL Y 0:\n",
            "    PULSE 0 \"xy\" gaussian(duration: 1, fwhm: 2, t0: 3)\n",
            "X 0\n"
        ),
    )]
    #[case(
        "Measure-Calibration",
        concat!(
            "DEFCAL MEASURE 0 addr:\n",
            "    PRAGMA INCORRECT_ORDERING\n",
            "DEFCAL MEASURE 0 addr:\n",
            "    PRAGMA CORRECT\n",
            "DEFCAL MEASURE q addr:\n",
            "    PRAGMA INCORRECT_PRECEDENCE\n",
            "DEFCAL MEASURE 1 addr:\n",
            "    PRAGMA INCORRECT_QUBIT\n",
            "DEFCAL MEASURE addr:\n",
            "    PRAGMA INCORRECT_PRECEDENCE\n",
            "MEASURE 0 ro\n",
        ),
    )]
    #[case(
        "Calibration-Variable-Qubit",
        concat!("DEFCAL I %q:\n", "    DELAY q 4e-8\n", "I 0\n",),
    )]
    #[case(
        "Precedence-Fixed-Match",
        concat!(
            "DEFCAL MEASURE addr:\n",
            "    PRAGMA INCORRECT_PRECEDENCE\n",
            "DEFCAL MEASURE q addr:\n",
            "    PRAGMA INCORRECT_PRECEDENCE\n",
            "DEFCAL MEASURE 0 addr:\n",
            "    PRAGMA INCORRECT_ORDER\n",
            "DEFCAL MEASURE 0 addr:\n",
            "    PRAGMA CORRECT\n",
            "MEASURE 0 ro\n",
        )
    )]
    #[case(
        "Precedence-Variable-Match",
        concat!(
            "DEFCAL MEASURE addr:\n",
            "    PRAGMA INCORRECT_PRECEDENCE\n",
            "DEFCAL MEASURE q addr:\n",
            "    PRAGMA INCORRECT_PRECEDENCE\n",
            "DEFCAL MEASURE b addr:\n",
            "    PRAGMA CORRECT\n",
            "MEASURE 0 ro\n",
        )
    )]
    #[case(
        "Precedence-No-Qubit-Match",
        concat!(
            "DEFCAL MEASURE addr:\n",
            "    PRAGMA INCORRECT_PRECEDENCE\n",
            "DEFCAL MEASURE addr:\n",
            "    PRAGMA CORRECT\n",
            "MEASURE 0 ro\n",
        )
    )]
    #[case(
        "ShiftPhase",
        concat!(
            "DEFCAL RZ(%theta) %q:\n",
            "    SHIFT-PHASE %q \"rf\" -%theta\n",
            "RZ(pi) 0\n",
        )
    )]
    #[case(
        "FenceVariableQubit",
        concat!(
            "DEFCAL FENCES q0 q1:\n",
            "    FENCE q0\n",
            "    FENCE q1\n",
            "FENCES 0 1\n",
        )
    )]
    fn test_expansion(#[case] description: &str, #[case] input: &str) {
        let program = Program::from_str(input).unwrap();
        let calibrated_program = program.expand_calibrations().unwrap();
        insta::with_settings!({
            snapshot_suffix => description,
        }, {
            assert_snapshot!(calibrated_program.to_quil_or_debug())
        })
    }

    /// Assert that instruction expansion yields the expected [`SourceMap`] and resulting instructions.
    #[test]
    fn expand_with_detail_recursive() {
        let input = r#"
DEFCAL X 0:
    Y 0
    MEASURE 0 ro
    Y 0

DEFCAL Y 0:
    NOP
    Z 0

DEFCAL Z 0:
    WAIT

DEFCAL MEASURE 0 addr:
    HALT

X 0
"#;

        let program = Program::from_str(input).unwrap();
        let instruction = program.instructions.last().unwrap();
        let expansion = program
            .calibrations
            .expand_with_detail(instruction, &[])
            .unwrap();
        let expected = CalibrationExpansionOutput {
            new_instructions: vec![
                crate::instruction::Instruction::Nop,
                crate::instruction::Instruction::Wait,
                crate::instruction::Instruction::Halt,
                crate::instruction::Instruction::Nop,
                crate::instruction::Instruction::Wait,
            ],
            detail: CalibrationExpansion {
                calibration_used: CalibrationSource::Calibration(CalibrationIdentifier {
                    modifiers: vec![],
                    name: "X".to_string(),
                    parameters: vec![],
                    qubits: vec![crate::instruction::Qubit::Fixed(0)],
                }),
                range: InstructionIndex(0)..InstructionIndex(5),
                expansions: SourceMap {
                    entries: vec![
                        SourceMapEntry {
                            source_location: InstructionIndex(0),
                            target_location: CalibrationExpansion {
                                calibration_used: CalibrationSource::Calibration(
                                    CalibrationIdentifier {
                                        modifiers: vec![],
                                        name: "Y".to_string(),
                                        parameters: vec![],
                                        qubits: vec![crate::instruction::Qubit::Fixed(0)],
                                    },
                                ),
                                range: InstructionIndex(0)..InstructionIndex(2),
                                expansions: SourceMap {
                                    entries: vec![SourceMapEntry {
                                        source_location: InstructionIndex(1),
                                        target_location: CalibrationExpansion {
                                            calibration_used: CalibrationSource::Calibration(
                                                CalibrationIdentifier {
                                                    modifiers: vec![],
                                                    name: "Z".to_string(),
                                                    parameters: vec![],
                                                    qubits: vec![crate::instruction::Qubit::Fixed(
                                                        0,
                                                    )],
                                                },
                                            ),
                                            range: InstructionIndex(1)..InstructionIndex(2),
                                            expansions: SourceMap::default(),
                                        },
                                    }],
                                },
                            },
                        },
                        SourceMapEntry {
                            source_location: InstructionIndex(1),
                            target_location: CalibrationExpansion {
                                calibration_used: CalibrationSource::MeasureCalibration(
                                    MeasureCalibrationIdentifier {
                                        qubit: Some(crate::instruction::Qubit::Fixed(0)),
                                        parameter: "addr".to_string(),
                                    },
                                ),
                                range: InstructionIndex(2)..InstructionIndex(3),
                                expansions: SourceMap::default(),
                            },
                        },
                        SourceMapEntry {
                            source_location: InstructionIndex(2),
                            target_location: CalibrationExpansion {
                                calibration_used: CalibrationSource::Calibration(
                                    CalibrationIdentifier {
                                        modifiers: vec![],
                                        name: "Y".to_string(),
                                        parameters: vec![],
                                        qubits: vec![crate::instruction::Qubit::Fixed(0)],
                                    },
                                ),
                                range: InstructionIndex(3)..InstructionIndex(5),
                                expansions: SourceMap {
                                    entries: vec![SourceMapEntry {
                                        source_location: InstructionIndex(1),
                                        target_location: CalibrationExpansion {
                                            calibration_used: CalibrationSource::Calibration(
                                                CalibrationIdentifier {
                                                    modifiers: vec![],
                                                    name: "Z".to_string(),
                                                    parameters: vec![],
                                                    qubits: vec![crate::instruction::Qubit::Fixed(
                                                        0,
                                                    )],
                                                },
                                            ),
                                            range: InstructionIndex(1)..InstructionIndex(2),
                                            expansions: SourceMap::default(),
                                        },
                                    }],
                                },
                            },
                        },
                    ],
                },
            },
        };

        pretty_assertions::assert_eq!(expansion, Some(expected));
    }

    #[test]
    fn test_eq() {
        let input = "DEFCAL X 0:
    PULSE 0 \"xy\" gaussian(duration: 1, fwhm: 2, t0: 3)
X 0";
        let a = Program::from_str(input);
        let b = Program::from_str(input);
        assert_eq!(a, b);
    }

    #[test]
    fn test_ne() {
        let input_a = "DEFCAL X 0:
    PULSE 0 \"xy\" gaussian(duration: 1, fwhm: 2, t0: 3)
X 0";
        let input_b = "DEFCAL X 1:
    PULSE 1 \"xy\" gaussian(duration: 1, fwhm: 2, t0: 3)
X 1";
        let a = Program::from_str(input_a);
        let b = Program::from_str(input_b);
        assert_ne!(a, b);
    }
}
