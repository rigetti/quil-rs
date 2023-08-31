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

use itertools::FoldWhile::{Continue, Done};
use itertools::Itertools;

use crate::quil::Quil;
use crate::{
    expression::Expression,
    instruction::{
        Calibration, Capture, Delay, FrameIdentifier, Gate, Instruction,
        MeasureCalibrationDefinition, Measurement, Pulse, Qubit, RawCapture, SetFrequency,
        SetPhase, SetScale, ShiftFrequency, ShiftPhase,
    },
};

use super::ProgramError;

/// A collection of Quil calibrations (`DEFCAL` instructions) with utility methods.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct CalibrationSet {
    pub calibrations: Vec<Calibration>,
    pub measure_calibrations: Vec<MeasureCalibrationDefinition>,
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

impl CalibrationSet {
    /// Return all [`Calibration`]s in the set
    pub fn calibrations(&self) -> &[Calibration] {
        self.calibrations.as_ref()
    }

    /// Return all [`MeasureCalibrationDefinition`]s in the set
    pub fn measure_calibrations(&self) -> &[MeasureCalibrationDefinition] {
        self.measure_calibrations.as_ref()
    }

    /// Given an instruction, return the instructions to which it is expanded if there is a match.
    /// Recursively calibrate instructions, returning an error if a calibration directly or indirectly
    /// expands into itself.
    pub fn expand(
        &self,
        instruction: &Instruction,
        previous_calibrations: &[Instruction],
    ) -> Result<Option<Vec<Instruction>>, ProgramError> {
        if previous_calibrations.contains(instruction) {
            return Err(ProgramError::RecursiveCalibration(instruction.clone()));
        }
        let expanded_once_instructions = match instruction {
            Instruction::Gate(gate) => {
                let matching_calibration = self.get_match_for_gate(gate);

                match matching_calibration {
                    Some(calibration) => {
                        let mut qubit_expansions: HashMap<&String, Qubit> = HashMap::new();
                        for (index, calibration_qubit) in calibration.qubits.iter().enumerate() {
                            if let Qubit::Variable(identifier) = calibration_qubit {
                                qubit_expansions.insert(identifier, gate.qubits[index].clone());
                            }
                        }

                        // Variables used within the calibration's definition should be replaced with the actual expressions used by the gate.
                        // That is, `DEFCAL RX(%theta): ...` should have `%theta` replaced by `pi` throughout if it's used to expand `RX(pi)`.
                        let variable_expansions: HashMap<String, Expression> = calibration
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
                                }) => {
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

                        Some(instructions)
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
                                        && pragma.data.as_ref() == Some(&calibration.parameter)
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
                        Some(instructions)
                    }
                    None => None,
                }
            }
            _ => None,
        };

        // Add this instruction to the breadcrumb trail before recursion
        let mut downstream_previous_calibrations =
            Vec::with_capacity(previous_calibrations.len() + 1);
        downstream_previous_calibrations.push(instruction.clone());
        downstream_previous_calibrations.extend_from_slice(previous_calibrations);

        Ok(match expanded_once_instructions {
            Some(instructions) => {
                let mut recursively_expanded_instructions = vec![];

                for instruction in instructions {
                    let expanded_instructions =
                        self.expand(&instruction, &downstream_previous_calibrations)?;
                    match expanded_instructions {
                        Some(instructions) => {
                            recursively_expanded_instructions.extend(instructions)
                        }
                        None => recursively_expanded_instructions.push(instruction),
                    };
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
            .iter()
            .rev()
            .fold_while(None, |best_match, calibration| {
                if let Some(qubit) = &calibration.qubit {
                    match qubit {
                        Qubit::Fixed(_) if qubit == &measurement.qubit => Done(Some(calibration)),
                        Qubit::Variable(_)
                            if best_match.is_none()
                                || best_match.is_some_and(|c| c.qubit.is_none()) =>
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

        for calibration in &self.calibrations {
            // Filter out non-matching calibrations: check rules 1-4
            if calibration.name != gate.name
                || calibration.modifiers != gate.modifiers
                || calibration.parameters.len() != gate.parameters.len()
                || calibration.qubits.len() != gate.qubits.len()
            {
                continue;
            }

            let fixed_qubits_match =
                calibration
                    .qubits
                    .iter()
                    .enumerate()
                    .all(|(calibration_index, _)| {
                        match (
                            &calibration.qubits[calibration_index],
                            &gate.qubits[calibration_index],
                        ) {
                            // Placeholders never match
                            (Qubit::Placeholder(_), _) | (_, Qubit::Placeholder(_)) => false,
                            // If they're both fixed, test if they're fixed to the same qubit
                            (
                                Qubit::Fixed(calibration_fixed_qubit),
                                Qubit::Fixed(gate_fixed_qubit),
                            ) => calibration_fixed_qubit == gate_fixed_qubit,
                            // If the calibration is variable, it matches any fixed qubit
                            (Qubit::Variable(_), _) => true,
                            // If the calibration is fixed, but the gate's qubit is variable, it's not a match
                            (Qubit::Fixed(_), _) => false,
                        }
                    });
            if !fixed_qubits_match {
                continue;
            }

            let fixed_parameters_match =
                calibration
                    .parameters
                    .iter()
                    .enumerate()
                    .all(|(calibration_index, _)| {
                        let calibration_parameters = calibration.parameters[calibration_index]
                            .clone()
                            .into_simplified();
                        let gate_parameters =
                            gate.parameters[calibration_index].clone().into_simplified();
                        match (calibration_parameters, gate_parameters) {
                            // If the calibration is variable, it matches any fixed qubit
                            (Expression::Variable(_), _) => true,
                            // If the calibration is fixed, but the gate's qubit is variable, it's not a match
                            (calib, gate) => calib == gate,
                        }
                    });
            if !fixed_parameters_match {
                continue;
            }

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

    /// Add another gate calibration to the set.
    /// Deprecated in favor of [`Self::push_calibration`]
    #[deprecated = "use ScheduledProgram#push_calibration instead"]
    pub fn push(&mut self, calibration: Calibration) {
        self.push_calibration(calibration)
    }

    /// Add another gate calibration (`DEFCAL`) to the set.
    pub fn push_calibration(&mut self, calibration: Calibration) {
        self.calibrations.push(calibration)
    }

    /// Add another measurement calibration (`DEFCAL MEASURE`) to the set.
    pub fn push_measurement_calibration(&mut self, calibration: MeasureCalibrationDefinition) {
        self.measure_calibrations.push(calibration)
    }

    /// Append another [`CalibrationSet`] onto this one
    pub fn extend(&mut self, other: CalibrationSet) {
        self.calibrations.extend(other.calibrations);
        self.measure_calibrations.extend(other.measure_calibrations);
    }

    /// Return the Quil instructions which describe the contained calibrations, consuming the [`CalibrationSet`].
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
        self.calibrations
            .iter()
            .cloned()
            .map(Instruction::CalibrationDefinition)
            .chain(
                self.measure_calibrations
                    .iter()
                    .cloned()
                    .map(Instruction::MeasureCalibrationDefinition),
            )
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use crate::program::Program;
    use crate::quil::Quil;

    use insta::assert_snapshot;
    use rstest::rstest;

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
    fn test_expansion(#[case] description: &str, #[case] input: &str) {
        let program = Program::from_str(input).unwrap();
        let calibrated_program = program.expand_calibrations().unwrap();
        insta::with_settings!({
            snapshot_suffix => description,
        }, {
            assert_snapshot!(calibrated_program.to_quil_or_debug())
        })
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
