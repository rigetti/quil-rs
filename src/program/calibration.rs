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

use crate::{
    expression::Expression,
    instruction::{Calibration, GateModifier, Instruction, Qubit},
};

/// A collection of Quil calibrations (`DEFCAL` instructions) with utility methods.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct CalibrationSet {
    calibrations: Vec<Calibration>,
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
                    Qubit::Variable(_) => false,
                })
                .count(),
        }
    }
}

impl CalibrationSet {
    pub fn new() -> Self {
        CalibrationSet {
            calibrations: vec![],
        }
    }

    /// Given an instruction, return the instructions to which it is expanded if there is a match.
    pub fn expand(&self, instruction: &Instruction) -> Option<Vec<Instruction>> {
        match instruction {
            Instruction::Gate {
                name,
                modifiers,
                parameters,
                qubits,
            } => {
                let matching_calibration =
                    self.get_match_for_gate(modifiers, name, parameters, qubits);

                match matching_calibration {
                    Some(calibration) => {
                        let mut qubit_expansions: HashMap<&String, Qubit> = HashMap::new();
                        for (index, calibration_qubit) in calibration.qubits.iter().enumerate() {
                            if let Qubit::Variable(identifier) = calibration_qubit {
                                qubit_expansions.insert(identifier, qubits[index].clone());
                            }
                        }

                        let mut instructions = calibration.instructions.clone();

                        for instruction in instructions.iter_mut() {
                            if let Instruction::Gate { qubits, .. } = instruction {
                                // Swap all qubits for their concrete implementations
                                for qubit in qubits {
                                    match qubit {
                                        Qubit::Variable(name) => {
                                            if let Some(expansion) = qubit_expansions.get(name) {
                                                *qubit = expansion.clone();
                                            }
                                        }
                                        Qubit::Fixed(_) => {}
                                    }
                                }
                            }
                        }

                        Some(instructions)
                    }
                    None => None,
                }
            }
            _ => None,
        }
    }

    /// Return the final calibration which matches the gate per the QuilT specification:
    ///
    /// A calibration matches a gate iff:
    /// 1. It has the same name
    /// 2. It has the same modifiers
    /// 3. It has the same qubit count (any mix of fixed & variable)
    /// 4. It has the same parameter count (both specified and unspecified)
    /// 5. All fixed qubits in the calibration definition match those in the gate
    /// 6. All specified parameters in the calibration definition match those in the gate
    pub fn get_match_for_gate(
        &self,
        gate_modifiers: &[GateModifier],
        gate_name: &str,
        gate_parameters: &[Expression],
        gate_qubits: &[Qubit],
    ) -> Option<&Calibration> {
        let mut matched_calibration: Option<MatchedCalibration> = None;

        for calibration in &self.calibrations {
            // Filter out non-matching calibrations: check rules 1-4
            if calibration.name != gate_name
                || calibration.modifiers != gate_modifiers
                || calibration.parameters.len() != gate_parameters.len()
                || calibration.qubits.len() != gate_qubits.len()
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
                            &gate_qubits[calibration_index],
                        ) {
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
                            .evaluate(&HashMap::new(), None);
                        let gate_parameters = gate_parameters[calibration_index]
                            .clone()
                            .evaluate(&HashMap::new(), None);
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

    /// Add another calibration to the set.
    pub fn push(&mut self, calibration: Calibration) {
        self.calibrations.push(calibration)
    }

    /// Return the Quil instructions which describe the contained calibrations.
    pub fn to_instructions(&self) -> Vec<Instruction> {
        self.calibrations
            .iter()
            .map(|c| Instruction::CalibrationDefinition(c.clone()))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::program::Program;
    use std::str::FromStr;

    #[test]
    fn test_expansion() {
        struct TestCase<'a> {
            input: &'a str,
            expected: &'a str,
        }

        let cases = vec![
            // Test match ordering/precedence
            TestCase {
                input: concat!(
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
                expected: concat!(
                    "PULSE 1 \"xy\" gaussian(duration: 1, fwhm: 2, t0: 3)\n",
                    "PULSE 2 \"xy\" gaussian(duration: 1, fwhm: 2, t0: 3)\n",
                    "PULSE 3 \"xy\" gaussian(duration: 1, fwhm: 2, t0: 3)\n"
                ),
            },
            TestCase {
                input: concat!(
                    "DEFCAL X 0:\n",
                    "    PULSE 0 \"xy\" gaussian(duration: 1, fwhm: 2, t0: 3)\n",
                    "X 0\n"
                ),
                expected: "PULSE 0 \"xy\" gaussian(duration: 1, fwhm: 2, t0: 3)\n",
            },
        ];

        for case in &cases {
            let mut program = Program::from_str(case.input).unwrap();
            program.expand_calibrations();
            assert_eq!(program.to_string(false).as_str(), case.expected);
        }
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
