//! Higher-level construct which represents a program in a way that can be converted to a diagram.

use std::collections::BTreeSet;

use once_cell::sync::Lazy;
use regex::Regex;

use crate::{
    instruction::{Gate, GateModifier, Instruction, Measurement, Qubit},
    quil::Quil,
};

use super::RenderSettings;

#[derive(Debug)]
pub(super) enum CircuitElement {
    SingleQubitGate {
        dagger_count: u64,
        name: String,
        parameters: Vec<String>,
        qubit: u64,
        controls: BTreeSet<u64>,
    },
    Measurement {
        qubit: u64,
    },
}

impl Gate {
    /// A single gate can be represented in multiple ways. This function converts the gate to one canonical form
    /// for easier processing.
    fn to_canonical_form(&self) -> Self {
        let mut new = self.clone();

        static ABBREVIATED_CONTROLLED_GATE: Lazy<Regex> =
            Lazy::new(|| Regex::new("(?P<count>C+)(?P<base>PHASE|X|Y|Z|NOT)").unwrap());

        if let Some(captures) = ABBREVIATED_CONTROLLED_GATE.captures(&new.name) {
            let count = captures.name("count").unwrap().as_str().len();
            let base = captures.name("base").unwrap().as_str();

            new.name = base.to_string();
            new.modifiers.extend(vec![GateModifier::Controlled; count]);
        }

        new
    }

    /// Remove the qubits from the gate which are used as its control qubits, and
    /// return them as a set.
    ///
    /// Return an error if any control qubit is also a target qubit.
    fn extract_fixed_controls(&mut self) -> BuildCircuitResult<BTreeSet<u64>> {
        let mut control_count = 0;

        self.modifiers.retain(|modifier| {
            if modifier == &GateModifier::Controlled {
                control_count += 1;
                false
            } else {
                true
            }
        });

        let new_qubits = self.qubits.split_off(control_count);
        let controls = self
            .qubits
            .iter()
            .map(|qubit| match qubit {
                Qubit::Fixed(index) => Ok(*index),
                Qubit::Variable(_) | Qubit::Placeholder(_) => todo!(),
            })
            .collect::<Result<BTreeSet<_>, _>>()?;

        if let Some(overlap) = controls
            .iter()
            .find(|control| new_qubits.contains(&Qubit::Fixed(**control)))
        {
            return Err(BuildCircuitError::ControlAndTargetQubitOverlap {
                qubit: Qubit::Fixed(*overlap),
            });
        }

        self.qubits = new_qubits;

        Ok(controls)
    }

    /// Remove the dagger modifiers from the gate and return their count.
    fn extract_dagger_count(&mut self) -> u64 {
        let mut count = 0;

        self.modifiers.retain(|modifier| {
            if modifier == &GateModifier::Dagger {
                count += 1;
                false
            } else {
                true
            }
        });

        count
    }
}

impl TryFrom<&Gate> for CircuitElement {
    type Error = BuildCircuitError;

    fn try_from(gate: &Gate) -> Result<Self, Self::Error> {
        let mut canonical_gate = gate.to_canonical_form();
        let controls = canonical_gate.extract_fixed_controls()?;
        let dagger_count = canonical_gate.extract_dagger_count();

        match canonical_gate.qubits.as_slice() {
            &[Qubit::Fixed(qubit)] if canonical_gate.modifiers.is_empty() => {
                Ok(Self::SingleQubitGate {
                    controls,
                    parameters: gate
                        .parameters
                        .iter()
                        .map(|p| p.to_quil_or_debug())
                        .collect(),
                    dagger_count,
                    name: canonical_gate.name.clone(),
                    qubit,
                })
            }
            _ => Err(BuildCircuitError::UnsupportedGate { gate: gate.clone() }),
        }
    }
}

impl TryFrom<&Measurement> for CircuitElement {
    type Error = BuildCircuitError;

    fn try_from(measurement: &Measurement) -> Result<Self, Self::Error> {
        match measurement.qubit {
            Qubit::Fixed(qubit) => Ok(Self::Measurement { qubit }),
            Qubit::Variable(_) | Qubit::Placeholder(_) => {
                Err(BuildCircuitError::UnsupportedMeasurement {
                    measurement: measurement.clone(),
                })
            }
        }
    }
}

#[derive(Debug)]
pub(super) struct CircuitColumn {
    // TODO: this could be Vec<CircuitElement> if we compress columns
    pub element: CircuitElement,
}

#[derive(Debug, Default)]
pub(super) struct Circuit {
    pub columns: Vec<CircuitColumn>,

    pub settings: RenderSettings,
}

impl Circuit {
    pub fn get_qubit_indices(&self) -> BTreeSet<u64> {
        let mut indices = BTreeSet::new();

        for column in &self.columns {
            match &column.element {
                CircuitElement::SingleQubitGate {
                    qubit, controls, ..
                } => {
                    indices.insert(*qubit);
                    indices.extend(controls)
                }
                CircuitElement::Measurement { qubit } => {
                    indices.insert(*qubit);
                }
            }
        }

        indices
    }
}

#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
#[allow(clippy::enum_variant_names)]
pub enum BuildCircuitError {
    #[error("control qubit {} is also a target qubit", qubit.to_quil_or_debug())]
    ControlAndTargetQubitOverlap { qubit: Qubit },

    #[error("cannot render gate as LaTeX: {gate:?}")]
    UnsupportedGate { gate: Gate },

    #[error("cannot render measurement as LaTeX: {measurement:?}")]
    UnsupportedMeasurement { measurement: Measurement },

    #[error("cannot render instruction as LaTeX: {}", instruction.to_quil_or_debug())]
    UnsupportedInstruction { instruction: Instruction },
}

pub type BuildCircuitResult<T> = Result<T, BuildCircuitError>;

impl Circuit {
    pub(super) fn try_from_program(
        program: &crate::Program,
        settings: RenderSettings,
    ) -> BuildCircuitResult<Self> {
        let mut circuit = Circuit {
            settings, // TODO not ideal, reconsider how and when these should be used. Not all settings are inherent to the circuit's construction
            ..Default::default()
        };

        for instruction in &program.to_instructions() {
            let element = match instruction {
                Instruction::Gate(gate) => gate.try_into().map(Some),
                Instruction::Measurement(measurement) => measurement.try_into().map(Some),
                Instruction::GateDefinition(_) | Instruction::Reset(_) => Ok(None),
                _ => Err(BuildCircuitError::UnsupportedInstruction {
                    instruction: instruction.clone(),
                }),
            }?;

            if let Some(element) = element {
                let column = CircuitColumn { element };

                circuit.columns.push(column);
            }
        }

        Ok(circuit)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use rstest::rstest;

    use crate::{instruction::Gate, quil::Quil};

    #[rstest]
    #[case("CPHASE 0 1", "CONTROLLED PHASE 0 1")]
    #[case("CZ 0 1", "CONTROLLED Z 0 1")]
    #[case("DAGGER CZ 0 1", "DAGGER CONTROLLED Z 0 1")]
    fn canonicalize_gate(#[case] input: &str, #[case] expected: &str) {
        let input_gate = <Gate as std::str::FromStr>::from_str(input).unwrap();
        let canonical = input_gate.to_canonical_form();
        assert_eq!(canonical.to_quil_or_debug().as_str(), expected);
    }

    #[rstest]
    #[case("CONTROLLED PHASE 1 0", "PHASE 0", vec![1])]
    #[case("CONTROLLED CONTROLLED PHASE 2 1 0", "PHASE 0", vec![1, 2])]
    #[case("CNOT 1 2", "NOT 2", vec![1])]
    fn extract_controls(
        #[case] input: &str,
        #[case] expected: &str,
        #[case] expected_controls: Vec<u64>,
    ) {
        let input_gate = <Gate as std::str::FromStr>::from_str(input).unwrap();
        let mut canonical = input_gate.to_canonical_form();
        let controls = canonical.extract_fixed_controls().unwrap();
        assert_eq!(canonical.to_quil_or_debug().as_str(), expected);
        assert_eq!(
            controls,
            expected_controls.into_iter().collect::<BTreeSet<u64>>()
        );
    }
}
