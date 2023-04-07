use std::{
    collections::{BTreeMap, HashSet},
    fmt,
};

use crate::instruction::{Gate, Qubit};

use self::wire::Wire;
use super::{LatexGenError, RenderCommand, RenderSettings};

pub(crate) mod wire;

/// A Diagram represents a collection of wires in a Circuit. The size of the
/// Diagram can be measured by multiplying the number of Instructions in a
/// Program with the length of the Circuit. This is an [m x n] matrix where n,
/// is the number of Quil instructions (or columns), and m, is the number of
/// wires (or rows). Each individual element of the matrix represents an item
/// that is serializable into LaTeX using the ``Quantikz`` RenderCommands.
#[derive(Clone, Debug, Default)]
pub(super) struct Diagram {
    /// customizes how the diagram renders the circuit
    pub(crate) settings: RenderSettings,
    /// Wires (diagram rows) keyed by qubit index
    pub(crate) circuit: BTreeMap<u64, Box<Wire>>,
}

impl Diagram {
    /// Compares qubits from a single instruction associated with a column on
    /// the circuit to all of the qubits used in the quil program. If a qubit
    /// from the quil program is not found in the qubits in the single
    /// instruction line, then an empty slot is added to that column on the
    /// qubit wire of the circuit indicating a "do nothing" at that column.
    ///
    /// # Arguments
    /// `qubits` - exposes the qubits used in the Program
    /// `instruction` - exposes the qubits in a single Instruction
    pub(crate) fn apply_empty(&mut self, qubits: &HashSet<Qubit>, gate: &Gate) {
        qubits
            .difference(&gate.qubits.iter().cloned().collect())
            .filter_map(|q| match q {
                Qubit::Fixed(index) => Some(index),
                _ => None,
            })
            .for_each(|index| {
                if let Some(wire) = self.circuit.get_mut(index) {
                    wire.set_empty()
                }
            });
    }

    /// Applies a gate from an instruction to the associated wires in the
    /// circuit of this diagram.
    ///
    /// # Arguments
    /// `gate` - the Gate of the Instruction from `to_latex`.
    pub(crate) fn apply_gate(&mut self, gate: &Gate) -> Result<(), LatexGenError> {
        // set the parameters for each qubit in the instruction
        for qubit in &gate.qubits {
            if let Qubit::Fixed(qubit) = qubit {
                if let Some(wire) = self.circuit.get_mut(qubit) {
                    for expression in &gate.parameters {
                        wire.set_param(expression, self.settings.texify_numerical_constants);
                    }
                }
            }
        }

        // get the names of the qubits in the circuit before circuit is borrowed as mutable
        let circuit_qubits: Vec<u64> = self.circuit.keys().cloned().collect();

        // set gate for each qubit in the instruction
        let last_qubit = gate.qubits.last().unwrap();
        for (i, qubit) in gate.qubits.iter().enumerate() {
            if let Qubit::Fixed(instruction_qubit) = qubit {
                if let Some(wire) = self.circuit.get_mut(instruction_qubit) {
                    // push the gate to the wire
                    wire.gates.push(wire::T::try_from(gate.clone())?);

                    // update the last gate to a control gate if the instruction has more than one qubit
                    if gate.qubits.len() > 1 {
                        if let wire::T::StdGate {
                            name,
                            dagger_count: _,
                            ctrl_count,
                        } = wire.gates.last().unwrap()
                        {
                            if i != gate.qubits.len() - 1 {
                                // reset the last gate to a control gate
                                wire.gates.pop();
                                wire.set_ctrl(qubit, last_qubit, &circuit_qubits);
                            } else if i == gate.qubits.len() - 1 && ctrl_count - i > 1 {
                                // there should be no more than one control modifier from the last qubit in a control and target instruction
                                return Err(LatexGenError::UnsupportedGate { gate: name.clone() });
                            }
                        }
                    }

                    // parameterized non-PHASE gates are unsupported
                    if !wire.parameters.is_empty() && !gate.name.contains("PHASE") {
                        // parameterized single qubit gates are unsupported
                        return Err(LatexGenError::UnsupportedGate {
                            gate: gate.name.clone(),
                        });
                    }
                }
            }
        }

        Ok(())
    }
}

impl fmt::Display for Diagram {
    /// Returns a result containing the body of the Document.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // write a newline between the body and the Document header
        writeln!(f)?;

        // write the LaTeX string for each wire in the circuit
        let last = self.circuit.keys().last().unwrap_or(&0);
        for (qubit, wire) in &self.circuit {
            // are labels on in settings?
            if self.settings.label_qubit_lines {
                // write the label to the left side of wire
                write!(f, "{}", RenderCommand::LeftWireLabel(*qubit))?;
            } else {
                // write an empty column buffer as the first column
                write!(f, "{}", RenderCommand::Qw)?;
            }

            // write the LaTeX string for the wire
            write!(f, "{wire}")?;

            // chain an empty column to the end of the line
            write!(f, " & ")?;
            write!(f, "{}", &RenderCommand::Qw)?;

            // omit a new row if this is the last qubit wire
            if *qubit != *last {
                // otherwise, write a new row to the end of the line
                write!(f, " ")?;
                write!(f, "{}", &RenderCommand::Nr)?;
            }

            // write a newline between each row and or the body and the document footer
            writeln!(f)?;
        }

        Ok(())
    }
}
