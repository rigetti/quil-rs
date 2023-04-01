use std::{
    collections::{BTreeMap, HashSet},
    fmt,
    str::FromStr,
};

use crate::instruction::{Gate, Qubit};

use self::wire::Wire;
use super::{LatexGenError, RenderCommand, RenderSettings};

pub(crate) mod wire;

/// Gates written in shorthand notation, i.e. composite form, that may be
/// decomposed into modifiers and single gate instructions, i.e. canonical form.
#[derive(Clone, Debug, strum::EnumString, derive_more::Display, PartialEq, Eq, Hash)]
#[strum(serialize_all = "UPPERCASE")]
enum CompositeGate {
    /// `CNOT` is `CONTROLLED X`
    #[display(fmt = "X")]
    Cnot,
    /// `CCNOT` is `CONTROLLED CONTROLLED X`
    #[display(fmt = "X")]
    Ccnot,
    /// `CPHASE` is `CONTROLLED PHASE`
    #[display(fmt = "PHASE")]
    Cphase,
    /// `CZ` is `CONTROLLED Z`
    #[display(fmt = "Z")]
    Cz,
}

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
    /// a BTreeMap of wires with the name of the wire as the key
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
                    // increment the wire column
                    wire.column += 1;
                    wire.set_empty()
                }
            });
    }

    /// Applies a gate from an instruction to the wires on the circuit
    /// associate with the qubits from the gate. If the gate name matches a
    /// composite gate, then the composite gate is applied to the circuit,
    /// otherwise, the original gate is applied to the circuit.
    ///
    /// # Arguments
    /// `gate` - the Gate of the Instruction from `to_latex`.
    pub(crate) fn apply_gate(&mut self, gate: &Gate) -> Result<(), LatexGenError> {
        // for each fixed qubit in the gate
        for qubit in &gate.qubits {
            if let Qubit::Fixed(qubit) = qubit {
                if let Some(wire) = self.circuit.get_mut(qubit) {
                    // increment the wire column
                    wire.column += 1;

                    // set modifiers at this column for all qubits
                    wire.set_daggers(&gate.modifiers)?;

                    // set parameters at this column for all qubits
                    for expression in &gate.parameters {
                        wire.set_param(expression, self.settings.texify_numerical_constants);
                    }
                }
            }
        }

        // get display of gate name from composite gate or original gate
        let canonical_gate = CompositeGate::from_str(&gate.name)
            .map(|g| g.to_string())
            .unwrap_or(gate.name.clone());

        // get the names of the qubits in the circuit before circuit is borrowed as mutable
        let circuit_qubits: Vec<u64> = self.circuit.keys().cloned().collect();

        // set gate for each qubit in the instruction
        for qubit in &gate.qubits {
            if let Qubit::Fixed(instruction_qubit) = qubit {
                if let Some(wire) = self.circuit.get_mut(instruction_qubit) {
                    // set the control and target qubits
                    if gate.qubits.len() > 1 || canonical_gate == "PHASE" {
                        // set the target qubit if the qubit is equal to the last qubit in gate
                        if qubit == gate.qubits.last().unwrap() {
                            wire.set_targ();
                        // otherwise, set the control qubit
                        } else {
                            wire.set_ctrl(qubit, gate.qubits.last().unwrap(), &circuit_qubits);
                        }
                    } else if wire.parameters.get(&wire.column).is_some() {
                        // parameterized single qubit gates are unsupported
                        return Err(LatexGenError::UnsupportedGate {
                            gate: gate.name.clone(),
                        });
                    }

                    // set modifiers at this column for all qubits
                    wire.gates.insert(wire.column, canonical_gate.clone());
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
        for (qubit, wire) in &self.circuit {
            // are labels on in settings?
            if self.settings.label_qubit_lines {
                // write the label to the left side of wire
                write!(f, "{}", RenderCommand::Lstick(*qubit))?;
            } else {
                // write an empty column buffer as the first column
                write!(f, "{}", RenderCommand::Qw)?;
            }

            // write the LaTeX string for the wire
            write!(f, "{wire}")?;

            // chain an empty column to the end of the line
            write!(f, " & ")?;
            write!(f, "{}", &RenderCommand::Qw)?;

            // if this is the last iteration, omit a new row from the end of the line
            if *qubit != *self.circuit.keys().last().unwrap() {
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
