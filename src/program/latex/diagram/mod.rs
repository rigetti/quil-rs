use std::{
    collections::{BTreeMap, HashSet},
    fmt,
};

use crate::instruction::{Gate, Qubit};

use self::wire::{QuantikzColumn, Wire};
use super::{LatexGenError, Parameter, RenderCommand, RenderSettings};

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
    /// `qubits` - the qubits from the Quil program.
    /// `gate` - the Gate of the Instruction from `to_latex`.
    pub(crate) fn apply_empty(&mut self, qubits: &HashSet<Qubit>, gate: &Gate) {
        qubits
            .difference(&gate.qubits.iter().cloned().collect())
            .filter_map(|q| q.as_fixed())
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
        // circuit needed immutably but also used mutably
        let circuit = self.circuit.clone();

        // Quantum Gates operate on at least one qubit
        let last_qubit = gate.qubits[gate.qubits.len() - 1]
            .clone()
            .into_fixed()
            .map_err(|_| LatexGenError::UnsupportedQubit)?;

        // set gate for each qubit in the instruction
        for qubit in gate.qubits.iter() {
            let mut column = QuantikzColumn::default();

            let instruction_qubit = qubit
                .clone()
                .into_fixed()
                .map_err(|_| LatexGenError::UnsupportedQubit)?;

            let wire = self
                .circuit
                .get_mut(&instruction_qubit)
                .ok_or(LatexGenError::QubitNotFound(instruction_qubit))?;

            // set the parameters for each qubit in the instruction
            for expression in &gate.parameters {
                column.set_param(expression, self.settings.texify_numerical_constants);
            }

            let quantikz_gate = wire::QuantikzGate::try_from(gate.clone())?;

            if gate.qubits.len() - quantikz_gate.ctrl_count > 1 {
                return Err(LatexGenError::UnsupportedGate {
                    gate: gate.name.to_string(),
                });
            }

            // if the instruction qubit a control qubit
            if quantikz_gate.ctrl_count > 0 && instruction_qubit != last_qubit {
                // get the distance between the instruction qubit and the target qubit
                let distance = if instruction_qubit < last_qubit {
                    circuit.range(instruction_qubit..last_qubit).count() as i64
                } else {
                    -(circuit.range(last_qubit..instruction_qubit).count() as i64)
                };
                column.cell = wire::QuantikzCellType::Ctrl(distance);

            // otherwise, the instruction qubit is the target qubit or a single qubit gate
            } else {
                column.cell = wire::QuantikzCellType::Gate(quantikz_gate);
            }

            // parameterized non-PHASE gates are unsupported
            if column.parameter != Parameter::None && !gate.name.contains("PHASE") {
                // parameterized single qubit gates are unsupported
                return Err(LatexGenError::UnsupportedGate {
                    gate: gate.name.clone(),
                });
            }

            // push the gate to the wire
            wire.columns.push(column);
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
            write!(f, "{}{}", &RenderCommand::Separate, &RenderCommand::Qw)?;

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
