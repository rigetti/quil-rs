use std::{collections::BTreeMap, fmt};

use crate::instruction::Gate;

use self::cell::{CellSettings, QuantikzCell};
use super::{LatexGenError, RenderCommand, RenderSettings};

pub(crate) mod cell;

/// A Diagram represents a collection of wires in a Circuit. The size of the
/// Diagram can be measured by multiplying the number of Instructions in a
/// Program with the length of the Circuit. This is an [m x n] matrix where n,
/// is the number of Quil instructions (or columns), and m, is the number of
/// wires (or rows). Each individual element of the matrix represents an item
/// that is serializable into LaTeX using the ``Quantikz`` RenderCommands.
pub(crate) struct Diagram {
    settings: RenderSettings,
    wires: BTreeMap<u64, usize>, // qubit index -> column index
    columns: Vec<DiagramColumn>,
}

struct DiagramColumn {
    cells: Vec<QuantikzCell>, // number of wires
}

impl Diagram {
    pub(crate) fn new(qubits: Vec<u64>, settings: RenderSettings) -> Result<Self, LatexGenError> {
        // Initialize a new `Diagram` with the given qubits
        // If settings.impute_missing qubits, then pad the gaps.

        let mut diagram = Self {
            settings,
            wires: BTreeMap::new(),
            columns: Vec::new(),
        };

        diagram.build_wire_map(qubits)?;

        Ok(diagram)
    }

    /// Adds missing qubits between the first qubit and last qubit in a
    /// diagram's circuit. If a missing qubit is found, a new wire is created
    /// and pushed to the diagram's circuit.
    ///
    ///  # Arguments
    /// `last_column` - total number of instructions from Program
    /// `circuit` - the circuit of the diagram
    ///
    /// # Examples
    /// ```
    /// use quil_rs::{Program, program::latex::RenderSettings};
    /// use std::str::FromStr;
    /// let program = Program::from_str("H 0\nCNOT 0 1").expect("");
    /// let settings = RenderSettings {
    ///     impute_missing_qubits: true,
    ///     ..Default::default()
    /// };
    /// program.to_latex(settings).expect("");
    /// ```
    pub(crate) fn build_wire_map(&mut self, mut qubits: Vec<u64>) -> Result<(), LatexGenError> {
        if qubits.is_empty() {
            return Err(LatexGenError::RequiresQubits);
        }

        // sort so that mapping contains the correct row number for each qubit
        qubits.sort();
        if self.settings.impute_missing_qubits {
            for (i, qubit) in
                (qubits[0]..*qubits.last().expect("qubits is not empty") + 1).enumerate()
            {
                self.wires.insert(qubit, i);
            }
        } else {
            for (i, qubit) in qubits.iter().enumerate() {
                self.wires.insert(*qubit, i);
            }
        }

        Ok(())
    }

    /// Applies a gate from an instruction to the associated wires in the
    /// circuit of this diagram.
    ///
    /// # Arguments
    /// `gate` - the Gate of the Instruction from `to_latex`.
    pub(crate) fn add_gate(&mut self, gate: &Gate) -> Result<(), LatexGenError> {
        // Initialize a column full of empty cells to be filled in.
        let mut cells: Vec<QuantikzCell> = vec![QuantikzCell::Empty; self.wires.len()];

        let quantikz_gate = cell::QuantikzGate::try_from(gate.clone())?;

        if gate.qubits.len() - quantikz_gate.ctrl_count > 1 {
            return Err(LatexGenError::UnsupportedGate {
                gate: gate.name.to_string(),
            });
        }

        // parameterized non-PHASE gates are unsupported
        if quantikz_gate.parameter.is_some() && !gate.name.contains("PHASE") {
            // parameterized single qubit gates are unsupported
            return Err(LatexGenError::UnsupportedGate {
                gate: gate.name.clone(),
            });
        }

        // TODO: Error handling instead of unwrap
        let mut qubit_iterator = gate
            .qubits
            .iter()
            .map(|q| q.clone().into_fixed().unwrap())
            .rev();

        // start iterator at target qubit
        let target_qubit = qubit_iterator.next().unwrap();

        // TODO: Return error instead of unwrap
        let target_index = *self.wires.get(&target_qubit).unwrap();

        cells[target_index] = QuantikzCell::Gate(quantikz_gate);

        // This is oversimplified for the example. There will be a little
        // bit of pre-processing necessary to get your "canonical gate",
        // and it probably makes sense to go in reverse order so you can start
        // with the target qubit.
        for qubit in qubit_iterator {
            // TODO: Error handling instead of unwrap
            let column_index = *self.wires.get(&qubit).unwrap();
            cells[column_index] = QuantikzCell::Control(target_index as i64 - column_index as i64)
        }

        self.columns.push(DiagramColumn { cells });

        Ok(())
    }
}

impl fmt::Display for Diagram {
    /// Returns a result containing the body of the Document.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // write the LaTeX string for each wire in the circuit
        let last = self.wires.keys().last().unwrap_or(&0);
        let cell_settings = CellSettings {
            texify: self.settings.texify_numerical_constants,
        };

        for (qubit, cell_index) in &self.wires {
            // are labels on in settings?
            if self.settings.label_qubit_lines {
                // write the label to the left side of wire
                write!(f, "{}", RenderCommand::LeftWireLabel(*qubit))?;
            } else {
                // write an empty column buffer as the first column
                write!(f, "{}", RenderCommand::Qw)?;
            }

            for column in &self.columns {
                let command = &column.cells[*cell_index];
                write!(
                    f,
                    "{}{}",
                    &RenderCommand::Separate,
                    command.to_latex(&cell_settings).map_err(|_| fmt::Error)?
                )?;
            }

            write!(f, "{}{}", &RenderCommand::Separate, &RenderCommand::Qw)?;

            // omit a new row if this is the last qubit wire
            if *qubit != *last {
                // otherwise, write a new row to the end of the line
                write!(f, " {}", &RenderCommand::Nr)?;
            }

            // write a newline between each row and or the body and the document footer
            writeln!(f)?;
        }

        Ok(())
    }
}
