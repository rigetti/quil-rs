use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
};

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
    wires: BTreeMap<u64, usize>,
    columns: Vec<DiagramColumn>,
}

struct DiagramColumn {
    cells: Vec<QuantikzCell>,
}

impl Diagram {
    pub(crate) fn new(
        qubits: BTreeSet<u64>,
        settings: RenderSettings,
    ) -> Result<Self, LatexGenError> {
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
    pub(crate) fn build_wire_map(&mut self, qubits: BTreeSet<u64>) -> Result<(), LatexGenError> {
        if qubits.is_empty() {
            return Err(LatexGenError::RequiresQubits);
        }

        if self.settings.impute_missing_qubits {
            let first = *qubits.first().expect("qubits is not empty");
            let last = *qubits.last().expect("qubits is not empty");
            for (i, qubit) in (first..last + 1).enumerate() {
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
        let mut cells: Vec<QuantikzCell> = vec![QuantikzCell::Empty; self.wires.len()];

        let quantikz_gate = cell::QuantikzGate::try_from(gate.clone())?;

        if gate.qubits.len() - quantikz_gate.ctrl_count > 1 {
            return Err(LatexGenError::UnsupportedGate {
                gate: gate.name.to_string(),
            });
        }

        // parameterized non-PHASE gates are unsupported
        if quantikz_gate.parameter.is_some() && !gate.name.contains("PHASE") {
            return Err(LatexGenError::UnsupportedGate {
                gate: gate.name.clone(),
            });
        }

        let mut qubit_iterator = gate
            .qubits
            .iter()
            .cloned()
            .map(|q| {
                q
                    .into_fixed()
                    .map_err(|_| LatexGenError::UnsupportedQubit)
            })
            .rev();

        let target_qubit = qubit_iterator
            .next()
            .ok_or(LatexGenError::RequiresQubits)??;

        let target_index = *self
            .wires
            .get(&target_qubit)
            .ok_or(LatexGenError::QubitNotFound(target_qubit))?;

        cells[target_index] = QuantikzCell::Gate(quantikz_gate);

        for qubit in qubit_iterator {
            let column_index = self
                .wires
                .get(&qubit?)
                .copied()
                .ok_or(LatexGenError::QubitNotFound(target_qubit))?;
            cells[column_index] = QuantikzCell::Control(target_index as i64 - column_index as i64)
        }

        self.columns.push(DiagramColumn { cells });

        Ok(())
    }
}

impl fmt::Display for Diagram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let last = self.wires.keys().last().copied().unwrap_or(0);
        let cell_settings = CellSettings::from(self.settings);

        for (qubit, cell_index) in &self.wires {
            if self.settings.label_qubit_lines {
                write!(f, "{}", RenderCommand::LeftWireLabel(*qubit))?;
            } else {
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

            // omit LaTeX line break (`\\`) if this is the last qubit
            if *qubit != *last {
                write!(f, " {}", &RenderCommand::Nr)?;
            }

            writeln!(f)?;
        }

        Ok(())
    }
}
