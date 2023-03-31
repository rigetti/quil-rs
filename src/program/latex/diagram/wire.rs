use std::{collections::HashMap, str::FromStr};

use crate::{
    expression::Expression,
    instruction::{GateModifier, Qubit},
};

use super::super::{LatexGenError, Parameter, Symbol};

/// A Wire represents a single qubit. This is a row vector, or [1 x n] matrix,
/// where n, is the total number of Quil instructions (or columns) plus one
/// empty column. Each column on the wire maps to some item that can be
/// rendered onto the LaTeX document using the ``Quantikz`` RenderCommands. A
/// wire is part of the Circuit which is an [m x n] matrix where m, is the
/// number of wires.
#[derive(Clone, Debug, Default)]
pub(crate) struct Wire {
    /// the column of the wire
    pub(crate) column: u32,
    /// the Gates on the wire callable by the column
    pub(crate) gates: HashMap<u32, String>,
    /// at this column the wire is a control some distance from the target
    pub(crate) ctrl: HashMap<u32, i64>,
    /// at this column is the wire a target?
    pub(crate) targ: HashMap<u32, bool>,
    /// the Parameters on the wire at this column
    pub(crate) parameters: HashMap<u32, Vec<Parameter>>,
    /// the Dagger modifiers on the wire at this column
    pub(crate) daggers: HashMap<u32, Vec<GateModifier>>,
    /// empty column
    pub(crate) empty: HashMap<u32, bool>,
}

impl Wire {
    /// Set empty at the current column.
    pub(crate) fn set_empty(&mut self) {
        self.empty.insert(self.column, true);
    }

    /// Iterates over the modifiers from the gate instruction and pushes DAGGER
    /// modifiers to daggers vector at the current column. Returns an Err for
    /// FORKED modifiers, and does nothing for modifiers.
    ///
    /// # Arguments
    /// `modifiers` - the modifiers from the Gate
    pub(crate) fn set_daggers(
        &mut self,
        modifiers: &Vec<GateModifier>,
    ) -> Result<(), LatexGenError> {
        // set modifers
        for modifier in modifiers {
            match modifier {
                // return error for unsupported modifier FORKED
                GateModifier::Forked => {
                    return Err(LatexGenError::UnsupportedModifierForked);
                }
                // insert DAGGER
                GateModifier::Dagger => {
                    self.daggers
                        .entry(self.column)
                        .and_modify(|m| m.push(modifier.clone()))
                        .or_insert_with(|| vec![modifier.clone()]);
                }
                // do nothing for CONTROLLED
                _ => (),
            }
        }

        Ok(())
    }

    /// Retrieves a gate's parameters from Expression and matches them with its
    /// symbolic definition which is then stored into wire at the specific
    /// column.
    ///
    /// # Arguments
    /// `expression` - expression from Program to get name of Parameter
    /// `texify` - is texify_numerical_constants setting on?
    pub(crate) fn set_param(&mut self, expression: &Expression, texify: bool) {
        // get the name of the supported expression
        let text = match expression {
            Expression::Address(mr) => mr.name.to_string(),
            Expression::Number(c) => c.re.to_string(),
            expression => expression.to_string(),
        };

        // if texify_numerical_constants
        let param = if texify {
            // set the texified symbol
            let symbol = Parameter::Symbol(Symbol::from_str(&text).unwrap_or(Symbol::Text(text)));

            vec![symbol]
        } else {
            // set the symbol as text
            vec![Parameter::Symbol(Symbol::Text(text))]
        };

        self.parameters.insert(self.column, param);
    }

    /// Set target qubit at the current column.
    pub(crate) fn set_targ(&mut self) {
        self.targ.insert(self.column, true);
    }

    /// Set control qubit at the current column some distance from the target.
    /// The distance is determined by the relative position of the control and
    /// target qubits in the circuit.
    ///
    /// # Arguments
    /// `ctrl` - the control qubit
    /// `targ` - the target qubit
    /// `circuit_qubits` - the qubits in the circuit
    pub(crate) fn set_ctrl(&mut self, ctrl: &Qubit, targ: &Qubit, circuit_qubits: &[u64]) {
        if let Qubit::Fixed(ctrl) = ctrl {
            if let Qubit::Fixed(targ) = targ {
                // get the index of the control and target qubits
                let ctrl_index = circuit_qubits.iter().position(|&x| x == *ctrl);
                let targ_index = circuit_qubits.iter().position(|&x| x == *targ);

                // if the control and target qubits are found
                if let Some(ctrl_index) = ctrl_index {
                    if let Some(targ_index) = targ_index {
                        self.ctrl
                            .insert(self.column, targ_index as i64 - ctrl_index as i64);
                    }
                }
            }
        }
    }
}
