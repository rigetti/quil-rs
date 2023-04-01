use std::{collections::HashMap, fmt, str::FromStr};

use crate::{
    expression::Expression,
    instruction::{GateModifier, Qubit},
};

use super::super::{LatexGenError, Parameter, RenderCommand, Symbol};

/// A Wire represents a single qubit. This is a row vector, or [1 x n] matrix,
/// where n, is the total number of Quil instructions (or columns). Each column
/// on the wire maps to some item that can be serialized into LaTeX using the
/// ``Quantikz`` RenderCommands. A wire is part of the Circuit which is an [m x
/// n] matrix where m, is the total number of wires, or length, of the Circuit.
#[derive(Clone, Debug, Default)]
pub(crate) struct Wire {
    /// the current column of the wire
    pub(crate) column: usize,
    /// the Gates on the wire callable by the column
    pub(crate) gates: HashMap<usize, String>,
    /// at this column the wire is a control some distance from the target
    pub(crate) ctrl: HashMap<usize, i64>,
    /// at this column is the wire a target?
    pub(crate) targ: HashMap<usize, bool>,
    /// the Parameters on the wire at this column
    pub(crate) parameters: HashMap<usize, Vec<Parameter>>,
    /// the Dagger modifiers on the wire at this column
    pub(crate) daggers: HashMap<usize, Vec<GateModifier>>,
    /// empty column
    pub(crate) empty: HashMap<usize, bool>,
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

impl fmt::Display for Wire {
    /// Returns a result containing the LaTeX string for the wire
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // write the LaTeX string for each item at each column with a length the total number of instructions in the Wire plus one empty column
        for column in 0..=self.column + 1 {
            // write the string for some item at this column
            if let Some(gate) = self.gates.get(&column) {
                write!(f, " & ")?;

                // appended to the end of the gate name
                let mut superscript = String::new();

                // iterate over daggers and build superscript
                if let Some(daggers) = self.daggers.get(&column) {
                    daggers.iter().for_each(|_| {
                        superscript
                            .push_str(&RenderCommand::Super(String::from("dagger")).to_string());
                    });
                }

                // if the wire has a control at this column write the control string and continue
                if self.ctrl.get(&column).is_some() {
                    if let Some(targ) = self.ctrl.get(&column) {
                        write!(f, "{}", &(RenderCommand::Ctrl(*targ)))?;
                    }
                    continue;

                // if the wire has a target at this column determine if it is associated with an X gate or a PHASE gate
                } else if self.targ.get(&column).is_some() {
                    // if the target is associated with an X gate determine if it is associated with dagger superscripts
                    if gate == "X" {
                        // if it is associated with dagger superscripts write it as an X gate with superscripts
                        if !superscript.is_empty() {
                            write!(f, "{}", &RenderCommand::Gate(gate.to_string(), superscript))?;

                        // otherwise, write it as an open dot
                        } else {
                            write!(f, "{}", &RenderCommand::Targ)?;
                        }
                        continue;

                    // otherwise, if the target is associated with a PHASE gate write it as a PHASE gate with parameters
                    } else if gate == "PHASE" {
                        if let Some(parameters) = self.parameters.get(&column) {
                            parameters.iter().for_each(|p| {
                                write!(
                                    f,
                                    "{}",
                                    &RenderCommand::Phase(p.clone(), superscript.clone())
                                )
                                .ok();
                            });
                        }
                        continue;
                    }
                }

                // write all other items as a generic gate with superscripts if applicable
                write!(f, "{}", &RenderCommand::Gate(gate.to_string(), superscript))?;

            // otherwise, write the string as an empty column
            } else if self.empty.get(&column).is_some() {
                // chain an empty column qw to the end of the line
                write!(f, " & ")?;
                write!(f, "{}", &RenderCommand::Qw)?;
            }
        }

        Ok(())
    }
}
