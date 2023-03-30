use std::{collections::HashMap, str::FromStr};

use crate::{
    expression::Expression,
    instruction::{GateModifier, Qubit},
    program::latex::diagram::Symbol,
};

use super::{Parameter, RenderCommand};

/// A Wire represents a single qubit. A wire only needs to keep track of all
/// the elements it contains mapped to some arbitrary column. Diagram keeps
/// track of where the Wire belongs in the larger circuit, its row, and knows
/// how each Wire relates to each other at that column. When Diagram parses the
/// wires as a collection, if the Wire relates to another at some column, then
/// its field will be updated at that column based on the knowledge Diagram has
/// about this connection. This updated value also looks arbitrary to Wire, it
/// does not explicitly define which qubit it relates to, but a digit that
/// describes how far away it is from the related qubit based on Quantikz.
#[derive(Clone, Debug, Default)]
pub(crate) struct Wire {
    /// the Gates on the wire callable by the column
    pub(crate) gates: HashMap<u32, String>,
    /// at this column the wire is a control
    pub(crate) ctrl: HashMap<u32, i64>,
    /// at this column is the wire a target?
    pub(crate) targ: HashMap<u32, bool>,
    /// the Parameters on the wire callable by the column
    pub(crate) parameters: HashMap<u32, Vec<Parameter>>,
    /// the Dagger modifiers on the wire callable by the column
    pub(crate) daggers: HashMap<u32, Vec<GateModifier>>,
    /// empty column
    pub(crate) empty: HashMap<u32, RenderCommand>,
}

impl Wire {
    /// Retrieves a gate's parameters from Expression and matches them with its
    /// symbolic definition which is then stored into wire at the specific
    /// column.
    ///
    /// # Arguments
    /// `&mut self` - exposes the Wire's parameters at this column
    /// `expression` - expression from Program to get name of Parameter
    /// `column` - the column taking the parameters
    /// `texify` - is texify_numerical_constants setting on?
    pub(crate) fn set_param(&mut self, expression: &Expression, column: u32, texify: bool) {
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

        self.parameters.insert(column, param);
    }

    /// Set target qubit at this column.
    ///
    /// # Arguments
    /// `&mut self` - exposes the Wire's targ at this column
    /// `column` - the column taking the target
    pub(crate) fn set_targ(&mut self, column: &u32) {
        self.targ.insert(*column, true);
    }

    /// Set control qubit at this column at some distance from the target. The
    /// distance is determined by the relative position of the control and
    /// target qubits in the circuit.
    ///
    /// # Arguments
    /// `&mut self` - exposes the Wire's ctrl at this column
    /// `column` - the column taking the control
    /// `ctrl` - the control qubit
    /// `targ` - the target qubit
    /// `circuit_qubits` - the qubits in the circuit
    pub(crate) fn set_ctrl(
        &mut self,
        column: &u32,
        ctrl: &Qubit,
        targ: &Qubit,
        circuit_qubits: &[u64],
    ) {
        if let Qubit::Fixed(ctrl) = ctrl {
            if let Qubit::Fixed(targ) = targ {
                // get the index of the control and target qubits
                let ctrl_index = circuit_qubits.iter().position(|&x| x == *ctrl);
                let targ_index = circuit_qubits.iter().position(|&x| x == *targ);

                // if the control and target qubits are found
                if let Some(ctrl_index) = ctrl_index {
                    if let Some(targ_index) = targ_index {
                        self.ctrl
                            .insert(*column, targ_index as i64 - ctrl_index as i64);
                    }
                }
            }
        }
    }
}
