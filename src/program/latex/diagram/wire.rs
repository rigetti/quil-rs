use std::{collections::HashMap, fmt, str::FromStr};

use lazy_regex::{Lazy, Regex};

use crate::{
    expression::Expression,
    instruction::{Gate, GateModifier, Qubit},
};

use super::super::{LatexGenError, Parameter, RenderCommand, Symbol};

/// A Wire represents a single qubit. This is a row vector, or [1 x n] matrix,
/// where n, is the total number of Quil instructions (or columns). Each column
/// on the wire maps to some item that can be serialized into LaTeX using the
/// ``Quantikz`` RenderCommands. A wire is part of the Circuit which is an [m x
/// n] matrix where m, is the total number of wires, or length, of the Circuit.
#[derive(Clone, Debug, Default)]
pub(crate) struct Wire {
    /// the Gates on the wire callable by the column
    pub(crate) gates: Vec<T>,
    /// the Parameters on the wire at this column
    pub(crate) parameters: HashMap<usize, Parameter>,
}

/// A T represents a Gate that can be pushed onto a Wire. The ``Gate`` struct
/// in the ``instruction`` module is used to form this T variant which is
/// pushed onto the Wire and then serialized into LaTeX using the associated
/// ``Quantikz`` RenderCommand.
#[derive(Clone, Debug, Default)]
pub(crate) enum T {
    #[default]
    Empty,
    StdGate {
        name: String,
        dagger_count: usize,
        ctrl_count: usize,
    },
    Ctrl(i64),
}

impl TryFrom<Gate> for T {
    type Error = LatexGenError;

    /// Returns a StdGate that can be pushed onto the wire. This gate is first
    /// decomposed into canonical form and may contain modifiers. The modifiers
    /// are counted and applied to the gate. If the modifier is FORKED, then an
    /// error returned. This is because FORKED is not supported by the
    /// ``Quantikz`` library.
    ///
    /// # Arguments
    /// `gate` - the Gate of the Instruction from `to_latex`.
    fn try_from(gate: Gate) -> Result<Self, Self::Error> {
        // if the gate is a composite gate, then apply the composite gate
        static ABBREVIATED_CONTROLLED_GATE: Lazy<Regex> =
            Lazy::new(|| Regex::new("(?P<count>C+)(?P<base>PHASE|X|Y|Z|NOT)").unwrap());

        let mut canonical_gate = gate.name.to_string();
        let mut ctrl_count = 0;
        if let Some(captures) = ABBREVIATED_CONTROLLED_GATE.captures(&gate.name) {
            let base = captures.name("base").unwrap().as_str();
            ctrl_count = captures.name("count").unwrap().as_str().len();

            // convert abbreviated controlled gates to canonical form. If the base is NOT, then this is an X gate.
            match base {
                "NOT" => canonical_gate = "X".to_string(),
                _ => canonical_gate = base.to_string(),
            }
        }

        // count the supported modifiers
        let mut dagger_count = 0;
        for modifier in gate.modifiers {
            match modifier {
                // return error for unsupported modifier FORKED
                GateModifier::Forked => {
                    return Err(LatexGenError::UnsupportedModifierForked);
                }
                GateModifier::Dagger => {
                    dagger_count += 1;
                }
                GateModifier::Controlled => {
                    ctrl_count += 1;
                }
            }
        }

        // return the StdGate
        Ok(T::StdGate {
            name: canonical_gate,
            dagger_count,
            ctrl_count,
        })
    }
}

impl Wire {
    /// Set empty at the current column.
    pub(crate) fn set_empty(&mut self) {
        self.gates.push(T::Empty);
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

            Parameter::Symbol(Symbol::from_str(&text).unwrap_or(Symbol::Text(text)))
        } else {
            // set the symbol as text
            Parameter::Symbol(Symbol::Text(text))
        };

        self.parameters.insert(self.gates.len(), param);
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
                        self.gates
                            .push(T::Ctrl(targ_index as i64 - ctrl_index as i64));
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
        for column in 0..self.gates.len() {
            write!(f, " & ")?;

            // appended to the end of the gate name
            let mut superscript = String::new();

            match &self.gates[column] {
                T::Empty => {
                    // chain an empty column qw to the end of the line
                    // write!(f, " & ")?;
                    write!(f, "{}", &RenderCommand::Qw)?;
                }
                T::StdGate {
                    name,
                    dagger_count,
                    ctrl_count,
                } => {
                    (0..*dagger_count).for_each(|_| {
                        superscript
                            .push_str(&RenderCommand::Super(String::from("dagger")).to_string());
                    });

                    if name == "PHASE" {
                        if let Some(p) = self.parameters.get(&column) {
                            write!(
                                f,
                                "{}",
                                &RenderCommand::Phase(p.clone(), superscript.clone())
                            )?;
                        }
                    } else if name == "X" {
                        // if it is associated with dagger superscripts write it as an X gate with superscripts
                        if dagger_count > &0 || ctrl_count == &0 {
                            write!(f, "{}", &RenderCommand::Gate(name.to_string(), superscript))?;
                        } else {
                            write!(f, "{}", &RenderCommand::Targ)?;
                        }
                    } else {
                        write!(f, "{}", &RenderCommand::Gate(name.to_string(), superscript))?;
                    }
                }
                T::Ctrl(targ) => {
                    write!(f, "{}", &(RenderCommand::Ctrl(*targ)))?;
                }
            }
        }

        Ok(())
    }
}
