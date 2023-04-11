use std::{fmt, str::FromStr};

use lazy_regex::{Lazy, Regex};

use crate::{
    expression::Expression,
    instruction::{Gate, GateModifier},
};

use super::super::{LatexGenError, Parameter, RenderCommand, Symbol};

/// The QuantikzGate is a valid cell of a QuantikzCellType that is the form of
/// the ``Gate`` struct in the ``Instruction`` containing the components that
/// are used to render a gate on the wire in the circuit diagram.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub(crate) struct QuantikzGate {
    pub(crate) name: String,
    pub(crate) dagger_count: usize,
    pub(crate) ctrl_count: usize,
}

/// Convert a ``Gate`` struct into a ``QuantikzGate`` struct.
impl TryFrom<Gate> for QuantikzGate {
    type Error = LatexGenError;

    fn try_from(gate: Gate) -> Result<Self, Self::Error> {
        // regex to match canonical controlled gates
        static ABBREVIATED_CONTROLLED_GATE: Lazy<Regex> =
            Lazy::new(|| Regex::new("(?P<count>C+)(?P<base>PHASE|X|Y|Z|NOT)").unwrap());

        let mut canonical_gate = gate.name.to_string();
        let mut ctrl_count = 0;
        if let Some(captures) = ABBREVIATED_CONTROLLED_GATE.captures(&gate.name) {
            // get the base gate and the number of controls
            let base = captures.name("base").unwrap().as_str();
            ctrl_count = captures.name("count").unwrap().as_str().len();

            // set the canonical gate name
            match base {
                // NOT is an alias for X
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

        // return the QuantikzGate form of Gate
        Ok(QuantikzGate {
            name: canonical_gate,
            dagger_count,
            ctrl_count,
        })
    }
}

/// The type of cell that can be stored in a column on the wire.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub(crate) enum QuantikzCellType {
    #[default]
    Empty,
    Gate(QuantikzGate),
    Ctrl(i64),
}

/// A single column on the wire containing cells and associated parameters.
#[derive(Clone, Debug, Default)]
pub(crate) struct QuantikzColumn {
    /// a column on the wire containing renderable items
    pub(crate) cell: QuantikzCellType,
    /// the Parameter on the wire at some column
    pub(crate) parameter: Parameter,
}

impl QuantikzColumn {
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

        self.parameter = param;
    }
}

/// A Wire represents a single qubit. This is a row vector, or [1 x n] matrix,
/// where n, is the total number of Quil instructions (or columns). Each column
/// on the wire maps to some item that can be serialized into LaTeX using the
/// ``Quantikz`` RenderCommands. A wire is part of the Circuit which is an [m x
/// n] matrix where m, is the total number of wires, or length, of the Circuit.
#[derive(Clone, Debug, Default)]
pub(crate) struct Wire {
    /// the columns on the wire that can be serialized into LaTeX
    pub(crate) columns: Vec<QuantikzColumn>,
}

impl Wire {
    /// Set empty at the current column.
    pub(crate) fn set_empty(&mut self) {
        // get the current column and set it to empty
        self.columns.push(QuantikzColumn::default())
    }
}

impl fmt::Display for Wire {
    /// Returns a result containing the LaTeX string for the wire
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // write the LaTeX string for each cell in the column
        for column in 0..self.columns.len() {
            write!(f, "{}", &RenderCommand::Separate)?;

            // appended to the end of the gate name
            let mut superscript = String::new();

            // match the cell type
            match &self.columns[column].cell {
                QuantikzCellType::Empty => {
                    // write an empty column
                    write!(f, "{}", &RenderCommand::Qw)?;
                }
                QuantikzCellType::Gate(QuantikzGate {
                    name,
                    dagger_count,
                    ctrl_count,
                }) => {
                    // build the dagger superscript
                    (0..*dagger_count).for_each(|_| {
                        superscript
                            .push_str(&RenderCommand::Super(String::from("dagger")).to_string());
                    });

                    // write a phase gate with its rotation parameter
                    if name == "PHASE" {
                        write!(
                            f,
                            "{}",
                            &RenderCommand::Phase(
                                self.columns[column].parameter.clone(),
                                superscript.clone()
                            )
                        )?;
                    // the conditional defines a target gate
                    } else if name == "X" && ctrl_count > &0 {
                        // if there a daggers then write it as an X gate
                        if dagger_count > &0 {
                            write!(f, "{}", &RenderCommand::Gate(name.to_string(), superscript))?;
                        // otherwise write it as a closed dot
                        } else {
                            write!(f, "{}", &RenderCommand::Targ)?;
                        }
                    // write a gate with its name and superscript
                    } else {
                        write!(f, "{}", &RenderCommand::Gate(name.to_string(), superscript))?;
                    }
                }
                QuantikzCellType::Ctrl(targ) => {
                    // write a control gate pointing to its target
                    write!(f, "{}", &(RenderCommand::Ctrl(*targ)))?;
                }
            }
        }

        Ok(())
    }
}
