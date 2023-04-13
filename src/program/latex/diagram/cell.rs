use std::fmt;

use lazy_regex::{Lazy, Regex};

use crate::instruction::{Gate, GateModifier};

use super::super::{LatexGenError, RenderCommand, Symbol};

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub(crate) enum QuantikzCell {
    Gate(QuantikzGate),
    Control(i64),
    #[default]
    Empty,
}

pub(crate) struct CellSettings {
    pub(crate) texify: bool,
}

impl QuantikzCell {
    pub(crate) fn to_latex(&self, settings: &CellSettings) -> Result<String, LatexGenError> {
        // match the cell type
        match self {
            QuantikzCell::Empty => {
                // write an empty column
                Ok(format!("{}", &RenderCommand::Qw))
            }
            QuantikzCell::Control(target) => {
                // write a control gate pointing to its target
                Ok(format!("{}", &(RenderCommand::Ctrl(*target))))
            }
            QuantikzCell::Gate(QuantikzGate {
                name,
                parameter,
                dagger_count,
                ctrl_count,
            }) => {
                // build the dagger superscript
                // appended to the end of the gate name
                let mut superscript = String::new();
                (0..*dagger_count).for_each(|_| {
                    superscript.push_str(&RenderCommand::Super(String::from("dagger")).to_string());
                });

                let symbol = parameter
                    .clone()
                    .map(|p| if settings.texify { p.texify() } else { p });

                // write a phase gate with its rotation parameter
                if name == "PHASE" {
                    Ok(format!(
                        "{}",
                        &RenderCommand::Phase(symbol.unwrap(), superscript)
                    ))
                // the conditional defines a target gate
                } else if name == "X" && ctrl_count > &0 {
                    // if there a daggers then write it as an X gate
                    if dagger_count > &0 {
                        Ok(format!(
                            "{}",
                            &RenderCommand::Gate(name.to_string(), superscript)
                        ))
                    // otherwise write it as a closed dot
                    } else {
                        Ok(format!("{}", &RenderCommand::Targ))
                    }
                // write a gate with its name and superscript
                } else {
                    Ok(format!(
                        "{}",
                        &RenderCommand::Gate(name.to_string(), superscript)
                    ))
                }
            }
        }
    }
}

impl fmt::Display for QuantikzCell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // match the cell type
        match self {
            QuantikzCell::Empty => {
                // write an empty column
                write!(f, "{}", &RenderCommand::Qw)?;
            }
            QuantikzCell::Control(target) => {
                // write a control gate pointing to its target
                write!(f, "{}", &(RenderCommand::Ctrl(*target)))?;
            }
            QuantikzCell::Gate(QuantikzGate {
                name,
                parameter,
                dagger_count,
                ctrl_count,
            }) => {
                // build the dagger superscript
                // appended to the end of the gate name
                let mut superscript = String::new();
                (0..*dagger_count).for_each(|_| {
                    superscript.push_str(&RenderCommand::Super(String::from("dagger")).to_string());
                });

                // write a phase gate with its rotation parameter
                if name == "PHASE" {
                    write!(
                        f,
                        "{}",
                        &RenderCommand::Phase(parameter.as_ref().unwrap().clone(), superscript)
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
        }

        Ok(())
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub(crate) struct QuantikzGate {
    pub(crate) name: String,
    pub(crate) parameter: Option<Symbol>,
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

        if gate.parameters.len() > 1 {
            // TODO: Create separate error for unsupported parameter length?
            return Err(LatexGenError::UnsupportedGate { gate: gate.name });
        }

        let mut parameter = None;
        if !gate.parameters.is_empty() {
            parameter = Some(Symbol::from_expression(gate.parameters[0].clone()))
        }

        // return the QuantikzGate form of Gate
        Ok(QuantikzGate {
            name: canonical_gate,
            parameter,
            dagger_count,
            ctrl_count,
        })
    }
}
