use lazy_regex::{Lazy, Regex};

use crate::instruction::{Gate, GateModifier};

use super::super::{LatexGenError, RenderCommand, RenderSettings, Symbol};

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub(crate) enum QuantikzCell {
    Gate(QuantikzGate),
    Control(i64),
    #[default]
    Empty,
}

pub(crate) struct CellSettings {
    pub(crate) texify_numerical_constants: bool,
}

impl From<RenderSettings> for CellSettings {
    fn from(render_settings: RenderSettings) -> Self {
        Self {
            texify_numerical_constants: render_settings.texify_numerical_constants,
        }
    }
}

impl QuantikzCell {
    pub(crate) fn to_latex(&self, settings: &CellSettings) -> Result<String, LatexGenError> {
        match self {
            QuantikzCell::Empty => Ok(RenderCommand::Qw.to_string()),
            QuantikzCell::Control(target) => Ok(RenderCommand::Ctrl(*target).to_string()),
            QuantikzCell::Gate(QuantikzGate {
                name,
                parameter,
                dagger_count,
                ctrl_count,
            }) => {
                let mut superscript = String::new();
                (0..*dagger_count).for_each(|_| {
                    superscript.push_str(&RenderCommand::Super(String::from("dagger")).to_string());
                });

                let symbol = parameter.clone().map(|p| {
                    if settings.texify_numerical_constants {
                        p.texify()
                    } else {
                        p
                    }
                });
                if name == "PHASE" {
                    Ok(format!(
                        "{}",
                        &RenderCommand::Phase(
                            symbol.ok_or(LatexGenError::MissingParameter)?,
                            superscript
                        )
                    ))
                // the conditional defines a target gate
                } else if name == "X" && *ctrl_count > 0 {
                    if *dagger_count > 0 {
                        Ok(format!(
                            "{}",
                            &RenderCommand::Gate(name.to_string(), superscript)
                        ))
                    } else {
                        Ok(format!("{}", &RenderCommand::Targ))
                    }
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

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub(crate) struct QuantikzGate {
    pub(crate) name: String,
    pub(crate) parameter: Option<Symbol>,
    pub(crate) dagger_count: usize,
    pub(crate) ctrl_count: usize,
}

impl TryFrom<Gate> for QuantikzGate {
    type Error = LatexGenError;

    fn try_from(gate: Gate) -> Result<Self, Self::Error> {
        // regex to match canonical controlled gates
        static ABBREVIATED_CONTROLLED_GATE: Lazy<Regex> = Lazy::new(|| {
            Regex::new("(?P<count>C+)(?P<base>PHASE|X|Y|Z|NOT)").expect("regex should be valid")
        });

        let mut canonical_gate = gate.name.to_string();
        let mut ctrl_count = 0;
        if let Some(captures) = ABBREVIATED_CONTROLLED_GATE.captures(&gate.name) {
            let base = captures
                .name("base")
                .expect("capture group should have value")
                .as_str();

            ctrl_count = captures
                .name("count")
                .expect("capture group should have value")
                .as_str()
                .len();

            match base {
                // NOT is an alias for X
                "NOT" => canonical_gate = "X".to_string(),
                _ => canonical_gate = base.to_string(),
            }
        }

        let mut dagger_count = 0;
        for modifier in gate.modifiers {
            match modifier {
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
            return Err(LatexGenError::UnsupportedParameterLength {
                gate: gate.name,
                length: gate.parameters.len(),
            });
        }

        let mut parameter = None;
        if !gate.parameters.is_empty() {
            parameter = Some(Symbol::from_expression(gate.parameters[0].clone()))
        }

        Ok(QuantikzGate {
            name: canonical_gate,
            parameter,
            dagger_count,
            ctrl_count,
        })
    }
}
