use std::{collections::HashMap, fmt};

use crate::{expression::Expression, instruction::write_comma_separated_list};

use super::write_parameter_string;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Waveform {
    pub matrix: Vec<Expression>,
    pub parameters: Vec<String>,
}

impl Waveform {
    pub fn new(matrix: Vec<Expression>, parameters: Vec<String>) -> Self {
        Self { matrix, parameters }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WaveformDefinition {
    pub name: String,
    pub definition: Waveform,
}

impl WaveformDefinition {
    pub fn new(name: String, definition: Waveform) -> Self {
        Self { name, definition }
    }
}

impl fmt::Display for WaveformDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "DEFWAVEFORM {}", self.name)?;
        write_parameter_string(f, &self.definition.parameters)?;
        write!(f, ":\n\t")?;
        write_comma_separated_list(f, &self.definition.matrix)
    }
}

#[cfg(test)]
mod test_waveform_definition {
    use super::{Waveform, WaveformDefinition};
    use crate::expression::Expression;
    use crate::real;

    use insta::assert_snapshot;
    use rstest::rstest;

    #[rstest]
    #[case(
        "Simple WaveformDefinition",
        WaveformDefinition {
            name: "WF".to_string(),
            definition: Waveform {
                matrix: vec![
                    Expression::Number(real!(0.0)),
                    Expression::Number(real!(0.0)),
                    Expression::Number(real!(0.00027685415721916584))
                ],
                parameters: vec!["theta".to_string()]
            }
        }
    )]
    fn test_display(#[case] description: &str, #[case] waveform_def: WaveformDefinition) {
        insta::with_settings!({
            snapshot_suffix => description,
        }, {
            assert_snapshot!(waveform_def.to_string())
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WaveformInvocation {
    pub name: String,
    pub parameters: HashMap<String, Expression>,
}

impl WaveformInvocation {
    pub fn new(name: String, parameters: HashMap<String, Expression>) -> Self {
        Self { name, parameters }
    }
}

impl fmt::Display for WaveformInvocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut key_value_pairs = self
            .parameters
            .iter()
            .collect::<Vec<(&String, &Expression)>>();

        key_value_pairs.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));

        if key_value_pairs.is_empty() {
            write!(f, "{}", self.name,)
        } else {
            write!(
                f,
                "{}({})",
                self.name,
                key_value_pairs
                    .iter()
                    .map(|(k, v)| format!("{k}: {v}"))
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        }
    }
}

#[cfg(test)]
mod waveform_invocation_tests {
    use std::collections::HashMap;

    use crate::instruction::WaveformInvocation;

    #[test]
    fn format_no_parameters() {
        let wfi = WaveformInvocation {
            name: "CZ".into(),
            parameters: HashMap::new(),
        };
        assert_eq!(format!("{wfi}"), "CZ".to_string());
    }
}
