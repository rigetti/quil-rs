use std::collections::HashMap;

use crate::{
    expression::Expression,
    quil::{write_join_quil, Quil},
};

use super::write_parameter_string;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Waveform {
    pub matrix: Vec<Expression>,
    pub parameters: Vec<String>,
}

impl Waveform {
    pub fn new(matrix: Vec<Expression>, parameters: Vec<String>) -> Self {
        Self { matrix, parameters }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct WaveformDefinition {
    pub name: String,
    pub definition: Waveform,
}

impl WaveformDefinition {
    pub fn new(name: String, definition: Waveform) -> Self {
        Self { name, definition }
    }
}

impl Quil for WaveformDefinition {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(f, "DEFWAVEFORM {}", self.name)?;
        write_parameter_string(f, &self.definition.parameters)?;
        write!(f, ":\n\t")?;
        write_join_quil(f, fall_back_to_debug, &self.definition.matrix, ", ", "")
            .map_err(Into::into)
    }
}

#[cfg(test)]
mod test_waveform_definition {
    use super::{Waveform, WaveformDefinition};
    use crate::expression::Expression;
    use crate::quil::Quil;
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
            assert_snapshot!(waveform_def.to_quil_or_debug())
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

impl Quil for WaveformInvocation {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        _fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        let mut key_value_pairs = self
            .parameters
            .iter()
            .collect::<Vec<(&String, &Expression)>>();

        key_value_pairs.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));

        if key_value_pairs.is_empty() {
            write!(f, "{}", self.name,)?;
        } else {
            write!(
                f,
                "{}({})",
                self.name,
                key_value_pairs
                    .iter()
                    .map(|(k, v)| format!("{k}: {}", v.to_quil_or_debug()))
                    .collect::<Vec<String>>()
                    .join(", ")
            )?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod waveform_invocation_tests {
    use std::collections::HashMap;

    use crate::{instruction::WaveformInvocation, quil::Quil};

    #[test]
    fn format_no_parameters() {
        let invocation = WaveformInvocation {
            name: "CZ".into(),
            parameters: HashMap::new(),
        };
        assert_eq!(invocation.to_quil_or_debug(), "CZ".to_string());
    }
}
