use std::{collections::HashMap, fmt};

use serde::{Deserialize, Serialize};

use crate::expression::Expression;

use super::get_string_parameter_string;

#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub struct Waveform {
    pub matrix: Vec<Expression>,
    pub parameters: Vec<String>,
}

impl Waveform {
    pub fn new(matrix: Vec<Expression>, parameters: Vec<String>) -> Self {
        Self { matrix, parameters }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
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
        write!(
            f,
            "DEFWAVEFORM {}{}:\n\t{}",
            self.name,
            get_string_parameter_string(&self.definition.parameters),
            self.definition
                .matrix
                .iter()
                .map(|e| format!("{e}"))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
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
