use std::fmt;

use super::{get_string_parameter_string, Instruction};

#[derive(Clone, Debug, PartialEq)]
pub struct CircuitDefinition {
    pub name: String,
    pub parameters: Vec<String>,
    // These cannot be fixed qubits and thus are not typed as `Qubit`
    pub qubit_variables: Vec<String>,
    pub instructions: Vec<Instruction>,
}

impl CircuitDefinition {
    pub fn new(
        name: String,
        parameters: Vec<String>,
        qubit_variables: Vec<String>,
        instructions: Vec<Instruction>,
    ) -> Self {
        Self {
            name,
            parameters,
            qubit_variables,
            instructions,
        }
    }
}

impl fmt::Display for CircuitDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "DEFCIRCUIT {}", self.name)?;
        if !self.parameters.is_empty() {
            let parameter_str = get_string_parameter_string(&self.parameters);
            write!(f, "{parameter_str}")?;
        }
        for qubit_variable in &self.qubit_variables {
            write!(f, " {qubit_variable}")?;
        }
        writeln!(f, ":")?;
        for instruction in &self.instructions {
            writeln!(f, "\t{instruction}")?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test_circuit_definition {
    use crate::expression::Expression;
    use crate::instruction::{Gate, Instruction, Qubit};

    use super::CircuitDefinition;

    use insta::assert_snapshot;
    use rstest::rstest;

    #[rstest]
    #[case(
        "CircuitDefinition No Params",
        CircuitDefinition {
            name: "BELL".to_owned(),
            parameters: vec![],
            qubit_variables: vec!["a".to_owned(), "b".to_owned()],
            instructions: vec![
                Instruction::Gate(Gate {
                    name: "H".to_owned(),
                    parameters: vec![],
                    qubits: vec![Qubit::Variable("a".to_owned())],
                    modifiers: vec![],
                }),
                Instruction::Gate(Gate {
                    name: "CNOT".to_owned(),
                    parameters: vec![],
                    qubits: vec![
                        Qubit::Variable("a".to_owned()),
                        Qubit::Variable("b".to_owned())
                    ],
                    modifiers: vec![],
                })
            ]
        }
    )]
    #[case(
        "CircuitDefinition With Params",
        CircuitDefinition {
            name: "BELL".to_owned(),
            parameters: vec!["a".to_owned()],
            qubit_variables: vec!["a".to_owned(), "b".to_owned()],
            instructions: vec![
                Instruction::Gate(Gate {
                    name: "RZ".to_owned(),
                    parameters: vec![Expression::Variable("a".to_owned())],
                    qubits: vec![Qubit::Variable("a".to_owned())],
                    modifiers: vec![],
                }),
                Instruction::Gate(Gate {
                    name: "RX".to_owned(),
                    parameters: vec![Expression::Variable("a".to_owned())],
                    qubits: vec![Qubit::Variable("a".to_owned())],
                    modifiers: vec![],
                }),
                Instruction::Gate(Gate {
                    name: "RZ".to_owned(),
                    parameters: vec![Expression::Variable("a".to_owned())],
                    qubits: vec![Qubit::Variable("a".to_owned())],
                    modifiers: vec![],
                }),
                Instruction::Gate(Gate {
                    name: "CNOT".to_owned(),
                    parameters: vec![],
                    qubits: vec![
                        Qubit::Variable("a".to_owned()),
                        Qubit::Variable("b".to_owned())
                    ],
                    modifiers: vec![],
                })
            ]
        }
    )]
    fn test_display(#[case] description: &str, #[case] circuit_def: CircuitDefinition) {
        insta::with_settings!({
            snapshot_suffix => description,
        }, {
            assert_snapshot!(circuit_def.to_string())
        })
    }
}
