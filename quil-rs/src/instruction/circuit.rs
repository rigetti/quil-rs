use crate::quil::Quil;

use super::Instruction;

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

impl Quil for CircuitDefinition {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> Result<(), crate::quil::ToQuilError> {
        write!(writer, "DEFCIRCUIT {}", self.name)?;
        if !self.parameters.is_empty() {
            write!(writer, "(")?;
            let mut iter = self.parameters.iter();
            if let Some(p) = iter.next() {
                write!(writer, "%{}", p)?;
            }
            for p in iter {
                write!(writer, "%{}", p)?;
            }
            write!(writer, ")")?;
        }
        for qubit_variable in &self.qubit_variables {
            write!(writer, " {}", qubit_variable)?;
        }
        writeln!(writer, ":")?;
        for instruction in &self.instructions {
            let lines = match fall_back_to_debug {
                true => instruction.to_quil_or_debug(),
                false => instruction.to_quil()?,
            };
            for line in lines.split('\n') {
                writeln!(writer, "\t{line}")?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod test_circuit_definition {
    use crate::expression::Expression;
    use crate::instruction::{Gate, Instruction, Qubit};
    use crate::quil::Quil;

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
            assert_snapshot!(circuit_def.to_quil_or_debug())
        })
    }
}
