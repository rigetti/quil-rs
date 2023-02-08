use std::fmt;

use crate::{
    expression::Expression,
    validation::identifier::{validate_identifier, IdentifierValidationError},
};

use super::{format_qubits, get_expression_parameter_string, Qubit};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Gate {
    pub name: String,
    pub parameters: Vec<Expression>,
    pub qubits: Vec<Qubit>,
    pub modifiers: Vec<GateModifier>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GateModifier {
    Controlled,
    Dagger,
    Forked,
}

#[derive(Debug, thiserror::Error)]
pub enum GateError {
    #[error("invalid name: {0}")]
    InvalidIdentifier(#[from] IdentifierValidationError),

    #[error("a gate must operate on 1 or more qubits")]
    EmptyQubits,

    #[error("expected {expected} parameters, but got {actual}")]
    ForkedParameterLength { expected: usize, actual: usize },
}

impl Gate {
    /// Build a new gate
    ///
    /// # Errors
    ///
    /// Returns an error if the given name isn't a valid Quil identifier or if no qubits are given.
    pub fn new(
        name: &str,
        parameters: Vec<Expression>,
        qubits: Vec<Qubit>,
        modifiers: Vec<GateModifier>,
    ) -> Result<Self, GateError> {
        if qubits.is_empty() {
            return Err(GateError::EmptyQubits);
        }

        validate_identifier(name).map_err(GateError::InvalidIdentifier)?;

        Ok(Self {
            name: name.to_string(),
            parameters,
            qubits,
            modifiers,
        })
    }

    /// Apply a DAGGER modifier to the gate
    pub fn dagger(mut self) -> Self {
        self.modifiers.insert(0, GateModifier::Dagger);
        self
    }

    /// Apply a CONTROLLED modifier to the gate
    pub fn controlled(mut self, control_qubit: Qubit) -> Self {
        self.qubits.insert(0, control_qubit);
        self.modifiers.insert(0, GateModifier::Controlled);
        self
    }

    /// Apply a FORKED modifier to the gate
    ///
    /// # Errors
    ///
    /// Returns an error if the number of provided alternate parameters don't
    /// equal the number of existing parameters.
    pub fn forked(
        mut self,
        fork_qubit: Qubit,
        alt_params: Vec<Expression>,
    ) -> Result<Self, GateError> {
        if alt_params.len() != self.parameters.len() {
            return Err(GateError::ForkedParameterLength {
                expected: self.parameters.len(),
                actual: alt_params.len(),
            });
        }
        self.modifiers.insert(0, GateModifier::Forked);
        self.qubits.insert(0, fork_qubit);
        self.parameters.extend(alt_params);
        Ok(self)
    }
}

impl fmt::Display for Gate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let parameter_str = get_expression_parameter_string(&self.parameters);

        let qubit_str = format_qubits(&self.qubits);
        let modifier_str = &self
            .modifiers
            .iter()
            .map(|m| format!("{m} "))
            .collect::<Vec<String>>()
            .join("");
        write!(
            f,
            "{}{}{} {}",
            modifier_str, self.name, parameter_str, qubit_str
        )
    }
}

impl fmt::Display for GateModifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use GateModifier::*;
        write!(
            f,
            "{}",
            match self {
                Controlled => "CONTROLLED",
                Dagger => "DAGGER",
                Forked => "FORKED",
            }
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GateSpecification {
    Matrix(Vec<Vec<Expression>>),
    Permutation(Vec<u64>),
}

impl fmt::Display for GateSpecification {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GateSpecification::Matrix(matrix) => {
                for row in matrix {
                    writeln!(
                        f,
                        "\t{}",
                        row.iter()
                            .map(|cell| format!("{cell}"))
                            .collect::<Vec<String>>()
                            .join(", ")
                    )?;
                }
            }
            GateSpecification::Permutation(permutation) => {
                writeln!(
                    f,
                    "\t{}",
                    permutation
                        .iter()
                        .map(|i| format!("{i}"))
                        .collect::<Vec<String>>()
                        .join(", ")
                )?;
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GateDefinition {
    pub name: String,
    pub parameters: Vec<String>,
    pub specification: GateSpecification,
}

impl fmt::Display for GateDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let parameter_str = match self.parameters.is_empty() {
            true => String::new(),
            false => format!(
                "({})",
                self.parameters
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<String>()
            ),
        };
        writeln!(
            f,
            "DEFGATE {}{} AS {}:",
            self.name,
            parameter_str,
            match self.specification {
                GateSpecification::Matrix(_) => "MATRIX",
                GateSpecification::Permutation(_) => "PERMUTATION",
            }
        )?;
        write!(f, "{}", self.specification)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GateType {
    Matrix,
    Permutation,
}

impl fmt::Display for GateType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use GateType::*;
        write!(
            f,
            "{}",
            match self {
                Matrix => "MATRIX",
                Permutation => "PERMUTATION",
            }
        )
    }
}
