use std::{collections::HashSet, fmt};

use crate::{
    expression::Expression,
    instruction::get_string_parameter_string,
    validation::identifier::{
        validate_identifier, validate_user_identifier, IdentifierValidationError,
    },
};

use super::{format_qubits, get_expression_parameter_string, Qubit};

/// A struct encapsulating all the properties of a Quil Quantum Gate.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Gate {
    pub name: String,
    pub parameters: Vec<Expression>,
    pub qubits: Vec<Qubit>,
    pub modifiers: Vec<GateModifier>,
}

/// An enum of all the possible modifiers on a quil [`Gate`]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum GateModifier {
    /// The `CONTROLLED` modifier makes the gate take an extra [`Qubit`] parameter as a control
    /// qubit.
    Controlled,
    /// The `DAGGER` modifier does a complex-conjugate transpose on the [`Gate`].
    Dagger,
    /// The `FORKED` modifier allows an alternate set of parameters to be used based on the state
    /// of a qubit.
    Forked,
}

#[derive(Clone, Debug, thiserror::Error, PartialEq, Eq)]
pub enum GateError {
    #[error("invalid name: {0}")]
    InvalidIdentifier(#[from] IdentifierValidationError),

    #[error("a gate must operate on 1 or more qubits")]
    EmptyQubits,

    #[error("expected {expected} parameters, but got {actual}")]
    ForkedParameterLength { expected: usize, actual: usize },

    #[error("expected the number of Pauli term arguments, {actual}, to match the length of the Pauli word, {expected}")]
    PauliTermArgumentLength { expected: usize, actual: usize },

    #[error("the Pauli term arguments {mismatches:?}, are not in the defined argument list: {expected_arguments:?}")]
    PauliSumArgumentMismatch {
        mismatches: Vec<String>,
        expected_arguments: Vec<String>,
    },
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
        for modifier in &self.modifiers {
            write!(f, "{modifier} ")?;
        }

        write!(f, "{}{} {}", self.name, parameter_str, qubit_str)
    }
}

impl fmt::Display for GateModifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Controlled => write!(f, "CONTROLLED"),
            Self::Dagger => write!(f, "DAGGER"),
            Self::Forked => write!(f, "FORKED"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, strum::Display, strum::EnumString)]
#[strum(serialize_all = "UPPERCASE")]
pub enum PauliGate {
    I,
    X,
    Y,
    Z,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PauliTerm {
    pub arguments: Vec<(PauliGate, String)>,
    pub expression: Expression,
}

impl PauliTerm {
    pub fn new(arguments: Vec<(PauliGate, String)>, expression: Expression) -> Self {
        Self {
            arguments,
            expression,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PauliSum {
    pub arguments: Vec<String>,
    pub terms: Vec<PauliTerm>,
}

impl PauliSum {
    pub fn new(arguments: Vec<String>, terms: Vec<PauliTerm>) -> Result<Self, GateError> {
        let term_arguments = HashSet::<String>::from_iter(
            terms
                .iter()
                .flat_map(|t| t.arguments.clone().into_iter().map(|(_, arg)| arg)),
        );
        let argument_set = HashSet::from_iter(arguments.clone().into_iter());
        let diff: Vec<&String> = term_arguments.difference(&argument_set).collect();

        if !diff.is_empty() {
            return Err(GateError::PauliSumArgumentMismatch {
                expected_arguments: arguments,
                mismatches: diff.into_iter().cloned().collect(),
            });
        }

        Ok(Self { arguments, terms })
    }
}

/// An enum representing a the specification of a [`GateDefinition`] for a given [`GateType`]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GateSpecification {
    /// A matrix of [`Expression`]s representing a unitary operation for a [`GateType::Matrix`].
    Matrix(Vec<Vec<Expression>>),
    /// A vector of integers that defines the permutation used for a [`GateType::Permutation`]
    Permutation(Vec<u64>),
    /// A Hermitian operator specified as a Pauli sum, a sum of combinations of Pauli operators,
    /// used for a [`GateType::PauliSum`]
    PauliSum(PauliSum),
}

impl fmt::Display for GateSpecification {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GateSpecification::Matrix(matrix) => {
                for row in matrix {
                    write!(f, "\t")?;
                    if let Some(first) = row.first() {
                        write!(f, "{first}")?;
                    }
                    for cell in row.iter().skip(1) {
                        write!(f, ", {cell}")?;
                    }
                    writeln!(f)?;
                }
            }
            GateSpecification::Permutation(permutation) => {
                write!(f, "\t")?;
                if let Some(i) = permutation.first() {
                    write!(f, "{i}")?;
                }
                for i in permutation.iter().skip(1) {
                    write!(f, ", {i}")?;
                }
                writeln!(f)?;
            }
            GateSpecification::PauliSum(pauli_sum) => {
                for term in &pauli_sum.terms {
                    write!(f, "\t")?;
                    for (word, _) in term.arguments.iter() {
                        write!(f, "{word}")?;
                    }
                    write!(f, "({})", term.expression)?;
                    for (_, argument) in term.arguments.iter() {
                        write!(f, " {argument}")?;
                    }
                    writeln!(f)?;
                }
            }
        }
        Ok(())
    }
}

/// A struct encapsulating a quil Gate Definition
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GateDefinition {
    pub name: String,
    pub parameters: Vec<String>,
    pub specification: GateSpecification,
}

impl GateDefinition {
    pub fn new(
        name: String,
        parameters: Vec<String>,
        specification: GateSpecification,
    ) -> Result<Self, GateError> {
        validate_user_identifier(&name)?;
        Ok(Self {
            name,
            parameters,
            specification,
        })
    }
}

impl fmt::Display for GateDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "DEFGATE {}{}",
            self.name,
            get_string_parameter_string(&self.parameters)
        )?;
        match &self.specification {
            GateSpecification::Matrix(_) => writeln!(f, " AS MATRIX:")?,
            GateSpecification::Permutation(_) => writeln!(f, " AS PERMUTATION:")?,
            GateSpecification::PauliSum(sum) => {
                for arg in &sum.arguments {
                    write!(f, " {arg}")?;
                }
                writeln!(f, " AS PAULI-SUM:")?
            }
        }
        write!(f, "{}", self.specification)
    }
}

#[cfg(test)]
mod test_gate_definition {
    use super::{GateDefinition, GateSpecification, PauliGate, PauliSum, PauliTerm};
    use crate::expression::{
        Expression, ExpressionFunction, FunctionCallExpression, InfixExpression, InfixOperator,
        PrefixExpression, PrefixOperator,
    };
    use crate::{imag, real};
    use insta::assert_snapshot;
    use rstest::rstest;

    #[rstest]
    #[case(
        "Permutation GateDefinition",
        GateDefinition{
            name: "PermGate".to_string(),
            parameters: vec![],
            specification: GateSpecification::Permutation(vec![0, 1, 2, 3, 4, 5, 7, 6]),

        }
    )]
    #[case(
        "Parameterized GateDefinition",
        GateDefinition{
            name: "ParamGate".to_string(),
            parameters: vec!["theta".to_string()],
            specification: GateSpecification::Matrix(vec![
                vec![
                    Expression::FunctionCall(FunctionCallExpression {
                        function: crate::expression::ExpressionFunction::Cosine,
                        expression: Box::new(Expression::Infix(InfixExpression {
                            left: Box::new(Expression::Variable("theta".to_string())),
                            operator: InfixOperator::Slash,
                            right: Box::new(Expression::Number(real!(2.0))),
                        })),
                    }),
                    Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Prefix(PrefixExpression {
                            operator: PrefixOperator::Minus,
                            expression: Box::new(Expression::Number(imag!(1f64)))
                        })),
                        operator: InfixOperator::Star,
                        right: Box::new(Expression::FunctionCall(FunctionCallExpression {
                            function: ExpressionFunction::Sine,
                            expression: Box::new(Expression::Infix(InfixExpression {
                                left: Box::new(Expression::Variable("theta".to_string())),
                                operator: InfixOperator::Slash,
                                right: Box::new(Expression::Number(real!(2.0))),
                            })),
                        })),
                    })
                ],
                vec![
                    Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Prefix(PrefixExpression {
                            operator: PrefixOperator::Minus,
                            expression: Box::new(Expression::Number(imag!(1f64)))
                        })),
                        operator: InfixOperator::Star,
                        right: Box::new(Expression::FunctionCall(FunctionCallExpression {
                            function: ExpressionFunction::Sine,
                            expression: Box::new(Expression::Infix(InfixExpression {
                                left: Box::new(Expression::Variable("theta".to_string())),
                                operator: InfixOperator::Slash,
                                right: Box::new(Expression::Number(real!(2.0))),
                            })),
                        })),
                    }),
                    Expression::FunctionCall(FunctionCallExpression {
                        function: crate::expression::ExpressionFunction::Cosine,
                        expression: Box::new(Expression::Infix(InfixExpression {
                            left: Box::new(Expression::Variable("theta".to_string())),
                            operator: InfixOperator::Slash,
                            right: Box::new(Expression::Number(real!(2.0))),
                        })),
                    }),
                ],
            ]),

        }
    )]
    #[case(
        "Pauli Sum GateDefinition",
        GateDefinition{
            name: "PauliSumGate".to_string(),
            parameters: vec!["theta".to_string()],
            specification: GateSpecification::PauliSum(PauliSum{arguments: vec!["p".to_string(), "q".to_string()], terms: vec![
                PauliTerm {
                    arguments: vec![(PauliGate::Z, "p".to_string()), (PauliGate::Z, "q".to_string())],
                    expression: Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Prefix(PrefixExpression {
                            operator: PrefixOperator::Minus,
                            expression: Box::new(Expression::Variable("theta".to_string()))
                        })),
                        operator: InfixOperator::Slash,
                        right: Box::new(Expression::Number(real!(4.0)))
                    }),
                },
                PauliTerm {
                    arguments: vec![(PauliGate::Y, "p".to_string())],
                    expression: Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Variable("theta".to_string())),
                        operator: InfixOperator::Slash,
                        right: Box::new(Expression::Number(real!(4.0)))
                    }),
                },
                PauliTerm {
                    arguments: vec![(PauliGate::X, "q".to_string())],
                    expression: Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Variable("theta".to_string())),
                        operator: InfixOperator::Slash,
                        right: Box::new(Expression::Number(real!(4.0)))
                    }),
                },
            ]})
        }
    )]
    fn test_display(#[case] description: &str, #[case] gate_def: GateDefinition) {
        insta::with_settings!({
            snapshot_suffix => description,
        }, {
            assert_snapshot!(gate_def.to_string())
        })
    }
}

/// The type of a [`GateDefinition`]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GateType {
    Matrix,
    Permutation,
    PauliSum,
}

impl fmt::Display for GateType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Matrix => write!(f, "MATRIX"),
            Self::Permutation => write!(f, "PERMUTATION"),
            Self::PauliSum => write!(f, "PAULI-SUM"),
        }
    }
}
