use std::fmt;

use crate::{
    instruction::{
        format_instructions, format_qubits, get_expression_parameter_string, Expression,
        GateModifier, Instruction, Qubit,
    },
    validation::identifier::{validate_identifier, IdentifierValidationError},
};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Calibration {
    pub instructions: Vec<Instruction>,
    pub modifiers: Vec<GateModifier>,
    pub name: String,
    pub parameters: Vec<Expression>,
    pub qubits: Vec<Qubit>,
}

impl Calibration {
    /// Builds a new calibration definition.
    ///
    /// # Errors
    ///
    /// Returns an error if the given name isn't a valid Quil identifier.
    pub fn new(
        name: &str,
        parameters: Vec<Expression>,
        qubits: Vec<Qubit>,
        instructions: Vec<Instruction>,
        modifiers: Vec<GateModifier>,
    ) -> Result<Self, IdentifierValidationError> {
        validate_identifier(name)?;
        Ok(Self {
            instructions,
            modifiers,
            name: name.to_string(),
            parameters,
            qubits,
        })
    }
}

impl fmt::Display for Calibration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let parameter_str = get_expression_parameter_string(&self.parameters);
        write!(
            f,
            "DEFCAL {}{} {}:",
            self.name,
            parameter_str,
            format_qubits(&self.qubits)
        )?;
        for instruction in &self.instructions {
            write!(f, "\n\t{instruction}")?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MeasureCalibrationDefinition {
    pub qubit: Option<Qubit>,
    pub parameter: String,
    pub instructions: Vec<Instruction>,
}

impl MeasureCalibrationDefinition {
    pub fn new(qubit: Option<Qubit>, parameter: String, instructions: Vec<Instruction>) -> Self {
        Self {
            qubit,
            parameter,
            instructions,
        }
    }
}

impl fmt::Display for MeasureCalibrationDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "DEFCAL MEASURE")?;
        if let Some(qubit) = &self.qubit {
            write!(f, " {qubit}")?;
        }

        writeln!(
            f,
            " {}:\n\t{}",
            self.parameter,
            format_instructions(&self.instructions)
        )
    }
}
