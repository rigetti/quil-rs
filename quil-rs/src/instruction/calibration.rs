use crate::{
    instruction::{
        write_expression_parameter_string, write_instruction_block, Expression, GateModifier,
        Instruction, Qubit,
    },
    quil::Quil,
    validation::identifier::{validate_identifier, IdentifierValidationError},
};

use super::write_qubit_parameters;

pub trait CalibrationSignature {
    type Signature<'a>
    where
        Self: 'a;

    fn signature(&self) -> Self::Signature<'_>;
    fn has_signature(&self, signature: &Self::Signature<'_>) -> bool;
}

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

impl Quil for Calibration {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(f, "DEFCAL {}", self.name)?;
        write_expression_parameter_string(f, fall_back_to_debug, &self.parameters)?;
        write_qubit_parameters(f, fall_back_to_debug, &self.qubits)?;
        write!(f, ":")?;
        for instruction in &self.instructions {
            write!(f, "\n\t")?;
            instruction.write(f, fall_back_to_debug)?;
        }
        Ok(())
    }
}

impl CalibrationSignature for Calibration {
    type Signature<'a> = (&'a str, &'a [Expression], &'a [Qubit]);

    fn signature(&self) -> Self::Signature<'_> {
        (
            self.name.as_str(),
            self.parameters.as_slice(),
            self.qubits.as_slice(),
        )
    }

    fn has_signature(&self, signature: &Self::Signature<'_>) -> bool {
        let (name, parameters, qubits) = signature;
        self.name == *name && self.parameters == *parameters && self.qubits == *qubits
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

impl CalibrationSignature for MeasureCalibrationDefinition {
    type Signature<'a> = (Option<&'a Qubit>, &'a str);

    fn signature(&self) -> Self::Signature<'_> {
        (self.qubit.as_ref(), self.parameter.as_str())
    }

    fn has_signature(&self, signature: &Self::Signature<'_>) -> bool {
        let (qubit, parameter) = signature;
        self.qubit.as_ref() == *qubit && self.parameter == *parameter
    }
}

impl Quil for MeasureCalibrationDefinition {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(f, "DEFCAL MEASURE")?;
        if let Some(qubit) = &self.qubit {
            write!(f, " ")?;
            qubit.write(f, fall_back_to_debug)?;
        }

        writeln!(f, " {}:", self.parameter,)?;

        write_instruction_block(f, fall_back_to_debug, &self.instructions)?;
        writeln!(f)?;
        Ok(())
    }
}

#[cfg(test)]
mod test_measure_calibration_definition {
    use super::MeasureCalibrationDefinition;
    use crate::expression::Expression;
    use crate::instruction::{Gate, Instruction, Qubit};
    use crate::quil::Quil;
    use insta::assert_snapshot;
    use rstest::rstest;

    #[rstest]
    #[case(
        "With Fixed Qubit",
        MeasureCalibrationDefinition {
            qubit: Some(Qubit::Fixed(0)),
            parameter: "theta".to_string(),
            instructions: vec![Instruction::Gate(Gate {
                name: "X".to_string(),
                parameters: vec![Expression::Variable("theta".to_string())],
                qubits: vec![Qubit::Fixed(0)],
                modifiers: vec![],

            })]},
    )]
    #[case(
        "With Variable Qubit",
        MeasureCalibrationDefinition {
            qubit: Some(Qubit::Variable("q".to_string())),
            parameter: "theta".to_string(),
            instructions: vec![Instruction::Gate(Gate {
                name: "X".to_string(),
                parameters: vec![Expression::Variable("theta".to_string())],
                qubits: vec![Qubit::Variable("q".to_string())],
                modifiers: vec![],
            })]},
    )]
    fn test_display(
        #[case] description: &str,
        #[case] measure_cal_def: MeasureCalibrationDefinition,
    ) {
        insta::with_settings!({
            snapshot_suffix => description,
        }, {
            assert_snapshot!(measure_cal_def.to_quil_or_debug())
        })
    }
}
