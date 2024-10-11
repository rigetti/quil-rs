use crate::{
    instruction::{
        write_expression_parameter_string, write_instruction_block, Expression, GateModifier,
        Instruction, Qubit,
    },
    quil::{Quil, INDENT},
    validation::identifier::{validate_identifier, IdentifierValidationError},
};

use super::{write_qubit_parameters, Gate};

pub trait CalibrationSignature {
    type Signature<'a>
    where
        Self: 'a;

    fn signature(&self) -> Self::Signature<'_>;
    fn has_signature(&self, signature: &Self::Signature<'_>) -> bool;
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Calibration {
    pub identifier: CalibrationIdentifier,
    pub instructions: Vec<Instruction>,
}

impl Calibration {
    /// Builds a new calibration definition.
    pub fn new(
        identifier: CalibrationIdentifier,
        instructions: Vec<Instruction>,
    ) -> Result<Self, IdentifierValidationError> {
        Ok(Self {
            identifier,
            instructions,
        })
    }
}

impl CalibrationSignature for Calibration {
    type Signature<'a> = (&'a str, &'a [Expression], &'a [Qubit]);

    fn signature(&self) -> Self::Signature<'_> {
        self.identifier.signature()
    }

    fn has_signature(&self, signature: &Self::Signature<'_>) -> bool {
        self.identifier.has_signature(signature)
    }
}

impl Quil for Calibration {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        self.identifier.write(f, fall_back_to_debug)?;
        write!(f, ":")?;
        for instruction in &self.instructions {
            write!(f, "\n{INDENT}")?;
            instruction.write(f, fall_back_to_debug)?;
        }
        Ok(())
    }
}

/// Unique identifier for a calibration definition within a program
#[derive(Clone, Debug, Default, PartialEq)]
pub struct CalibrationIdentifier {
    /// The modifiers applied to the gate
    pub modifiers: Vec<GateModifier>,

    /// The name of the gate
    pub name: String,

    /// The parameters of the gate - these are the variables in the calibration definition
    pub parameters: Vec<Expression>,

    /// The qubits on which the gate is applied
    pub qubits: Vec<Qubit>,
}

impl CalibrationIdentifier {
    /// Builds a new calibration identifier.
    ///
    /// # Errors
    ///
    /// Returns an error if the given name isn't a valid Quil identifier.
    pub fn new(
        name: String,
        modifiers: Vec<GateModifier>,
        parameters: Vec<Expression>,
        qubits: Vec<Qubit>,
    ) -> Result<Self, IdentifierValidationError> {
        validate_identifier(name.as_str())?;
        Ok(Self {
            modifiers,
            name,
            parameters,
            qubits,
        })
    }

    pub fn matches(&self, gate: &Gate) -> bool {
        // Filter out non-matching calibrations: check rules 1-4
        if self.name != gate.name
            || self.modifiers != gate.modifiers
            || self.parameters.len() != gate.parameters.len()
            || self.qubits.len() != gate.qubits.len()
        {
            return false;
        }

        let fixed_qubits_match = self
            .qubits
            .iter()
            .enumerate()
            .all(|(calibration_index, _)| {
                match (
                    &self.qubits[calibration_index],
                    &gate.qubits[calibration_index],
                ) {
                    // Placeholders never match
                    (Qubit::Placeholder(_), _) | (_, Qubit::Placeholder(_)) => false,
                    // If they're both fixed, test if they're fixed to the same qubit
                    (Qubit::Fixed(calibration_fixed_qubit), Qubit::Fixed(gate_fixed_qubit)) => {
                        calibration_fixed_qubit == gate_fixed_qubit
                    }
                    // If the calibration is variable, it matches any fixed qubit
                    (Qubit::Variable(_), _) => true,
                    // If the calibration is fixed, but the gate's qubit is variable, it's not a match
                    (Qubit::Fixed(_), _) => false,
                }
            });
        if !fixed_qubits_match {
            return false;
        }

        let fixed_parameters_match =
            self.parameters
                .iter()
                .enumerate()
                .all(|(calibration_index, _)| {
                    let calibration_parameters =
                        self.parameters[calibration_index].clone().into_simplified();
                    let gate_parameters =
                        gate.parameters[calibration_index].clone().into_simplified();
                    match (calibration_parameters, gate_parameters) {
                        // If the calibration is variable, it matches any fixed qubit
                        (Expression::Variable(_), _) => true,
                        // If the calibration is fixed, but the gate's qubit is variable, it's not a match
                        (calib, gate) => calib == gate,
                    }
                });
        fixed_parameters_match
    }
}

impl CalibrationSignature for CalibrationIdentifier {
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

impl Quil for CalibrationIdentifier {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(f, "DEFCAL {}", self.name)?;
        write_expression_parameter_string(f, fall_back_to_debug, &self.parameters)?;
        write_qubit_parameters(f, fall_back_to_debug, &self.qubits)?;
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MeasureCalibrationDefinition {
    pub identifier: MeasureCalibrationIdentifier,
    pub instructions: Vec<Instruction>,
}

impl MeasureCalibrationDefinition {
    pub fn new(identifier: MeasureCalibrationIdentifier, instructions: Vec<Instruction>) -> Self {
        Self {
            identifier,
            instructions,
        }
    }
}

impl CalibrationSignature for MeasureCalibrationDefinition {
    type Signature<'a> = (Option<&'a Qubit>, &'a str);

    fn signature(&self) -> Self::Signature<'_> {
        self.identifier.signature()
    }

    fn has_signature(&self, signature: &Self::Signature<'_>) -> bool {
        self.identifier.has_signature(signature)
    }
}

impl Quil for MeasureCalibrationDefinition {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        self.identifier.write(f, fall_back_to_debug)?;
        writeln!(f, ":")?;

        write_instruction_block(f, fall_back_to_debug, &self.instructions)?;
        writeln!(f)?;
        Ok(())
    }
}

/// A unique identifier for a measurement calibration definition within a program
#[derive(Clone, Debug, Default, PartialEq)]
pub struct MeasureCalibrationIdentifier {
    /// The qubit which is the target of measurement, if any
    pub qubit: Option<Qubit>,

    /// The memory region name to which the measurement result is written
    pub parameter: String,
}

impl MeasureCalibrationIdentifier {
    pub fn new(qubit: Option<Qubit>, parameter: String) -> Self {
        Self { qubit, parameter }
    }
}

impl CalibrationSignature for MeasureCalibrationIdentifier {
    type Signature<'a> = (Option<&'a Qubit>, &'a str);

    fn signature(&self) -> Self::Signature<'_> {
        (self.qubit.as_ref(), self.parameter.as_str())
    }

    fn has_signature(&self, signature: &Self::Signature<'_>) -> bool {
        let (qubit, parameter) = signature;
        self.qubit.as_ref() == *qubit && self.parameter == *parameter
    }
}

impl Quil for MeasureCalibrationIdentifier {
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
        write!(f, " {}", self.parameter,)?;
        Ok(())
    }
}

#[cfg(test)]
mod test_measure_calibration_definition {
    use super::MeasureCalibrationDefinition;
    use crate::expression::Expression;
    use crate::instruction::calibration::MeasureCalibrationIdentifier;
    use crate::instruction::{Gate, Instruction, Qubit};
    use crate::quil::Quil;
    use insta::assert_snapshot;
    use rstest::rstest;

    #[rstest]
    #[case(
        "With Fixed Qubit",
        MeasureCalibrationDefinition {
            identifier: MeasureCalibrationIdentifier {
                qubit: Some(Qubit::Fixed(0)),
                parameter: "theta".to_string(),
            },
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
            identifier: MeasureCalibrationIdentifier {
                qubit: Some(Qubit::Variable("q".to_string())),
                parameter: "theta".to_string(),
            },
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
