#[cfg(not(feature = "python"))]
use optipy::strip_pyo3;
#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::gen_stub_pyclass;

use crate::{
    instruction::{
        write_expression_parameter_string, write_instruction_block, Expression, GateModifier,
        Instruction, Qubit,
    },
    pickleable_new,
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

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.instructions", eq, get_all, set_all, subclass)
)]
#[cfg_attr(not(feature = "python"), strip_pyo3)]
pub struct CalibrationDefinition {
    #[pyo3(name = "identifier")]
    pub identifier: CalibrationIdentifier,
    pub instructions: Vec<Instruction>,
}

pickleable_new! {
    impl CalibrationDefinition {
        /// Builds a new calibration definition.
        pub fn new(
            identifier: CalibrationIdentifier,
            instructions: Vec<Instruction>,
        );
    }
}

impl CalibrationSignature for CalibrationDefinition {
    type Signature<'a> = <CalibrationIdentifier as CalibrationSignature>::Signature<'a>;

    fn signature(&self) -> Self::Signature<'_> {
        self.identifier.signature()
    }

    fn has_signature(&self, signature: &Self::Signature<'_>) -> bool {
        self.identifier.has_signature(signature)
    }
}

impl Quil for CalibrationDefinition {
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
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.instructions", eq, get_all, set_all, subclass)
)]
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
}

impl CalibrationIdentifier {
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
    type Signature<'a> = (&'a [GateModifier], &'a str, &'a [Expression], &'a [Qubit]);

    fn signature(&self) -> Self::Signature<'_> {
        let Self {
            modifiers,
            name,
            parameters,
            qubits,
        } = self;
        (
            modifiers.as_slice(),
            name.as_str(),
            parameters.as_slice(),
            qubits.as_slice(),
        )
    }

    fn has_signature(&self, signature: &Self::Signature<'_>) -> bool {
        &self.signature() == signature
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
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.instructions", eq, get_all, set_all, subclass)
)]
pub struct MeasureCalibrationDefinition {
    pub identifier: MeasureCalibrationIdentifier,
    pub instructions: Vec<Instruction>,
}

pickleable_new! {
    impl MeasureCalibrationDefinition {
        pub fn new(identifier: MeasureCalibrationIdentifier, instructions: Vec<Instruction>);
    }
}

impl CalibrationSignature for MeasureCalibrationDefinition {
    type Signature<'a> = <MeasureCalibrationIdentifier as CalibrationSignature>::Signature<'a>;

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
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.instructions", eq, get_all, set_all, subclass)
)]
pub struct MeasureCalibrationIdentifier {
    /// The Quil-T name of the measurement, if any.
    pub name: Option<String>,

    /// The qubit which is being measured.
    pub qubit: Qubit,

    /// The name the definition uses for the variable it will write the measurement result to, if
    /// this is a measurement for record.
    ///
    /// If this is missing, this is a calibration for a measurement for effect.
    pub target: Option<String>,
}

impl MeasureCalibrationIdentifier {
    pub const fn new(name: Option<String>, qubit: Qubit, target: Option<String>) -> Self {
        Self {
            name,
            qubit,
            target,
        }
    }
}

impl CalibrationSignature for MeasureCalibrationIdentifier {
    type Signature<'a> = (Option<&'a str>, &'a Qubit, Option<&'a str>);

    fn signature(&self) -> Self::Signature<'_> {
        let Self {
            name,
            qubit,
            target,
        } = self;

        (name.as_deref(), qubit, target.as_deref())
    }

    fn has_signature(&self, signature: &Self::Signature<'_>) -> bool {
        &self.signature() == signature
    }
}

impl Quil for MeasureCalibrationIdentifier {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        let Self {
            name,
            qubit,
            target,
        } = self;

        write!(f, "DEFCAL MEASURE")?;
        if let Some(name) = name {
            write!(f, "!{name}")?;
        }
        write!(f, " ")?;
        qubit.write(f, fall_back_to_debug)?;
        if let Some(target) = target {
            write!(f, " {target}")?;
        }

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
                name: None,
                qubit: Qubit::Fixed(0),
                target: Some("theta".to_string()),
            },
            instructions: vec![Instruction::Gate(Gate {
                name: "X".to_string(),
                parameters: vec![Expression::Variable("theta".to_string())],
                qubits: vec![Qubit::Fixed(0)],
                modifiers: vec![],

            })]},
    )]
    #[case(
        "Named With Fixed Qubit",
        MeasureCalibrationDefinition {
            identifier: MeasureCalibrationIdentifier {
                name: Some("midcircuit".to_string()),
                qubit: Qubit::Fixed(0),
                target: Some("theta".to_string()),
            },
            instructions: vec![Instruction::Gate(Gate {
                name: "X".to_string(),
                parameters: vec![Expression::Variable("theta".to_string())],
                qubits: vec![Qubit::Fixed(0)],
                modifiers: vec![],

            })]},
    )]
    #[case(
        "Effect With Fixed Qubit",
        MeasureCalibrationDefinition {
            identifier: MeasureCalibrationIdentifier {
                name: None,
                qubit: Qubit::Fixed(0),
                target: None,
            },
            instructions: vec![Instruction::Gate(Gate {
                name: "X".to_string(),
                parameters: vec![Expression::PiConstant()],
                qubits: vec![Qubit::Fixed(0)],
                modifiers: vec![],

            })]},
    )]
    #[case(
        "Named Effect With Fixed Qubit",
        MeasureCalibrationDefinition {
            identifier: MeasureCalibrationIdentifier {
                name: Some("midcircuit".to_string()),
                qubit: Qubit::Fixed(0),
                target: None,
            },
            instructions: vec![Instruction::Gate(Gate {
                name: "X".to_string(),
                parameters: vec![Expression::PiConstant()],
                qubits: vec![Qubit::Fixed(0)],
                modifiers: vec![],

            })]},
    )]
    #[case(
        "With Variable Qubit",
        MeasureCalibrationDefinition {
            identifier: MeasureCalibrationIdentifier {
                name: None,
                qubit: Qubit::Variable("q".to_string()),
                target: Some("theta".to_string()),
            },
            instructions: vec![Instruction::Gate(Gate {
                name: "X".to_string(),
                parameters: vec![Expression::Variable("theta".to_string())],
                qubits: vec![Qubit::Variable("q".to_string())],
                modifiers: vec![],
            })]},
    )]
    #[case(
        "Named With Variable Qubit",
        MeasureCalibrationDefinition {
            identifier: MeasureCalibrationIdentifier {
                name: Some("midcircuit".to_string()),
                qubit: Qubit::Variable("q".to_string()),
                target: Some("theta".to_string()),
            },
            instructions: vec![Instruction::Gate(Gate {
                name: "X".to_string(),
                parameters: vec![Expression::Variable("theta".to_string())],
                qubits: vec![Qubit::Variable("q".to_string())],
                modifiers: vec![],
            })]},
    )]
    #[case(
        "Effect Variable Qubit",
        MeasureCalibrationDefinition {
            identifier: MeasureCalibrationIdentifier {
                name: None,
                qubit: Qubit::Variable("q".to_string()),
                target: None,
            },
            instructions: vec![Instruction::Gate(Gate {
                name: "X".to_string(),
                parameters: vec![Expression::PiConstant()],
                qubits: vec![Qubit::Variable("q".to_string())],
                modifiers: vec![],
            })]},
    )]
    #[case(
        "Named Effect Variable Qubit",
        MeasureCalibrationDefinition {
            identifier: MeasureCalibrationIdentifier {
                name: Some("midcircuit".to_string()),
                qubit: Qubit::Variable("q".to_string()),
                target: None,
            },
            instructions: vec![Instruction::Gate(Gate {
                name: "X".to_string(),
                parameters: vec![Expression::PiConstant()],
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
