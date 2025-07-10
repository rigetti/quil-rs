use std::collections::{HashMap, HashSet};

use crate::{expression::Expression, instruction::{Gate, GateModifier, Qubit}, quil::Quil};

/// An error that can occur when expanding a gate that has a sequence gate definition.
#[derive(Debug, thiserror::Error, PartialEq, Eq, Clone)]
pub enum DefGateSequenceExpansionError {
    /// Unexpected number of arguments
    #[error("gate sequence expected {expected} arguments, found {found}")]
    ParameterCount {
        expected: usize,
        found: usize,
    },
    /// A cyclic sequence gate definition was detected
    #[error("cyclic sequence gate definition detected")]
    CyclicSequenceGateDefinition(Vec<String>),
    /// Unexpected number of qubits
    #[error("gate sequence expected {expected} qubits, found {found}")]
    QubitCount {
        expected: usize,
        found: usize,
    },
    /// Gate qubit arguments must be fixed
    #[error("expected fixed qubit argument, found {}", .0.to_quil_or_debug())]
    GateQubitArugment(Qubit),
    /// Gate modifiers on invocations of sequence gate definitions are currently unsupported.
    #[error("gate modifiers on invocations of sequence gate definitions are currently unsupported, found {0:?}")]
    GateModifiersUnsupported(Vec<GateModifier>),
    /// Gate sequence element is a gate application where the formal qubit must be an argument. Note,
    /// this error should never occur because gate sequence elements are validated in [`DefGateSequence::try_new`].
    /// We still prefer returning an error to panicking.
    #[error("gate sequence elements must reference parameters, found {}", .0.to_quil_or_debug())]
    InvalidGateSequenceElementQubit(Qubit),
    /// Qubit variable in gate sequence is undefined. Note, this error should never occur because gate
    /// sequence elements are validated in [`DefGateSequence::try_new`]. We still prefer returning
    /// an error to panicking.
    #[error("qubit variable {0} in gate sequence is undefined")]
    UndefinedGateSequenceElementQubit(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct DefGateSequence {
    qubits: Vec<String>,
    pub(crate) gates: Vec<Gate>,
}

impl DefGateSequence {
    pub fn try_new(qubits: Vec<String>, gates: Vec<Gate>) -> Result<Self, DefGateSequenceError> {
        println!("qubits {qubits:?}");
        validate_defgate_as_sequence_elements(&gates, &qubits)?;
        Ok(Self {
            qubits,
            gates,
        })
    }

    pub(crate) fn expand(&self, gate_parameter_arguments: HashMap<String, Expression>, qubit_arguments: Vec<Qubit>) -> Result<Vec<Gate>, DefGateSequenceExpansionError> {
        if qubit_arguments.len() != self.qubits.len() {
            return Err(DefGateSequenceExpansionError::QubitCount { expected: self.qubits.len(), found: qubit_arguments.len() })
        }

        let fixed_qubit_arguments = qubit_arguments.into_iter().map(|qubit| if let Qubit::Fixed(fixed_qubit) = qubit {
            Ok(fixed_qubit)
        } else {
            Err(DefGateSequenceExpansionError::GateQubitArugment(qubit))
        }).collect::<Result<Vec<u64>, _>>()?;

        let qubit_argument_map = self.qubits.iter().zip(fixed_qubit_arguments.iter()).map(|(qubit_variable, fixed_qubit)| {
            (qubit_variable.clone(), Qubit::Fixed(*fixed_qubit))
        }).collect::<HashMap<String, Qubit>>();

        let mut gates = vec![];
        for gate in self.gates.iter() {
            let gate_parameters = gate.parameters.iter().cloned().map(|parameter| parameter.substitute_variables(&gate_parameter_arguments)).collect::<Vec<_>>();
            let gate_qubits = gate.qubits.iter().cloned().map(|qubit| {
                if let Qubit::Variable(qubit_variable) = qubit {
                    if let Some(qubit) = qubit_argument_map.get(&qubit_variable) {
                        Ok(qubit.clone())
                    } else {
                        Err(DefGateSequenceExpansionError::UndefinedGateSequenceElementQubit(qubit_variable.clone()))
                    }
                } else {
                    // Spec states that a sequence element is effectively a gate application where the formal qubit must be an argument.
                    Err(DefGateSequenceExpansionError::InvalidGateSequenceElementQubit(qubit.clone()))
                }
            }).collect::<Result<Vec<Qubit>, _>>()?;
            gates.push(Gate::new(&gate.name, gate_parameters, gate_qubits, gate.modifiers.clone()).unwrap());
        }
        Ok(gates)
    }
}

/// DEFGATE AS SEQUENCE gate elements 
fn validate_defgate_as_sequence_elements(gates: &[Gate], qubit_parameters: &[String]) -> Result<(), DefGateSequenceError> {
    let qubit_parameters = qubit_parameters.iter().cloned().collect::<HashSet<_>>();

    for (i, gate) in gates.iter().enumerate() {
        for (j, qubit) in gate.qubits.iter().enumerate() {
            if let Qubit::Variable(argument) = qubit {
                if !qubit_parameters.contains(argument) {
                    return Err( DefGateSequenceError::UndefinedGateSequenceElementQubit { gate_index: i, qubit_argument_index: j, argument_name: argument.clone() })
                }                        
            } else {
                return Err(DefGateSequenceError::InvalidGateSequenceElementQubit{
                    gate_index: i,
                    qubit_argument_index: j,
                    qubit: qubit.clone()
                })
            }
        }
    }
    Ok(())
}

/// An error that can occur when initializing a sequence gate definition.
#[derive(Debug, thiserror::Error, PartialEq, Eq, Clone)]
pub enum DefGateSequenceError {
    /// Undefined qubit argument in DEFGATE AS SEQUENCE gate.
    #[error(
        "\"{argument_name}\" is undefined at qubit argument {qubit_argument_index} of gate {gate_index}"
    )]
    UndefinedGateSequenceElementQubit {
        gate_index: usize,
        qubit_argument_index: usize,
        argument_name: String
    },
    /// DEFGATE AS SEQUENCE elements must be gates with qubit arguments.
    #[error("DEFGATE AS SEQUENCE elements must be gates with qubit arguments, found {} at qubit argument {qubit_argument_index} of gate {gate_index}", qubit.to_quil_or_debug())]
    InvalidGateSequenceElementQubit { 
        gate_index: usize,
        qubit_argument_index: usize,
        qubit: Qubit
    }
}
