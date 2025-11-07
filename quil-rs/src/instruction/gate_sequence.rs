use std::collections::{HashMap, HashSet};

use crate::{
    expression::Expression,
    instruction::{Gate, GateModifier, Qubit},
    pickleable_new,
    quil::Quil,
};

/// An error that can occur when expanding a gate that has a sequence gate definition.
#[derive(Debug, thiserror::Error, PartialEq, Eq, Clone)]
pub enum DefGateSequenceExpansionError {
    #[error("gate sequence expected {expected} arguments, found {found}")]
    ParameterCount { expected: usize, found: usize },
    #[error("cyclic sequence gate definition detected: {0:?}")]
    CyclicSequenceGateDefinition(Vec<String>),
    #[error(
        "SEQUENCE gate expected to be applied to {expected} qubit{}, \
         but was applied to {found}",
        if *expected == 1 { "" } else { "s" },
    )]
    QubitCount { expected: usize, found: usize },
    #[error("expected fixed qubit argument, found {}", .0.to_quil_or_debug())]
    NonFixedQubitArgument(Qubit),
    #[error(
        "gate modifiers on invocations of sequence gate definitions are currently unsupported, \
         but found {0:?}"
    )]
    GateModifiersUnsupported(Vec<GateModifier>),
    /// Gate sequence element is a gate application where the formal qubit must be an argument. Note,
    /// this error should never occur because gate sequence elements are validated in [`DefGateSequence::try_new`].
    /// We still prefer returning an error to panicking.
    #[error("gate sequence elements must reference parameters, but found {}", .0.to_quil_or_debug())]
    InvalidGateSequenceElementQubit(Qubit),
    /// Qubit variable in gate sequence is undefined. Note, this error should never occur because gate
    /// sequence elements are validated in [`DefGateSequence::try_new`]. We still prefer returning
    /// an error to panicking.
    #[error("qubit variable {0} in gate sequence is undefined")]
    UndefinedGateSequenceElementQubit(String),
}

/// A sequence of gates that make up a defined gate (i.e. with `DEFGATE ... AS SEQUENCE`).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "stubs", pyo3_stub_gen::derive::gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.instructions", eq, frozen, hash, get_all, subclass)
)]
pub struct DefGateSequence {
    /// The list of qubit variable names in the gate signature.
    pub(crate) qubits: Vec<String>,
    /// The list of `Gate` objects that make up the sequence.
    pub(crate) gates: Vec<Gate>,
}

pickleable_new! {
    impl DefGateSequence {
        /// Creates a new `DefGateSequence` with the given qubits and gates.
        ///
        /// `qubits` should be a list of qubit names that the gates in the sequence will act on.
        /// `gates` should be a list of `Gate` objects that make up the sequence.
        /// Each gate must reference qubits in the `qubits` list by name.
        /// They may not specify a fixed qubit.
        pub fn try_new(qubits: Vec<String>, gates: Vec<Gate>) -> Result<DefGateSequence, DefGateSequenceError> {
            let (gates, qubits) = validate_defgate_as_sequence_elements(gates, qubits)?;
            Ok(Self { qubits, gates })
        }
    }
}

impl DefGateSequence {
    pub(crate) fn expand(
        &self,
        gate_parameter_arguments: HashMap<String, Expression>,
        qubit_arguments: Vec<Qubit>,
    ) -> Result<Vec<Gate>, DefGateSequenceExpansionError> {
        if qubit_arguments.len() != self.qubits.len() {
            return Err(DefGateSequenceExpansionError::QubitCount {
                expected: self.qubits.len(),
                found: qubit_arguments.len(),
            });
        }

        let fixed_qubit_arguments = qubit_arguments
            .into_iter()
            .map(|qubit| {
                if let Qubit::Fixed(fixed_qubit) = qubit {
                    Ok(fixed_qubit)
                } else {
                    Err(DefGateSequenceExpansionError::NonFixedQubitArgument(qubit))
                }
            })
            .collect::<Result<Vec<u64>, _>>()?;

        let qubit_argument_map: HashMap<_, _> = self
            .qubits
            .iter()
            .zip(fixed_qubit_arguments.iter())
            .map(|(qubit_variable, fixed_qubit)| {
                (qubit_variable.clone(), Qubit::Fixed(*fixed_qubit))
            })
            .collect();

        self.gates
            .iter()
            .map(|gate| {
                let gate_parameters = gate
                    .parameters
                    .iter()
                    .map(|parameter| parameter.substitute_variables(&gate_parameter_arguments))
                    .collect::<Vec<_>>();

                let gate_qubits = gate
                    .qubits
                    .iter()
                    .map(|qubit| {
                        if let Qubit::Variable(qubit_variable) = qubit {
                            qubit_argument_map.get(qubit_variable)
                                .map_or_else(||
                                    Err(DefGateSequenceExpansionError::UndefinedGateSequenceElementQubit(qubit_variable.clone())),
                                    |qubit| Ok(qubit.clone())
                                )
                        } else {
                            // Spec states that a sequence element is effectively a gate application where the formal qubit must be an argument.
                            Err(DefGateSequenceExpansionError::InvalidGateSequenceElementQubit(qubit.clone()))
                        }
                    })
                    .collect::<Result<Vec<Qubit>, _>>()?;

                // Note, we don't use `Gate::new` here because the gate name is already validated,
                // and we've performed all necessary validations on the gate parameters and qubits.
                Ok(Gate {
                    name: gate.name.clone(),
                    parameters: gate_parameters,
                    qubits: gate_qubits,
                    modifiers: gate.modifiers.clone(),
                })
            })
            .collect()
    }
}

fn validate_defgate_as_sequence_elements(
    gates: Vec<Gate>,
    qubit_parameters: Vec<String>,
) -> Result<(Vec<Gate>, Vec<String>), DefGateSequenceError> {
    if qubit_parameters.is_empty() {
        return Err(DefGateSequenceError::AtLeastOneQubitParameterRequired);
    }
    let qubit_parameter_set: HashSet<_> = qubit_parameters.iter().collect();

    gates.iter().enumerate().try_for_each(|(i, gate)| {
        gate.qubits.iter().enumerate().try_for_each(|(j, qubit)| {
            if let Qubit::Variable(argument) = qubit {
                if qubit_parameter_set.contains(argument) {
                    Ok(())
                } else {
                    Err(DefGateSequenceError::UndefinedGateSequenceElementQubit {
                        gate_index: i,
                        qubit_argument_index: j,
                        argument_name: argument.clone(),
                    })
                }
            } else {
                Err(DefGateSequenceError::InvalidGateSequenceElementQubit {
                    gate_index: i,
                    qubit_argument_index: j,
                    qubit: qubit.clone(),
                })
            }
        })
    })?;
    Ok((gates, qubit_parameters))
}

/// An error that can occur when initializing a sequence gate definition.
#[derive(Debug, thiserror::Error, PartialEq, Eq, Clone)]
pub enum DefGateSequenceError {
    #[error(
        "\"{argument_name}\" is undefined at qubit argument {qubit_argument_index} of gate {gate_index}"
    )]
    UndefinedGateSequenceElementQubit {
        gate_index: usize,
        qubit_argument_index: usize,
        argument_name: String,
    },
    #[error("DEFGATE AS SEQUENCE elements must be gates with qubit arguments, found {} at qubit argument {qubit_argument_index} of gate {gate_index}", qubit.to_quil_or_debug())]
    InvalidGateSequenceElementQubit {
        gate_index: usize,
        qubit_argument_index: usize,
        qubit: Qubit,
    },
    #[error("DEFGATE AS SEQUENCE must have at least one qubit parameter")]
    AtLeastOneQubitParameterRequired,
}
