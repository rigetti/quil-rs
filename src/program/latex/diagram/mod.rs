use std::{
    collections::{BTreeMap, HashSet},
    fmt,
    str::FromStr,
};

use self::{settings::RenderSettings, wire::Wire};
use super::LatexGenError;
use crate::instruction::{Gate, GateModifier, Instruction, Qubit};

pub(crate) mod settings;
pub(crate) mod wire;

/// Available commands used for building circuits with the same names taken
/// from the Quantikz documentation for easy reference. LaTeX string denoted
/// inside `backticks`.
#[derive(Clone, Debug, derive_more::Display, PartialEq, Eq, Hash)]
pub(crate) enum RenderCommand {
    /// Make a qubit "stick out" from the left.
    #[display(fmt = "\\lstick{{\\ket{{q_{{{_0}}}}}}}")]
    Lstick(u64),
    /// Make a gate on the wire.
    #[display(fmt = "\\gate{{{_0}}}")]
    Gate(String),
    /// Make a phase on the wire with a rotation
    #[display(fmt = "\\phase{{{_0}}}")]
    Phase(Parameter),
    /// Add a superscript to a gate
    #[display(fmt = "^{{\\{_0}}}")]
    Super(String),
    /// Connect the current cell to the previous cell i.e. "do nothing".
    #[display(fmt = "\\qw")]
    Qw,
    /// Start a new row
    #[display(fmt = "\\\\")]
    Nr,
    /// Make a control qubit.
    #[display(fmt = "\\ctrl{{{_0}}}")]
    Ctrl(i64),
    /// Make a target qubit.
    #[display(fmt = "\\targ{{}}")]
    Targ,
}

/// Types of parameters passed to commands.
#[derive(Clone, Debug, derive_more::Display, PartialEq, Eq, Hash)]
pub(crate) enum Parameter {
    /// Symbolic parameters
    #[display(fmt = "{_0}")]
    Symbol(Symbol),
}

/// Supported Greek and alphanumeric symbols.
#[derive(Clone, Debug, strum::EnumString, derive_more::Display, PartialEq, Eq, Hash)]
#[strum(serialize_all = "lowercase")]
pub(crate) enum Symbol {
    #[display(fmt = "\\alpha")]
    Alpha,
    #[display(fmt = "\\beta")]
    Beta,
    #[display(fmt = "\\gamma")]
    Gamma,
    #[display(fmt = "\\phi")]
    Phi,
    #[display(fmt = "\\pi")]
    Pi,
    #[display(fmt = "\\text{{{_0}}}")]
    Text(String),
}

/// Gates written in shorthand notation, i.e. composite form, that may be
/// decomposed into modifiers and single gate instructions, i.e. canonical form.
#[derive(Clone, Debug, strum::EnumString, derive_more::Display, PartialEq, Eq, Hash)]
#[strum(serialize_all = "UPPERCASE")]
enum CompositeGate {
    /// `CNOT` is `CONTROLLED X`
    #[display(fmt = "X")]
    Cnot,
    /// `CCNOT` is `CONTROLLED CONTROLLED X`
    #[display(fmt = "X")]
    Ccnot,
    /// `CPHASE` is `CONTROLLED PHASE`
    #[display(fmt = "PHASE")]
    Cphase,
    /// `CZ` is `CONTROLLED Z`
    #[display(fmt = "Z")]
    Cz,
}

/// A Diagram represents a collection of wires in a circuit. It encodes the
/// wires row in the diagram and its relationship to other wires. A row is one
/// of the wires in the circuit BTreeMap. Diagram tracks relationships between
/// wires with two pieces of information--1. the wires row (its order in the
/// BTreeMap), and 2. the column that spreads between all wires that pass
/// through a multi qubit gate, e.g. CNOT. The size of the diagram can be
/// measured by multiplying the column with the length of the circuit. This is
/// an [m x n] matrix where each element in the matrix represents an item to be
/// rendered onto the diagram using one of the Quantikz commands.
#[derive(Clone, Debug, Default)]
pub(super) struct Diagram {
    /// customizes how the diagram renders the circuit
    pub(crate) settings: RenderSettings,
    /// total number of elements on each wire
    pub(crate) column: u32,
    /// a BTreeMap of wires with the name of the wire as the key
    pub(crate) circuit: BTreeMap<u64, Box<Wire>>,
}

impl Diagram {
    /// Compares qubits from a single instruction associated with a column on
    /// the circuit to all of the qubits used in the quil program. If a qubit
    /// from the quil program is not found in the qubits in the single
    /// instruction line, then an empty slot is added to that column on the
    /// qubit wire of the circuit indicating a "do nothing" at that column.
    ///
    /// # Arguments
    /// `&mut self` - exposes the wires on the Circuit
    /// `qubits` - exposes the qubits used in the Program
    /// `instruction` - exposes the qubits in a single Instruction
    pub(crate) fn set_empty(&mut self, qubits: &HashSet<Qubit>, instruction: &Instruction) {
        if let Instruction::Gate(gate) = instruction {
            qubits
                .difference(&gate.qubits.iter().cloned().collect())
                .filter_map(|q| match q {
                    Qubit::Fixed(index) => Some(index),
                    _ => None,
                })
                .for_each(|index| {
                    self.circuit
                        .get_mut(index)
                        .map(|wire| wire.empty.insert(self.column, RenderCommand::Qw));
                });
        }
    }

    /// Iterates over the modifiers from the gate instruction and sets it as a
    /// dagger modifier of this Wire in the Circuit at the current column.
    /// Returns an Err for FORKED modifiers, and does nothing for CONTROLLED.
    ///
    /// # Arguments
    /// `wire` - an exposed wire on the Circuit
    /// `column` - the current column of the Circuit
    /// `modifiers` - the modifiers from the Gate
    fn extract_daggers(
        wire: &mut Wire,
        column: &u32,
        modifiers: &Vec<GateModifier>,
    ) -> Result<(), LatexGenError> {
        // set modifers
        for modifier in modifiers {
            match modifier {
                // return error for unsupported modifier FORKED
                GateModifier::Forked => {
                    return Err(LatexGenError::UnsupportedModifierForked);
                }
                // insert DAGGER
                GateModifier::Dagger => {
                    wire.daggers
                        .entry(*column)
                        .and_modify(|m| m.push(modifier.clone()))
                        .or_insert_with(|| vec![modifier.clone()]);
                }
                // do nothing for CONTROLLED
                _ => (),
            }
        }

        Ok(())
    }

    /// Applies a gate from an instruction to the wires on the circuit
    /// associate with the qubits from the gate. If the gate name matches a
    /// composite gate, then the composite gate is applied to the circuit,
    /// otherwise, the original gate is applied to the circuit.
    ///
    /// # Arguments
    /// `self` - exposes all attributes in the diagram
    /// `gate` - the Gate of the Instruction from `to_latex`.
    pub(crate) fn apply_gate(&mut self, gate: &Gate) -> Result<(), LatexGenError> {
        // set modifiers and parameters from gate instruction
        for qubit in &gate.qubits {
            if let Qubit::Fixed(qubit) = qubit {
                if let Some(wire) = self.circuit.get_mut(qubit) {
                    // set modifiers at this column for all qubits
                    Self::extract_daggers(wire, &self.column, &gate.modifiers)?;

                    // set parameters at this column for all qubits
                    for expression in &gate.parameters {
                        wire.set_param(
                            expression,
                            self.column,
                            self.settings.texify_numerical_constants,
                        );
                    }
                }
            }
        }

        // get display of gate name from composite gate or original gate
        let canonical_gate = CompositeGate::from_str(&gate.name)
            .map(|g| g.to_string())
            .unwrap_or(gate.name.clone());

        // get the names of the qubits in the circuit before circuit is borrowed as mutable
        let circuit_qubits: Vec<u64> = self.circuit.keys().cloned().collect();

        // set gate for each qubit in the instruction
        for qubit in &gate.qubits {
            if let Qubit::Fixed(instruction_qubit) = qubit {
                if let Some(wire) = self.circuit.get_mut(instruction_qubit) {
                    // set the control and target qubits
                    if gate.qubits.len() > 1 || canonical_gate == "PHASE" {
                        // set the target qubit if the qubit is equal to the last qubit in gate
                        if qubit == gate.qubits.last().unwrap() {
                            wire.set_targ(&self.column);
                        // otherwise, set the control qubit
                        } else {
                            wire.set_ctrl(
                                &self.column,
                                qubit,
                                gate.qubits.last().unwrap(),
                                &circuit_qubits,
                            );
                        }
                    } else if wire.parameters.get(&self.column).is_some() {
                        // parameterized single qubit gates are unsupported
                        return Err(LatexGenError::UnsupportedGate {
                            gate: gate.name.clone(),
                        });
                    }

                    // set modifiers at this column for all qubits
                    wire.gates.insert(self.column, canonical_gate.clone());
                }
            }
        }

        Ok(())
    }
}

impl fmt::Display for Diagram {
    /// Returns a result containing the Diagram Circuit as LaTeX string which
    /// can be input into the body of the Document.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // add a newline between the first line and the header
        writeln!(f)?;

        let mut i = 0; // used to omit trailing Nr

        // write the LaTeX string for each wire in the circuit
        for key in self.circuit.keys() {
            // are labels on in settings?
            if self.settings.label_qubit_lines {
                // add label to left side of wire
                write!(f, "{}", RenderCommand::Lstick(*key))?;
            } else {
                // add qw buffer to first column
                write!(f, "{}", RenderCommand::Qw)?;
            }

            // convert each column in the wire to string
            if let Some(wire) = self.circuit.get(key) {
                for c in 0..self.column {
                    if let Some(gate) = wire.gates.get(&c) {
                        write!(f, " & ")?;

                        let mut superscript = String::new();
                        // attach modifiers to gate name if any
                        if let Some(modifiers) = wire.daggers.get(&c) {
                            for modifier in modifiers {
                                if let GateModifier::Dagger = modifier {
                                    superscript.push_str(
                                        &RenderCommand::Super(String::from("dagger")).to_string(),
                                    );
                                }
                            }
                        }

                        if wire.ctrl.get(&c).is_some() {
                            // CONTROLLED qubits are displayed as `\ctrl{targ}`
                            if let Some(targ) = wire.ctrl.get(&c) {
                                write!(f, "{}", &(RenderCommand::Ctrl(*targ)))?;
                            }
                            continue;
                        } else if wire.targ.get(&c).is_some() {
                            // CONTROLLED X gates are displayed as `\targ{}`
                            if gate == "X" {
                                // set the qubit at this column as the target

                                let mut _gate = gate.clone();

                                // if the gate contains daggers, display target as X gate with dagger superscripts
                                if !superscript.is_empty() {
                                    _gate.push_str(&superscript);
                                    write!(f, "{}", &RenderCommand::Gate(_gate))?;
                                // else display X target as an open dot
                                } else {
                                    write!(f, "{}", &RenderCommand::Targ)?;
                                }
                                continue;
                            // PHASE gates are displayed as `\phase{param}`
                            } else if gate == "PHASE" {
                                // set the phase parameters
                                if let Some(parameters) = wire.parameters.get(&c) {
                                    for param in parameters {
                                        write!(f, "{}", &RenderCommand::Phase(param.clone()))?;
                                    }
                                }
                                continue;
                            }
                        }
                        // all other gates display as `\gate{name}`
                        let mut _gate = gate.clone();

                        // concatenate superscripts
                        _gate.push_str(&superscript);

                        write!(f, "{}", &RenderCommand::Gate(_gate))?;
                    } else if wire.empty.get(&c).is_some() {
                        // chain an empty column qw to the end of the line
                        write!(f, " & ")?;
                        write!(f, "{}", &RenderCommand::Qw)?;
                    }
                }
            }

            // chain an empty column qw to the end of the line
            write!(f, " & ")?;
            write!(f, "{}", &RenderCommand::Qw)?;

            // if this is the last key iteration, omit Nr from end of line
            if i < self.circuit.len() - 1 {
                // indicate a new row
                write!(f, " ")?;
                write!(f, "{}", &RenderCommand::Nr)?;
                i += 1;
            }

            // add a newline between each new line or the footer
            writeln!(f)?;
        }

        Ok(())
    }
}
