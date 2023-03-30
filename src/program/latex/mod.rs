//! LaTeX circuit generation for quil programs.
//!
//! This module enables generating quantum circuits using the LaTeX subpackage
//! TikZ/[`Quantikz`] for a given quil [`Program`]. This feature is callable on
//! [`Program`] and returns a LaTeX string which can be rendered in a LaTeX
//! visualization tool. Be aware that not all Programs can be serialized as
//! LaTeX. If a [`Program`] contains a gate or modifier not mentioned in the
//! [Supported Gates and Modifiers](#supported-gates-and-modifiers) section
//! below, an error will be returned detailing which instruction or gate is
//! unsupported in the Program being processed.
//!
//! # Supported Gates and Modifiers
//!
//!   - Pauli Gates:           `I`, `X`, `Y`, `Z`
//!   - Hadamard Gate:         `H`
//!   - Phase Gate:            `PHASE`, `S`, `T`
//!   - Controlled Phase Gate: `CZ`, `CPHASE`
//!   - Controlled X Gates:    `CNOT`, `CCNOT`
//!   - User-Defined Gates:    `DEFGATE`
//!   - Modifiers:             `CONTROLLED`, `DAGGER`
//!
//! [`Quantikz`]: https://arxiv.org/pdf/1809.03842.pdf

use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::Display;
use std::str::FromStr;

use crate::expression::Expression;
use crate::instruction::{Gate, GateModifier, Instruction, Qubit};
use crate::Program;

/// Available commands used for building circuits with the same names taken
/// from the Quantikz documentation for easy reference. LaTeX string denoted
/// inside `backticks`.
#[derive(Clone, Debug, derive_more::Display, PartialEq, Eq, Hash)]
enum RenderCommand {
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
    /// Make a control qubit--different from Control.
    #[display(fmt = "\\ctrl{{{_0}}}")]
    Ctrl(i64),
    /// Make a controlled-not gate.
    #[display(fmt = "\\targ{{}}")]
    Targ,
}

/// Types of parameters passed to commands.
#[derive(Clone, Debug, derive_more::Display, PartialEq, Eq, Hash)]
enum Parameter {
    /// Symbolic parameters
    #[display(fmt = "{_0}")]
    Symbol(Symbol),
}

/// Supported Greek and alphanumeric symbols.
#[derive(Clone, Debug, derive_more::Display, PartialEq, Eq, Hash)]
enum Symbol {
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

impl FromStr for Symbol {
    type Err = std::convert::Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "alpha" => Ok(Self::Alpha),
            "beta" => Ok(Self::Beta),
            "gamma" => Ok(Self::Gamma),
            "phi" => Ok(Self::Phi),
            "pi" => Ok(Self::Pi),
            _ => Ok(Self::Text(s.to_string())),
        }
    }
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
    /// gate is in canonical form
    None,
}

/// RenderSettings contains the metadata that allows the user to customize how
/// the circuit is rendered or use the default implementation.
#[derive(Clone, Copy, Debug)]
pub struct RenderSettings {
    /// Convert numerical constants, e.g. pi, to LaTeX form.
    pub texify_numerical_constants: bool,
    /// Include all qubits implicitly referenced in the Quil program.
    pub impute_missing_qubits: bool,
    /// Label qubit lines.
    pub label_qubit_lines: bool,
    /// Write controlled rotations in compact form.
    pub abbreviate_controlled_rotations: bool,
    /// Extend the length of open wires at the right of the diagram.
    pub qubit_line_open_wire_length: u32,
    /// Align measurement operations to appear at the end of the diagram.
    pub right_align_terminal_measurements: bool,
}

impl Default for RenderSettings {
    /// Returns the default RenderSettings.
    fn default() -> Self {
        Self {
            /// false: pi is π.
            texify_numerical_constants: true,
            /// true: `CNOT 0 2` would have three qubit lines: 0, 1, 2.
            impute_missing_qubits: false,
            /// false: remove Lstick/Rstick from latex.
            label_qubit_lines: true,
            /// true: `RX(pi)` displayed as `X_{\\pi}` instead of `R_X(\\pi)`.
            abbreviate_controlled_rotations: false,
            /// 0: condenses the size of subdiagrams.
            qubit_line_open_wire_length: 1,
            /// false: include Meter in the current column.
            right_align_terminal_measurements: true,
        }
    }
}

impl RenderSettings {
    /// Adds missing qubits between the first qubit and last qubit in a
    /// diagram's circuit. If a missing qubit is found, a new wire is created
    /// and pushed to the diagram's circuit.
    ///
    ///  # Arguments
    /// `last_column` - total number of instructions from Program
    /// `circuit` - the circuit of the diagram
    ///
    /// # Examples
    /// ```
    /// use quil_rs::{Program, program::latex::RenderSettings};
    /// use std::str::FromStr;
    /// let program = Program::from_str("H 0\nCNOT 0 1").expect("");
    /// let settings = RenderSettings {
    ///     impute_missing_qubits: true,
    ///     ..Default::default()
    /// };
    /// program.to_latex(settings).expect("");
    /// ```
    fn impute_missing_qubits(last_column: u32, circuit: &mut BTreeMap<u64, Box<Wire>>) {
        let mut keys_iter = circuit.keys();

        // get the first qubit in the BTreeMap
        let Some(first) = keys_iter
            .next()
            .map(|wire| wire + 1) else { return; };

        // get the last qubit in the BTreeMap
        let Some(last) = keys_iter
            .last()
            .map(|wire| wire - 1) else { return; };

        // search through the range of qubits
        for qubit in first..=last {
            // if the qubit is not found impute it
            circuit.entry(qubit).or_insert_with(|| {
                let mut wire = Wire {
                    ..Default::default()
                };

                // insert empties based on total number of columns
                for c in 0..=last_column {
                    wire.empty.insert(c, RenderCommand::Qw);
                }

                Box::new(wire)
            });
        }
    }
}

/// The structure of a LaTeX document. Typically a LaTeX document contains
/// metadata defining the setup and packages used in a document within a header
/// and footer while the body contains content and controls its presentation.
struct Document {
    header: String,
    body: String,
    footer: String,
}

impl Default for Document {
    fn default() -> Self {
        Self {
            header: r"\documentclass[convert={density=300,outext=.png}]{standalone}
\usepackage[margin=1in]{geometry}
\usepackage{tikz}
\usetikzlibrary{quantikz}
\begin{document}
\begin{tikzcd}"
                .to_string(),
            body: String::new(),
            footer: r"\end{tikzcd}
\end{document}"
                .to_string(),
        }
    }
}

impl Display for Document {
    /// Returns the entire document in LaTeX string.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}{}", self.header, self.body, self.footer)
    }
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
struct Diagram {
    /// customizes how the diagram renders the circuit
    settings: RenderSettings,
    /// total number of elements on each wire
    column: u32,
    /// a BTreeMap of wires with the name of the wire as the key
    circuit: BTreeMap<u64, Box<Wire>>,
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
    fn set_empty(&mut self, qubits: &HashSet<Qubit>, instruction: &Instruction) {
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

    /// Utility function to insert modifiers of wires in this Circuit at the
    /// current column. Returns an Err for unsupported modifiers.
    ///
    /// # Arguments
    /// `wire` - an exposed wire on the Circuit
    /// `column` - the current column of the Circuit
    /// `modifiers` - the modifiers from the Gate
    fn set_modifiers(
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
                // insert for CONTROLLED and DAGGER
                GateModifier::Controlled | GateModifier::Dagger => {
                    wire.modifiers
                        .entry(*column)
                        .and_modify(|m| m.push(modifier.clone()))
                        .or_insert_with(|| vec![modifier.clone()]);
                }
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
    fn apply_gate(&mut self, gate: &Gate) -> Result<(), LatexGenError> {
        // set modifiers and parameters from gate instruction
        for qubit in &gate.qubits {
            if let Qubit::Fixed(qubit) = qubit {
                if let Some(wire) = self.circuit.get_mut(qubit) {
                    // set modifiers at this column for all qubits
                    Self::set_modifiers(wire, &self.column, &gate.modifiers)?;

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

impl Display for Diagram {
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

                        let mut superscript = String::from("");
                        // attach modifiers to gate name if any
                        if let Some(modifiers) = wire.modifiers.get(&c) {
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

/// A Wire represents a single qubit. A wire only needs to keep track of all
/// the elements it contains mapped to some arbitrary column. Diagram keeps
/// track of where the Wire belongs in the larger circuit, its row, and knows
/// how each Wire relates to each other at that column. When Diagram parses the
/// wires as a collection, if the Wire relates to another at some column, then
/// its field will be updated at that column based on the knowledge Diagram has
/// about this connection. This updated value also looks arbitrary to Wire, it
/// does not explicitly define which qubit it relates to, but a digit that
/// describes how far away it is from the related qubit based on Quantikz.
#[derive(Clone, Debug, Default)]
struct Wire {
    /// the Gates on the wire callable by the column
    gates: HashMap<u32, String>,
    /// at this column the wire is a control
    ctrl: HashMap<u32, i64>,
    /// at this column is the wire a target?
    targ: HashMap<u32, bool>,
    /// the Parameters on the wire callable by the column
    parameters: HashMap<u32, Vec<Parameter>>,
    /// the Modifiers on the wire callable by the column
    modifiers: HashMap<u32, Vec<GateModifier>>,
    /// empty column
    empty: HashMap<u32, RenderCommand>,
}

impl Wire {
    /// Retrieves a gate's parameters from Expression and matches them with its
    /// symbolic definition which is then stored into wire at the specific
    /// column.
    ///
    /// # Arguments
    /// `&mut self` - exposes the Wire's parameters at this column
    /// `expression` - expression from Program to get name of Parameter
    /// `column` - the column taking the parameters
    /// `texify` - is texify_numerical_constants setting on?
    fn set_param(&mut self, expression: &Expression, column: u32, texify: bool) {
        // get the name of the supported expression
        let text = match expression {
            Expression::Address(mr) => mr.name.to_string(),
            Expression::Number(c) => c.re.to_string(),
            expression => expression.to_string(),
        };

        // if texify_numerical_constants
        let param = if texify {
            // get the matching symbol from text
            vec![Parameter::Symbol(text.parse().unwrap())]
        } else {
            // set the symbol as text
            vec![Parameter::Symbol(Symbol::Text(text))]
        };

        self.parameters.insert(column, param);
    }

    /// Set target qubit at this column.
    ///
    /// # Arguments
    /// `&mut self` - exposes the Wire's targ at this column
    /// `column` - the column taking the target
    fn set_targ(&mut self, column: &u32) {
        self.targ.insert(*column, true);
    }

    /// Set control qubit at this column at some distance from the target. The
    /// distance is determined by the relative position of the control and
    /// target qubits in the circuit.
    ///
    /// # Arguments
    /// `&mut self` - exposes the Wire's ctrl at this column
    /// `column` - the column taking the control
    /// `ctrl` - the control qubit
    /// `targ` - the target qubit
    /// `circuit_qubits` - the qubits in the circuit
    fn set_ctrl(&mut self, column: &u32, ctrl: &Qubit, targ: &Qubit, circuit_qubits: &[u64]) {
        if let Qubit::Fixed(ctrl) = ctrl {
            if let Qubit::Fixed(targ) = targ {
                // get the index of the control and target qubits
                let ctrl_index = circuit_qubits.iter().position(|&x| x == *ctrl);
                let targ_index = circuit_qubits.iter().position(|&x| x == *targ);

                // if the control and target qubits are found
                if let Some(ctrl_index) = ctrl_index {
                    if let Some(targ_index) = targ_index {
                        self.ctrl
                            .insert(*column, targ_index as i64 - ctrl_index as i64);
                    }
                }
            }
        }
    }
}

#[derive(Clone, Debug, thiserror::Error, PartialEq, Eq, Hash)]
pub enum LatexGenError {
    #[error("Found a target qubit with no control qubit.")]
    FoundTargetWithNoControl,
    #[error("The FORKED modifier is unsupported.")]
    UnsupportedModifierForked,
    #[error("This instruction is unsupported: {instruction}.")]
    UnsupportedInstruction { instruction: String },
    #[error("This gate is unsupported: {gate}.")]
    UnsupportedGate { gate: String },
}

impl Program {
    /// Returns a Result containing a quil [`Program`] as a LaTeX string or a
    /// [`LatexGenError`].
    ///
    /// This implementation of Latex can be viewed as a self-contained partial
    /// implementation of ``Quantikz`` with all available commands listed as
    /// variants in a Command enum. View ``Quantikz`` documentation for more
    /// information.
    ///
    /// # Arguments
    /// `settings` - Customizes the rendering of a circuit.
    ///
    /// # Examples
    /// ```
    /// // To LaTeX for the Bell State Program.
    /// use quil_rs::{Program, program::latex::RenderSettings};
    /// use std::str::FromStr;
    /// let program = Program::from_str("H 0\nCNOT 0 1").expect("");
    /// let latex = program.to_latex(RenderSettings::default()).expect("");
    /// ```
    ///
    /// ```
    /// // To LaTeX for the Toffoli Gate Program.
    /// use quil_rs::{Program, program::latex::RenderSettings};
    /// use std::str::FromStr;
    /// let program = Program::from_str("CONTROLLED CNOT 2 1 0").expect("");
    /// let latex = program.to_latex(RenderSettings::default()).expect("");
    /// ```
    pub fn to_latex(&self, settings: RenderSettings) -> Result<String, LatexGenError> {
        // get a reference to the current program
        let instructions = self.to_instructions(false);

        // initialize a new diagram
        let mut diagram = Diagram {
            settings,
            ..Default::default()
        };

        // initialize circuit with empty wires of all qubits in program
        let qubits = Program::get_used_qubits(self);
        for qubit in &qubits {
            if let Qubit::Fixed(name) = qubit {
                let wire = Wire::default();
                diagram.circuit.insert(*name, Box::new(wire));
            }
        }

        // are implicit qubits required in settings and are there at least two or more qubits in the diagram?
        if diagram.settings.impute_missing_qubits {
            // add implicit qubits to circuit
            RenderSettings::impute_missing_qubits(instructions.len() as u32, &mut diagram.circuit);
        }

        for instruction in instructions {
            // set QW for any unused qubits in this instruction
            diagram.set_empty(&qubits, &instruction);

            // parse gate instructions into a new circuit
            if let Instruction::Gate(gate) = instruction {
                // if there are any duplicate qubits in the gate return an error
                if gate.qubits.len()
                    != gate
                        .qubits
                        .iter()
                        .cloned()
                        .collect::<HashSet<Qubit>>()
                        .len()
                {
                    return Err(LatexGenError::FoundTargetWithNoControl);
                }

                diagram.apply_gate(&gate)?;
                diagram.column += 1;
            } else if let Instruction::GateDefinition(_) = instruction {
                // GateDefinition is supported but inserted into the circuit using its Gate instruction form
                continue;
            } else {
                return Err(LatexGenError::UnsupportedInstruction {
                    instruction: instruction.to_string(),
                });
            }
        }

        let body = diagram.to_string();
        let document = Document {
            body,
            ..Default::default()
        };
        Ok(document.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::{FromStr, Program, RenderSettings};

    /// Helper function takes instructions and return the LaTeX using the
    /// Latex::to_latex method.
    fn get_latex(instructions: &str, settings: RenderSettings) -> String {
        let program = Program::from_str(instructions).expect("Program should be returned");
        program
            .to_latex(settings)
            .expect("Program conversion to LaTeX should succeed")
    }

    #[test]
    /// Test functionality of to_latex using default settings.
    fn test_to_latex() {
        let latex = get_latex(
            "H 0\nCNOT 0 1",
            RenderSettings {
                impute_missing_qubits: true,
                ..Default::default()
            },
        );

        println!("{latex}");
    }

    /// Test module for the Document
    mod document {
        use crate::program::latex::{tests::get_latex, Document, RenderSettings};

        #[test]
        fn test_template() {
            insta::assert_snapshot!(get_latex("", RenderSettings::default()));
        }

        #[test]
        fn test_header() {
            let document = Document::default();
            insta::assert_snapshot!(document.header);
        }

        #[test]
        fn test_body_default() {
            let document = Document::default();
            insta::assert_snapshot!(document.body);
        }

        #[test]
        fn test_footer() {
            let document = Document::default();
            insta::assert_snapshot!(document.footer);
        }
    }

    /// Test module for gates
    mod gates {
        use crate::program::latex::{tests::get_latex, RenderSettings};

        #[test]
        fn test_gate_x() {
            insta::assert_snapshot!(get_latex("X 0", RenderSettings::default()));
        }

        #[test]
        fn test_gate_y() {
            insta::assert_snapshot!(get_latex("Y 1", RenderSettings::default()));
        }

        #[test]
        fn test_gates_x_and_y_single_qubit() {
            insta::assert_snapshot!(get_latex("X 0\nY 0", RenderSettings::default()));
        }

        #[test]
        fn test_gates_x_and_y_two_qubits() {
            insta::assert_snapshot!(get_latex("X 0\nY 1", RenderSettings::default()));
        }

        #[test]
        fn test_gates_phase_pi_rotation() {
            insta::assert_snapshot!(get_latex("PHASE(pi) 0", RenderSettings::default()));
        }

        #[test]
        fn test_gates_cnot_ctrl_0_targ_1() {
            insta::assert_snapshot!(get_latex("CNOT 0 1", RenderSettings::default()));
        }

        #[test]
        fn test_gates_cnot_ctrl_1_targ_0() {
            insta::assert_snapshot!(get_latex("CNOT 1 0", RenderSettings::default()));
        }

        #[test]
        #[should_panic]
        fn test_gates_cnot_error_single_qubit() {
            get_latex("CNOT 0 0", RenderSettings::default());
        }

        #[test]
        fn test_gates_h_and_cnot_ctrl_0_targ_1() {
            insta::assert_snapshot!(get_latex("H 0\nCNOT 0 1", RenderSettings::default()));
        }

        #[test]
        fn test_gates_h_and_cnot_ctrl_1_targ_0() {
            insta::assert_snapshot!(get_latex("H 1\nCNOT 1 0", RenderSettings::default()));
        }

        #[test]
        fn test_gate_toffoli() {
            insta::assert_snapshot!(get_latex("CCNOT 1 2 0", RenderSettings::default()));
        }

        #[test]
        fn test_gate_ccnot_and_controlled_cnot_equality() {
            let ccnot = get_latex("CCNOT 1 2 0", RenderSettings::default());

            let controlled = get_latex("CONTROLLED CNOT 1 2 0", RenderSettings::default());

            assert_eq!(ccnot, controlled);
        }

        #[test]
        fn test_gate_cphase() {
            insta::assert_snapshot!(get_latex("CPHASE(pi) 0 1", RenderSettings::default()));
        }

        #[test]
        fn test_gate_cz() {
            insta::assert_snapshot!(get_latex("CZ 0 1", RenderSettings::default()));
        }
    }

    /// Test module for modifiers
    mod modifiers {
        use crate::program::latex::{tests::get_latex, RenderSettings};

        #[test]
        fn test_modifier_toffoli_gate() {
            insta::assert_snapshot!(get_latex(
                "CONTROLLED CNOT 2 1 0",
                RenderSettings::default()
            ));
        }

        #[test]
        fn test_modifier_controlled_cnot_and_ccnot_equality() {
            let controlled = get_latex("CONTROLLED CNOT 2 1 0", RenderSettings::default());

            let ccnot = get_latex("CCNOT 2 1 0", RenderSettings::default());

            assert_eq!(controlled, ccnot);
        }

        #[test]
        fn test_modifier_dagger() {
            insta::assert_snapshot!(get_latex("DAGGER X 0", RenderSettings::default()));
        }

        #[test]
        fn test_modifier_dagger_dagger() {
            insta::assert_snapshot!(get_latex("DAGGER DAGGER Y 0", RenderSettings::default()));
        }

        #[test]
        fn test_modifier_dagger_cz() {
            insta::assert_snapshot!(get_latex("DAGGER CZ 0 1", RenderSettings::default()));
        }
    }

    /// Test module for Quantikz Commands
    mod commands {
        use crate::program::latex::{Parameter, RenderCommand, Symbol};

        #[test]
        fn test_command_left_ket() {
            insta::assert_snapshot!(RenderCommand::Lstick(0).to_string());
        }

        #[test]
        fn test_command_gate() {
            insta::assert_snapshot!(RenderCommand::Gate("X".to_string()).to_string());
        }

        #[test]
        fn test_command_phase() {
            insta::assert_snapshot!(RenderCommand::Phase(Parameter::Symbol(Symbol::Pi)).to_string());
        }

        #[test]
        fn test_command_super() {
            insta::assert_snapshot!(RenderCommand::Super("dagger".to_string()).to_string());
        }

        #[test]
        fn test_command_qw() {
            insta::assert_snapshot!(RenderCommand::Qw.to_string());
        }

        #[test]
        fn test_command_nr() {
            insta::assert_snapshot!(RenderCommand::Nr.to_string());
        }

        #[test]
        fn test_command_control() {
            insta::assert_snapshot!(RenderCommand::Ctrl(0).to_string());
        }

        #[test]
        fn test_command_cnot_target() {
            insta::assert_snapshot!(RenderCommand::Targ.to_string());
        }
    }

    /// Test module for RenderSettings
    mod settings {
        use crate::program::latex::{tests::get_latex, RenderSettings};

        #[test]
        fn test_settings_texify_numerical_constants_true_supported_symbol() {
            // default texify_numerical_constants is true
            let settings = RenderSettings {
                ..Default::default()
            };
            insta::assert_snapshot!(get_latex("CPHASE(alpha) 0 1", settings));
        }

        #[test]
        fn test_settings_texify_numerical_constants_false_supported_symbol() {
            let settings = RenderSettings {
                texify_numerical_constants: false,
                ..Default::default()
            };
            insta::assert_snapshot!(get_latex("CPHASE(alpha) 0 1", settings));
        }

        #[test]
        fn test_settings_texify_numerical_constants_unsupported_symbol() {
            // default texify_numerical_constants is true
            let settings_true = RenderSettings {
                ..Default::default()
            };

            let unsupported_true = get_latex("CPHASE(chi) 0 1", settings_true);

            let settings_false = RenderSettings {
                texify_numerical_constants: false,
                ..Default::default()
            };

            let unsupported_false = get_latex("CPHASE(chi) 0 1", settings_false);

            // unsupported symbols are treated as text regardless of setting
            assert_eq!(unsupported_true, unsupported_false);
        }

        #[test]
        fn test_settings_label_qubit_lines_false() {
            let settings = RenderSettings {
                label_qubit_lines: false,
                ..Default::default()
            };
            insta::assert_snapshot!(get_latex("H 0\nCNOT 0 1", settings));
        }

        #[test]
        fn test_settings_impute_missing_qubits_true_ctrl_less_than_targ() {
            let settings = RenderSettings {
                impute_missing_qubits: true,
                ..Default::default()
            };
            insta::assert_snapshot!(get_latex("H 0\nCNOT 0 3", settings));
        }

        #[test]
        fn test_settings_impute_missing_qubits_true_ctrl_greater_than_targ() {
            let settings = RenderSettings {
                impute_missing_qubits: true,
                ..Default::default()
            };
            insta::assert_snapshot!(get_latex("H 5\nCNOT 5 2", settings));
        }
    }

    /// Test various programs for LaTeX accuracy
    mod programs {
        use crate::program::latex::{tests::get_latex, RenderSettings};

        #[test]
        fn test_program_h0_cnot01_x1_cnot12() {
            let settings = RenderSettings {
                ..Default::default()
            };
            insta::assert_snapshot!(get_latex("H 0\nCNOT 0 1\nX 1\nCNOT 1 2", settings));
        }

        #[test]
        fn test_program_h5_cnot52_y2_cnot23() {
            let settings = RenderSettings {
                ..Default::default()
            };
            insta::assert_snapshot!(get_latex("H 5\nCNOT 5 2\nY 2\nCNOT 2 3", settings));
        }

        #[test]
        fn test_program_good_modifiers() {
            let latex = get_latex(
                r#"Y 0
CONTROLLED Y 0 1
CONTROLLED CONTROLLED Y 0 1 2
CONTROLLED CONTROLLED CONTROLLED Y 0 1 2 3

DAGGER Y 0
DAGGER DAGGER Y 0
DAGGER DAGGER DAGGER Y 0

CONTROLLED DAGGER Y 0 1
CONTROLLED DAGGER CONTROLLED Y 0 1 2
CONTROLLED DAGGER CONTROLLED DAGGER Y 0 1 2

DEFGATE G:
    1, 0
    0, 1

CONTROLLED G 0 1
DAGGER G 0"#,
                RenderSettings::default(),
            );

            insta::assert_snapshot!(latex);
        }

        #[test]
        fn test_program_good_simple_params() {
            insta::assert_snapshot!(get_latex(
                "CPHASE(1.0) 0 1\nCPHASE(1.0-2.0i) 1 0",
                RenderSettings::default()
            ));
        }

        #[test]
        fn test_program_good_complex_params() {
            insta::assert_snapshot!(get_latex(
                "CPHASE(pi/2) 1 0\nCPHASE(cos(sin(2*pi/3))*cis(-1)*exp(i*pi)) 3 4",
                RenderSettings::default()
            ));
        }

        #[test]
        fn test_program_good_basic_defgate() {
            let latex = get_latex(
                r#"DEFGATE H0:
    0.707, 0.707
    0.707, -0.707

DEFGATE H1:
    1/sqrt(2), 1/sqrt(2)
    1/sqrt(2), -1/sqrt(2)

H0 0
H1 1"#,
                RenderSettings::default(),
            );

            insta::assert_snapshot!(latex);
        }

        #[test]
        fn test_program_good_defgate_with_long_name() {
            let latex = get_latex(
                r#"DEFGATE ______________________________________ugly-python-convention______________________________________:
    1, 0
    0, 1
    
______________________________________ugly-python-convention______________________________________ 0
______________________________________ugly-python-convention______________________________________ 1"#,
                RenderSettings::default(),
            );

            insta::assert_snapshot!(latex);
        }
    }

    /// Test module for unsupported programs
    mod unsupported {
        use crate::program::latex::{tests::get_latex, RenderSettings};

        #[test]
        #[should_panic]
        fn test_supported_misc_instructions() {
            get_latex("NOP\nWAIT\nRESET\nHALT", RenderSettings::default());
        }

        #[test]
        #[should_panic]
        fn test_supported_measure() {
            get_latex(
                "DECLARE ro BIT\nMEASURE 0\nMEASURE 1 ro[0]",
                RenderSettings::default(),
            );
        }

        #[test]
        #[should_panic]
        fn test_supported_program_defcircuit() {
            get_latex(
                r#"DEFCIRCUIT EULER(%alpha, %beta, %gamma) q:
    RX(%alpha) q
    RY(%beta)  q
    RZ(%gamma) q
EULER(pi, 2*pi, 3*pi/2) 0"#,
                RenderSettings::default(),
            );
        }

        #[test]
        #[should_panic]
        fn test_supported_gate_rotation() {
            get_latex(
                r#"DECLARE ro BIT[1]
DECLARE theta REAL[1]
RX(pi/2) 0
RZ(theta) 0
RY(-pi/2) 0"#,
                RenderSettings::default(),
            );
        }

        #[test]
        #[should_panic]
        fn test_supported_arithmetic_instruction() {
            get_latex(
                "DECLARE b BIT\nDECLARE theta REAL\nMOVE theta -3.14\nLT b theta -3.14",
                RenderSettings::default(),
            );
        }

        #[test]
        #[should_panic]
        fn test_supported_modifier_forked() {
            get_latex(
                "FORKED CONTROLLED FORKED RX(a,b,c,d) 0 1 2 3",
                RenderSettings::default(),
            );
        }
    }
}
