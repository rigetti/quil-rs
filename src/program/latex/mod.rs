//! LaTeX circuit generation for quil programs.
//!
//! This module enables generating quantum circuits using the LaTeX subpackage
//! TikZ/[`Quantikz`] for a given quil [`Program`]. This feature is callable on
//! [`Program`] (see usage below) and returns a LaTeX string which can be
//! rendered in a LaTeX visualization tool. Be aware that not all Programs can
//! be serialized as LaTeX. If a [`Program`] contains a gate or modifier that
//! has not been implemented in the [Supported Gates and Modifiers]
//! (#supported-gates-and-modifiers) section below, unexpected results may
//! occur, one of which includes producing incorrect quantum circuits.
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
///   Single wire commands: lstick, rstick, qw, meter
///   Multi-wire commands: ctrl, targ, control, (swap, targx)
#[derive(Clone, Debug)]
enum Command {
    /// `\lstick{\ket{q_{u32}}}`: Make a qubit "stick out" from the left.
    Lstick(String),
    /// `\gate{name}`: Make a gate on the wire.
    Gate(String),
    /// `\phase{symbol}`: Make a phase on the wire with a rotation
    Phase(String),
    /// `^{\script}`: Add a superscript to a gate
    Super(String),
    /// `\qw`: Connect the current cell to the previous cell i.e. "do nothing".
    Qw,
    /// `\\`: Start a new row
    Nr,
    /// `\ctrl{wire}`: Make a control qubit--different from Control.
    Ctrl(String),
    /// `\targ{}`: Make a controlled-not gate.
    Targ,
}

impl Command {
    fn get_command(command: Self) -> String {
        match command {
            Self::Lstick(wire) => format!(r#"\lstick{{\ket{{q_{{{wire}}}}}}}"#),
            Self::Gate(name) => format!(r#"\gate{{{name}}}"#),
            Self::Phase(symbol) => format!(r#"\phase{{{symbol}}}"#),
            Self::Super(script) => format!(r#"^{{\{script}}}"#),
            Self::Qw => r"\qw".to_string(),
            Self::Nr => r"\\".to_string(),
            Self::Ctrl(wire) => format!(r#"\ctrl{{{wire}}}"#),
            Self::Targ => r"\targ{}".to_string(),
        }
    }
}

/// Types of parameters passed to commands.
#[derive(Clone, Debug)]
enum Parameter {
    /// Symbolic parameters
    Symbol(Symbol),
}

impl ToString for Parameter {
    fn to_string(&self) -> String {
        match self {
            Parameter::Symbol(symbol) => Symbol::to_string(symbol),
        }
    }
}

/// Supported Greek and alphanumeric symbols.
#[derive(Clone, Debug)]
enum Symbol {
    Alpha,
    Beta,
    Gamma,
    Phi,
    Pi,
    Text(String),
}

impl ToString for Symbol {
    fn to_string(&self) -> String {
        match self {
            Symbol::Alpha => r"\alpha".to_string(),
            Symbol::Beta => r"\beta".to_string(),
            Symbol::Gamma => r"\gamma".to_string(),
            Symbol::Phi => r"\phi".to_string(),
            Symbol::Pi => r"\pi".to_string(),
            Symbol::Text(text) => format!(r#"\text{{{text}}}"#),
        }
    }
}

impl Symbol {
    /// Returns the supported Symbol variant from text, otherwise, stores the
    /// unsupported symbol as text in the Text variant.
    ///
    /// # Arguments
    /// `text` - a String representing a Greek or alphanumeric symbol
    fn match_symbol(text: String) -> Symbol {
        match text.as_str() {
            "alpha" => Symbol::Alpha,
            "beta" => Symbol::Beta,
            "gamma" => Symbol::Gamma,
            "phi" => Symbol::Phi,
            "pi" => Symbol::Pi,
            _ => Symbol::Text(text),
        }
    }
}

/// Gates written in shorthand notation, i.e. composite form, that may be
/// decomposed into modifiers and single gate instructions, i.e. canonical form.
enum CanonicalGate {
    /// `CNOT` is `CONTROLLED X`
    Cnot(String),
    /// `CCNOT` is `CONTROLLED CONTROLLED X`
    Ccnot(String),
    /// `CPHASE` is `CONTROLLED PHASE`
    Cphase(String),
    /// `CZ` is `CONTROLLED Z`
    Cz(String),
    /// gate is in canonical form or is unsupported
    None,
}

impl CanonicalGate {
    fn get_canonical(gate_name: &str) -> CanonicalGate {
        match gate_name {
            "CNOT" => CanonicalGate::Cnot(String::from("CONTROLLED X")),
            "CCNOT" => CanonicalGate::Ccnot(String::from("CONTROLLED CONTROLLED X")),
            "CPHASE" => CanonicalGate::Cphase(String::from("CONTROLLED PHASE")),
            "CZ" => CanonicalGate::Cz(String::from("CONTROLLED Z")),
            _ => CanonicalGate::None,
        }
    }
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
    /// Returns a label as the qubit name to the left side of the wire.
    ///
    ///  # Arguments
    /// `name` - name of the qubit
    fn label_qubit_lines(&self, name: u64) -> String {
        Command::get_command(Command::Lstick(name.to_string()))
    }

    /// Adds missing qubits between the first qubit and last qubit in a
    /// diagram's circuit. If a missing qubit is found, a new wire is created
    /// and pushed to the diagram's circuit.
    ///
    ///  # Arguments
    /// `column` - the length of instructions from Program
    /// `&mut BTreeMap<u64, Box<Wire>> circuit` - the circuit of the diagram
    ///
    /// # Examples
    /// ```
    /// use quil_rs::{Program, program::latex::{RenderSettings, Latex}};
    /// use std::str::FromStr;
    /// let program = Program::from_str("H 0\nCNOT 0 1").expect("");
    /// let settings = RenderSettings {
    ///     impute_missing_qubits: true,
    ///     ..Default::default()
    /// };
    /// program.to_latex(settings).expect("");
    /// ```
    fn impute_missing_qubits(&self, column: u32, circuit: &mut BTreeMap<u64, Box<Wire>>) {
        // requires at least two qubits to impute missing qubits
        if circuit.len() < 2 {
            return;
        }

        // get the first qubit in the BTreeMap
        let first = circuit
            .first_key_value()
            .expect("previously checked that circuit is not empty")
            .0
            + 1;

        // get the last qubit in the BTreeMap
        let last = circuit
            .last_key_value()
            .expect("previously checked that circuit has at least two wires")
            .0
            - 1;

        // search through the range of qubits
        for qubit in first..=last {
            // if the qubit is not found impute it
            circuit.entry(qubit).or_insert_with(|| {
                let mut wire = Wire {
                    ..Default::default()
                };

                // insert empties based on total number of columns
                for c in 0..=column {
                    wire.empty.insert(c, Command::Qw);
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
/// through a multi qubit gate 'e.g. CNOT'. The size of the diagram can be
/// measured by multiplying the column with the length of the circuit. This is
/// an [m x n] matrix where each element in the matrix represents an item to be
/// rendered onto the diagram using one of the Quantikz commands.
#[derive(Clone, Debug, Default)]
struct Diagram {
    /// customizes how the diagram renders the circuit
    settings: RenderSettings,
    /// total number of elements on each wire
    column: u32,
    /// column at which qubits in positional order form relationships
    relationships: HashMap<u32, Vec<Qubit>>,
    /// a BTreeMap of wires with the name of the wire as the key
    circuit: BTreeMap<u64, Box<Wire>>,
}

impl Diagram {
    /// Compares qubits from a single instruction associated with a column on
    /// the circuit to all of the qubits used in the quil program. If a qubit
    /// from the quil program is not found in the qubits in the single
    /// instruction line, then an empty slot is added to that column on the
    /// qubit wire of the circuit.
    ///
    /// # Arguments
    /// `&mut self` - exposes the Circuit
    /// `qubits` - qubits used in the quil program
    /// `instruction` - exposes qubits in a single instruction
    fn set_qw(&mut self, qubits: &HashSet<Qubit>, instruction: &Instruction) {
        'program_loop: for program_qubit in qubits {
            if let Instruction::Gate(gate) = instruction {
                for gate_qubit in &gate.qubits {
                    if program_qubit == gate_qubit {
                        continue 'program_loop;
                    }
                }

                if let Qubit::Fixed(q) = program_qubit {
                    if let Some(wire) = self.circuit.get_mut(q) {
                        wire.empty.insert(self.column, Command::Qw);
                    }
                }
            }
        }
    }

    /// Utility function to insert modifiers of wires in this Circuit at the
    /// current column. Returns an Err for unsupported modifiers.
    ///
    /// # Arguments
    /// `column` - the current column of the Circuit
    /// `wire` - a wire on the Circuit
    /// `modifiers` - the modifiers from the Gate
    fn set_modifiers(
        wire: &mut Wire,
        column: &u32,
        modifiers: &Vec<GateModifier>,
    ) -> Result<(), LatexGenError> {
        // set modifers
        for modifier in modifiers {
            match modifier {
                // return error for FORKED
                GateModifier::Forked => {
                    return Err(LatexGenError::UnsupportedModifierForked);
                }
                // insert for CONTROLLED and DAGGER
                _ => {
                    wire.modifiers
                        .entry(*column)
                        .and_modify(|m| m.push(modifier.clone()))
                        .or_insert_with(|| vec![modifier.clone()]);
                }
            }
        }

        Ok(())
    }

    /// The logic of this function is visualized using a physical vector with
    /// the tail at the control qubit and the head pointing to the target
    /// qubit. The distance between the qubits represents the number of wires
    /// between them, i.e the space that the vector needs to traverse. If the
    /// control qubit comes before the target qubit the direction is positive,
    /// otherwise, it is negative. See Quantikz documentation on CNOT for some
    /// background that helps justify this approach.
    ///
    /// # Arguments
    /// `&mut self` - self as mutible allowing to update the circuit qubits
    fn set_ctrl_targ(&mut self) -> Result<(), LatexGenError> {
        if let Some(relationship) = self.relationships.get(&self.column) {
            // requires at least two qubits
            if relationship.len() < 2 {
                return Ok(());
            }
            // determine the physical vector where a positive vector points
            // from control to target, negative, from target to control. The
            // magnitude of the vector is the absolute value of the distance
            // between them
            if let Some(last) = relationship.last() {
                if let Qubit::Fixed(targ) = last {
                    // any qubit before last in the relationship
                    let mut pred = None;

                    // distance between qubits is the space between the ctrl and
                    // targ qubits in the circuit
                    for qubit in relationship.split(|q| q == last).next().unwrap() {
                        if let Qubit::Fixed(ctrl) = qubit {
                            pred = Some(ctrl);

                            // represent inclusive [open, close] brackets of a range
                            let mut open = None; // opening qubit in range
                            let mut close = None; // closing qubit in range

                            // find the range between the qubits
                            for (i, wire) in self.circuit.iter().enumerate() {
                                // get each existing qubit in the circuit
                                if *wire.0 == *ctrl || *wire.0 == *targ {
                                    // if the qubit is the ctrl or target
                                    if open.is_some() {
                                        close = Some(i);
                                        break;

                                    // open qubit in range not found, set open qubit
                                    } else {
                                        open = Some(i)
                                    }
                                }
                            }

                            let mut vector: i64 = 0;
                            if let Some(open) = open {
                                if let Some(close) = close {
                                    if ctrl < targ {
                                        // a vector with a head from the ctrl to the targ
                                        vector = (close as i64) - (open as i64);
                                    } else {
                                        // a vector with a head from the targ to the ctrl
                                        vector = -((close as i64) - (open as i64));
                                    }
                                }
                            }
                            // set wire at column as the control qubit of target qubit
                            // computed as the distance from the control qubit
                            self.circuit
                                .get_mut(ctrl)
                                .and_then(|wire| wire.ctrl.insert(self.column, vector));
                        }
                    }
                    // pred is None if relationship is split and no iterators are returned indicating this erroneous instruction
                    if pred.is_none() {
                        return Err(LatexGenError::FoundCNOTWithNoTarget);
                    }
                }
            }
        }

        Ok(())
    }

    /// Analyzes a Gate from an instruction and sets the gate at this column on
    /// the wire. If the gate name is a composite gate, the gate name is
    /// decomposed into canonical form. For example, CNOT is a composite gate
    /// that can be decomposed into the equivalent canonical form, CONTROLLED X.
    ///
    /// # Arguments
    /// `column` - the current empty column to set the gate at
    /// `gate` - the gate of the instruction being parsed in to_latex
    fn parse_gate(&mut self, gate: &Gate) -> Result<(), LatexGenError> {
        // preserve qubit order in instruction
        self.relationships.insert(self.column, gate.qubits.clone());

        // set modifiers from gate instruction
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

        // parse the gate to a canonical gate if supported
        let canonical_gate = match CanonicalGate::get_canonical(&gate.name) {
            CanonicalGate::Cnot(inst) => Some(inst),
            CanonicalGate::Ccnot(inst) => Some(inst),
            CanonicalGate::Cphase(inst) => Some(inst),
            CanonicalGate::Cz(inst) => Some(inst),
            CanonicalGate::None => None,
        };

        // add the qubits to the canonical gate to form an instruction
        let instruction = if let Some(mut canonical_gate) = canonical_gate {
            for qubit in &gate.qubits {
                if let Qubit::Fixed(qubit) = qubit {
                    canonical_gate.push(' ');
                    canonical_gate.push_str(&qubit.to_string());
                }
            }
            Some(canonical_gate)
        } else {
            None
        };

        // get gate from new program of canonical instruction
        if let Some(instruction) = instruction {
            let instructions = Program::from_str(&instruction)
                .expect("should return program {instruction}")
                .to_instructions(false);

            for instruction in instructions {
                if let Instruction::Gate(gate) = instruction {
                    // call until all composite gates are in canonical form
                    self.parse_gate(&gate)?;
                }
            }
        // gate is in canonical form, build wire
        } else {
            // set gates
            for fixed_qubit in &gate.qubits {
                if let Qubit::Fixed(qubit) = fixed_qubit {
                    if let Some(wire) = self.circuit.get_mut(qubit) {
                        // set the control and target qubits
                        if let Some(relationship) = self.relationships.get(&self.column) {
                            // requires at least 2 qubits or is a PHASE gate
                            if relationship.len() > 1 || gate.name == "PHASE" {
                                // target is the last qubit in the instruction or the qubit in PHASE
                                if let Some(target) = relationship.last() {
                                    if fixed_qubit == target {
                                        wire.targ.insert(self.column, true);
                                    // all other qubits are controls
                                    } else {
                                        wire.ctrl.insert(self.column, 0);
                                    }
                                }
                            } else if wire.parameters.get(&self.column).is_some() {
                                // parameterized single qubit gates are unsupported
                                return Err(LatexGenError::UnsupportedGate {
                                    gate: gate.name.clone(),
                                });
                            }
                        }

                        // set modifiers at this column for all qubits
                        wire.gates.insert(self.column, gate.clone());
                    }
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
                write!(f, "{}", &self.settings.label_qubit_lines(*key))?;
            } else {
                // add qw buffer to first column
                write!(f, "{}", &Command::get_command(Command::Qw))?;
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
                                    superscript.push_str(&Command::get_command(Command::Super(
                                        String::from("dagger"),
                                    )))
                                }
                            }
                        }

                        if wire.ctrl.get(&c).is_some() {
                            // CONTROLLED qubits are displayed as `\ctrl{targ}`
                            if let Some(targ) = wire.ctrl.get(&c) {
                                write!(
                                    f,
                                    "{}",
                                    &Command::get_command(Command::Ctrl(targ.to_string()))
                                )?;
                            }
                            continue;
                        } else if wire.targ.get(&c).is_some() {
                            // CONTROLLED X gates are displayed as `\targ{}`
                            if gate.name == "X" {
                                // set the qubit at this column as the target

                                let mut _gate = gate.name.clone();

                                // if the gate contains daggers, display target as X gate with dagger superscripts
                                if !superscript.is_empty() {
                                    _gate.push_str(&superscript);
                                    write!(f, "{}", &Command::get_command(Command::Gate(_gate)))?;
                                // else display X target as an open dot
                                } else {
                                    write!(f, "{}", &Command::get_command(Command::Targ))?;
                                }
                                continue;
                            // PHASE gates are displayed as `\phase{param}`
                            } else if gate.name == "PHASE" {
                                // set the phase parameters
                                if let Some(parameters) = wire.parameters.get(&c) {
                                    for param in parameters {
                                        write!(
                                            f,
                                            "{}",
                                            &Command::get_command(Command::Phase(
                                                param.to_string()
                                            ))
                                        )?;
                                    }
                                }
                                continue;
                            }
                        }
                        // all other gates display as `\gate{name}`
                        let mut _gate = gate.name.clone();

                        // concatenate superscripts
                        if !superscript.is_empty() {
                            _gate.push_str(&superscript);
                        }

                        write!(f, "{}", &Command::get_command(Command::Gate(_gate)))?;
                    } else if wire.empty.get(&c).is_some() {
                        // chain an empty column qw to the end of the line
                        write!(f, " & ")?;
                        write!(f, "{}", &Command::get_command(Command::Qw))?;
                    }
                }
            }

            // chain an empty column qw to the end of the line
            write!(f, " & ")?;
            write!(f, "{}", &Command::get_command(Command::Qw))?;

            // if this is the last key iteration, omit Nr from end of line
            if i < self.circuit.len() - 1 {
                // indicate a new row
                write!(f, " ")?;
                write!(f, "{}", &Command::get_command(Command::Nr))?;
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
    /// gate elements placed at column on wire using the Gate command
    gates: HashMap<u32, Gate>,
    /// control at column with distance from targ wire
    ctrl: HashMap<u32, i64>,
    /// at this column is the wire a target?
    targ: HashMap<u32, bool>,
    /// any parameters required at column on the wire for gates
    parameters: HashMap<u32, Vec<Parameter>>,
    /// total number of controlled modifiers added to the wire at this column
    modifiers: HashMap<u32, Vec<GateModifier>>,
    /// empty column
    empty: HashMap<u32, Command>,
}

impl Wire {
    /// Retrieves a gate's parameters from Expression and matches them with its
    /// symbolic definition which is then stored into wire at the specific
    /// column.
    ///
    /// # Arguments
    /// `&mut self` - exposes the Wire's parameters at this column
    /// `expression` - expression from Program to get name of parameter
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
            vec![Parameter::Symbol(Symbol::match_symbol(text))]
        } else {
            // set the symbol as text
            vec![Parameter::Symbol(Symbol::Text(text))]
        };

        self.parameters.insert(column, param);
    }
}

#[derive(Clone, Debug, thiserror::Error)]
pub enum LatexGenError {
    #[error("Tried to parse CNOT and found a control qubit without a target.")]
    FoundCNOTWithNoTarget,
    #[error("The FORKED modifier is unsupported.")]
    UnsupportedModifierForked,
    #[error("This instruction is unsupported: {instruction}.")]
    UnsupportedInstruction { instruction: String },
    #[error("This gate is unsupported: {gate}.")]
    UnsupportedGate { gate: String },
}

pub trait Latex {
    fn to_latex(self, settings: RenderSettings) -> Result<String, LatexGenError>;
}

impl Latex for Program {
    /// This implementation of Latex can be viewed as a self-contained partial
    /// implementation of ``Quantikz`` with all available commands listed as
    /// variants in a Command enum. View ``Quantikz`` documentation for more
    /// information.
    ///
    /// This function returns a Result containing a quil [`Program`] as a LaTeX
    /// string or a [`LatexGenError`] defined using thiserror. Called on a
    /// Program, the function starts with a check to ensure the [`Program`]
    /// contains supported gates and modifers that can be serialized to LaTeX.
    ///
    /// # Arguments
    /// `settings` - Customizes the rendering of a circuit.
    ///
    /// # Examples
    /// ```
    /// // To LaTeX for the Bell State Program.
    /// use quil_rs::{Program, program::latex::{RenderSettings, Latex}};
    /// use std::str::FromStr;
    /// let program = Program::from_str("H 0\nCNOT 0 1").expect("");
    /// let latex = program.to_latex(RenderSettings::default()).expect("");
    /// ```
    ///
    /// ```
    /// // To LaTeX for the Toffoli Gate Program.
    /// use quil_rs::{Program, program::latex::{RenderSettings, Latex}};
    /// use std::str::FromStr;
    /// let program = Program::from_str("CONTROLLED CNOT 2 1 0").expect("");
    /// let latex = program.to_latex(RenderSettings::default()).expect("");
    /// ```
    fn to_latex(self, settings: RenderSettings) -> Result<String, LatexGenError> {
        // get a reference to the current program
        let instructions = self.to_instructions(false);

        // initialize a new diagram
        let mut diagram = Diagram {
            settings,
            ..Default::default()
        };

        // initialize circuit with empty wires of all qubits in program
        let qubits = Program::get_used_qubits(&self);
        for qubit in &qubits {
            if let Qubit::Fixed(name) = qubit {
                let wire = Wire {
                    ..Default::default()
                };
                diagram.circuit.insert(*name, Box::new(wire));
            }
        }

        // are implicit qubits required in settings and are there at least two or more qubits in the diagram?
        if diagram.settings.impute_missing_qubits {
            // add implicit qubits to circuit
            diagram
                .settings
                .impute_missing_qubits(instructions.len() as u32, &mut diagram.circuit);
        }

        for instruction in instructions {
            // set QW for any unused qubits in this instruction
            diagram.set_qw(&qubits, &instruction);

            // parse gate instructions into a new circuit
            if let Instruction::Gate(gate) = instruction {
                diagram.parse_gate(&gate)?;
                diagram.set_ctrl_targ()?;
                diagram.column += 1;
            } else if let Instruction::GateDefinition(_) = instruction {
                // GateDefinition is supported and parsed in Gate
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
    use super::{Latex, RenderSettings};
    use crate::Program;
    use std::str::FromStr;

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
        use crate::program::latex::{Command, Symbol};

        #[test]
        fn test_command_left_ket() {
            insta::assert_snapshot!(Command::get_command(Command::Lstick("0".to_string())));
        }

        #[test]
        fn test_command_gate() {
            insta::assert_snapshot!(Command::get_command(Command::Gate("X".to_string())));
        }

        #[test]
        fn test_command_phase() {
            insta::assert_snapshot!(Command::get_command(Command::Phase(Symbol::Pi.to_string())));
        }

        #[test]
        fn test_command_super() {
            insta::assert_snapshot!(Command::get_command(Command::Super("dagger".to_string())));
        }

        #[test]
        fn test_command_qw() {
            insta::assert_snapshot!(Command::get_command(Command::Qw));
        }

        #[test]
        fn test_command_nr() {
            insta::assert_snapshot!(Command::get_command(Command::Nr));
        }

        #[test]
        fn test_command_control() {
            insta::assert_snapshot!(Command::get_command(Command::Ctrl("0".to_string())));
        }

        #[test]
        fn test_command_cnot_target() {
            insta::assert_snapshot!(Command::get_command(Command::Targ));
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

    /// Test module for unsupported programs (remove #[should_panic] and move
    /// unit test to programs test module when supported)
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
