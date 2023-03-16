//! LaTeX circuit generation for quil programs.
//!
//! This module enables generating quantum circuits using the LaTeX subpackage
//! TikZ/[`Quantikz`] for a given quil [`Program`]. This feature is callable on
//! [`Program`] (see usage below) and returns a LaTeX string which can be rendered in
//! a LaTeX visualization tool. Be aware that not all Programs can be serialized as
//! LaTeX. If a [`Program`] contains a gate or modifier that has not been
//! implemented in the [Supported Gates and Modifiers](#supported-gates-and-modifiers)
//! section below, an error will be returned detailing whether the entire [`Program`] or which line
//! of instruction containing the gate or modifier is unsupported.
//!
//! # Supported Gates and Modifiers
//!
//!     - Pauli Gates:           `I`, `X`, `Y`, `Z`
//!     - Hadamard Gate:         `H`
//!     - Phase Gate:            `PHASE`, `S`, `T`
//!     - Controlled Phase Gate: `CZ`, `CPHASE`
//!     - Controlled X Gates:    `CNOT`, `CCNOT`
//!     - User-Defined Gates:             `DEFGATE`
//!     - Modifiers:             `CONTROLLED`, `DAGGER`
//!
//! - Usage: `Program.to_latex(settings: Settings);`
//!
//! This module can be viewed as a self-contained partial implementation of
//! [`Quantikz`] with all available commands listed as variants in a Command
//! enum. View [`Quantikz`] documentation for more information.
//!
//! [`Quantikz`]: https://arxiv.org/pdf/1809.03842.pdf

use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::{format, Display};

use crate::expression::Expression;
use crate::instruction::{self, Gate, Instruction, Qubit};
use crate::Program;

/// Available commands used for building circuits with the same names taken
/// from the Quantikz documentation for easy reference. LaTeX string denoted
/// inside `backticks`.
///     Single wire commands: lstick, rstick, qw, meter
///     Multi-wire commands: ctrl, targ, control, (swap, targx)
#[derive(Debug)]
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
    /// Returns the LaTeX String for a given Command variant.
    ///
    /// # Arguments
    /// `command` - A Command variant.
    fn get_command(command: Self) -> String {
        match command {
            Self::Lstick(wire) => format(format_args!(r#"\lstick{{\ket{{q_{{{wire}}}}}}}"#)),
            Self::Gate(name) => format(format_args!(r#"\gate{{{name}}}"#)),
            Self::Phase(symbol) => format(format_args!(r#"\phase{{{symbol}}}"#)),
            Self::Super(script) => format(format_args!(r#"^{{\{script}}}"#)),
            Self::Qw => r"\qw".to_string(),
            Self::Nr => r"\\".to_string(),
            Self::Ctrl(wire) => format(format_args!(r#"\ctrl{{{wire}}}"#)),
            Self::Targ => r"\targ{}".to_string(),
        }
    }
}

/// Types of parameters passed to commands.
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
            Symbol::Text(text) => format(format_args!(r#"\text{{{text}}}"#)),
        }
    }
}

impl Symbol {
    /// Returns the supported Symbol variant from text otherwise stores the
    /// unsupported symbol as text in the Text variant.
    ///
    /// # Arguments
    /// `text` - a String representing a greek or alaphanumeric symbol
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

/// Settings contains the metadata that allows the user to customize how the
/// circuit is rendered or use the default implementation.
#[derive(Debug)]
pub struct Settings {
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

impl Default for Settings {
    /// Returns the default Settings.
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

impl Settings {
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
    /// `&mut BTreeMap<u64, Box<Wire>> circuit` - the circuit of the diagram
    ///
    /// # Examples
    /// ```
    /// use quil_rs::{Program, program::latex::{Settings, Latex}};
    /// use std::str::FromStr;
    /// let program = Program::from_str("H 0\nCNOT 0 1").expect("");
    /// let settings = Settings {
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
        let mut first = 0;
        if let Some(f) = circuit.first_key_value() {
            first = *f.0 + 1;
        }

        // get the last qubit in the BTreeMap
        let mut last = 0;
        if let Some(l) = circuit.last_key_value() {
            last = *l.0 - 1;
        }

        // search through the range of qubits
        for qubit in first..=last {
            // if the qubit is not found impute it
            match circuit.get(&qubit) {
                Some(_) => (),
                None => {
                    let mut wire = Wire {
                        name: qubit,
                        ..Default::default()
                    };

                    // insert empties based on total number of columns
                    for c in 0..column {
                        wire.empty.insert(c, Command::Qw);
                    }

                    circuit.insert(qubit, Box::new(wire));
                }
            }
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
            body: "".to_string(),
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
/// rendered onto the diagram using one of the [`Quantikz`] commands.
#[derive(Debug)]
struct Diagram {
    /// customizes how the diagram renders the circuit
    settings: Settings,
    /// total number of elements on each wire
    column: u32,
    /// column at which qubits in positional order form relationships
    relationships: HashMap<u32, Vec<u64>>,
    /// a BTreeMap of wires with the name of the wire as the key
    circuit: BTreeMap<u64, Box<Wire>>,
}

impl Default for Diagram {
    fn default() -> Self {
        Self {
            settings: Settings::default(),
            column: 0,
            relationships: HashMap::new(),
            circuit: BTreeMap::new(),
        }
    }
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
        for program_qubit in qubits {
            let mut found = false;
            match &instruction {
                instruction::Instruction::Gate(gate) => {
                    for gate_qubit in &gate.qubits {
                        if program_qubit == gate_qubit {
                            found = true;
                        }
                    }

                    if !found {
                        match program_qubit {
                            instruction::Qubit::Fixed(q) => {
                                self.circuit
                                    .get_mut(&q)
                                    .and_then(|wire| wire.empty.insert(self.column, Command::Qw));
                            }
                            _ => (),
                        }
                    }
                }
                _ => (),
            }
        }
    }

    /// Returns a reformatted gate name based on the modifiers used in a single
    /// instruction line of a quil program or the original name. Gates with
    /// CONTROLLED modifiers are reformatted such that each CONTROLLED modifier
    /// prepends a `C` to the beginning of the gate name. For other modifiers
    /// such as DAGGER, no special reformatting is required.
    ///
    /// For example, for an instruction line `CONTROLLED CONTROLLED Y 0 1 2` the
    /// gate name is reformatted to `CCY` where each C is mapped to an
    /// associated qubit with the last qubit to the original gate name. For an
    /// instruction line `DAGGER DAGGER Y 0`, the gate name remains `Y`,
    /// instead each of the modifiers are added to the wire at the current
    /// column where it is to be applied using the Command::Super variant.
    ///
    /// # Arguments
    /// `&mut self` - exposes the current column of the Circuit
    /// `gate` - exposes the modifiers associated with the instruction gate
    /// `wire` - exposes the wire to be pushed to in Circuit
    fn set_modifiers(&mut self, gate: &Gate, wire: &mut Wire) -> String {
        let mut gate_name = gate.name.clone();

        // set modifers
        if !gate.modifiers.is_empty() {
            for modifer in &gate.modifiers {
                match modifer {
                    instruction::GateModifier::Dagger => {
                        if let Some(modifiers) = wire.modifiers.get_mut(&self.column) {
                            modifiers.push("dagger".to_string())
                        } else {
                            wire.modifiers
                                .insert(self.column, vec!["dagger".to_string()]);
                        }
                    }
                    instruction::GateModifier::Controlled => {
                        // prepend a C to the gate
                        gate_name.insert_str(0, "C");
                    }
                    _ => (),
                }
            }
        }

        gate_name
    }

    /// For every instruction containing control and target qubits this method
    /// identifies which qubit is the target and which qubit is controlling it.
    /// The logic of this function is visualized using a physical vector with
    /// the tail at the control qubit and the head pointing to the target
    /// qubit. The distance between the qubits represents the number of wires
    /// between them, i.e the space that the vector needs to traverse. If the
    /// control qubit comes before the target qubit the direction is positive,
    /// otherwise, it is negative. See [`Quantikz`] documentation on CNOT for
    /// some background that helps justify this approach.
    ///
    /// This function is expensive with a time complexity of O(n^2). In the
    /// worst case scenario every column contains a multi qubit gate with every
    /// qubit as either a target or control. [`Quantikz`] uses the space
    /// between wires to determine how long a line should stretch between
    /// control and target qubits. Since it is impossible to determine how many
    /// wires will be inserted between control and target qubits (e.g. a user
    /// decides to impute missing qubits or some number of other instructions
    /// are added containing qubits between them) for a custom body diagram,
    /// this method can only be run after all wires are inserted into the
    /// cicuit. Only run this method if a program contains multi qubit gates.
    ///
    /// # Arguments
    /// `&mut self` - self as mutible allowing to update the circuit qubits
    fn set_ctrl_targ(&mut self) -> Result<(), LatexGenError> {
        // ensure every column preserves the connection between ctrl and targ
        'column: for c in 0..=self.column {
            let mut ctrls = vec![]; // the control qubits
            let mut targ = None; // the targ qubit

            // determine if a relationship exists at this column
            if let Some(relationship) = self.relationships.get(&c) {
                // determine the control and target qubits
                for qubit in relationship {
                    // relationships require at least two qubits
                    if relationship.len() < 2 {
                        continue 'column;
                    }

                    // the last qubit is the targ
                    if *qubit == relationship[relationship.len() - 1] {
                        if let Some(wire) = self.circuit.get_mut(qubit) {
                            // insert as target at this column
                            wire.targ.insert(c, true);

                            // set 'column loop targ variable to this targ that
                            // control qubits will find distance from on their
                            // respective wires
                            targ = Some(wire.name)
                        }
                    // all other qubits are the controls
                    } else {
                        if let Some(wire) = self.circuit.get_mut(qubit) {
                            // insert as control at this column with initial
                            // value 0, targeting themselves
                            wire.ctrl.insert(c, 0);

                            // push ctrl to 'column loop ctrl variables with
                            // initial value requiring update based on targ
                            ctrls.push(wire.name);
                        }
                    }
                }
            } else {
                // no relationships found on this column, go to next
                continue 'column;
            }

            // determine the physical vector where a positive vector points
            // from control to target, negative, from target to control. The
            // magnitude of the vector is the absolute value of the distance
            // between them
            if let Some(targ) = targ {
                // distance between qubits is the space between the ctrl and
                // targ qubits in the circuit
                for ctrl in ctrls {
                    // represent inclusive [open, close] brackets of a range
                    let mut open = None; // opening qubit in range
                    let mut close = None; // closing qubit in range

                    // find the range between the qubits
                    let mut i = 0;
                    for wire in &self.circuit {
                        // get each existing qubit in the circuit
                        if *wire.0 == ctrl || *wire.0 == targ {
                            // if the qubit is the ctrl or target
                            if let Some(_) = open {
                                close = Some(i);
                                break;

                            // open qubit in range not found, set open qubit
                            } else {
                                open = Some(i)
                            }
                        }

                        i += 1;
                    }

                    let mut vector: i64 = 0;
                    if let Some(open) = open {
                        if let Some(close) = close {
                            if ctrl < targ {
                                // a vector with a head from the ctrl to the targ
                                vector = 1 * (close - open);
                            } else {
                                // a vector with a head from the targ to the ctrl
                                vector = -1 * (close - open);
                            }
                        }
                    }
                    // set wire at column as the control qubit of target qubit
                    // computed as the distance from the control qubit
                    self.circuit
                        .get_mut(&ctrl)
                        .and_then(|wire| wire.ctrl.insert(c, vector));
                }
            }
        }

        Ok(())
    }

    /// Takes a new or existing wire and adds or updates it using the name
    /// (String) as the key. If a wire exists with the same name, then the
    /// contents of the new wire are added to it by updating the next column
    /// using the Quantikz command associated with its attributes (e.g. gate,
    /// do_nothing, etc).
    ///
    /// # Arguments
    /// `&mut self` - exposes HashMap<String, Box<Circuit>>
    /// `wire` - the wire to be pushed or updated to in circuits
    fn push_wire(&mut self, wire: Wire) -> Result<(), LatexGenError> {
        let qubit = wire.name;

        // find wire in circuit collection
        match self.circuit.get_mut(&wire.name) {
            Some(wire_in_circuit) => {
                // get the new gate from the wire and insert into existing wire
                if let Some(gate) = wire.gates.get(&self.column) {
                    // add gates to wire in circuit
                    wire_in_circuit.gates.insert(self.column, gate.to_string());
                }

                // add modifiers to gate in circuit
                if let Some(modifier) = wire.modifiers.get(&self.column) {
                    wire_in_circuit
                        .modifiers
                        .insert(self.column, modifier.to_vec());
                }

                // add modifiers to gate in circuit
                if let Some(parameters) = wire.parameters.get(&self.column) {
                    wire_in_circuit
                        .parameters
                        .insert(self.column, parameters.to_vec());
                }
            }
            _ => (),
        }

        // initalize relationships between multi qubit gates
        if let Some(wire) = self.circuit.get(&qubit) {
            // get the newly added gate if any at the column it was added
            if let Some(gate) = wire.gates.get(&self.column) {
                // tag relationships for multi qubit gates
                if gate.starts_with('C') {
                    // add the qubits to the set of related qubits in the current column
                    if let Some(qubits) = self.relationships.get_mut(&self.column) {
                        // ensure relationships are valid
                        for _self in qubits.iter() {
                            // qubit cannot control and target itself
                            if *_self == qubit {
                                return Err(LatexGenError::FoundCNOTWithNoTarget);
                            }
                        }

                        qubits.push(qubit);
                    } else {
                        self.relationships.insert(self.column, vec![qubit]);
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
        let mut body = String::from('\n');

        let mut i = 0; // used to omit trailing Nr

        // write the LaTeX string for each wire in the circuit
        for key in self.circuit.keys() {
            // a single line of LaTeX representing a wire from the circuit
            let mut line = String::from("");

            // are labels on in settings?
            if self.settings.label_qubit_lines {
                // add label to left side of wire
                line.push_str(&self.settings.label_qubit_lines(*key));
            } else {
                // add qw buffer to first column
                line.push_str(&Command::get_command(Command::Qw));
            }

            // convert each column in the wire to string
            if let Some(wire) = self.circuit.get(key) {
                for c in 0..self.column {
                    if let Some(gate) = wire.gates.get(&c) {
                        line.push_str(" & ");

                        let mut superscript = String::from("");
                        // attach modifiers to gate name if any
                        if let Some(modifiers) = wire.modifiers.get(&c) {
                            for modifier in modifiers {
                                superscript.push_str(&Command::get_command(Command::Super(
                                    modifier.to_string(),
                                )))
                            }
                        }

                        if gate.starts_with('C') {
                            // set qubit at this column as the control
                            if let Some(targ) = wire.ctrl.get(&c) {
                                line.push_str(&Command::get_command(Command::Ctrl(
                                    targ.to_string(),
                                )));
                            } else if let Some(_) = wire.targ.get(&c) {
                                // if this is a target and has a PHASE gate display `\phase{param}`
                                if gate.contains("PHASE") {
                                    // set the phase parameters
                                    if let Some(parameters) = wire.parameters.get(&c) {
                                        for param in parameters {
                                            line.push_str(&Command::get_command(Command::Phase(
                                                param.to_string(),
                                            )));
                                        }
                                    }
                                // if target has a CNOT gate, display as targ{}
                                } else if gate.contains("NOT") {
                                    line.push_str(&Command::get_command(Command::Targ));
                                // if target has a 'char' gate display `gate{char}` gate
                                } else {
                                    let mut gate = String::from(gate.chars().last().unwrap());

                                    // concatenate superscripts
                                    if !superscript.is_empty() {
                                        gate.push_str(&superscript);
                                    }

                                    line.push_str(&Command::get_command(Command::Gate(gate)))
                                }
                            }
                        // PHASE gates are displayed as `\phase{param}`
                        } else if gate.contains("PHASE") {
                            // set the phase parameters
                            if let Some(parameters) = wire.parameters.get(&c) {
                                for param in parameters {
                                    line.push_str(&Command::get_command(Command::Phase(
                                        param.to_string(),
                                    )));
                                }
                            }
                        // all other gates display as `\gate{name}`
                        } else {
                            let mut gate = String::from(gate);

                            // concatenate superscripts
                            if !superscript.is_empty() {
                                gate.push_str(&superscript);
                            }

                            line.push_str(&Command::get_command(Command::Gate(gate)));
                        }
                    } else if let Some(_) = wire.empty.get(&c) {
                        // chain an empty column qw to the end of the line
                        line.push_str(" & ");
                        line.push_str(&Command::get_command(Command::Qw));
                    }
                }
            }

            // chain an empty column qw to the end of the line
            line.push_str(" & ");
            line.push_str(&Command::get_command(Command::Qw));

            // if this is the last key iteration, omit Nr from end of line
            if i < self.circuit.len() - 1 {
                // indicate a new row
                line.push(' ');
                line.push_str(&Command::get_command(Command::Nr));
                i += 1;
            }

            // add a newline between each new line or the footer
            line.push('\n');
            body.push_str(&line);
        }

        write!(f, "{}", body)
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
/// describes how far away it is from the related qubit based on [`Quantikz`].
#[derive(Debug)]
struct Wire {
    /// the name of ket(qubit) placed using the Lstick or Rstick commands
    name: u64,
    /// gate elements placed at column on wire using the Gate command
    gates: HashMap<u32, String>,
    /// control at column with distance from targ wire
    ctrl: HashMap<u32, i64>,
    /// at this column is the wire a target?
    targ: HashMap<u32, bool>,
    /// any parameters required at column on the wire for gates
    parameters: HashMap<u32, Vec<Parameter>>,
    /// any modifiers added to the wire at column
    modifiers: HashMap<u32, Vec<String>>,
    /// empty column
    empty: HashMap<u32, Command>,
}

impl Default for Wire {
    fn default() -> Self {
        Self {
            name: 0,
            gates: HashMap::new(),
            ctrl: HashMap::new(),
            targ: HashMap::new(),
            parameters: HashMap::new(),
            modifiers: HashMap::new(),
            empty: HashMap::new(),
        }
    }
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
        let text: String;

        // get the name of the supported expression
        match expression {
            Expression::Address(mr) => {
                text = mr.name.to_string();
            }
            Expression::Number(c) => {
                text = c.re.to_string();
            }
            expression => text = expression.to_string(),
        }

        let param;
        // if texify_numerical_constants
        if texify {
            // get the matching symbol from text
            param = vec![Parameter::Symbol(Symbol::match_symbol(text))];
        } else {
            // set the symbol as text
            param = vec![Parameter::Symbol(Symbol::Text(text))];
        }
        self.parameters.insert(column, param);
    }
}

#[derive(thiserror::Error, Debug)]
pub enum LatexGenError {
    #[error("Cannot parse LaTeX for unsupported program: {program}")]
    UnsupportedProgram { program: String },
    #[error("Cannot parse LaTeX for unsupported gate: {gate}")]
    UnsupportedGate { gate: String },
    #[error("Tried to parse CNOT and found a control qubit without a target.")]
    FoundCNOTWithNoTarget,
}

/// Supported types
enum Supported {
    Gate(SupportedGate),
    New,  // New initialization of a non-None variant
    None, // ()
}

/// Set of all Gates that can be parsed to LaTeX
#[derive(PartialEq)]
enum SupportedGate {
    Pauli(String),
    Hadamard(String),
    Phase(String),
    ControlledPhase(String),
    ControlledX(String),
    DefGate(String),
    Modifiers(String),
    Unsupported(String), // for error handling
}

impl SupportedGate {
    /// Returns a variant of self for any supported standard gate.
    ///
    /// # Arguments
    /// `name` - the name of the gate from instruction
    fn get_supported_standard_gate(name: String) -> Self {
        if name == "I" || name == "X" || name == "Y" || name == "Z" {
            return Self::Pauli(name);
        } else if name == "H" {
            return Self::Hadamard(name);
        } else if name == "PHASE" || name == "S" || name == "T" {
            return Self::Phase(name);
        } else if name == "CZ" || name == "CPHASE" {
            return Self::ControlledPhase(name);
        } else if name == "CNOT" || name == "CCNOT" {
            return Self::ControlledX(name);
        }

        return Self::Unsupported(name);
    }

    /// Returns a variant of self for any defined gate.
    ///
    /// # Arguments
    /// `name` - the name of the defined gate from instruction
    /// `defgate` - a vector of all previously defined gates
    fn get_supported_defgate(name: String, defgate: &Vec<String>) -> Self {
        // check all previously defined DEFGATES
        for defgate in defgate {
            // return supported if gate name is of DEFGATE
            if defgate == &name {
                return SupportedGate::DefGate(name.to_string());
            }
        }

        return Self::Unsupported(name);
    }

    /// Returns a variant of self for any supported modifier.
    ///
    /// # Arguments
    /// `name` - the name of the defined gate from instruction
    fn get_supported_modifier(name: String) -> Self {
        if name == "CONTROLLED" || name == "DAGGER" {
            return Self::Modifiers(name);
        }

        return Self::Unsupported(name);
    }
}

impl Supported {
    /// Returns new variant of self as variant of supported gate.
    ///
    /// # Arguments
    /// `name` - the name of the defined gate from instruction
    /// `defgate` - a vector of all previously defined gates    
    fn new(&self, gate: &Gate, defgate: &Vec<String>) -> Self {
        // check is standard gate
        let mut is_supported = SupportedGate::get_supported_standard_gate(gate.name.to_string());

        // check if defgate if not already identified as a standard gate
        if is_supported == SupportedGate::Unsupported(gate.name.to_string()) {
            is_supported = SupportedGate::get_supported_defgate(gate.name.to_string(), defgate);
        }

        // check supported modifers
        for modifier in &gate.modifiers {
            is_supported = SupportedGate::get_supported_modifier(modifier.to_string())
        }

        match self {
            Supported::New => Self::Gate(is_supported),
            _ => Supported::None, // same as ()
        }
    }

    /// Returns a result indicating whether or not the LaTeX feature can parse
    /// a given Program to LaTeX.
    ///
    /// # Arguments
    /// `self` - exposes variants of a Supported program
    /// `program` - the Program to be validated before parsing to LaTeX
    fn is_supported(&self, program: &Program) -> Result<Vec<Instruction>, LatexGenError> {
        let instructions = program.to_instructions(false);

        // store DEFGATE names for reference
        let mut defgate: Vec<String> = vec![];

        // check each instruction to determine if it is supported
        for instruction in &instructions {
            match instruction {
                // check is gate is supported
                instruction::Instruction::Gate(gate) => match Self::new(self, gate, &defgate) {
                    // new Supported checks if this instruction is supported
                    Supported::Gate(is_supported) => match is_supported {
                        // unsupported if SupportedGate is not returned
                        SupportedGate::Unsupported(gate) => {
                            return Err(LatexGenError::UnsupportedGate { gate })
                        }
                        // SupportedGate returned so instruction is supported
                        _ => (),
                    },
                    // do nothing for non-New Self variants
                    _ => (),
                },
                // DEFGATE is supported
                instruction::Instruction::GateDefinition(gate) => {
                    defgate.push(gate.name.to_string())
                }
                // unless explicitly matched, program is unsupported
                _ => {
                    return Err(LatexGenError::UnsupportedProgram {
                        program: program.to_string(false),
                    })
                }
            }
        }

        Ok(instructions)
    }
}

pub trait Latex {
    fn to_latex(self, settings: Settings) -> Result<String, LatexGenError>;
}

impl Latex for Program {
    /// Main function of LaTeX feature, returns a Result containing a quil
    /// Program as a LaTeX string or an Error. Called on a Program, the
    /// function starts with a check to ensure the Program contains gates and
    /// modifers that are implemented and can therefore be parsed to LaTeX.
    ///
    /// # Arguments
    /// `settings` - Customizes the rendering of a circuit.
    ///
    /// # Examples
    /// ```
    /// // To LaTeX for the Bell State Program.
    /// use quil_rs::{Program, program::latex::{Settings, Latex}};
    /// use std::str::FromStr;
    /// let program = Program::from_str("H 0\nCNOT 0 1").expect("");
    /// let latex = program.to_latex(Settings::default()).expect("");
    /// ```
    ///
    /// ```
    /// // To LaTeX for the Toffoli Gate Program.
    /// use quil_rs::{Program, program::latex::{Settings, Latex}};
    /// use std::str::FromStr;
    /// let program = Program::from_str("CONTROLLED CNOT 2 1 0").expect("");
    /// let latex = program.to_latex(Settings::default()).expect("");
    /// ```
    fn to_latex(self, settings: Settings) -> Result<String, LatexGenError> {
        // get a reference to the current supported program
        let instructions = Supported::is_supported(&Supported::New, &self)?;

        // initialize a new diagram
        let mut diagram = Diagram {
            settings,
            ..Default::default()
        };

        // initialize circuit with empty wires of all qubits in program
        let qubits = Program::get_used_qubits(&self);
        for qubit in &qubits {
            match qubit {
                instruction::Qubit::Fixed(name) => {
                    let wire = Wire {
                        name: *name,
                        ..Default::default()
                    };
                    diagram.circuit.insert(*name, Box::new(wire));
                }
                _ => (),
            }
        }

        // ensures set_ctrl_targ is called only if program has controlled gates
        let mut has_ctrl_targ = false;
        for instruction in instructions {
            // set QW for any unused qubits in this instruction
            diagram.set_qw(&qubits, &instruction);

            match instruction {
                // parse gate instructions into a new circuit
                instruction::Instruction::Gate(gate) => {
                    // for each qubit in a single gate instruction
                    for qubit in &gate.qubits {
                        match qubit {
                            instruction::Qubit::Fixed(qubit) => {
                                // create a new wire
                                let mut wire = Wire {
                                    name: *qubit,
                                    ..Default::default()
                                };

                                // set parameters for phase gates
                                if gate.name.contains("PHASE") {
                                    for expression in &gate.parameters {
                                        wire.set_param(
                                            expression,
                                            diagram.column,
                                            diagram.settings.texify_numerical_constants,
                                        );
                                    }
                                }

                                // update the gate name based on the modifiers
                                let gate_name = diagram.set_modifiers(&gate, &mut wire);

                                if let Some(_) = diagram.circuit.get(&qubit) {
                                    // has ctrl gate, must identify ctrls and targs after filling circuit
                                    if gate_name.starts_with('C') {
                                        has_ctrl_targ = true;
                                    }

                                    // add the gate to the wire at column 0
                                    wire.gates.insert(diagram.column, gate_name);
                                }

                                // push wire to diagram circuit
                                diagram.push_wire(wire)?;
                            }
                            _ => (),
                        }
                    }

                    diagram.column += 1;
                }
                // do nothing for all other instructions
                _ => (),
            }
        }

        // are implicit qubits required in settings and are there at least two or more qubits in the diagram?
        if diagram.settings.impute_missing_qubits {
            // add implicit qubits to circuit
            diagram
                .settings
                .impute_missing_qubits(diagram.column, &mut diagram.circuit);
        }

        // only call method for programs with control and target gates
        if has_ctrl_targ {
            // identify control and target qubits
            diagram.set_ctrl_targ()?;
        }

        let body = diagram.to_string();
        let document = Document {
            body: body,
            ..Default::default()
        };
        println!("{}", document.to_string());

        Ok(document.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::{Latex, Settings};
    use crate::Program;
    use std::str::FromStr;

    /// Helper function takes instructions and return the LaTeX using the
    /// Latex::to_latex method.
    fn get_latex(instructions: &str, settings: Settings) -> String {
        let program = Program::from_str(instructions).expect("Program should be returned");
        program
            .to_latex(settings)
            .expect("Program conversion to LaTeX should succeed")
    }

    #[test]
    /// Test functionality of to_latex using default settings.
    fn test_to_latex() {
        get_latex(
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
            Settings::default(),
        );
    }

    /// Test module for the Document
    mod document {
        use crate::program::latex::{tests::get_latex, Document, Settings};

        #[test]
        fn test_template() {
            insta::assert_snapshot!(get_latex("", Settings::default()));
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

    /// Test module for Supported (remove #[should_panic] when supported)
    mod supported {
        use crate::program::latex::{tests::get_latex, Settings};

        #[test]
        #[should_panic]
        fn test_supported_misc_instructions() {
            get_latex("NOP\nWAIT\nRESET\nHALT", Settings::default());
        }

        #[test]
        #[should_panic]
        fn test_supported_measure() {
            get_latex(
                "DECLARE ro BIT\nMEASURE 0\nMEASURE 1 ro[0]",
                Settings::default(),
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
                Settings::default(),
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
                Settings::default(),
            );
        }

        #[test]
        #[should_panic]
        fn test_supported_arithmetic_instruction() {
            get_latex(
                "DECLARE b BIT\nDECLARE theta REAL\nMOVE theta -3.14\nLT b theta -3.14",
                Settings::default(),
            );
        }

        #[test]
        #[should_panic]
        fn test_supported_modifier_forked() {
            get_latex(
                "FORKED CONTROLLED FORKED RX(a,b,c,d) 0 1 2 3",
                Settings::default(),
            );
        }
    }

    /// Test module for gates
    mod gates {
        use crate::program::latex::{tests::get_latex, Settings};

        #[test]
        fn test_gate_x() {
            insta::assert_snapshot!(get_latex("X 0", Settings::default()));
        }

        #[test]
        fn test_gate_y() {
            insta::assert_snapshot!(get_latex("Y 1", Settings::default()));
        }

        #[test]
        fn test_gates_x_and_y_single_qubit() {
            insta::assert_snapshot!(get_latex("X 0\nY 0", Settings::default()));
        }

        #[test]
        fn test_gates_x_and_y_two_qubits() {
            insta::assert_snapshot!(get_latex("X 0\nY 1", Settings::default()));
        }

        #[test]
        fn test_gates_phase_pi_rotation() {
            insta::assert_snapshot!(get_latex("PHASE(pi) 0", Settings::default()));
        }

        #[test]
        fn test_gates_cnot_ctrl_0_targ_1() {
            insta::assert_snapshot!(get_latex("CNOT 0 1", Settings::default()));
        }

        #[test]
        fn test_gates_cnot_ctrl_1_targ_0() {
            insta::assert_snapshot!(get_latex("CNOT 1 0", Settings::default()));
        }

        #[test]
        #[should_panic]
        fn test_gates_cnot_error_single_qubit() {
            get_latex("CNOT 0 0", Settings::default());
        }

        #[test]
        fn test_gates_h_and_cnot_ctrl_0_targ_1() {
            insta::assert_snapshot!(get_latex("H 0\nCNOT 0 1", Settings::default()));
        }

        #[test]
        fn test_gates_h_and_cnot_ctrl_1_targ_0() {
            insta::assert_snapshot!(get_latex("H 1\nCNOT 1 0", Settings::default()));
        }

        #[test]
        fn test_gate_toffoli() {
            insta::assert_snapshot!(get_latex("CCNOT 1 2 0", Settings::default()));
        }

        #[test]
        fn test_gate_ccnot_and_controlled_cnot_equality() {
            let ccnot = get_latex("CCNOT 1 2 0", Settings::default());

            let controlled = get_latex("CONTROLLED CNOT 1 2 0", Settings::default());

            assert_eq!(ccnot, controlled);
        }

        #[test]
        fn test_gate_cphase() {
            insta::assert_snapshot!(get_latex("CPHASE(pi) 0 1", Settings::default()));
        }

        #[test]
        fn test_gate_cz() {
            insta::assert_snapshot!(get_latex("CZ 0 1", Settings::default()));
        }
    }

    /// Test module for modifiers
    mod modifiers {
        use crate::program::latex::{tests::get_latex, Settings};

        #[test]
        fn test_modifier_toffoli_gate() {
            insta::assert_snapshot!(get_latex("CONTROLLED CNOT 2 1 0", Settings::default()));
        }

        #[test]
        fn test_modifier_controlled_cnot_and_ccnot_equality() {
            let controlled = get_latex("CONTROLLED CNOT 2 1 0", Settings::default());

            let ccnot = get_latex("CCNOT 2 1 0", Settings::default());

            assert_eq!(controlled, ccnot);
        }

        #[test]
        fn test_modifier_dagger() {
            insta::assert_snapshot!(get_latex("DAGGER X 0", Settings::default()));
        }

        #[test]
        fn test_modifier_dagger_dagger() {
            insta::assert_snapshot!(get_latex("DAGGER DAGGER Y 0", Settings::default()));
        }

        #[test]
        fn test_modifier_dagger_cz() {
            insta::assert_snapshot!(get_latex("DAGGER CZ 0 1", Settings::default()));
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

    /// Test module for Settings
    mod settings {
        use crate::program::latex::{tests::get_latex, Settings};

        #[test]
        fn test_settings_texify_numerical_constants_true_supported_symbol() {
            // default texify_numerical_constants is true
            let settings = Settings {
                ..Default::default()
            };
            insta::assert_snapshot!(get_latex("CPHASE(alpha) 0 1", settings));
        }

        #[test]
        fn test_settings_texify_numerical_constants_false_supported_symbol() {
            let settings = Settings {
                texify_numerical_constants: false,
                ..Default::default()
            };
            insta::assert_snapshot!(get_latex("CPHASE(alpha) 0 1", settings));
        }

        #[test]
        fn test_settings_texify_numerical_constants_unsupported_symbol() {
            // default texify_numerical_constants is true
            let settings_true = Settings {
                ..Default::default()
            };

            let unsupported_true = get_latex("CPHASE(chi) 0 1", settings_true);

            let settings_false = Settings {
                texify_numerical_constants: false,
                ..Default::default()
            };

            let unsupported_false = get_latex("CPHASE(chi) 0 1", settings_false);

            // unsupported symbols are treated as text regardless of setting
            assert_eq!(unsupported_true, unsupported_false);
        }

        #[test]
        fn test_settings_label_qubit_lines_false() {
            let settings = Settings {
                label_qubit_lines: false,
                ..Default::default()
            };
            insta::assert_snapshot!(get_latex("H 0\nCNOT 0 1", settings));
        }

        #[test]
        fn test_settings_impute_missing_qubits_true_ctrl_less_than_targ() {
            let settings = Settings {
                impute_missing_qubits: true,
                ..Default::default()
            };
            insta::assert_snapshot!(get_latex("H 0\nCNOT 0 3", settings));
        }

        #[test]
        fn test_settings_impute_missing_qubits_true_ctrl_greater_than_targ() {
            let settings = Settings {
                impute_missing_qubits: true,
                ..Default::default()
            };
            insta::assert_snapshot!(get_latex("H 5\nCNOT 5 2", settings));
        }
    }

    /// Test various programs for LaTeX accuracy
    mod programs {
        use crate::program::latex::{tests::get_latex, Settings};

        #[test]
        fn test_program_h0_cnot01_x1_cnot12() {
            let settings = Settings {
                ..Default::default()
            };
            insta::assert_snapshot!(get_latex("H 0\nCNOT 0 1\nX 1\nCNOT 1 2", settings));
        }

        #[test]
        fn test_program_h5_cnot52_y2_cnot23() {
            let settings = Settings {
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
                Settings::default(),
            );

            insta::assert_snapshot!(latex);
        }

        #[test]
        fn test_program_good_simple_params() {
            insta::assert_snapshot!(get_latex(
                "CPHASE(1.0) 0 1\nCPHASE(1.0-2.0i) 1 0",
                Settings::default()
            ));
        }

        #[test]
        fn test_program_good_complex_params() {
            insta::assert_snapshot!(get_latex(
                "CPHASE(pi/2) 1 0\nCPHASE(cos(sin(2*pi/3))*cis(-1)*exp(i*pi)) 3 4",
                Settings::default()
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
                Settings::default(),
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
                Settings::default(),
            );

            insta::assert_snapshot!(latex);
        }
    }
}
