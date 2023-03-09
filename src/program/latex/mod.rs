//! LaTeX diagram generation for quil programs.
//!
//! Provides a feature to generate diagrams using the LaTeX subpackage TikZ/
//! Quantikz for a given quil Program.
//!
//! - Usage: `Program.to_latex(settings: Settings);`
//!
//! - Description:
//! [`Quantikz`] is a subpackage in the TikZ package used to generate qubit
//! circuits. A qubit is represented as a wire separated into multiple columns.
//! Each column contains a symbol of an operation on the qubit. Multiple qubits
//! can be stacked into rows with interactions between any number of them drawn
//! as a connecting bar to each involved qubit wire. Commands are used to
//! control what is rendered on a circuit, e.g. names of qubits, identifying
//! control/target qubits, gates, etc. View [`Quantikz`] for the documentation
//! on its usage and full set of commands.
//!
//! This module should be viewed as a self contained partial implementation of
//! [`Quantikz`] with all available commands listed as variants in a Command
//! enum. This feature provides the user variability in how they wish to render
//! their Program circuits with metadata contained in a Settings struct.
//!
//! [`Quantikz`]: https://arxiv.org/pdf/1809.03842.pdf

use std::collections::{BTreeMap, HashMap};
use std::fmt::{format, Display};

use crate::instruction;
use crate::Program;

/// Available commands used for building circuits with the same names taken
/// from the Quantikz documentation for easy reference. LaTeX string denoted
/// inside `backticks`.
///     Single wire commands: lstick, rstick, qw, meter
///     Multi-wire commands: ctrl, targ, control, (swap, targx)
pub enum Command {
    /// `\lstick{\ket{q_{u32}}}`: Make a qubit "stick out" from the left.
    Lstick(String),
    /// `\rstick{\ket{q_{u32}}}`: Make a qubit "stick out" from the right.
    Rstick(String),
    /// ` \gate{name}`: Make a gate on the wire.
    Gate(String),
    /// `\qw`: Connect the current cell to the previous cell i.e. "do nothing".
    Qw,
    /// `\\`: Start a new row
    Nr,
    /// `\meter{wire}`: Measure a qubit.
    Meter(String),
    /// `\ctrl{wire}`: Make a control qubit--different from Control.
    Ctrl(String),
    /// `\targ{}`: Make a controlled-not gate.
    Targ,
    /// `\control{}`: Make a controlled-phase gate--different from Ctrl.
    Control,
    /// `\swap{wire}`: Make a swap gate--used with TargX.
    Swap(String),
    /// `\targX{}`: Make a qubit the target for a swap--used with Swap.
    TargX,
}

impl Command {
    /// Returns the LaTeX String for a given Command variant.
    ///
    /// # Arguments
    /// `command` - A Command variant.
    ///
    /// # Examples
    /// ```
    /// use quil_rs::program::latex::Command;
    /// let ket_0 = "0".to_string();
    /// let lstick_ket_0 = Command::get_command(Command::Lstick(ket_0));
    /// ```
    pub fn get_command(command: Self) -> String {
        match command {
            Self::Lstick(wire) => format(format_args!(r#"\lstick{{\ket{{q_{{{wire}}}}}}}"#)),
            Self::Rstick(wire) => format(format_args!(r#"\rstick{{\ket{{q_{{{wire}}}}}}}"#)),
            Self::Gate(name) => format(format_args!(r#"\gate{{{name}}}"#)),
            Self::Qw => r"\qw".to_string(),
            Self::Nr => r"\\".to_string(),
            Self::Meter(wire) => format(format_args!(r#"\meter{{{wire}}}"#)),
            Self::Ctrl(wire) => format(format_args!(r#"\ctrl{{{wire}}}"#)),
            Self::Targ => r"\targ{}".to_string(),
            Self::Control => r"\control{}".to_string(),
            Self::Swap(wire) => format(format_args!(r#"\swap{{{wire}}}"#)),
            Self::TargX => r"\targX{}".to_string(),
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
            /// false: π is pi.
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

// TODO: Implement functions to update the settings that allows the user customzie the rendering of the circuit.
impl Settings {
    pub fn label_qubit_lines(&self, name: u64) -> String {
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
    pub fn impute_missing_qubits(&self, circuit: &mut BTreeMap<u64, Box<Wire>>) {
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
                    let wire = Wire {
                        name: qubit,
                        ..Default::default()
                    };
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

// TODO: Move TikZ/Quantikz into a separate struct. Keep Document abstract enough to represent any variant of LaTeX Documents.
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
    /// cicuit. In particular, only run this method if a Quil program contains
    /// multi qubit gates.
    ///
    /// # Arguments
    /// `&mut self` - self as mutible allowing to update the circuit qubits
    fn set_ctrl_targ(&mut self) {
        // ensure every column preserves the connection between ctrl and targ
        'column: for c in 0..=self.column {
            let mut ctrls = vec![]; // the control qubits
            let mut targ = None; // the targ qubit

            // determine if a relationship exists at this column
            if let Some(relationship) = self.relationships.get(&c) {
                // determine the control and target qubits
                for qubit in relationship {
                    // a relationship with one qubit is invalid
                    if relationship.len() < 2 {
                        panic!("{}", LatexGenError::FoundCNOTWithNoTarget);
                    }

                    // the last qubit is the targ
                    if *qubit == relationship[relationship.len() - 1] {
                        if let Some(wire) = self.circuit.get_mut(qubit) {
                            // insert as target at this column
                            wire.targ.insert(c, true);

                            // set 'column loop targ variable to this targ that control qubits will find distance from on their respective wires
                            targ = Some(wire.name)
                        }
                    // all other qubits are the controls
                    } else {
                        if let Some(wire) = self.circuit.get_mut(qubit) {
                            // insert as control at this column with initial value 0, targeting themselves
                            wire.ctrl.insert(c, 0);

                            // push ctrl to 'column loop ctrl variables with initial value requiring update based on targ
                            ctrls.push(wire.name);
                        }
                    }
                }
            } else {
                // no relationships found on this column, go to next
                continue 'column;
            }

            // determine the physical vector where a positive vector points from control to target, negative, from target to control. The magnitude of the vector is the absolute value of the distance between them
            if let Some(targ) = targ {
                // distance between qubits is the space between the ctrl and targ qubits in the circuit
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
                    // set wire at column as the control qubit of target qubit computed as the distance from the control qubit
                    self.circuit
                        .get_mut(&ctrl)
                        .and_then(|wire| wire.ctrl.insert(c, vector));
                }
            }
        }
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
    fn push_wire(&mut self, wire: Wire) {
        let qubit = wire.name;

        // find wire in circuit collection
        match self.circuit.get_mut(&wire.name) {
            // wire found, push to existing wire
            Some(wire_in_circuit) => {
                // indicate a new item to be added by incrementing column
                self.column += 1;

                // get the new gate from the wire and insert into existing wire
                if let Some(gate) = wire.gates.get(&0) {
                    // add gates to wire in circuit
                    wire_in_circuit.gates.insert(self.column, gate.to_string());
                }
            }
            // no wire found insert new wire
            None => {
                self.circuit.insert(wire.name, Box::new(wire));
            }
        }

        // initalize relationships between multi qubit gates
        if let Some(wire) = self.circuit.get(&qubit) {
            // get the newly added gate if any at the column it was added
            if let Some(gate) = wire.gates.get(&self.column) {
                // tag relationships for multi qubit gates
                if gate.starts_with('C') {
                    // add the qubits to the set of related qubits in the current column
                    if let Some(qubits) = self.relationships.get_mut(&self.column) {
                        qubits.push(qubit);
                    } else {
                        self.relationships.insert(self.column, vec![qubit]);
                    }
                }
            }
        }
    }
}

impl Display for Diagram {
    /// Converts the Diagram Circuit to LaTeX string. Returns a Result.
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
                for c in 0..=self.column {
                    if let Some(gate) = wire.gates.get(&c) {
                        line.push_str(" & ");

                        if gate.starts_with('C') {
                            if let Some(targ) = wire.ctrl.get(&c) {
                                line.push_str(&Command::get_command(Command::Ctrl(
                                    targ.to_string(),
                                )));
                            } else if let Some(_) = wire.targ.get(&c) {
                                line.push_str(&Command::get_command(Command::Targ));
                            }
                        } else {
                            line.push_str(&Command::get_command(Command::Gate(gate.to_string())));
                        }
                    } else {
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
pub struct Wire {
    /// the name of ket(qubit) placed using the Lstick or Rstick commands
    name: u64,
    /// gate elements placed at column on wire using the Gate command
    gates: HashMap<u32, String>,
    /// control at column with distance from targ wire
    ctrl: HashMap<u32, i64>,
    /// at this column is the wire a target?
    targ: HashMap<u32, bool>,
}

impl Default for Wire {
    fn default() -> Self {
        Self {
            name: 0,
            gates: HashMap::new(),
            ctrl: HashMap::new(),
            targ: HashMap::new(),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum LatexGenError {
    #[error("Tried to parse CNOT and found a control qubit without a target.")]
    FoundCNOTWithNoTarget,
}

pub trait Latex {
    /// Returns a Result containing a quil Program as a LaTeX string.
    ///
    /// # Arguments
    /// `settings` - Customizes the rendering of a circuit.
    fn to_latex(self, settings: Settings) -> Result<String, LatexGenError>;
}

impl Latex for Program {
    fn to_latex(self, settings: Settings) -> Result<String, LatexGenError> {
        // get a reference to the current program
        let instructions = Program::to_instructions(&self, false);

        // store circuit strings
        let mut diagram = Diagram {
            settings,
            ..Default::default()
        };
        let mut has_ctrl_targ = false;
        for instruction in instructions {
            match instruction {
                // parse gate instructions into a new circuit
                instruction::Instruction::Gate(gate) => {
                    // for each qubit in a single gate instruction
                    for qubit in gate.qubits {
                        match qubit {
                            instruction::Qubit::Fixed(qubit) => {
                                // create a new wire
                                let mut wire = Wire::default();

                                // set name of wire for any qubit variant as String
                                wire.name = qubit;

                                // TODO: reduce code duplication
                                if let Some(_) = diagram.circuit.get(&qubit) {
                                    // add the gate to the wire at column 0
                                    wire.gates.insert(0, gate.name.clone());
                                } else {
                                    if gate.name.starts_with('C') {
                                        wire.gates.insert(diagram.column, gate.name.clone());

                                        if !has_ctrl_targ {
                                            has_ctrl_targ = true;
                                        }
                                    } else {
                                        // add the gate to the wire at column 0
                                        wire.gates.insert(0, gate.name.clone());
                                    }
                                }

                                // push wire to diagram circuit
                                diagram.push_wire(wire);
                            }
                            _ => (),
                        }
                    }
                }
                // do nothing for all other instructions
                _ => (),
            }
        }

        // are implicit qubits required in settings and are there at least two or more qubits in the diagram?
        if diagram.settings.impute_missing_qubits {
            // add implicit qubits to circuit
            diagram.settings.impute_missing_qubits(&mut diagram.circuit);
        }

        // only call method for programs with control and target gates
        if has_ctrl_targ {
            // identify control and target qubits
            diagram.set_ctrl_targ();
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
    pub fn get_latex(instructions: &str, settings: Settings) -> String {
        let program =
            Program::from_str(instructions).expect("program `{instructions}` should be returned");
        program
            .to_latex(settings)
            .expect("LaTeX should generate for program `{instructions}`")
    }

    #[test]
    /// Test functionality of to_latex using default settings.
    fn test_to_latex() {
        let program = Program::from_str("H 5\nCNOT 5 2").expect("Quil program should be returned");

        let settings = Settings {
            impute_missing_qubits: true,
            ..Default::default()
        };

        program
            .to_latex(settings)
            .expect("LaTeX should generate for Quil program");
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
    }

    /// Test module for Quantikz Commands
    mod commands {
        use crate::program::latex::Command;

        #[test]
        fn test_command_left_ket() {
            insta::assert_snapshot!(Command::get_command(Command::Lstick("0".to_string())));
        }

        #[test]
        fn test_command_right_ket() {
            insta::assert_snapshot!(Command::get_command(Command::Rstick("0".to_string())));
        }

        #[test]
        fn test_command_gate() {
            insta::assert_snapshot!(Command::get_command(Command::Gate("X".to_string())));
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
        fn test_command_measure() {
            insta::assert_snapshot!(Command::get_command(Command::Meter("0".to_string())));
        }

        #[test]
        fn test_command_control() {
            insta::assert_snapshot!(Command::get_command(Command::Ctrl("0".to_string())));
        }

        #[test]
        fn test_command_cnot_target() {
            insta::assert_snapshot!(Command::get_command(Command::Targ));
        }

        #[test]
        fn test_command_cphase_target() {
            insta::assert_snapshot!(Command::get_command(Command::Control));
        }

        #[test]
        fn test_command_swap() {
            insta::assert_snapshot!(Command::get_command(Command::Swap("0".to_string())));
        }

        #[test]
        fn test_command_swap_target() {
            insta::assert_snapshot!(Command::get_command(Command::TargX));
        }
    }

    /// Test module for Settings
    mod settings {
        use crate::program::latex::{tests::get_latex, Settings};

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
    }
}
