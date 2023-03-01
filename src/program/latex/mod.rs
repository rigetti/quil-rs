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

use std::collections::HashMap;
use std::fmt::{format, Display};

use crate::Program;
use crate::instruction;

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
            Self::Lstick(wire) => 
                format(format_args!(r#"\lstick{{\ket{{q_{{{wire}}}}}}}"#)),
            Self::Rstick(wire) => 
                format(format_args!(r#"\rstick{{\ket{{q_{{{wire}}}}}}}"#)),
            Self::Gate(name) => 
                format(format_args!(r#"\gate{{{name}}}"#)),
            Self::Qw => r"\qw".to_string(),
            Self::Nr => r"\\".to_string(),
            Self::Meter(wire) => 
                format(format_args!(r#"\meter{{{wire}}}"#)),
            Self::Ctrl(wire) => 
                format(format_args!(r#"\ctrl{{{wire}}}"#)),
            Self::Targ => r"\targ{}".to_string(),
            Self::Control => r"\control{}".to_string(),
            Self::Swap(wire) => 
                format(format_args!(r#"\swap{{{wire}}}"#)),
            Self::TargX => r"\targX{}".to_string(),
        }
    }
}

/// Settings contains the metadata that allows the user to customize how the 
/// circuit is rendered or use the default implementation.
#[derive(Debug)]
pub struct Settings {
    /// Convert numerical constants, e.g. pi, to LaTeX form.
    texify_numerical_constants: bool,
    /// Include all qubits implicitly referenced in the Quil program.
    impute_missing_qubits: bool,
    /// Label qubit lines.
    label_qubit_lines: bool,
    /// Write controlled rotations in compact form.
    abbreviate_controlled_rotations: bool,
    /// Extend the length of open wires at the right of the diagram.
    qubit_line_open_wire_length: u32,
    /// Align measurement operations to appear at the end of the diagram.
    right_align_terminal_measurements: bool,
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
            header:
r"\documentclass[convert={density=300,outext=.png}]{standalone}
\usepackage[margin=1in]{geometry}
\usepackage{tikz}
\usetikzlibrary{quantikz}
\begin{document}
\begin{tikzcd}".to_string(), 
            body: "".to_string(), 
            footer:
r"\end{tikzcd}
\end{document}".to_string(),
        }
    }
}

impl Display for Document {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}{}", self.header, self.body, self.footer)
    }
}

/// A Diagram represents a collection of circuits. It encodes the relationships 
/// between the circuits and their positions or the row that it fills. A row is 
/// one of the Circuits in the HashMap. At this view over the circuits, Diagram 
/// can form a relationship between circuits based on information about the 
/// column and row. For example, one row, say qubit 0, at some column can hold 
/// information that it is the control. If another row, say qubit 1, at this 
/// same exact column says that it is the target, then it can inform qubit 0 
/// that it is controlling qubit 1. This information is then placed into the 
/// circuit as the diagram forms the equivalent LaTeX form for each qubit.
#[derive(Debug)]
struct Diagram {
    /// Settings
    settings: Settings,
    /// preserves the order of wires through indexing the circuit keys
    order: Vec<String>,
    /// a HashMap of wires with the name of the wire as the key
    circuit: HashMap<String, Box<Wire>>,
}

impl Diagram {
    /// Takes a new or existing wire and adds or updates it using the name
    /// (String) as the key. If a wire exists with the same name, then the 
    /// contents of the new wire are added to it by updating the next column 
    /// using the Quantikz command associated with its attributes (e.g. gate, 
    /// do_nothing, etc).
    /// 
    /// # Arguments
    /// `&mut self` - exposes HashMap<String, Box<Circuit>>
    /// `circuit` - the circuit to be pushed or updated in circuits
    fn push_wire(&mut self, wire: Wire) {
        // find wire in circuit collection
        match self.circuit.get_mut(&wire.name) {
            // wire found, push to existing wire
            Some(wire_in_circuit) => {
                // indicate a new item to be added by incrementing column
                wire_in_circuit.column += 1;

                // if wire contains gates to add
                if !wire.gates.is_empty() {
                    if let Some(gate) = wire.gates.get(&0) {
                        // add gates to wire in circuit
                        wire_in_circuit.gates.insert(wire_in_circuit.column, gate.to_string());
                    }
                }

            },
            // no wire found, preserve insertion order and insert new wire
            None => {
                self.order.push(wire.name.clone());
                self.circuit.insert(wire.name.clone(), Box::new(wire));
            },
        }
        
        // println!("{:?}", wire);
    }
}

impl Display for Diagram {
    /// Converts the Diagram Circuit to LaTeX string. Returns a Result.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // add a newline between the first line and the header
        let mut body = String::from('\n');

        let mut i = 0; // used to omit trailing Nr
        for key in &self.order {
            // a single line of LaTeX representing a wire from the circuit   
            let mut line = String::from("");

            // are labels on in settings?
            if self.settings.label_qubit_lines {
                // add label to left side of wire
                line.push_str(&Command::get_command(Command::Lstick(key.to_string())));
            }

            // convert each attribute other than the default to string.
            if let Some(wire) = self.circuit.get(key) {
                for c in 0..=wire.column {
                    if let Some(gate) = wire.gates.get(&c) {
                        line.push_str(" & ");
                        line.push_str(&Command::get_command(Command::Gate(gate.to_string())));
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

/// A circuit represents a single wire. A wire chains columns together of 
/// various Quantikz elements (using `&`). Encoded in each column is an index 
/// which determines the placement of the element on the circuit. Each column 
/// can hold only one element, therefore, each encoded index is unique between 
/// all of the attributes. Using this property, a String can be generated. 
#[derive(Debug)]
struct Wire {
    /// abstract attribute representing total-1 elements on the circuit 
    column: u32,
    /// a wire, ket(qubit) placed using the Lstick or Rstick commands
    name: String,
    /// gate element(s) placed at column_u32 on wire using the Gate command
    gates: HashMap<u32, String>,
    /// a column_u32 that contains nothing placed using the Qw command  
    do_nothing: Vec<u32>,
}

impl Default for Wire {
    fn default() -> Self {
        Self { 
            column: 0,
            name: String::from(""), 
            gates: HashMap::new(), 
            do_nothing: vec![],
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum LatexGenError {
    // TODO: Add variants for each error type using `thiserror` crate to return detailed Result::Err. Example error below.
    #[error("Tried to pop gate from new circuit and append to wire={wire} but found None.")]
    NoGateInInst{wire: String},
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

        // instruction
        // X 0, Y 1, 

        // store circuit strings
        let mut diagram = Diagram {settings, order: vec![], circuit: HashMap::new()};

        for instruction in instructions {
            match instruction {
                // parse gate instructions into a new circuit
                instruction::Instruction::Gate(gate) => {
                    // println!("GATE: {:?}", gate.name);

                    // for each qubit in a single gate instruction
                    for qubit in gate.qubits {
                        // create a new wire
                        let mut wire = Wire::default();

                        match qubit {
                            // for Fixed and Variable qubit variants
                            _ => {
                                // set the name of the wire to the qubit name
                                wire.name = qubit.to_string();
                            }
                        }

                        // add the gate to the wire at column 0
                        wire.gates.insert(wire.column, gate.name.clone());

                        // push wire to diagram circuit
                        diagram.push_wire(wire);

                        // println!("WIRE: {:?}", wire);
                    }
                },
                // do nothing for all other instructions
                _ => (),
            }
        }

        let body = diagram.to_string();
        let document = Document {body: body, ..Default::default()};
        println!("{}", document.to_string());

        Ok(document.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::{Settings, Latex};
    use crate::Program;
    use std::str::FromStr;

    /// Helper function takes instructions and return the LaTeX using the 
    /// Latex::to_latex method.
    pub fn get_latex(instructions: &str) -> String {
        let program = Program::from_str(instructions).expect("program `{instructions}` should be returned");
        program
            .to_latex(Settings::default())
            .expect("LaTeX should generate for program `{instructions}`")
    }

    #[test]
    /// Test functionality of to_latex using default settings.
    fn test_to_latex() {
        let program = Program::from_str("X 0\nY 1").expect("");
        program.to_latex(Settings::default()).expect("");
    }

    mod document {
        use crate::program::latex::{Document, tests::get_latex};

        #[test]
        fn test_template() {
            insta::assert_snapshot!(get_latex(""));
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

    mod gates {
        use crate::program::latex::tests::get_latex;

        #[test]
        fn test_gate_x() {
            insta::assert_snapshot!(get_latex("X 0"));
        }

        #[test]
        fn test_gate_y() {
            insta::assert_snapshot!(get_latex("Y 1"));
        }

        #[test]
        fn test_gates_x_and_y_single_qubit() {
            insta::assert_snapshot!(get_latex("X 0\nY 0"));
        }

        #[test]
        fn test_gates_x_and_y_two_qubits() {
            insta::assert_snapshot!(get_latex("X 0\nY 1"));
        }

        // #[test]
        // fn test_gate_controlled() {
        //     insta::assert_snapshot!(get_latex("CONTROLLED H 3 2"));
        // }
    }

    /// Test module for command Operators
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
}
