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
    /// let lstick_ket_0 = Command::get_command(Command::Lstick(0));
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
        write!(f, "{}\n{}\n{}", self.header, self.body, self.footer)
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
struct Diagram {
    /// A HashMap of circuits and the name of the wire as the key.
    circuits: HashMap<String, Box<Circuit>>,
}

impl Diagram {
    /// Takes a new or existing circuit and adds or updates it using the name
    /// (String) as the key. If the wire exists, then the circuit chains onto 
    /// it by updating the next column using the Quantikz command associated 
    /// with its attributes (e.g. gate, do_nothing, etc).
    /// 
    /// # Arguments
    /// `&mut self` - exposes HashMap<String, Box<Circuit>>
    /// `circuit` - the circuit to be pushed or updated in circuits
    fn push_circuit(&mut self, circuit: Circuit) {
        // allocate a new circuit onto the heap then push it to the circuits vec with a 
        match self.circuits.get_mut(&circuit.wire) {
            // update the exisiting circuit
            Some(circuit) => {
                // chain onto the old circuit
            },
            // add a new circuit
            None => {
                self.circuits.insert(circuit.wire.clone(), Box::new(circuit));},
        }
    }
}

impl Display for Diagram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TODO: Make me LaTeX")
    }
}

/// A circuit represents a single wire. A wire chains columns together of 
/// various Quantikz elements (using `&`). Encoded in each column is an index 
/// which determines the placement of the element on the circuit. Each column 
/// can hold only one element, therefore, each encoded index is unique between 
/// all of the attributes. Using this property, a String can be generated. 
#[derive(Debug)]
struct Circuit {
    /// abstract attribute representing total-1 elements on the circuit 
    column: u32,
    /// a wire, ket(qubit) placed using the Lstick or Rstick commands
    wire: String,
    /// gate element(s) placed at column_u32 on wire using the Gate command
    gate: Vec<(u32, String)>,
    /// a column_u32 that contains nothing placed using the Qw command  
    do_nothing: Vec<u32>,
}

impl Default for Circuit {
    fn default() -> Self {
        Self { 
            column: 0,
            wire: String::from(""), 
            gate: vec![], 
            do_nothing: vec![],
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum LatexGenError {
    // TODO: Add variants for each error type using `thiserror` crate to return detailed Result::Err. Example error below.
    #[error("This is an error on {qubit_index}.")]
    SomeError{qubit_index: u32},
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
        let mut diagram = Diagram { circuits: HashMap::new() };

        for instruction in instructions {
            let mut circuit = Circuit::default();

            match instruction {
                instruction::Instruction::Gate(gate) => {
                    // println!("{:?}", gate.name);

                    for qubit in gate.qubits {
                        match qubit {
                            _ => {
                                circuit.wire = qubit.to_string();
                            }
                        }
                    }

                    circuit.gate.push((circuit.column, gate.name));

                    // println!("{:?}", circuit);
                },
                _ => (),
            }

            diagram.push_circuit(circuit)
        }

        // Program of single qubit passing through two gates
        // X 0
        // Y 0
        // Circuit is a single wire with an X gate and a Y gate

        // If using a vector and stacking the program the following would produce
        // diagram => ["\lstick{\ket{0}} \gate{X} \qw", "\lstick{\ket{0}} \gate{Y} \qw"]
        // 
        // """
        // \lstick{\ket{0}} \gate{X} \qw
        // \lstick{\ket{0}} \gate{Y} \qw
        // """
        //
        // The program should produce:
        // \lstick{\ket{0}} \gate{x} \gate{y} \qw
        //
        // Need to manage this situation. Best way to do this would be to manage the state of a Diagram that contains Wires. Where each Wire Contains a Circuit with entries that contain details on how to manage particular cases, 
        // Questions to ask
        // is this qubit a control or target at this circuit entry at this point on the wire? 


        // TODO: Build the document body.
        let body = diagram.to_string();

        let document = Document {body: body, ..Default::default()};

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
        let program = Program::from_str("X 0").expect("");
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
        fn test_gate_controlled() {
            insta::assert_snapshot!(get_latex("CONTROLLED H 3 2"));
        }

        #[test]
        fn text_gates_x_and_y_single_qubit() {
            insta::assert_snapshot!(get_latex("X 0\nY 0"));
        }
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
