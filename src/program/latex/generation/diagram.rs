use self::tikz::{Tikz, quantikz::Quantikz};
use super::LatexFactory;

pub mod tikz;

pub struct DiagramFactory;

impl LatexFactory for DiagramFactory {
    fn create_tikz_diagram(&self) -> Box<dyn Tikz> {
        Box::new(Quantikz {})
    }
}

#[derive(Debug)]
/// Settings to control the layout and rendering of circuits.
pub struct DiagramSettings {
    /// Convert numerical constants, such as pi, to LaTeX form.
    texify_numerical_constants: bool,

    /// Include qubits with indices between those explicitly referenced in the Quil program.
    /// For example, if true, the diagram for `CNOT 0 2` would have three qubit lines: 0, 1, 2.
    impute_missing_qubits: bool,

    /// Label qubit lines.
    label_qubit_lines: bool,

    /// Write controlled rotations in a compact form.
    /// For example,  `RX(pi)` as `X_{\\pi}`, instead of the longer `R_X(\\pi)`
    abbreviate_controlled_rotations: bool,

    /// The length by which qubit lines should be extended with open wires at the right of the diagram.
    /// The default of 1 is the natural choice. The main reason for including this option
    /// is that it may be appropriate for this to be 0 in subdiagrams.
    qubit_line_open_wire_length: u32,

    /// Align measurement operations which appear at the end of the program.
    right_align_terminal_measurements: bool,
}

impl Default for DiagramSettings {
    fn default() -> Self {
        Self { 
            texify_numerical_constants: true, 
            impute_missing_qubits: false, 
            label_qubit_lines: true, 
            abbreviate_controlled_rotations: false, 
            qubit_line_open_wire_length: 1, 
            right_align_terminal_measurements: true,
        }
    }
}