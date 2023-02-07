use crate::Program;

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

#[derive(thiserror::Error, Debug)]
pub enum LatexGenError {
    // TODO: Add variants for each error type using `thiserror` crate to return detailed Result::Err.
    #[error("This is an error on {qubit_index}.")]
    SomeError{qubit_index: u32},
}

pub trait ToLatex {
    fn to_latex(self, diagram_settings: DiagramSettings) -> Result<String, LatexGenError>;
}

impl ToLatex for Program {
    fn to_latex(self, diagram_settings: DiagramSettings) -> Result<String, LatexGenError> {
        // TODO: Generate the Program LaTeX.
        let latex = "";

        Ok(latex.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::{DiagramSettings, ToLatex};
    use crate::Program;
    use std::str::FromStr;

    /// Take an instruction and return the LaTeX using the to_latex method.
    pub fn get_latex(instructions: &str) -> String {
        let program = Program::from_str(instructions).expect("Program should be returned.");
        program
            .to_latex(DiagramSettings::default())
            .expect("LaTeX should generate without error.")
    }

    #[test]
    /// Test functionality of to_latex using default settings.
    fn test_to_latex() {
        let program = Program::from_str("").expect("");
        program.to_latex(DiagramSettings::default()).expect("");
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
    }

    #[test]
    fn test_controlled_gate() {
        insta::assert_snapshot!(get_latex("CONTROLLED H 3 2"));
    }
}