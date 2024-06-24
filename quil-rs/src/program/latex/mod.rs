//! LaTeX circuit generation for quil programs.
//!
//! This module enables generating quantum circuits using the LaTeX subpackage
//! TikZ/[`Quantikz`] for a given quil [`Program`]. This feature is callable on
//! [`Program`] and returns a LaTeX string which can be rendered in a LaTeX
//! visualization tool. Be aware that not all Programs can be serialized as
//! LaTeX. If a [`Program`] contains a gate or modifier not mentioned in the
//! [Supported Gates and Modifiers](#supported-gates-and-modifiers) section
//! below, unexpected results may occur, one of which includes producing
//! incorrect quantum circuits, or an error will be returned detailing which
//! instruction or gate is unsupported in the Program being processed.
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

mod circuit;
mod diagram;

use crate::Program;

use self::circuit::Circuit;

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

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::Alpha => write!(f, r"\alpha"),
            Symbol::Beta => write!(f, r"\beta"),
            Symbol::Gamma => write!(f, r"\gamma"),
            Symbol::Phi => write!(f, r"\phi"),
            Symbol::Pi => write!(f, r"\pi"),
            Symbol::Text(text) => write!(f, r#"\text{{{}}}"#, text),
        }
    }
}

impl From<String> for Symbol {
    fn from(text: String) -> Self {
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
            // true: pi is π.
            texify_numerical_constants: true,
            // true: `CNOT 0 2` would have three qubit lines: 0, 1, 2.
            impute_missing_qubits: false,
            // false: remove Lstick/Rstick from latex.
            label_qubit_lines: true,
            // true: `RX(pi)` displayed as `X_{\\pi}` instead of `R_X(\\pi)`.
            abbreviate_controlled_rotations: false,
            // 0: condenses the size of subdiagrams.
            qubit_line_open_wire_length: 1,
            // false: include Meter in the current column.
            right_align_terminal_measurements: true,
        }
    }
}

#[derive(Clone, Debug, thiserror::Error)]
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

pub trait ToLatex {
    fn to_latex(self, settings: RenderSettings) -> Result<String, LatexGenError>;
}

impl ToLatex for Program {
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
    /// use quil_rs::{Program, program::latex::{RenderSettings, ToLatex}};
    /// use std::str::FromStr;
    /// let program = Program::from_str("H 0\nCNOT 0 1").expect("");
    /// let latex = program.to_latex(RenderSettings::default()).expect("");
    /// ```
    ///
    /// ```
    /// // To LaTeX for the Toffoli Gate Program.
    /// use quil_rs::{Program, program::latex::{RenderSettings, ToLatex}};
    /// use std::str::FromStr;
    /// let program = Program::from_str("CONTROLLED CNOT 2 1 0").expect("");
    /// let latex = program.to_latex(RenderSettings::default()).expect("");
    /// ```
    ///
    /// Ryan: this should take &self, no need for latex generation to consume a program
    ///  esp if it fails to render
    fn to_latex(self, settings: RenderSettings) -> Result<String, LatexGenError> {
        let circuit = Circuit::try_from_program(&self, settings).unwrap(); // TODO: error
        let diagram = diagram::Diagram::from(circuit);
        Ok(diagram.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::{RenderSettings, ToLatex};
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
        use crate::program::latex::{tests::get_latex, RenderSettings};

        #[test]
        fn test_template() {
            insta::assert_snapshot!(get_latex("", RenderSettings::default()));
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
