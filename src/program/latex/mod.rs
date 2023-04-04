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

mod diagram;

use std::{collections::HashSet, fmt};

use crate::instruction::{Instruction, Qubit};
use crate::Program;

use self::settings::RenderSettings;
use diagram::{wire::Wire, Diagram};

pub mod settings;

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

impl fmt::Display for Document {
    /// Returns the entire document in LaTeX string.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}{}", self.header, self.body, self.footer)
    }
}

/// Available commands used for building circuits with the same names taken
/// from the ``Quantikz`` documentation for easy reference. LaTeX string denoted
/// inside `backticks`.
#[derive(Clone, Debug, derive_more::Display, PartialEq, Eq, Hash)]
pub(crate) enum RenderCommand {
    /// Make a qubit "stick out" from the left.
    #[display(fmt = "\\lstick{{\\ket{{q_{{{_0}}}}}}}")]
    Lstick(u64),
    /// Make a gate on the wire with an optional superscript.
    #[display(fmt = "\\gate{{{_0}{_1}}}")]
    Gate(String, String),
    /// Make a phase on the wire with a rotation and optional superscript
    #[display(fmt = "\\phase{{{_0}{_1}}}")]
    Phase(Parameter, String),
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
    /// use quil_rs::{Program, program::latex::settings::RenderSettings};
    /// use std::str::FromStr;
    /// let program = Program::from_str("H 0\nCNOT 0 1").expect("");
    /// let latex = program.to_latex(RenderSettings::default()).expect("");
    /// ```
    ///
    /// ```
    /// // To LaTeX for the Toffoli Gate Program.
    /// use quil_rs::{Program, program::latex::settings::RenderSettings};
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
                let wire = Wire {
                    gates: Vec::with_capacity(instructions.len()),
                    ..Default::default()
                };
                diagram.circuit.insert(*name, Box::new(wire));
            }
        }

        // are implicit qubits required in settings and are there at least two or more qubits in the diagram?
        if diagram.settings.impute_missing_qubits {
            // add implicit qubits to circuit
            RenderSettings::impute_missing_qubits(instructions.len(), &mut diagram.circuit);
        }

        for instruction in instructions {
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
                diagram.apply_empty(&qubits, &gate);
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
    use super::{Program, RenderSettings};
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
            "DAGGER CPHASE(alpha) 0 1",
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

        #[test]
        #[should_panic]
        fn test_gate_xy() {
            get_latex("XY 0 1", RenderSettings::default());
        }

        #[test]
        #[should_panic]
        fn test_gate_controlled_xy() {
            get_latex("CONTROLLED XY 0 1 2", RenderSettings::default());
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
        fn test_modifier_phase_dagger() {
            insta::assert_snapshot!(get_latex("DAGGER PHASE(pi) 0", RenderSettings::default()));
        }

        #[test]
        fn test_modifier_dagger_cz() {
            insta::assert_snapshot!(get_latex("DAGGER CZ 0 1", RenderSettings::default()));
        }

        #[test]
        fn test_modifier_cphase_dagger() {
            insta::assert_snapshot!(get_latex(
                "DAGGER CPHASE(alpha) 0 1",
                RenderSettings::default()
            ));
        }
    }

    /// Test module for ``Quantikz`` Commands
    mod commands {
        use crate::program::latex::{Parameter, RenderCommand, Symbol};

        #[test]
        fn test_command_left_ket() {
            insta::assert_snapshot!(RenderCommand::Lstick(0).to_string());
        }

        #[test]
        fn test_command_gate() {
            insta::assert_snapshot!(RenderCommand::Gate("X".to_string(), String::new()).to_string());
        }

        #[test]
        fn test_command_phase() {
            insta::assert_snapshot!(RenderCommand::Phase(
                Parameter::Symbol(Symbol::Pi),
                String::new()
            )
            .to_string());
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
