use std::collections::{BTreeMap, BTreeSet, HashSet};

use super::{
    circuit::{Circuit, CircuitElement},
    RenderSettings,
};

// Write every element from an iterable
macro_rules! write_many {
    ($f:expr, $iter:expr) => {{
        for item in $iter {
            write!($f, "{item}")?;
        }

        Ok(())
    }};
}

// Write every element from an iterable, wrapped with a left and right element
macro_rules! write_many_delimited {
    ($f:expr, $left:expr, $iter:expr, $right: expr) => {{
        write!($f, "{}", $left)?;

        write_many!($f, $iter)?;

        write!($f, "{}", $right)
    }};
}

/// Available commands used for building circuits with the same names taken
/// from the Quantikz documentation for easy reference. LaTeX string denoted
/// inside `backticks`.
///
/// # Available Commands
///
///   Single wire commands: lstick, gate, phase, super, qw, nr
///   Multi-wire commands: ctrl, targ
/// Ryan: derive_more::Display would be great here. All of the symbols are repeated between here and the display implementation
/// for discussion: tradeoffs of literal representation of Quantikz vs more ergonomics (both are acceptable, just consider the audience)
///   in our case, we don't expect this to be a general-purpose quantikz library, so it should be whatever makes this module most
///   readable.
#[derive(Clone, Debug)]
enum DiagramElement {
    /// `\lstick{<contents>}`: Make a qubit "stick out" from the left.
    ///
    /// Example: `\lstick{\ket{q_{u32}}}`
    Lstick(Vec<DiagramElement>),
    /// `\gate{<contents>}`: Make a gate on the wire.
    Gate(Vec<DiagramElement>),
    /// `\phase{<contents>}`: Make a phase on the wire with a rotation
    Phase(Vec<DiagramElement>),
    /// `^{<contents>}`: Add a superscript to a gate
    Super(Vec<DiagramElement>),
    /// `\qw`: Connect the current cell to the previous cell i.e. "do nothing".
    Qw,
    /// `\\`: Start a new row
    Nr,
    /// `\ctrl{wire}`: Make a control qubit
    Ctrl {
        distance: i64,
    },
    /// `\targ{}`: Make a controlled-not gate.
    Targ,

    /// `&`: Start a new cell in the same row/wire
    Ampersand,

    /// `\meter{}` - Measurement
    Meter,

    Ket(String),

    Text(String),

    String(String),

    Dagger,

    Symbol(String),
}

impl DiagramElement {
    fn daggers(count: u64) -> Vec<Self> {
        (0..count)
            .map(|_| Self::Super(vec![Self::Dagger]))
            .collect()

        // This would work fine IIUC but would not pass snapshot
        // Self::Super(vec![Self::Dagger; count as usize])
    }

    fn texify_parameter(name: String) -> Self {
        // TODO: make static
        let supported = [
            "alpha", "beta", "gamma", "theta", "pi", //TODO
        ]
        .into_iter()
        .collect::<HashSet<_>>();

        if supported.contains(&name.as_str()) {
            Self::Symbol(name)
        } else {
            Self::Text(name)
        }
    }

    fn parameters(parameters: Vec<String>, settings: &RenderSettings) -> Vec<Self> {
        if settings.texify_numerical_constants {
            parameters.into_iter().map(Self::texify_parameter).collect()
        } else {
            parameters.into_iter().map(Self::Text).collect()
        }
    }

    fn gate(
        name: String,
        parameters: Vec<String>,
        dagger_count: u64,
        controlled: bool,
        settings: &RenderSettings,
    ) -> Self {
        match name.as_str() {
            // TODO: DAGGER CPHASE?
            "PHASE" if dagger_count == 0 => Self::Phase(Self::parameters(parameters, settings)),
            // TODO: DAGGER NOT?
            "NOT" | "X" if dagger_count == 0 && controlled => Self::Targ,
            _ => {
                let mut gate_elements = vec![DiagramElement::String(name)];
                gate_elements.append(&mut DiagramElement::daggers(dagger_count));
                // if !parameters.is_empty() //TODO

                Self::Gate(gate_elements)
            }
        }
    }

    fn qubit(index: u64) -> Self {
        Self::Ket(format!("q_{{{index}}}"))
    }
}

impl std::fmt::Display for DiagramElement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DiagramElement::Lstick(wire) => {
                write!(f, r#"\lstick{{"#)?;
                write_many!(f, wire)?;
                write!(f, "}}")
            }
            DiagramElement::Gate(elements) => {
                write!(f, r"\gate{{")?;
                write_many!(f, elements)?;
                write!(f, "}}")
            }
            DiagramElement::Phase(inner) => write_many_delimited!(f, r"\phase{", inner, "}"),
            DiagramElement::Super(inner) => write_many_delimited!(f, r"^{", inner, "}"),
            DiagramElement::Qw => write!(f, r"\qw"),
            DiagramElement::Nr => write!(f, r"\\"),
            DiagramElement::Ctrl { distance } => write!(f, r#"\ctrl{{{distance}}}"#),
            DiagramElement::Targ => write!(f, r"\targ{{}}"),
            DiagramElement::Ampersand => write!(f, "&"),
            DiagramElement::Meter => write!(f, r"\meter{{}}"),
            DiagramElement::Ket(contents) => write!(f, r"\ket{{{contents}}}"),
            DiagramElement::Text(string) => write!(f, r"\text{{{string}}}"),
            DiagramElement::Dagger => write!(f, r"\dagger"),
            DiagramElement::String(string) => write!(f, "{string}"),
            DiagramElement::Symbol(inner) => write!(f, r#"\{inner}"#),
        }
    }
}

#[derive(Debug)]
pub struct Diagram {
    footer: Option<String>,
    header: Option<String>,
    wires: BTreeMap<u64, DiagramWire>,
}

impl Diagram {
    const DEFAULT_FOOTER: &'static str = r"\end{tikzcd}
\end{document}";

    const DEFAULT_HEADER: &'static str = r"\documentclass[convert={density=300,outext=.png}]{standalone}
\usepackage[margin=1in]{geometry}
\usepackage{tikz}
\usetikzlibrary{quantikz}
\begin{document}
\begin{tikzcd}";

    fn push_to_cell(&mut self, row: u64, column: u64, element: DiagramElement) {
        self.wires
            .entry(row)
            .or_default()
            .push_at_column(column, element);
    }

    fn from_qubit_indices<I>(qubits: I) -> Self
    where
        I: IntoIterator<Item = u64>,
    {
        let mut diagram = Self::default();
        for qubit in qubits {
            diagram.wires.insert(qubit, DiagramWire::default());
        }

        diagram
    }

    fn with_default_header_and_footer(&mut self) {
        self.with_footer(Self::DEFAULT_FOOTER);
        self.with_header(Self::DEFAULT_HEADER);
    }

    fn with_header<S>(&mut self, header: S)
    where
        S: Into<String>,
    {
        self.header = Some(header.into())
    }

    fn with_footer<S>(&mut self, footer: S)
    where
        S: Into<String>,
    {
        self.footer = Some(footer.into())
    }
}

impl Default for Diagram {
    fn default() -> Self {
        let mut new = Self {
            footer: None,
            header: None,
            wires: Default::default(),
        };
        new.with_default_header_and_footer();
        new
    }
}

impl std::fmt::Display for Diagram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(header) = &self.header {
            writeln!(f, "{header}")?;
        }

        let column_count = self
            .wires
            .values()
            .filter_map(|wire| wire.elements.last_key_value().map(|(k, _)| *k))
            .max()
            .unwrap_or(0);

        if !self.wires.is_empty() {
            let mut wires = self.wires.values().peekable();

            while let Some(wire) = wires.next() {
                wire.fmt(f, column_count)?;

                if wires.peek().is_some() {
                    writeln!(f, " {}", DiagramElement::Nr)?;
                }
            }

            writeln!(f)?;
        }

        if let Some(footer) = &self.footer {
            write!(f, "{footer}")?;
        }

        Ok(())
    }
}

impl From<Circuit> for Diagram {
    fn from(circuit: Circuit) -> Self {
        let qubit_indices = circuit.get_qubit_indices();

        let mut diagram = if circuit.settings.impute_missing_qubits {
            get_set_min_and_max_values(&qubit_indices)
                .map(|(&min, &max)| Diagram::from_qubit_indices(min..=max))
                .unwrap_or_default()
        } else {
            Diagram::from_qubit_indices(qubit_indices.iter().cloned())
        };

        for (column_index, column) in circuit.columns.into_iter().enumerate() {
            match column.element {
                CircuitElement::SingleQubitGate {
                    dagger_count,
                    parameters,
                    name,
                    qubit,
                    controls,
                } => {
                    let gate_element = DiagramElement::gate(
                        name,
                        parameters,
                        dagger_count,
                        !controls.is_empty(),
                        &circuit.settings,
                    );
                    diagram.push_to_cell(qubit, column_index as u64, gate_element);

                    for control in controls {
                        let distance = if qubit > control {
                            diagram.wires.range(control..qubit).count() as i64
                        } else {
                            -(diagram.wires.range(qubit..control).count() as i64)
                        };

                        diagram.push_to_cell(
                            control,
                            column_index as u64,
                            DiagramElement::Ctrl { distance },
                        );
                    }
                }
                CircuitElement::Measurement { qubit } => {
                    diagram.push_to_cell(qubit, column_index as u64, DiagramElement::Meter)
                }
            }

            if circuit.settings.label_qubit_lines {
                for (qubit, wire) in &mut diagram.wires {
                    wire.label_left = vec![DiagramElement::qubit(*qubit)]
                }
            }
        }

        diagram
    }
}

/// Wire == row
#[derive(Debug, Default)]
struct DiagramWire {
    pub label_left: Vec<DiagramElement>,
    pub elements: BTreeMap<u64, Vec<DiagramElement>>, // TODO: this could result in an invalid state, ie with lstick in the middle of a diagram (that's invalid, right?)
}

impl DiagramWire {
    fn push_at_column(&mut self, column: u64, element: DiagramElement) {
        self.elements.entry(column).or_default().push(element);
    }
}

impl DiagramWire {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, column_count: u64) -> std::fmt::Result {
        if self.label_left.is_empty() {
            write!(f, "{} &", DiagramElement::Qw)?;
        } else {
            write!(f, "{} &", DiagramElement::Lstick(self.label_left.clone()))?;
        }

        let mut last_column_index: i64 = -1;
        let mut elements = self.elements.iter().peekable();

        while let Some((&column, cell_elements)) = elements.next() {
            let gap = column as i64 - last_column_index;

            if cell_elements.is_empty() {
                continue;
            }

            for _ in 1..gap {
                write!(f, " {} {}", DiagramElement::Qw, DiagramElement::Ampersand)?;
            }

            for element in cell_elements {
                write!(f, " {element}")?;
            }

            if elements.peek().is_some() {
                write!(f, " {}", DiagramElement::Ampersand)?;
            }

            last_column_index = column as i64;
        }

        if last_column_index <= column_count as i64 {
            // Handle the first column specially; no leading ampersand needed
            if last_column_index == -1 {
                write!(f, " {}", DiagramElement::Qw)?;
                last_column_index = 0;
            }

            for _ in last_column_index..=(column_count as i64) {
                write!(f, " {} {}", DiagramElement::Ampersand, DiagramElement::Qw,)?;
            }
        }

        Ok(())
    }
}

fn get_set_min_and_max_values<T>(set: &BTreeSet<T>) -> Option<(&T, &T)> {
    if set.is_empty() {
        None
    } else {
        let min = set.iter().next().unwrap();
        let max = set.iter().next_back().unwrap();
        Some((min, max))
    }
}
