/**
 * Copyright 2021 Rigetti Computing
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 **/
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, fmt};

use crate::expression::Expression;

#[derive(Clone, Debug, PartialEq)]
pub enum ArithmeticOperand {
    LiteralInteger(i64),
    LiteralReal(f64),
    MemoryReference(MemoryReference),
}

impl fmt::Display for ArithmeticOperand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ArithmeticOperand::LiteralInteger(value) => write!(f, "{}", value),
            ArithmeticOperand::LiteralReal(value) => write!(f, "{}", value),
            ArithmeticOperand::MemoryReference(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ArithmeticOperator {
    Add,
    Subtract,
    Divide,
    Multiply,
}

impl fmt::Display for ArithmeticOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ArithmeticOperator::Add => write!(f, "ADD"),
            ArithmeticOperator::Divide => write!(f, "DIV"),
            ArithmeticOperator::Multiply => write!(f, "MUL"),
            ArithmeticOperator::Subtract => write!(f, "SUB"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum AttributeValue {
    String(String),
    Expression(Expression),
}

impl fmt::Display for AttributeValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use AttributeValue::*;
        match self {
            String(value) => write!(f, "{}", value),
            Expression(value) => write!(f, "{}", value),
        }
    }
}

pub type FrameAttributes = HashMap<String, AttributeValue>;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Calibration {
    pub instructions: Vec<Instruction>,
    pub modifiers: Vec<GateModifier>,
    pub name: String,
    pub parameters: Vec<Expression>,
    pub qubits: Vec<Qubit>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct FrameIdentifier {
    pub name: String,
    pub qubits: Vec<Qubit>,
}

impl fmt::Display for FrameIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} \"{}\"", format_qubits(&self.qubits), self.name)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum GateModifier {
    Controlled,
    Dagger,
    Forked,
}

impl fmt::Display for GateModifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use GateModifier::*;
        write!(
            f,
            "{}",
            match self {
                Controlled => "CONTROLLED",
                Dagger => "DAGGER",
                Forked => "FORKED",
            }
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum GateType {
    Matrix,
    Permutation,
}

impl fmt::Display for GateType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use GateType::*;
        write!(
            f,
            "{}",
            match self {
                Matrix => "MATRIX",
                Permutation => "PERMUTATION",
            }
        )
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum ScalarType {
    Bit,
    Integer,
    Octet,
    Real,
}

impl fmt::Display for ScalarType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ScalarType::*;
        write!(
            f,
            "{}",
            match self {
                Bit => "BIT",
                Integer => "INTEGER",
                Octet => "OCTET",
                Real => "REAL",
            }
        )
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct Vector {
    pub data_type: ScalarType,
    pub length: u64,
}

impl fmt::Display for Vector {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}[{}]", self.data_type, self.length)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct WaveformInvocation {
    pub name: String,
    pub parameters: HashMap<String, Expression>,
}

impl fmt::Display for WaveformInvocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut key_value_pairs = self
            .parameters
            .iter()
            .collect::<Vec<(&String, &Expression)>>();

        key_value_pairs.sort_by(|(k1, _), (k2, _)| k1.cmp(&k2));

        write!(
            f,
            "{}({})",
            self.name,
            key_value_pairs
                .iter()
                .map(|(k, v)| format!("{}: {}", k, v))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct MemoryReference {
    pub name: String,
    pub index: u64,
}

impl Eq for MemoryReference {}

impl fmt::Display for MemoryReference {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}[{}]", self.name, self.index)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    Gate {
        name: String,
        parameters: Vec<Expression>,
        qubits: Vec<Qubit>,
        modifiers: Vec<GateModifier>,
    },
    CircuitDefinition {
        name: String,
        parameters: Vec<String>,
        // These cannot be fixed qubits and thus are not typed as `Qubit`
        qubit_variables: Vec<String>,
        instructions: Vec<Instruction>,
    },
    GateDefinition {
        name: String,
        parameters: Vec<String>,
        matrix: Vec<Vec<Expression>>,
        r#type: GateType,
    },
    Declaration {
        name: String,
        size: Vector,
        sharing: Option<String>,
    },
    Measurement {
        qubit: Qubit,
        target: Option<MemoryReference>,
    },
    Reset {
        qubit: Option<Qubit>,
    },
    CalibrationDefinition(Box<Calibration>),
    Capture {
        frame: FrameIdentifier,
        memory_reference: MemoryReference,
        waveform: Box<WaveformInvocation>,
    },
    Delay {
        duration: Expression,
        frame_names: Vec<String>,
        qubits: Vec<Qubit>,
    },
    Fence {
        qubits: Vec<Qubit>,
    },
    FrameDefinition {
        identifier: FrameIdentifier,
        attributes: HashMap<String, AttributeValue>,
    },
    MeasureCalibrationDefinition {
        qubit: Option<Qubit>,
        parameter: String,
        instructions: Vec<Instruction>,
    },
    Pragma {
        name: String,
        arguments: Vec<String>,
        data: Option<String>,
    },
    Pulse {
        blocking: bool,
        frame: FrameIdentifier,
        waveform: Box<WaveformInvocation>,
    },
    RawCapture {
        frame: FrameIdentifier,
        duration: Expression,
        memory_reference: MemoryReference,
    },
    SetFrequency {
        frame: FrameIdentifier,
        frequency: f64,
    },
    SetPhase {
        frame: FrameIdentifier,
        phase: Expression,
    },
    SetScale {
        frame: FrameIdentifier,
        scale: Expression,
    },
    ShiftFrequency {
        frame: FrameIdentifier,
        frequency: f64,
    },
    ShiftPhase {
        frame: FrameIdentifier,
        phase: Expression,
    },
    SwapPhases {
        frame_1: FrameIdentifier,
        frame_2: FrameIdentifier,
    },
    WaveformDefinition {
        name: String,
        definition: Waveform,
    },
    Arithmetic {
        operator: ArithmeticOperator,
        destination: ArithmeticOperand,
        source: ArithmeticOperand,
    },
    Halt,
    Label(String),
    Move {
        destination: ArithmeticOperand,
        source: ArithmeticOperand,
    },
    Exchange {
        left: ArithmeticOperand,
        right: ArithmeticOperand,
    },
    Load {
        destination: MemoryReference,
        source: String,
        offset: MemoryReference,
    },
    Store {
        destination: String,
        offset: MemoryReference,
        source: ArithmeticOperand,
    },
    Jump {
        target: String,
    },
    JumpWhen {
        target: String,
        condition: MemoryReference,
    },
    JumpUnless {
        target: String,
        condition: MemoryReference,
    },
}

#[derive(Clone, Debug)]
pub enum InstructionRole {
    ClassicalCompute,
    ControlFlow,
    ProgramComposition,
    RFControl,
}

impl From<&Instruction> for InstructionRole {
    fn from(instruction: &Instruction) -> Self {
        match instruction {
            Instruction::CalibrationDefinition(_)
            | Instruction::CircuitDefinition { .. }
            | Instruction::Declaration { .. }
            | Instruction::FrameDefinition { .. }
            | Instruction::Gate { .. }
            | Instruction::GateDefinition { .. }
            | Instruction::Label(_)
            | Instruction::MeasureCalibrationDefinition { .. }
            | Instruction::Measurement { .. }
            | Instruction::Pragma { .. }
            | Instruction::WaveformDefinition { .. } => InstructionRole::ProgramComposition,
            Instruction::Reset { .. }
            | Instruction::Capture { .. }
            | Instruction::Delay { .. }
            | Instruction::Fence { .. }
            | Instruction::Pulse { .. }
            | Instruction::RawCapture { .. }
            | Instruction::SetFrequency { .. }
            | Instruction::SetPhase { .. }
            | Instruction::SetScale { .. }
            | Instruction::ShiftFrequency { .. }
            | Instruction::ShiftPhase { .. }
            | Instruction::SwapPhases { .. } => InstructionRole::RFControl,
            Instruction::Arithmetic { .. }
            | Instruction::Move { .. }
            | Instruction::Exchange { .. }
            | Instruction::Load { .. }
            | Instruction::Store { .. } => InstructionRole::ClassicalCompute,
            Instruction::Halt
            | Instruction::Jump { .. }
            | Instruction::JumpWhen { .. }
            | Instruction::JumpUnless { .. } => InstructionRole::ControlFlow,
        }
    }
}

pub fn format_instructions(values: &[Instruction]) -> String {
    values
        .iter()
        .map(|i| format!("{}", i))
        .collect::<Vec<String>>()
        .join("\n\t")
}

pub fn format_integer_vector(values: &[u64]) -> String {
    values
        .iter()
        .map(|q| format!("{}", q))
        .collect::<Vec<String>>()
        .join(" ")
}

pub fn format_matrix(matrix: &[Vec<Expression>]) -> String {
    matrix
        .iter()
        .map(|row| {
            row.iter()
                .map(|cell| format!("{}", cell))
                .collect::<Vec<String>>()
                .join(", ")
        })
        .collect::<Vec<String>>()
        .join("\n\t")
}

pub fn format_qubits(qubits: &[Qubit]) -> String {
    qubits
        .iter()
        .map(|q| format!("{}", q))
        .collect::<Vec<String>>()
        .join(" ")
}

pub fn get_parameter_string(parameters: &[Expression]) -> String {
    if parameters.is_empty() {
        return String::from("");
    }

    let parameter_str: String = parameters.iter().map(|e| format!("{}", e)).collect();
    format!("({})", parameter_str)
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Instruction::*;
        match self {
            Arithmetic {
                operator,
                destination,
                source,
            } => write!(f, "{} {} {}", operator, destination, source),
            CalibrationDefinition(calibration) => {
                let parameter_str = get_parameter_string(&calibration.parameters);
                write!(
                    f,
                    "DEFCAL {}{} {}:",
                    calibration.name,
                    parameter_str,
                    format_qubits(&calibration.qubits)
                )?;
                for instruction in &calibration.instructions {
                    write!(f, "\n\t{}", instruction)?;
                }
                writeln!(f)
            }
            Capture {
                frame,
                waveform,
                memory_reference,
            } => write!(f, "CAPTURE {} {} {}", frame, waveform, memory_reference),
            CircuitDefinition {
                name,
                parameters,
                qubit_variables,
                instructions,
            } => {
                let mut parameter_str: String = parameters
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                if !parameter_str.is_empty() {
                    parameter_str = format!("({})", parameter_str)
                }
                write!(f, "DEFCIRCUIT {}{}", name, parameter_str)?;
                for qubit_variable in qubit_variables {
                    write!(f, " {}", qubit_variable)?;
                }
                writeln!(f, ":")?;
                for instruction in &**instructions {
                    writeln!(f, "\t{}", instruction)?;
                }
                Ok(())
            }
            Declaration {
                name,
                size,
                sharing,
            } => {
                write!(f, "DECLARE {} {}", name, size)?;
                match sharing {
                    Some(shared) => write!(f, "SHARING {}", shared)?,
                    None => {}
                }
                Ok(())
            }
            Delay {
                qubits,
                frame_names,
                duration,
            } => {
                write!(f, "DELAY {}", format_qubits(&qubits))?;
                for frame_name in frame_names {
                    write!(f, " \"{}\"", frame_name)?;
                }
                write!(f, " {}", duration)
            }
            Fence { qubits } => write!(f, "FENCE {}", format_qubits(&qubits)),
            FrameDefinition {
                identifier,
                attributes,
            } => write!(
                f,
                "DEFFRAME {}:\n{}",
                identifier,
                attributes
                    .iter()
                    .map(|(k, v)| format!("\n\t{}: {}", k, v))
                    .collect::<String>()
            ),
            Gate {
                name,
                parameters,
                qubits,
                modifiers,
            } => {
                let parameter_str = get_parameter_string(parameters);

                let qubit_str = format_qubits(&qubits);
                let modifier_str = modifiers
                    .iter()
                    .map(|m| format!("{} ", m))
                    .collect::<Vec<String>>()
                    .join("");
                write!(f, "{}{}{} {}", modifier_str, name, parameter_str, qubit_str)
            }
            GateDefinition {
                name,
                parameters,
                matrix,
                r#type,
            } => {
                let parameter_str: String = parameters.iter().map(|p| p.to_string()).collect();
                writeln!(f, "DEFGATE {}{} AS {}:", name, parameter_str, r#type)?;
                for row in matrix {
                    writeln!(
                        f,
                        "\t{}",
                        row.iter()
                            .map(|cell| format!("{}", cell))
                            .collect::<Vec<String>>()
                            .join(",")
                    )?;
                }
                Ok(())
            }
            MeasureCalibrationDefinition {
                qubit,
                parameter,
                instructions,
            } => {
                write!(f, "DEFCAL MEASURE")?;
                match qubit {
                    Some(qubit) => {
                        write!(f, " {}", qubit)?;
                    }
                    None => {}
                }

                writeln!(
                    f,
                    " %{}:\n\t{}",
                    parameter,
                    format_instructions(instructions)
                )
            }
            Measurement { qubit, target } => match target {
                Some(reference) => write!(f, "MEASURE {} {}", qubit, reference),
                None => write!(f, "MEASURE {}", qubit),
            },
            Move {
                destination,
                source,
            } => write!(f, "MOVE {} {}", destination, source),
            Exchange { left, right } => write!(f, "EXCHANGE {} {}", left, right),
            Load {
                destination,
                source,
                offset,
            } => {
                write!(f, "LOAD {} {} {}", destination, source, offset)
            }
            Store {
                destination,
                offset,
                source,
            } => {
                write!(f, "STORE {} {} {}", destination, offset, source)
            }
            Pulse {
                blocking,
                frame,
                waveform,
            } => {
                if !blocking {
                    write!(f, "NONBLOCKING ")?;
                }
                write!(f, "PULSE {} {}", frame, waveform)
            }
            Pragma {
                name,
                arguments,
                data,
            } => match data {
                // FIXME: Handle empty argument lists
                Some(data) => write!(f, "PRAGMA {} {} {}", name, arguments.join(" "), data),
                None => write!(f, "PRAGMA {} {}", name, arguments.join(" ")),
            },
            RawCapture {
                frame,
                duration,
                memory_reference,
            } => write!(f, "RAW-CAPTURE {} {} {}", frame, duration, memory_reference),
            Reset { qubit } => match qubit {
                Some(qubit) => write!(f, "RESET {}", qubit),
                None => write!(f, "RESET"),
            },
            SetFrequency { frame, frequency } => write!(f, "SET-FREQUENCY {} {}", frame, frequency),
            SetPhase { frame, phase } => write!(f, "SET-PHASE {} {}", frame, phase),
            SetScale { frame, scale } => write!(f, "SET-SCALE {} {}", frame, scale),
            ShiftFrequency { frame, frequency } => {
                write!(f, "SHIFT-FREQUENCY {} {}", frame, frequency)
            }
            ShiftPhase { frame, phase } => write!(f, "SHIFT-PHASE {} {}", frame, phase),
            SwapPhases { frame_1, frame_2 } => write!(f, "SWAP-PHASES {} {}", frame_1, frame_2),
            WaveformDefinition { name, definition } => write!(
                f,
                "DEFWAVEFORM {} {} {}:{}",
                name,
                definition.parameters.join(","),
                definition.sample_rate,
                definition
                    .matrix
                    .iter()
                    .map(|e| format!("{}", e))
                    .collect::<String>()
            ),
            Halt => write!(f, "HALT"),
            Jump { target } => write!(f, "JUMP @{}", target),
            JumpUnless { condition, target } => write!(f, "JUMP-UNLESS @{} {}", target, condition),
            JumpWhen { condition, target } => write!(f, "JUMP-WHEN @{} {}", target, condition),
            Label(label) => write!(f, "LABEL @{}", label),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Qubit {
    Fixed(u64),
    Variable(String),
}

impl fmt::Display for Qubit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Qubit::*;
        match self {
            Fixed(value) => write!(f, "{}", value),
            Variable(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Waveform {
    pub matrix: Vec<Expression>,
    pub parameters: Vec<String>,
    pub sample_rate: f64,
}
