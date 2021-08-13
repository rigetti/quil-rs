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
use nom::{
    branch::alt,
    combinator::{map_res, opt},
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, tuple},
};

use super::{
    common::{
        self, parse_frame_attribute, parse_frame_identifier, parse_gate_modifier,
        parse_memory_reference, parse_qubit, parse_waveform_invocation,
    },
    expression::parse_expression,
    instruction, ParserInput, ParserResult,
};
use crate::parser::common::parse_variable_qubit;
use crate::parser::instruction::parse_block;
use crate::{
    instruction::{ArithmeticOperand, ArithmeticOperator, Calibration, Instruction, Waveform},
    parser::{
        error::{Error, ErrorKind},
        lexer::Token,
    },
    token,
};

/// Parse an arithmetic instruction of the form `destination source`.
/// Called using the arithmetic operator itself (such as `ADD`) which should be previously parsed.
pub fn parse_arithmetic(
    operator: ArithmeticOperator,
    input: ParserInput,
) -> ParserResult<Instruction> {
    let (input, destination_memory_reference) = common::parse_memory_reference(input)?;
    let destination = ArithmeticOperand::MemoryReference(destination_memory_reference);
    let (input, source) = common::parse_arithmetic_operand(input)?;
    Ok((
        input,
        Instruction::Arithmetic {
            operator,
            destination,
            source,
        },
    ))
}

/// Parse the contents of a `DECLARE` instruction.
pub fn parse_declare<'a>(input: ParserInput<'a>) -> ParserResult<'a, Instruction> {
    let (input, name) = token!(Identifier(v))(input)?;
    let (input, size) = common::parse_vector(input)?;
    Ok((
        input,
        Instruction::Declaration {
            name,
            sharing: None,
            size,
        },
    ))
}

/// Parse the contents of a `CAPTURE` instruction.
///
/// Unlike most other instructions, this can be _prefixed_ with the NONBLOCKING keyword,
/// and thus it expects and parses the CAPTURE token itself.
pub fn parse_capture(input: ParserInput, blocking: bool) -> ParserResult<Instruction> {
    let (input, frame) = common::parse_frame_identifier(input)?;
    let (input, waveform) = common::parse_waveform_invocation(input)?;
    let (input, memory_reference) = common::parse_memory_reference(input)?;

    Ok((
        input,
        Instruction::Capture {
            blocking,
            frame,
            waveform,
            memory_reference,
        },
    ))
}

/// Parse the contents of a `DEFCAL` instruction.
pub fn parse_defcal<'a>(input: ParserInput<'a>) -> ParserResult<'a, Instruction> {
    let (input, modifiers) = many0(parse_gate_modifier)(input)?;
    let (input, name) = token!(Identifier(v))(input)?;
    let (input, parameters) = opt(delimited(
        token!(LParenthesis),
        separated_list0(token!(Comma), parse_expression),
        token!(RParenthesis),
    ))(input)?;
    let parameters = parameters.unwrap_or_default();
    let (input, qubits) = many0(parse_qubit)(input)?;
    let (input, _) = token!(Colon)(input)?;
    let (input, instructions) = instruction::parse_block(input)?;
    Ok((
        input,
        Instruction::CalibrationDefinition(Calibration {
            instructions,
            modifiers,
            name,
            parameters,
            qubits,
        }),
    ))
}

/// Parse the contents of a `DEFFRAME` instruction.
pub fn parse_defframe<'a>(input: ParserInput<'a>) -> ParserResult<'a, Instruction> {
    let (input, identifier) = parse_frame_identifier(input)?;
    let (input, _) = token!(Colon)(input)?;
    let (input, attribute_pairs) = many1(parse_frame_attribute)(input)?;
    let attributes = attribute_pairs.iter().cloned().collect();

    Ok((
        input,
        Instruction::FrameDefinition {
            identifier,
            attributes,
        },
    ))
}

/// Parse the contents of a `DEFWAVEFORM` instruction.
pub fn parse_defwaveform<'a>(input: ParserInput<'a>) -> ParserResult<'a, Instruction> {
    let (input, name) = token!(Identifier(v))(input)?;
    let (input, parameters) = opt(delimited(
        token!(LParenthesis),
        separated_list0(token!(Comma), token!(Variable(v))),
        token!(RParenthesis),
    ))(input)?;
    let parameters = parameters.unwrap_or_default();

    let (input, sample_rate) = alt((
        map_res(token!(Float(v)), |v| Ok(v) as Result<f64, Error<&[Token]>>),
        map_res(token!(Integer(v)), |v| {
            let result = v as f64;
            if result as u64 != v {
                Err(Error {
                    input,
                    error: ErrorKind::UnsupportedPrecision,
                })
            } else {
                Ok(result)
            }
        }),
    ))(input)?;
    let (input, _) = tuple((token!(Colon), token!(NewLine), token!(Indentation)))(input)?;
    let (input, matrix) = separated_list1(token!(Comma), parse_expression)(input)?;

    Ok((
        input,
        Instruction::WaveformDefinition {
            name,
            definition: Waveform {
                matrix,
                parameters,
                sample_rate,
            },
        },
    ))
}

pub fn parse_defcircuit<'a>(input: ParserInput<'a>) -> ParserResult<'a, Instruction> {
    let (input, name) = token!(Identifier(v))(input)?;
    let (input, parameters) = opt(delimited(
        token!(LParenthesis),
        separated_list0(token!(Comma), token!(Variable(v))),
        token!(RParenthesis),
    ))(input)?;
    let parameters = parameters.unwrap_or_default();
    let (input, qubit_variables) = many0(parse_variable_qubit)(input)?;
    let (input, _) = token!(Colon)(input)?;
    let (input, instructions) = parse_block(input)?;

    Ok((
        input,
        Instruction::CircuitDefinition {
            name,
            parameters,
            qubit_variables,
            instructions,
        },
    ))
}

/// Parse the contents of a `DELAY` instruction.
pub fn parse_delay<'a>(input: ParserInput<'a>) -> ParserResult<'a, Instruction> {
    let (input, qubits) = many0(parse_qubit)(input)?;
    let (input, frame_names) = many0(token!(String(v)))(input)?;
    let (input, duration) = parse_expression(input)?;

    Ok((
        input,
        Instruction::Delay {
            frame_names,
            qubits,
            duration,
        },
    ))
}

/// Parse the contents of an `EXCHANGE` instruction.
pub fn parse_exchange(input: ParserInput) -> ParserResult<Instruction> {
    let (input, left) = common::parse_memory_reference(input)?;
    let (input, right) = common::parse_memory_reference(input)?;

    Ok((
        input,
        Instruction::Exchange {
            left: ArithmeticOperand::MemoryReference(left),
            right: ArithmeticOperand::MemoryReference(right),
        },
    ))
}

/// Parse the contents of a `FENCE` instruction.
pub fn parse_fence(input: ParserInput) -> ParserResult<Instruction> {
    let (input, qubits) = many0(parse_qubit)(input)?;

    Ok((input, Instruction::Fence { qubits }))
}

/// Parse the contents of a `JUMP` instruction.
pub fn parse_jump<'a>(input: ParserInput<'a>) -> ParserResult<'a, Instruction> {
    let (input, target) = token!(Label(v))(input)?;
    Ok((input, Instruction::Jump { target }))
}

/// Parse the contents of a `JUMP-WHEN` instruction.
pub fn parse_jump_when<'a>(input: ParserInput<'a>) -> ParserResult<'a, Instruction> {
    let (input, target) = token!(Label(v))(input)?;
    let (input, condition) = common::parse_memory_reference(input)?;
    Ok((input, Instruction::JumpWhen { condition, target }))
}

/// Parse the contents of a `JUMP-UNLESS` instruction.
pub fn parse_jump_unless<'a>(input: ParserInput<'a>) -> ParserResult<'a, Instruction> {
    let (input, target) = token!(Label(v))(input)?;
    let (input, condition) = common::parse_memory_reference(input)?;
    Ok((input, Instruction::JumpUnless { condition, target }))
}

/// Parse the contents of a `DECLARE` instruction.
pub fn parse_label<'a>(input: ParserInput<'a>) -> ParserResult<'a, Instruction> {
    let (input, name) = token!(Label(v))(input)?;
    Ok((input, Instruction::Label(name)))
}

/// Parse the contents of a `MOVE` instruction.
pub fn parse_move(input: ParserInput) -> ParserResult<Instruction> {
    let (input, destination) = common::parse_arithmetic_operand(input)?;
    let (input, source) = common::parse_arithmetic_operand(input)?;
    Ok((
        input,
        Instruction::Move {
            destination,
            source,
        },
    ))
}

/// Parse the contents of a `LOAD` instruction.
pub fn parse_load<'a>(input: ParserInput<'a>) -> ParserResult<'a, Instruction> {
    let (input, destination) = common::parse_memory_reference(input)?;
    let (input, source) = token!(Identifier(v))(input)?;
    let (input, offset) = common::parse_memory_reference(input)?;

    Ok((
        input,
        Instruction::Load {
            destination,
            source,
            offset,
        },
    ))
}

/// Parse the contents of a `STORE` instruction.
pub fn parse_store<'a>(input: ParserInput<'a>) -> ParserResult<'a, Instruction> {
    let (input, destination) = token!(Identifier(v))(input)?;
    let (input, offset) = common::parse_memory_reference(input)?;
    let (input, source) = common::parse_arithmetic_operand(input)?;

    Ok((
        input,
        Instruction::Store {
            destination,
            source,
            offset,
        },
    ))
}

/// Parse the contents of a `PRAGMA` instruction.
pub fn parse_pragma<'a>(input: ParserInput<'a>) -> ParserResult<'a, Instruction> {
    let (input, pragma_type) = token!(Identifier(v))(input)?;
    // FIXME: Also allow Int (not just Identifier)
    let (input, arguments) = many0(token!(Identifier(v)))(input)?;
    let (input, data) = opt(token!(String(v)))(input)?;
    Ok((
        input,
        Instruction::Pragma {
            name: pragma_type,
            arguments,
            data,
        },
    ))
}

/// Parse the contents of a `PULSE` instruction.
pub fn parse_pulse(input: ParserInput, blocking: bool) -> ParserResult<Instruction> {
    let (input, frame) = parse_frame_identifier(input)?;
    let (input, waveform) = parse_waveform_invocation(input)?;

    Ok((
        input,
        Instruction::Pulse {
            blocking,
            frame,
            waveform,
        },
    ))
}

/// Parse the contents of a `RAW-CAPTURE` instruction.
pub fn parse_raw_capture(input: ParserInput, blocking: bool) -> ParserResult<Instruction> {
    let (input, frame) = parse_frame_identifier(input)?;
    let (input, duration) = parse_expression(input)?;
    let (input, memory_reference) = parse_memory_reference(input)?;

    Ok((
        input,
        Instruction::RawCapture {
            blocking,
            frame,
            duration,
            memory_reference,
        },
    ))
}

/// Parse the contents of a `SET-FREQUENCY` instruction.
pub fn parse_set_frequency(input: ParserInput) -> ParserResult<Instruction> {
    let (input, frame) = parse_frame_identifier(input)?;
    let (input, frequency) = parse_expression(input)?;

    Ok((input, Instruction::SetFrequency { frame, frequency }))
}

/// Parse the contents of a `SET-PHASE` instruction.
pub fn parse_set_phase(input: ParserInput) -> ParserResult<Instruction> {
    let (input, frame) = parse_frame_identifier(input)?;
    let (input, phase) = parse_expression(input)?;

    Ok((input, Instruction::SetPhase { frame, phase }))
}

/// Parse the contents of a `SET-SCALE` instruction.
pub fn parse_set_scale(input: ParserInput) -> ParserResult<Instruction> {
    let (input, frame) = parse_frame_identifier(input)?;
    let (input, scale) = parse_expression(input)?;

    Ok((input, Instruction::SetScale { frame, scale }))
}

/// Parse the contents of a `SHIFT-FREQUENCY` instruction.
pub fn parse_shift_frequency(input: ParserInput) -> ParserResult<Instruction> {
    let (input, frame) = parse_frame_identifier(input)?;
    let (input, frequency) = parse_expression(input)?;

    Ok((input, Instruction::ShiftFrequency { frame, frequency }))
}

/// Parse the contents of a `SHIFT-PHASE` instruction.
pub fn parse_shift_phase(input: ParserInput) -> ParserResult<Instruction> {
    let (input, frame) = parse_frame_identifier(input)?;
    let (input, phase) = parse_expression(input)?;

    Ok((input, Instruction::ShiftPhase { frame, phase }))
}

/// Parse the contents of a `MEASURE` instruction.
pub fn parse_measurement(input: ParserInput) -> ParserResult<Instruction> {
    let (input, qubit) = parse_qubit(input)?;
    let (input, target) = match parse_memory_reference(input) {
        Ok((input, target)) => (input, Some(target)),
        Err(_) => (input, None),
    };

    Ok((input, Instruction::Measurement { qubit, target }))
}

#[cfg(test)]
mod tests {
    use crate::parser::lexer::lex;
    use crate::{
        instruction::{Instruction, MemoryReference, Qubit, ScalarType, Vector},
        make_test,
    };

    use super::{parse_declare, parse_defcircuit, parse_measurement, parse_pragma};
    use crate::expression::Expression;

    make_test!(
        declare_instruction_length_1,
        parse_declare,
        "ro BIT",
        Instruction::Declaration {
            name: "ro".to_owned(),
            sharing: None,
            size: Vector {
                data_type: ScalarType::Bit,
                length: 1
            }
        }
    );

    make_test!(
        declare_instruction_length_n,
        parse_declare,
        "ro INTEGER[5]",
        Instruction::Declaration {
            name: "ro".to_owned(),
            sharing: None,
            size: Vector {
                data_type: ScalarType::Integer,
                length: 5
            }
        }
    );

    make_test!(
        measure_into_register,
        parse_measurement,
        "0 ro[0]",
        Instruction::Measurement {
            qubit: Qubit::Fixed(0),
            target: Some(MemoryReference {
                name: String::from("ro"),
                index: 0
            })
        }
    );

    make_test!(
        measure_discard,
        parse_measurement,
        "0",
        Instruction::Measurement {
            qubit: Qubit::Fixed(0),
            target: None
        }
    );

    make_test!(
        measure_named_qubit,
        parse_measurement,
        "q0 ro[0]",
        Instruction::Measurement {
            qubit: Qubit::Variable(String::from("q0")),
            target: Some(MemoryReference {
                name: String::from("ro"),
                index: 0
            })
        }
    );

    make_test!(
        measure_named_qubit_discard,
        parse_measurement,
        "q0",
        Instruction::Measurement {
            qubit: Qubit::Variable(String::from("q0")),
            target: None
        }
    );

    make_test!(
        pragma_inline_json,
        parse_pragma,
        "FILTER-NODE q35_unclassified \"{'module':'lodgepole.filters.io','filter_type':'DataBuffer','source':'q35_ro_rx/filter','publish':true,'params':{},'_type':'FilterNode'}\"",
        Instruction::Pragma {
            name: "FILTER-NODE".to_owned(),
            arguments: vec!["q35_unclassified".to_owned()],
            data: Some("{'module':'lodgepole.filters.io','filter_type':'DataBuffer','source':'q35_ro_rx/filter','publish':true,'params':{},'_type':'FilterNode'}".to_owned())
        }
    );

    make_test!(
        defcircuit_no_params,
        parse_defcircuit,
        "BELL a b:
    H a
    CNOT a b",
        Instruction::CircuitDefinition {
            name: "BELL".to_owned(),
            parameters: vec![],
            qubit_variables: vec!["a".to_owned(), "b".to_owned()],
            instructions: vec![
                Instruction::Gate {
                    name: "H".to_owned(),
                    parameters: vec![],
                    qubits: vec![Qubit::Variable("a".to_owned())],
                    modifiers: vec![],
                },
                Instruction::Gate {
                    name: "CNOT".to_owned(),
                    parameters: vec![],
                    qubits: vec![
                        Qubit::Variable("a".to_owned()),
                        Qubit::Variable("b".to_owned())
                    ],
                    modifiers: vec![],
                }
            ]
        }
    );

    make_test!(
        defcircuit_with_params,
        parse_defcircuit,
        "BELL(%a) a b:
    RZ(%a) a
    RX(%a) a
    RZ(%a) a
    CNOT a b",
        Instruction::CircuitDefinition {
            name: "BELL".to_owned(),
            parameters: vec!["a".to_owned()],
            qubit_variables: vec!["a".to_owned(), "b".to_owned()],
            instructions: vec![
                Instruction::Gate {
                    name: "RZ".to_owned(),
                    parameters: vec![Expression::Variable("a".to_owned())],
                    qubits: vec![Qubit::Variable("a".to_owned())],
                    modifiers: vec![],
                },
                Instruction::Gate {
                    name: "RX".to_owned(),
                    parameters: vec![Expression::Variable("a".to_owned())],
                    qubits: vec![Qubit::Variable("a".to_owned())],
                    modifiers: vec![],
                },
                Instruction::Gate {
                    name: "RZ".to_owned(),
                    parameters: vec![Expression::Variable("a".to_owned())],
                    qubits: vec![Qubit::Variable("a".to_owned())],
                    modifiers: vec![],
                },
                Instruction::Gate {
                    name: "CNOT".to_owned(),
                    parameters: vec![],
                    qubits: vec![
                        Qubit::Variable("a".to_owned()),
                        Qubit::Variable("b".to_owned())
                    ],
                    modifiers: vec![],
                }
            ]
        }
    );
}
