// Copyright 2021 Rigetti Computing
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

use nom::{
    branch::alt,
    combinator::{map, opt},
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, preceded, tuple},
};

use crate::{
    instruction::{GateSpecification, GateType, PragmaArgument},
    parser::common::parse_variable_qubit,
};

use crate::instruction::{
    Arithmetic, ArithmeticOperand, ArithmeticOperator, BinaryLogic, BinaryOperator, Calibration,
    Capture, CircuitDefinition, Comparison, ComparisonOperator, Declaration, Delay, Exchange,
    Fence, FrameDefinition, GateDefinition, Instruction, Jump, JumpUnless, JumpWhen, Label, Load,
    MeasureCalibrationDefinition, Measurement, Move, Pragma, Pulse, Qubit, RawCapture, Reset,
    SetFrequency, SetPhase, SetScale, ShiftFrequency, ShiftPhase, Store, UnaryLogic, UnaryOperator,
    Waveform, WaveformDefinition,
};
use crate::parser::common::parse_permutation;
use crate::parser::instruction::parse_block;
use crate::parser::InternalParserResult;
use crate::token;

use super::{
    common::{
        self, parse_frame_attribute, parse_frame_identifier, parse_gate_modifier, parse_matrix,
        parse_memory_reference, parse_qubit, parse_waveform_invocation,
    },
    expression::parse_expression,
    instruction, ParserInput,
};

/// Parse an arithmetic instruction of the form `destination source`.
/// Called using the arithmetic operator itself (such as `ADD`) which should be previously parsed.
pub(crate) fn parse_arithmetic(
    operator: ArithmeticOperator,
    input: ParserInput,
) -> InternalParserResult<Instruction> {
    let (input, destination_memory_reference) = common::parse_memory_reference(input)?;
    let destination = ArithmeticOperand::MemoryReference(destination_memory_reference);
    let (input, source) = common::parse_arithmetic_operand(input)?;

    Ok((
        input,
        Instruction::Arithmetic(Arithmetic {
            operator,
            destination,
            source,
        }),
    ))
}

/// Parse a comparison instruction of the form `addr addr ( addr | number )`.
/// Called using the comparison operator itself (such as `EQ`) which should be previously parsed.
pub(crate) fn parse_comparison(
    operator: ComparisonOperator,
    input: ParserInput,
) -> InternalParserResult<Instruction> {
    let (input, destination) = common::parse_memory_reference(input)?;
    let (input, left) = common::parse_memory_reference(input)?;
    let (input, right) = common::parse_comparison_operand(input)?;

    Ok((
        input,
        Instruction::Comparison(Comparison {
            operator,
            operands: (destination, left, right),
        }),
    ))
}

/// Parse a logical binary instruction of the form `addr ( addr | INT )`.
/// Called using the logical operator itself (such as `AND`) which should be previously parsed.
pub(crate) fn parse_logical_binary(
    operator: BinaryOperator,
    input: ParserInput,
) -> InternalParserResult<Instruction> {
    let (input, left) = common::parse_memory_reference(input)?;
    let (input, right) = common::parse_binary_logic_operand(input)?;

    Ok((
        input,
        Instruction::BinaryLogic(BinaryLogic {
            operator,
            operands: (left, right),
        }),
    ))
}

/// Parse a logical unary instruction of the form `addr`.
/// Called using the logical operator itself (such as `NOT`) which should be previously parsed.
pub(crate) fn parse_logical_unary(
    operator: UnaryOperator,
    input: ParserInput,
) -> InternalParserResult<Instruction> {
    let (input, operand) = common::parse_memory_reference(input)?;

    Ok((
        input,
        Instruction::UnaryLogic(UnaryLogic { operator, operand }),
    ))
}

/// Parse the contents of a `DECLARE` instruction.
pub(crate) fn parse_declare<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, Instruction> {
    let (input, name) = token!(Identifier(v))(input)?;
    let (input, size) = common::parse_vector(input)?;
    Ok((
        input,
        Instruction::Declaration(Declaration {
            name,
            sharing: None,
            size,
        }),
    ))
}

/// Parse the contents of a `CAPTURE` instruction.
///
/// Unlike most other instructions, this can be _prefixed_ with the NONBLOCKING keyword,
/// and thus it expects and parses the CAPTURE token itself.
pub(crate) fn parse_capture(
    input: ParserInput,
    blocking: bool,
) -> InternalParserResult<Instruction> {
    let (input, frame) = common::parse_frame_identifier(input)?;
    let (input, waveform) = common::parse_waveform_invocation(input)?;
    let (input, memory_reference) = common::parse_memory_reference(input)?;

    Ok((
        input,
        Instruction::Capture(Capture {
            blocking,
            frame,
            memory_reference,
            waveform,
        }),
    ))
}

/// Parse the contents of a `DEFCAL` instruction (including `DEFCAL MEASURE`),
/// following the `DEFCAL` token.
pub(crate) fn parse_defcal<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, Instruction> {
    use crate::parser::lexer::Command::Measure;
    let (input, defcal_measure) = opt(token!(Command(Measure)))(input)?;
    match defcal_measure {
        Some(_) => parse_defcal_measure(input),
        None => parse_defcal_gate(input),
    }
}

/// Parse the contents of a `DEFCAL` instruction (but not `DEFCAL MEASURE`),
/// following the `DEFCAL` token.
pub(crate) fn parse_defcal_gate<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, Instruction> {
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

/// Parse the contents of a `DEFCAL MEASURE` instruction, following the `MEASURE` token.
pub(crate) fn parse_defcal_measure<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, Instruction> {
    let (input, qubit_index) = opt(token!(Integer(v)))(input)?;
    let qubit = qubit_index.map(Qubit::Fixed);
    let (input, destination) = token!(Identifier(v))(input)?;
    let (input, _) = token!(Colon)(input)?;
    let (input, instructions) = instruction::parse_block(input)?;
    Ok((
        input,
        Instruction::MeasureCalibrationDefinition(MeasureCalibrationDefinition {
            instructions,
            parameter: destination,
            qubit,
        }),
    ))
}

/// Parse the contents of a `DEFFRAME` instruction.
pub(crate) fn parse_defframe<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, Instruction> {
    let (input, identifier) = parse_frame_identifier(input)?;
    let (input, _) = token!(Colon)(input)?;
    let (input, attribute_pairs) = many1(parse_frame_attribute)(input)?;
    let attributes = attribute_pairs.iter().cloned().collect();

    Ok((
        input,
        Instruction::FrameDefinition(FrameDefinition {
            identifier,
            attributes,
        }),
    ))
}

/// Parse the contents of a `DEFGATE` instruction.
pub(crate) fn parse_defgate<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, Instruction> {
    let (input, name) = token!(Identifier(v))(input)?;
    let (input, parameters) = opt(delimited(
        token!(LParenthesis),
        separated_list1(token!(Comma), token!(Variable(v))),
        token!(RParenthesis),
    ))(input)?;
    let (input, gate_type) = opt(preceded(
        token!(As),
        alt((
            map(token!(Matrix), |()| GateType::Matrix),
            map(token!(Permutation), |()| GateType::Permutation),
        )),
    ))(input)?;
    let (input, _) = token!(Colon)(input)?;

    let gate_type = gate_type.unwrap_or(GateType::Matrix);
    let (input, specification) = match gate_type {
        GateType::Matrix => map(parse_matrix, GateSpecification::Matrix)(input)?,
        GateType::Permutation => map(parse_permutation, GateSpecification::Permutation)(input)?,
    };

    Ok((
        input,
        Instruction::GateDefinition(GateDefinition {
            name,
            parameters: parameters.unwrap_or_default(),
            specification,
        }),
    ))
}

/// Parse the contents of a `DEFWAVEFORM` instruction.
pub(crate) fn parse_defwaveform<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, Instruction> {
    let (input, name) = common::parse_waveform_name(input)?;
    let (input, parameters) = opt(delimited(
        token!(LParenthesis),
        separated_list0(token!(Comma), token!(Variable(v))),
        token!(RParenthesis),
    ))(input)?;
    let parameters = parameters.unwrap_or_default();

    let (input, _) = tuple((token!(Colon), token!(NewLine), token!(Indentation)))(input)?;
    let (input, matrix) = separated_list1(token!(Comma), parse_expression)(input)?;

    Ok((
        input,
        Instruction::WaveformDefinition(WaveformDefinition {
            name,
            definition: Waveform { matrix, parameters },
        }),
    ))
}

pub(crate) fn parse_defcircuit<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, Instruction> {
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
        Instruction::CircuitDefinition(CircuitDefinition {
            name,
            parameters,
            qubit_variables,
            instructions,
        }),
    ))
}

/// Parse the contents of a `DELAY` instruction.
pub(crate) fn parse_delay<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, Instruction> {
    let (input, qubits) = many0(parse_qubit)(input)?;
    let (input, frame_names) = many0(token!(String(v)))(input)?;
    let (input, duration) = parse_expression(input)?;

    Ok((
        input,
        Instruction::Delay(Delay {
            duration,
            frame_names,
            qubits,
        }),
    ))
}

/// Parse the contents of an `EXCHANGE` instruction.
pub(crate) fn parse_exchange(input: ParserInput) -> InternalParserResult<Instruction> {
    let (input, left) = common::parse_memory_reference(input)?;
    let (input, right) = common::parse_memory_reference(input)?;

    Ok((
        input,
        Instruction::Exchange(Exchange {
            left: ArithmeticOperand::MemoryReference(left),
            right: ArithmeticOperand::MemoryReference(right),
        }),
    ))
}

/// Parse the contents of a `FENCE` instruction.
pub(crate) fn parse_fence(input: ParserInput) -> InternalParserResult<Instruction> {
    let (input, qubits) = many0(parse_qubit)(input)?;

    Ok((input, Instruction::Fence(Fence { qubits })))
}

/// Parse the contents of a `JUMP` instruction.
pub(crate) fn parse_jump<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, Instruction> {
    let (input, target) = token!(Label(v))(input)?;
    Ok((input, Instruction::Jump(Jump { target })))
}

/// Parse the contents of a `JUMP-WHEN` instruction.
pub(crate) fn parse_jump_when<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, Instruction> {
    let (input, target) = token!(Label(v))(input)?;
    let (input, condition) = common::parse_memory_reference(input)?;
    Ok((input, Instruction::JumpWhen(JumpWhen { target, condition })))
}

/// Parse the contents of a `JUMP-UNLESS` instruction.
pub(crate) fn parse_jump_unless<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, Instruction> {
    let (input, target) = token!(Label(v))(input)?;
    let (input, condition) = common::parse_memory_reference(input)?;
    Ok((
        input,
        Instruction::JumpUnless(JumpUnless { target, condition }),
    ))
}

/// Parse the contents of a `DECLARE` instruction.
pub(crate) fn parse_label<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, Instruction> {
    let (input, name) = token!(Label(v))(input)?;
    Ok((input, Instruction::Label(Label(name))))
}

/// Parse the contents of a `MOVE` instruction.
pub(crate) fn parse_move(input: ParserInput) -> InternalParserResult<Instruction> {
    let (input, destination) = common::parse_arithmetic_operand(input)?;
    let (input, source) = common::parse_arithmetic_operand(input)?;
    Ok((
        input,
        Instruction::Move(Move {
            destination,
            source,
        }),
    ))
}

/// Parse the contents of a `LOAD` instruction.
pub(crate) fn parse_load<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, Instruction> {
    let (input, destination) = common::parse_memory_reference(input)?;
    let (input, source) = token!(Identifier(v))(input)?;
    let (input, offset) = common::parse_memory_reference(input)?;

    Ok((
        input,
        Instruction::Load(Load {
            destination,
            source,
            offset,
        }),
    ))
}

/// Parse the contents of a `STORE` instruction.
pub(crate) fn parse_store<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, Instruction> {
    let (input, destination) = token!(Identifier(v))(input)?;
    let (input, offset) = common::parse_memory_reference(input)?;
    let (input, source) = common::parse_arithmetic_operand(input)?;

    Ok((
        input,
        Instruction::Store(Store {
            destination,
            offset,
            source,
        }),
    ))
}

/// Parse the contents of a `PRAGMA` instruction.
pub(crate) fn parse_pragma<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, Instruction> {
    let (input, pragma_type) = token!(Identifier(v))(input)?;
    let (input, arguments) = many0(alt((
        map(token!(Identifier(v)), PragmaArgument::Identifier),
        map(token!(Integer(i)), PragmaArgument::Integer),
    )))(input)?;
    let (input, data) = opt(token!(String(v)))(input)?;
    Ok((
        input,
        Instruction::Pragma(Pragma {
            name: pragma_type,
            arguments,
            data,
        }),
    ))
}

/// Parse the contents of a `PULSE` instruction.
pub(crate) fn parse_pulse(input: ParserInput, blocking: bool) -> InternalParserResult<Instruction> {
    let (input, frame) = parse_frame_identifier(input)?;
    let (input, waveform) = parse_waveform_invocation(input)?;

    Ok((
        input,
        Instruction::Pulse(Pulse {
            blocking,
            frame,
            waveform,
        }),
    ))
}

/// Parse the contents of a `RAW-CAPTURE` instruction.
pub(crate) fn parse_raw_capture(
    input: ParserInput,
    blocking: bool,
) -> InternalParserResult<Instruction> {
    let (input, frame) = parse_frame_identifier(input)?;
    let (input, duration) = parse_expression(input)?;
    let (input, memory_reference) = parse_memory_reference(input)?;

    Ok((
        input,
        Instruction::RawCapture(RawCapture {
            blocking,
            frame,
            duration,
            memory_reference,
        }),
    ))
}

/// Parse the contents of a `RESET` instruction.
pub(crate) fn parse_reset(input: ParserInput) -> InternalParserResult<Instruction> {
    let (input, qubit) = opt(parse_qubit)(input)?;

    Ok((input, Instruction::Reset(Reset { qubit })))
}

/// Parse the contents of a `SET-FREQUENCY` instruction.
pub(crate) fn parse_set_frequency(input: ParserInput) -> InternalParserResult<Instruction> {
    let (input, frame) = parse_frame_identifier(input)?;
    let (input, frequency) = parse_expression(input)?;

    Ok((
        input,
        Instruction::SetFrequency(SetFrequency { frame, frequency }),
    ))
}

/// Parse the contents of a `SET-PHASE` instruction.
pub(crate) fn parse_set_phase(input: ParserInput) -> InternalParserResult<Instruction> {
    let (input, frame) = parse_frame_identifier(input)?;
    let (input, phase) = parse_expression(input)?;

    Ok((input, Instruction::SetPhase(SetPhase { frame, phase })))
}

/// Parse the contents of a `SET-SCALE` instruction.
pub(crate) fn parse_set_scale(input: ParserInput) -> InternalParserResult<Instruction> {
    let (input, frame) = parse_frame_identifier(input)?;
    let (input, scale) = parse_expression(input)?;

    Ok((input, Instruction::SetScale(SetScale { frame, scale })))
}

/// Parse the contents of a `SHIFT-FREQUENCY` instruction.
pub(crate) fn parse_shift_frequency(input: ParserInput) -> InternalParserResult<Instruction> {
    let (input, frame) = parse_frame_identifier(input)?;
    let (input, frequency) = parse_expression(input)?;

    Ok((
        input,
        Instruction::ShiftFrequency(ShiftFrequency { frame, frequency }),
    ))
}

/// Parse the contents of a `SHIFT-PHASE` instruction.
pub(crate) fn parse_shift_phase(input: ParserInput) -> InternalParserResult<Instruction> {
    let (input, frame) = parse_frame_identifier(input)?;
    let (input, phase) = parse_expression(input)?;

    Ok((input, Instruction::ShiftPhase(ShiftPhase { frame, phase })))
}

/// Parse the contents of a `MEASURE` instruction.
pub(crate) fn parse_measurement(input: ParserInput) -> InternalParserResult<Instruction> {
    let (input, qubit) = parse_qubit(input)?;
    let (input, target) = match parse_memory_reference(input) {
        Ok((input, target)) => (input, Some(target)),
        Err(_) => (input, None),
    };

    Ok((
        input,
        Instruction::Measurement(Measurement { qubit, target }),
    ))
}

#[cfg(test)]
mod tests {
    use crate::expression::{Expression, ExpressionFunction, InfixOperator, PrefixOperator};
    use crate::instruction::{GateDefinition, GateSpecification, PragmaArgument};
    use crate::parser::lexer::lex;
    use crate::{imag, real};
    use crate::{
        instruction::{
            CircuitDefinition, Declaration, Gate, Instruction, Measurement, MemoryReference,
            Pragma, Qubit, ScalarType, Vector,
        },
        make_test,
    };

    use super::{parse_declare, parse_defcircuit, parse_defgate, parse_measurement, parse_pragma};

    make_test!(
        declare_instruction_length_1,
        parse_declare,
        "ro BIT",
        Instruction::Declaration(Declaration {
            name: "ro".to_owned(),
            sharing: None,
            size: Vector {
                data_type: ScalarType::Bit,
                length: 1
            }
        })
    );

    make_test!(
        declare_instruction_length_n,
        parse_declare,
        "ro INTEGER[5]",
        Instruction::Declaration(Declaration {
            name: "ro".to_owned(),
            sharing: None,
            size: Vector {
                data_type: ScalarType::Integer,
                length: 5
            }
        })
    );

    make_test!(
        measure_into_register,
        parse_measurement,
        "0 ro[0]",
        Instruction::Measurement(Measurement {
            qubit: Qubit::Fixed(0),
            target: Some(MemoryReference {
                name: String::from("ro"),
                index: 0
            })
        })
    );

    make_test!(
        measure_discard,
        parse_measurement,
        "0",
        Instruction::Measurement(Measurement {
            qubit: Qubit::Fixed(0),
            target: None
        })
    );

    make_test!(
        measure_named_qubit,
        parse_measurement,
        "q0 ro[0]",
        Instruction::Measurement(Measurement {
            qubit: Qubit::Variable(String::from("q0")),
            target: Some(MemoryReference {
                name: String::from("ro"),
                index: 0
            })
        })
    );

    make_test!(
        measure_named_qubit_discard,
        parse_measurement,
        "q0",
        Instruction::Measurement(Measurement {
            qubit: Qubit::Variable(String::from("q0")),
            target: None
        })
    );

    make_test!(
        pragma_inline_json,
        parse_pragma,
        "FILTER-NODE q35_unclassified \"{'module':'lodgepole.filters.io','filter_type':'DataBuffer','source':'q35_ro_rx/filter','publish':true,'params':{},'_type':'FilterNode'}\"",
        Instruction::Pragma(Pragma {
            name: "FILTER-NODE".to_owned(),
            arguments: vec![PragmaArgument::Identifier("q35_unclassified".to_string())],
            data: Some("{'module':'lodgepole.filters.io','filter_type':'DataBuffer','source':'q35_ro_rx/filter','publish':true,'params':{},'_type':'FilterNode'}".to_owned())
        })
    );

    make_test!(
        pragma_integer_argument,
        parse_pragma,
        "READOUT-POVM 0 \"(0.9 0.19999999999999996 0.09999999999999998 0.8)\"",
        Instruction::Pragma(Pragma {
            name: "READOUT-POVM".to_string(),
            arguments: vec![PragmaArgument::Integer(0)],
            data: Some("(0.9 0.19999999999999996 0.09999999999999998 0.8)".to_string()),
        })
    );

    make_test!(
        pragma_identifier_and_integer_argument,
        parse_pragma,
        "NAME identifier 0 \"data\"",
        Instruction::Pragma(Pragma {
            name: "NAME".to_string(),
            arguments: vec![
                PragmaArgument::Identifier("identifier".to_string()),
                PragmaArgument::Integer(0)
            ],
            data: Some("data".to_string()),
        })
    );

    make_test!(
        defcircuit_no_params,
        parse_defcircuit,
        "BELL a b:
    H a
    CNOT a b",
        Instruction::CircuitDefinition(CircuitDefinition {
            name: "BELL".to_owned(),
            parameters: vec![],
            qubit_variables: vec!["a".to_owned(), "b".to_owned()],
            instructions: vec![
                Instruction::Gate(Gate {
                    name: "H".to_owned(),
                    parameters: vec![],
                    qubits: vec![Qubit::Variable("a".to_owned())],
                    modifiers: vec![],
                }),
                Instruction::Gate(Gate {
                    name: "CNOT".to_owned(),
                    parameters: vec![],
                    qubits: vec![
                        Qubit::Variable("a".to_owned()),
                        Qubit::Variable("b".to_owned())
                    ],
                    modifiers: vec![],
                })
            ]
        })
    );

    make_test!(
        defcircuit_with_params,
        parse_defcircuit,
        "BELL(%a) a b:
    RZ(%a) a
    RX(%a) a
    RZ(%a) a
    CNOT a b",
        Instruction::CircuitDefinition(CircuitDefinition {
            name: "BELL".to_owned(),
            parameters: vec!["a".to_owned()],
            qubit_variables: vec!["a".to_owned(), "b".to_owned()],
            instructions: vec![
                Instruction::Gate(Gate {
                    name: "RZ".to_owned(),
                    parameters: vec![Expression::Variable("a".to_owned())],
                    qubits: vec![Qubit::Variable("a".to_owned())],
                    modifiers: vec![],
                }),
                Instruction::Gate(Gate {
                    name: "RX".to_owned(),
                    parameters: vec![Expression::Variable("a".to_owned())],
                    qubits: vec![Qubit::Variable("a".to_owned())],
                    modifiers: vec![],
                }),
                Instruction::Gate(Gate {
                    name: "RZ".to_owned(),
                    parameters: vec![Expression::Variable("a".to_owned())],
                    qubits: vec![Qubit::Variable("a".to_owned())],
                    modifiers: vec![],
                }),
                Instruction::Gate(Gate {
                    name: "CNOT".to_owned(),
                    parameters: vec![],
                    qubits: vec![
                        Qubit::Variable("a".to_owned()),
                        Qubit::Variable("b".to_owned())
                    ],
                    modifiers: vec![],
                })
            ]
        })
    );

    make_test!(
        defgate,
        parse_defgate,
        r#"H:
    1/sqrt(2), 1/sqrt(2)
    1/sqrt(2), -1/sqrt(2)"#,
        {
            // 1/sqrt(2)
            let expression = Expression::Infix {
                left: Box::new(Expression::Number(real!(1.0))),
                operator: InfixOperator::Slash,
                right: Box::new(Expression::FunctionCall {
                    function: crate::expression::ExpressionFunction::SquareRoot,
                    expression: Box::new(Expression::Number(real!(2.0))),
                }),
            };

            // -1/sqrt(2)
            let negative_expression = Expression::Infix {
                left: Box::new(Expression::Prefix {
                    operator: PrefixOperator::Minus,
                    expression: Box::new(Expression::Number(real!(1.0))),
                }),
                operator: InfixOperator::Slash,
                right: Box::new(Expression::FunctionCall {
                    function: crate::expression::ExpressionFunction::SquareRoot,
                    expression: Box::new(Expression::Number(real!(2.0))),
                }),
            };

            Instruction::GateDefinition(GateDefinition {
                name: "H".to_string(),
                parameters: vec![],
                specification: GateSpecification::Matrix(vec![
                    vec![expression.clone(), expression.clone()],
                    vec![expression, negative_expression],
                ]),
            })
        }
    );

    make_test!(
        defgate_parameterized,
        parse_defgate,
        r#"RX(%theta):
    cos(%theta/2), -i*sin(%theta/2)
    -i*sin(%theta/2), cos(%theta/2)"#,
        Instruction::GateDefinition(GateDefinition {
            name: "RX".to_string(),
            parameters: vec!["theta".to_string()],
            specification: GateSpecification::Matrix(vec![
                vec![
                    Expression::FunctionCall {
                        function: crate::expression::ExpressionFunction::Cosine,
                        expression: Box::new(Expression::Infix {
                            left: Box::new(Expression::Variable("theta".to_string())),
                            operator: InfixOperator::Slash,
                            right: Box::new(Expression::Number(real!(2.0))),
                        }),
                    },
                    Expression::Infix {
                        left: Box::new(Expression::Prefix {
                            operator: PrefixOperator::Minus,
                            expression: Box::new(Expression::Number(imag!(1f64)))
                        }),
                        operator: InfixOperator::Star,
                        right: Box::new(Expression::FunctionCall {
                            function: ExpressionFunction::Sine,
                            expression: Box::new(Expression::Infix {
                                left: Box::new(Expression::Variable("theta".to_string())),
                                operator: InfixOperator::Slash,
                                right: Box::new(Expression::Number(real!(2.0))),
                            }),
                        }),
                    }
                ],
                vec![
                    Expression::Infix {
                        left: Box::new(Expression::Prefix {
                            operator: PrefixOperator::Minus,
                            expression: Box::new(Expression::Number(imag!(1f64)))
                        }),
                        operator: InfixOperator::Star,
                        right: Box::new(Expression::FunctionCall {
                            function: ExpressionFunction::Sine,
                            expression: Box::new(Expression::Infix {
                                left: Box::new(Expression::Variable("theta".to_string())),
                                operator: InfixOperator::Slash,
                                right: Box::new(Expression::Number(real!(2.0))),
                            }),
                        }),
                    },
                    Expression::FunctionCall {
                        function: crate::expression::ExpressionFunction::Cosine,
                        expression: Box::new(Expression::Infix {
                            left: Box::new(Expression::Variable("theta".to_string())),
                            operator: InfixOperator::Slash,
                            right: Box::new(Expression::Number(real!(2.0))),
                        }),
                    },
                ],
            ]),
        })
    );

    make_test!(
        defgate_permutation,
        parse_defgate,
        r#"CCNOT AS PERMUTATION:
    0, 1, 2, 3, 4, 5, 7, 6"#,
        Instruction::GateDefinition(GateDefinition {
            name: "CCNOT".to_string(),
            parameters: vec![],
            specification: GateSpecification::Permutation(vec![0, 1, 2, 3, 4, 5, 7, 6]),
        })
    );
}
