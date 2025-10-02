use nom::branch::alt;
use nom::combinator::{map, map_res, opt};
use nom::multi::{many0, many1, separated_list0, separated_list1};
use nom::sequence::{delimited, preceded, tuple};

use crate::expression::Expression;
use crate::instruction::{
    Arithmetic, ArithmeticOperator, BinaryLogic, BinaryOperator, CalibrationDefinition,
    CalibrationIdentifier, Call, Capture, CircuitDefinition, Comparison, ComparisonOperator,
    Convert, Declaration, DefGateSequence, Delay, Exchange, Fence, FrameDefinition, GateDefinition,
    GateSpecification, GateType, Include, Instruction, Jump, JumpUnless, JumpWhen, Label, Load,
    MeasureCalibrationDefinition, MeasureCalibrationIdentifier, Measurement, Move, PauliSum,
    Pragma, PragmaArgument, Pulse, Qubit, RawCapture, Reset, SetFrequency, SetPhase, SetScale,
    ShiftFrequency, ShiftPhase, Store, SwapPhases, Target, UnaryLogic, UnaryOperator,
    UnresolvedCallArgument, ValidationError, Waveform, WaveformDefinition,
};

use crate::parser::common::parse_sequence_elements;
use crate::parser::instruction::parse_block;
use crate::parser::InternalParserResult;
use crate::{real, token};

use super::common::{parse_memory_reference_with_brackets, parse_variable_qubit};
use super::{
    common::{
        parse_arithmetic_operand, parse_binary_logic_operand, parse_comparison_operand,
        parse_frame_attribute, parse_frame_identifier, parse_gate_modifier, parse_matrix,
        parse_memory_reference, parse_pauli_terms, parse_permutation, parse_qubit, parse_sharing,
        parse_vector, parse_waveform_invocation, parse_waveform_name,
    },
    expression::parse_expression,
    InternalParseError, ParserErrorKind, ParserInput,
};

/// Parse an arithmetic instruction of the form `destination source`.
/// Called using the arithmetic operator itself (such as `ADD`) which should be previously parsed.
pub(crate) fn parse_arithmetic(
    operator: ArithmeticOperator,
    input: ParserInput,
) -> InternalParserResult<Instruction> {
    let (input, destination) = parse_memory_reference(input)?;
    let (input, source) = parse_arithmetic_operand(input)?;

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
    let (input, destination) = parse_memory_reference(input)?;
    let (input, lhs) = parse_memory_reference(input)?;
    let (input, rhs) = parse_comparison_operand(input)?;

    Ok((
        input,
        Instruction::Comparison(Comparison {
            operator,
            destination,
            lhs,
            rhs,
        }),
    ))
}

/// Parse a logical binary instruction of the form `addr ( addr | INT )`.
/// Called using the logical operator itself (such as `AND`) which should be previously parsed.
pub(crate) fn parse_logical_binary(
    operator: BinaryOperator,
    input: ParserInput,
) -> InternalParserResult<Instruction> {
    let (input, destination) = parse_memory_reference(input)?;
    let (input, source) = parse_binary_logic_operand(input)?;

    Ok((
        input,
        Instruction::BinaryLogic(BinaryLogic {
            operator,
            destination,
            source,
        }),
    ))
}

/// Parse a logical unary instruction of the form `addr`.
/// Called using the logical operator itself (such as `NOT`) which should be previously parsed.
pub(crate) fn parse_logical_unary(
    operator: UnaryOperator,
    input: ParserInput,
) -> InternalParserResult<Instruction> {
    let (input, operand) = parse_memory_reference(input)?;

    Ok((
        input,
        Instruction::UnaryLogic(UnaryLogic { operator, operand }),
    ))
}

/// Parse the contents of a `DECLARE` instruction.
pub(crate) fn parse_declare<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, Instruction> {
    let (input, name) = token!(Identifier(v))(input)?;
    let (input, size) = parse_vector(input)?;
    let (input, sharing) = parse_sharing(input)?;

    Ok((
        input,
        Instruction::Declaration(Declaration {
            name,
            size,
            sharing,
        }),
    ))
}

/// Parse the contents of a `CALL` instruction.
///
/// Note, the `CALL` instruction here is unresolved; it can only be resolved within the
/// full context of a program from an [`crate::instruction::extern_call::ExternSignatureMap`].
///
/// Call instructions are of the form:
///     `CALL @ms{Identifier} @rep[:min 1]{@group{@ms{Identifier} @alt @ms{Memory Reference} @alt @ms{Complex}}}`
///
/// For additional detail, see ["Call" in the Quil specification](https://github.com/quil-lang/quil/blob/7f532c7cdde9f51eae6abe7408cc868fba9f91f6/specgen/spec/sec-other.s).
pub(crate) fn parse_call<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, Instruction> {
    let (input, name) = token!(Identifier(v))(input)?;

    let (input, arguments) = many0(parse_call_argument)(input)?;
    let call = Call { name, arguments };

    Ok((input, Instruction::Call(call)))
}

fn parse_call_argument<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, UnresolvedCallArgument> {
    alt((
        map(
            parse_memory_reference_with_brackets,
            UnresolvedCallArgument::MemoryReference,
        ),
        map(token!(Identifier(v)), UnresolvedCallArgument::Identifier),
        map(
            super::expression::parse_immediate_value,
            UnresolvedCallArgument::Immediate,
        ),
    ))(input)
}

/// Parse the contents of a `CAPTURE` instruction.
///
/// Unlike most other instructions, this can be _prefixed_ with the NONBLOCKING keyword,
/// and thus it expects and parses the CAPTURE token itself.
pub(crate) fn parse_capture(
    input: ParserInput,
    blocking: bool,
) -> InternalParserResult<Instruction> {
    let (input, frame) = parse_frame_identifier(input)?;
    let (input, waveform) = parse_waveform_invocation(input)?;
    let (input, memory_reference) = parse_memory_reference(input)?;

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

/// Parse the contents of a `CONVERT` instruction.
pub(crate) fn parse_convert(input: ParserInput) -> InternalParserResult<Instruction> {
    let (input, to) = parse_memory_reference(input)?;
    let (input, from) = parse_memory_reference(input)?;
    Ok((
        input,
        Instruction::Convert(Convert {
            destination: to,
            source: from,
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
    let (input, instructions) = parse_block(input)?;
    Ok((
        input,
        Instruction::CalibrationDefinition(CalibrationDefinition {
            identifier: CalibrationIdentifier {
                name,
                parameters,
                qubits,
                modifiers,
            },
            instructions,
        }),
    ))
}

/// Parse the contents of a `DEFCAL MEASURE` instruction, following the `MEASURE` token.
pub(crate) fn parse_defcal_measure<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, Instruction> {
    let (input, name) = opt(preceded(token!(Bang), token!(Identifier(name))))(input)?;
    let (input, qubit) = parse_qubit(input)?;
    let (input, target) = opt(token!(Identifier(t)))(input)?;
    let (input, _) = token!(Colon)(input)?;
    let (input, instructions) = parse_block(input)?;
    Ok((
        input,
        Instruction::MeasureCalibrationDefinition(MeasureCalibrationDefinition {
            identifier: MeasureCalibrationIdentifier {
                name,
                qubit,
                target,
            },
            instructions,
        }),
    ))
}

/// Parse the contents of a `DEFFRAME` instruction.
pub(crate) fn parse_defframe<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, Instruction> {
    let (input, identifier) = parse_frame_identifier(input)?;
    let (input, _) = token!(Colon)(input)?;
    let (input, attribute_pairs) = many1(parse_frame_attribute)(input)?;
    let attributes = attribute_pairs.into_iter().collect();

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
        separated_list0(token!(Comma), token!(Variable(v))),
        token!(RParenthesis),
    ))(input)?;
    let (input, mut arguments) = opt(many0(token!(Identifier(v))))(input)?;
    let (input, gate_type) = opt(preceded(
        token!(As),
        alt((
            map(token!(Matrix), |()| GateType::Matrix),
            map(token!(Permutation), |()| GateType::Permutation),
            map(token!(PauliSum), |()| GateType::PauliSum),
            map(token!(Sequence), |()| GateType::Sequence),
        )),
    ))(input)?;

    let (input, _) = token!(Colon)(input)?;

    let gate_type = gate_type.unwrap_or(GateType::Matrix);
    let (input, specification) = match gate_type {
        GateType::Matrix => map(parse_matrix, GateSpecification::Matrix)(input)?,
        GateType::Permutation => map(parse_permutation, GateSpecification::Permutation)(input)?,
        GateType::PauliSum => map_res(parse_pauli_terms, |terms| {
            Ok(GateSpecification::PauliSum(
                PauliSum::new(arguments.take().unwrap_or_default(), terms)
                    .map_err(ValidationError::from)
                    .map_err(|e| InternalParseError::from_kind(input, ParserErrorKind::from(e)))?,
            ))
        })(input)?,
        GateType::Sequence => map_res(parse_sequence_elements, |gates| {
            let gate_sequence =
                DefGateSequence::try_new(arguments.take().unwrap_or_default(), gates)
                    .map_err(ValidationError::from)
                    .map_err(ParserErrorKind::from)
                    .map_err(|e| InternalParseError::from_kind(input, e))?;
            Ok(GateSpecification::Sequence(gate_sequence))
        })(input)?,
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
    let (input, name) = parse_waveform_name(input)?;
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
    let (input, mut qubits) = many0(parse_qubit)(input)?;
    let (input, frame_names) = many0(token!(String(v)))(input)?;
    // If there is no intervening frame name and the delay is an integer, it will have been parsed
    // as a qubit. We check for and correct that condition here.
    let (input, duration) = parse_expression(input).or_else(|e| {
        if let Some(Qubit::Fixed(index)) = qubits.last() {
            let duration = *index as f64;
            qubits.pop();
            Ok((input, Expression::Number(real!(duration))))
        } else {
            Err(e)
        }
    })?;

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
    let (input, left) = parse_memory_reference(input)?;
    let (input, right) = parse_memory_reference(input)?;

    Ok((input, Instruction::Exchange(Exchange { left, right })))
}

/// Parse the contents of a `FENCE` instruction.
pub(crate) fn parse_fence(input: ParserInput) -> InternalParserResult<Instruction> {
    let (input, qubits) = many0(parse_qubit)(input)?;

    Ok((input, Instruction::Fence(Fence { qubits })))
}

/// Parse the contents of a `JUMP` instruction.
pub(crate) fn parse_jump<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, Instruction> {
    let (input, target) = token!(Target(v))(input)?;
    Ok((
        input,
        Instruction::Jump(Jump {
            target: Target::Fixed(target),
        }),
    ))
}

/// Parse the contents of a `JUMP-WHEN` instruction.
pub(crate) fn parse_jump_when<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, Instruction> {
    let (input, target) = token!(Target(v))(input)?;
    let (input, condition) = parse_memory_reference(input)?;
    Ok((
        input,
        Instruction::JumpWhen(JumpWhen {
            target: Target::Fixed(target),
            condition,
        }),
    ))
}

/// Parse the contents of a `JUMP-UNLESS` instruction.
pub(crate) fn parse_jump_unless<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, Instruction> {
    let (input, target) = token!(Target(v))(input)?;
    let (input, condition) = parse_memory_reference(input)?;
    Ok((
        input,
        Instruction::JumpUnless(JumpUnless {
            target: Target::Fixed(target),
            condition,
        }),
    ))
}

/// Parse the contents of a `DECLARE` instruction.
pub(crate) fn parse_label<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, Instruction> {
    let (input, name) = token!(Target(v))(input)?;
    Ok((
        input,
        Instruction::Label(Label {
            target: Target::Fixed(name),
        }),
    ))
}

/// Parse the contents of a `MOVE` instruction.
pub(crate) fn parse_move(input: ParserInput) -> InternalParserResult<Instruction> {
    let (input, destination) = parse_memory_reference(input)?;
    let (input, source) = parse_arithmetic_operand(input)?;
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
    let (input, destination) = parse_memory_reference(input)?;
    let (input, source) = token!(Identifier(v))(input)?;
    let (input, offset) = parse_memory_reference(input)?;

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
    let (input, offset) = parse_memory_reference(input)?;
    let (input, source) = parse_arithmetic_operand(input)?;

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

/// Parse the contents of a `SWAP-PHASES` instruction.
pub(crate) fn parse_swap_phases(input: ParserInput) -> InternalParserResult<Instruction> {
    let (input, frame_1) = parse_frame_identifier(input)?;
    let (input, frame_2) = parse_frame_identifier(input)?;

    Ok((
        input,
        Instruction::SwapPhases(SwapPhases { frame_1, frame_2 }),
    ))
}

/// Parse the contents of a `MEASURE` instruction.
pub(crate) fn parse_measurement<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, Instruction> {
    let (input, name) = opt(preceded(token!(Bang), token!(Identifier(name))))(input)?;
    let (input, qubit) = parse_qubit(input)?;
    let (input, target) = match parse_memory_reference(input) {
        Ok((input, target)) => (input, Some(target)),
        Err(_) => (input, None),
    };

    Ok((
        input,
        Instruction::Measurement(Measurement {
            name,
            qubit,
            target,
        }),
    ))
}

/// Parse the contents of an `INCLUDE` instruction.
pub(crate) fn parse_include<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, Instruction> {
    let (input, filename) = token!(String(v))(input)?;
    Ok((input, Instruction::Include(Include { filename })))
}

#[cfg(test)]
mod tests {
    use crate::expression::{
        Expression, ExpressionFunction, FunctionCallExpression, InfixExpression, InfixOperator,
        PrefixExpression, PrefixOperator,
    };
    use crate::instruction::{
        Call, DefGateSequence, DefGateSequenceError, GateDefinition, GateSpecification, Offset,
        PauliGate, PauliSum, PauliTerm, PragmaArgument, Sharing, UnresolvedCallArgument,
    };
    use crate::parser::lexer::lex;
    use crate::parser::Token;
    use crate::{imag, real};
    use crate::{
        instruction::{
            CircuitDefinition, Declaration, Gate, Instruction, Measurement, MemoryReference,
            Pragma, Qubit, ScalarType, Vector,
        },
        make_test,
    };
    use internment::ArcIntern;
    use num_complex::Complex64;
    use rstest::*;

    use super::{parse_declare, parse_defcircuit, parse_defgate, parse_measurement, parse_pragma};

    make_test!(
        declare_instruction_length_1,
        parse_declare,
        "ro BIT",
        Instruction::Declaration(Declaration {
            name: "ro".to_owned(),
            size: Vector {
                data_type: ScalarType::Bit,
                length: 1
            },
            sharing: None,
        })
    );

    make_test!(
        declare_instruction_length_n,
        parse_declare,
        "ro INTEGER[5]",
        Instruction::Declaration(Declaration {
            name: "ro".to_owned(),
            size: Vector {
                data_type: ScalarType::Integer,
                length: 5
            },
            sharing: None,
        })
    );

    make_test!(
        declare_instruction_sharing,
        parse_declare,
        "ro REAL[1] SHARING bar",
        Instruction::Declaration(Declaration {
            name: "ro".to_owned(),
            size: Vector {
                data_type: ScalarType::Real,
                length: 1,
            },
            sharing: Some(Sharing {
                name: "bar".to_string(),
                offsets: vec![]
            })
        })
    );

    make_test!(
        declare_instruction_sharing_offsets,
        parse_declare,
        "ro REAL[1] SHARING bar OFFSET 2 BIT 3 INTEGER",
        Instruction::Declaration(Declaration {
            name: "ro".to_owned(),
            size: Vector {
                data_type: ScalarType::Real,
                length: 1,
            },
            sharing: Some(Sharing {
                name: "bar".to_string(),
                offsets: vec![
                    Offset {
                        offset: 2,
                        data_type: ScalarType::Bit
                    },
                    Offset {
                        offset: 3,
                        data_type: ScalarType::Integer
                    }
                ],
            })
        })
    );

    make_test!(
        measure_into_register,
        parse_measurement,
        "0 ro[0]",
        Instruction::Measurement(Measurement {
            name: None,
            qubit: Qubit::Fixed(0),
            target: Some(MemoryReference {
                name: String::from("ro"),
                index: 0
            })
        })
    );

    make_test!(
        named_measure_into_register,
        parse_measurement,
        "!midcircuit 0 ro[0]",
        Instruction::Measurement(Measurement {
            name: Some(String::from("midcircuit")),
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
            name: None,
            qubit: Qubit::Fixed(0),
            target: None
        })
    );

    make_test!(
        named_measure_discard,
        parse_measurement,
        "!midcircuit 0",
        Instruction::Measurement(Measurement {
            name: Some(String::from("midcircuit")),
            qubit: Qubit::Fixed(0),
            target: None
        })
    );

    make_test!(
        measure_named_qubit,
        parse_measurement,
        "q0 ro[0]",
        Instruction::Measurement(Measurement {
            name: None,
            qubit: Qubit::Variable(String::from("q0")),
            target: Some(MemoryReference {
                name: String::from("ro"),
                index: 0
            })
        })
    );

    make_test!(
        named_measure_named_qubit,
        parse_measurement,
        "!midcircuit q0 ro[0]",
        Instruction::Measurement(Measurement {
            name: Some(String::from("midcircuit")),
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
            name: None,
            qubit: Qubit::Variable(String::from("q0")),
            target: None
        })
    );

    make_test!(
        named_measure_named_qubit_discard,
        parse_measurement,
        "!midcircuit q0",
        Instruction::Measurement(Measurement {
            name: Some(String::from("midcircuit")),
            qubit: Qubit::Variable(String::from("q0")),
            target: None
        })
    );

    make_test!(
        pragma_inline_json_single_quotes,
        parse_pragma,
        "FILTER-NODE q35_unclassified \"{'module':'lodgepole.filters.io','filter_type':'DataBuffer','source':'q35_ro_rx/filter','publish':true,'params':{},'_type':'FilterNode'}\"",
        Instruction::Pragma(Pragma {
            name: "FILTER-NODE".to_owned(),
            arguments: vec![PragmaArgument::Identifier("q35_unclassified".to_string())],
            data: Some("{'module':'lodgepole.filters.io','filter_type':'DataBuffer','source':'q35_ro_rx/filter','publish':true,'params':{},'_type':'FilterNode'}".to_owned())
        })
    );

    make_test!(
        pragma_inline_json_double_quotes,
        parse_pragma,
        r#"FILTER-NODE q35_unclassified "{\"module\":\"lodgepole.filters.io\",\"filter_type\":\"DataBuffer\",\"source\":\"q35_ro_rx/filter\",\"publish\":true,\"params\":{},\"_type\":\"FilterNode\"}""#,
        Instruction::Pragma(Pragma {
            name: "FILTER-NODE".to_owned(),
            arguments: vec![PragmaArgument::Identifier("q35_unclassified".to_string())],
            data: Some(r#"{"module":"lodgepole.filters.io","filter_type":"DataBuffer","source":"q35_ro_rx/filter","publish":true,"params":{},"_type":"FilterNode"}"#.to_owned())
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
            let expression = Expression::Infix(InfixExpression {
                left: ArcIntern::new(Expression::Number(real!(1.0))),
                operator: InfixOperator::Slash,
                right: ArcIntern::new(Expression::FunctionCall(FunctionCallExpression {
                    function: crate::expression::ExpressionFunction::SquareRoot,
                    expression: ArcIntern::new(Expression::Number(real!(2.0))),
                })),
            });

            // -1/sqrt(2)
            let negative_expression = Expression::Infix(InfixExpression {
                left: ArcIntern::new(Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    expression: ArcIntern::new(Expression::Number(real!(1.0))),
                })),
                operator: InfixOperator::Slash,
                right: ArcIntern::new(Expression::FunctionCall(FunctionCallExpression {
                    function: crate::expression::ExpressionFunction::SquareRoot,
                    expression: ArcIntern::new(Expression::Number(real!(2.0))),
                })),
            });

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
                    Expression::FunctionCall(FunctionCallExpression {
                        function: crate::expression::ExpressionFunction::Cosine,
                        expression: ArcIntern::new(Expression::Infix(InfixExpression {
                            left: ArcIntern::new(Expression::Variable("theta".to_string())),
                            operator: InfixOperator::Slash,
                            right: ArcIntern::new(Expression::Number(real!(2.0))),
                        })),
                    }),
                    Expression::Infix(InfixExpression {
                        left: ArcIntern::new(Expression::Prefix(PrefixExpression {
                            operator: PrefixOperator::Minus,
                            expression: ArcIntern::new(Expression::Number(imag!(1f64)))
                        })),
                        operator: InfixOperator::Star,
                        right: ArcIntern::new(Expression::FunctionCall(FunctionCallExpression {
                            function: ExpressionFunction::Sine,
                            expression: ArcIntern::new(Expression::Infix(InfixExpression {
                                left: ArcIntern::new(Expression::Variable("theta".to_string())),
                                operator: InfixOperator::Slash,
                                right: ArcIntern::new(Expression::Number(real!(2.0))),
                            })),
                        })),
                    })
                ],
                vec![
                    Expression::Infix(InfixExpression {
                        left: ArcIntern::new(Expression::Prefix(PrefixExpression {
                            operator: PrefixOperator::Minus,
                            expression: ArcIntern::new(Expression::Number(imag!(1f64)))
                        })),
                        operator: InfixOperator::Star,
                        right: ArcIntern::new(Expression::FunctionCall(FunctionCallExpression {
                            function: ExpressionFunction::Sine,
                            expression: ArcIntern::new(Expression::Infix(InfixExpression {
                                left: ArcIntern::new(Expression::Variable("theta".to_string())),
                                operator: InfixOperator::Slash,
                                right: ArcIntern::new(Expression::Number(real!(2.0))),
                            })),
                        })),
                    }),
                    Expression::FunctionCall(FunctionCallExpression {
                        function: crate::expression::ExpressionFunction::Cosine,
                        expression: ArcIntern::new(Expression::Infix(InfixExpression {
                            left: ArcIntern::new(Expression::Variable("theta".to_string())),
                            operator: InfixOperator::Slash,
                            right: ArcIntern::new(Expression::Number(real!(2.0))),
                        })),
                    }),
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

    make_test!(
        defgate_pauli_sum,
        parse_defgate,
        r#"PauliSumGate(%theta) p q AS PAULI-SUM:
    ZZ((-%theta)/4) p q
    Y(%theta/4) p
    X(%theta/4) q"#,
        Instruction::GateDefinition(GateDefinition {
            name: "PauliSumGate".to_string(),
            parameters: vec!["theta".to_string()],
            specification: GateSpecification::PauliSum(PauliSum {
                arguments: vec!["p".to_string(), "q".to_string()],
                terms: vec![
                    PauliTerm {
                        arguments: vec![
                            (PauliGate::Z, "p".to_string()),
                            (PauliGate::Z, "q".to_string())
                        ],
                        expression: Expression::Infix(InfixExpression {
                            left: ArcIntern::new(Expression::Prefix(PrefixExpression {
                                operator: PrefixOperator::Minus,
                                expression: ArcIntern::new(Expression::Variable(
                                    "theta".to_string()
                                ))
                            })),
                            operator: InfixOperator::Slash,
                            right: ArcIntern::new(Expression::Number(real!(4.0)))
                        }),
                    },
                    PauliTerm {
                        arguments: vec![(PauliGate::Y, "p".to_string())],
                        expression: Expression::Infix(InfixExpression {
                            left: ArcIntern::new(Expression::Variable("theta".to_string())),
                            operator: InfixOperator::Slash,
                            right: ArcIntern::new(Expression::Number(real!(4.0)))
                        }),
                    },
                    PauliTerm {
                        arguments: vec![(PauliGate::X, "q".to_string())],
                        expression: Expression::Infix(InfixExpression {
                            left: ArcIntern::new(Expression::Variable("theta".to_string())),
                            operator: InfixOperator::Slash,
                            right: ArcIntern::new(Expression::Number(real!(4.0)))
                        }),
                    },
                ]
            })
        })
    );

    /// Test case for parsing a `CALL` instruction.
    struct ParseCallTestCase {
        /// The input to parse.
        input: &'static str,
        /// The remaining tokens after parsing.
        remainder: Vec<Token>,
        /// The expected result.
        expected: Result<Call, String>,
    }

    impl ParseCallTestCase {
        /// Basic call with arguments.
        fn case_01() -> Self {
            Self {
                input: "foo integer[0] 1.0 bar",
                remainder: vec![],
                expected: Ok(Call {
                    name: "foo".to_string(),
                    arguments: vec![
                        UnresolvedCallArgument::MemoryReference(MemoryReference {
                            name: "integer".to_string(),
                            index: 0,
                        }),
                        UnresolvedCallArgument::Immediate(real!(1.0)),
                        UnresolvedCallArgument::Identifier("bar".to_string()),
                    ],
                }),
            }
        }

        /// No arguments does in fact parse.
        fn case_02() -> Self {
            Self {
                input: "foo",
                remainder: vec![],
                expected: Ok(Call {
                    name: "foo".to_string(),
                    arguments: vec![],
                }),
            }
        }

        /// Invalid identifier.
        fn case_03() -> Self {
            Self {
                input: "INCLUDE",
                remainder: vec![],
                expected: Err(
                    "ExpectedToken { actual: COMMAND(INCLUDE), expected: \"Identifier\" }"
                        .to_string(),
                ),
            }
        }

        /// Valid with leftover
        fn case_04() -> Self {
            Self {
                input: "foo integer[0] 1.0 bar; baz",
                remainder: vec![Token::Semicolon, Token::Identifier("baz".to_string())],
                expected: Ok(Call {
                    name: "foo".to_string(),
                    arguments: vec![
                        UnresolvedCallArgument::MemoryReference(MemoryReference {
                            name: "integer".to_string(),
                            index: 0,
                        }),
                        UnresolvedCallArgument::Immediate(real!(1.0)),
                        UnresolvedCallArgument::Identifier("bar".to_string()),
                    ],
                }),
            }
        }
    }

    /// Test that the `parse_call` function works as expected.
    #[rstest]
    #[case(ParseCallTestCase::case_01())]
    #[case(ParseCallTestCase::case_02())]
    #[case(ParseCallTestCase::case_03())]
    #[case(ParseCallTestCase::case_04())]
    fn test_parse_call(#[case] test_case: ParseCallTestCase) {
        let input = ::nom_locate::LocatedSpan::new(test_case.input);
        let tokens = lex(input).unwrap();
        match (test_case.expected, super::parse_call(&tokens)) {
            (Ok(expected), Ok((remainder, parsed))) => {
                assert_eq!(parsed, Instruction::Call(expected));
                let remainder: Vec<_> = remainder.iter().map(|t| t.as_token().clone()).collect();
                assert_eq!(remainder, test_case.remainder);
            }
            (Ok(expected), Err(e)) => {
                panic!("Expected {expected:?}, got error: {e:?}");
            }
            (Err(expected), Ok((_, parsed))) => {
                panic!("Expected error: {expected:?}, got {parsed:?}");
            }
            (Err(expected), Err(found)) => {
                let found = format!("{found:?}");
                assert!(found.contains(&expected), "`{expected}` not in `{found}`");
            }
        }
    }

    struct ParseGateDefinitionTestCase {
        /// The input to parse.
        input: &'static str,
        /// The remaining tokens after parsing.
        remainder: Vec<Token>,
        /// The expected result.
        expected: Result<GateDefinition, String>,
    }

    impl ParseGateDefinitionTestCase {
        fn simple_sequence() -> Self {
            const SIMPLE_SEQUENCE: &str = r"seq1(%param01) q1 AS SEQUENCE:
    RX(%param01) q1";
            let gate1 = Gate::new(
                "RX",
                vec![Expression::Variable("param01".to_string())],
                vec![Qubit::Variable("q1".to_string())],
                vec![],
            )
            .expect("must be valid gate");
            let gate_sequence = DefGateSequence::try_new(vec!["q1".to_string()], vec![gate1])
                .expect("must be valid sequence");
            Self {
                input: SIMPLE_SEQUENCE,
                remainder: vec![],
                expected: Ok(GateDefinition {
                    name: "seq1".to_string(),
                    parameters: vec!["param01".to_string()],
                    specification: GateSpecification::Sequence(gate_sequence),
                }),
            }
        }

        fn simple_2q() -> Self {
            const SIMPLE_2Q: &str = r"seq1(%param01, %param02) q1 q2 AS SEQUENCE:
    RX(%param01) q1
    RX(%param02) q2";
            let gate1 = Gate::new(
                "RX",
                vec![Expression::Variable("param01".to_string())],
                vec![Qubit::Variable("q1".to_string())],
                vec![],
            )
            .expect("must be valid gate");
            let gate2 = Gate::new(
                "RX",
                vec![Expression::Variable("param02".to_string())],
                vec![Qubit::Variable("q2".to_string())],
                vec![],
            )
            .expect("must be valid gate");
            let gate_sequence = DefGateSequence::try_new(
                vec!["q1".to_string(), "q2".to_string()],
                vec![gate1, gate2],
            )
            .expect("must be valid sequence");
            Self {
                input: SIMPLE_2Q,
                remainder: vec![],
                expected: Ok(GateDefinition {
                    name: "seq1".to_string(),
                    parameters: vec!["param01".to_string(), "param02".to_string()],
                    specification: GateSpecification::Sequence(gate_sequence),
                }),
            }
        }

        fn no_parameters() -> Self {
            const NO_PARAMETERS: &str = r"seq1() q1 AS SEQUENCE:
    RX(pi/2) q1";
            let gate1 = Gate::new(
                "RX",
                vec![Expression::Infix(InfixExpression {
                    left: ArcIntern::new(Expression::PiConstant()),
                    operator: InfixOperator::Slash,
                    right: ArcIntern::new(Expression::Number(Complex64 { re: 2.0, im: 0.0 })),
                })],
                vec![Qubit::Variable("q1".to_string())],
                vec![],
            )
            .expect("must be valid gate");
            let gate_sequence = DefGateSequence::try_new(vec!["q1".to_string()], vec![gate1])
                .expect("must be valid sequence");
            Self {
                input: NO_PARAMETERS,
                remainder: vec![],
                expected: Ok(GateDefinition {
                    name: "seq1".to_string(),
                    parameters: vec![],
                    specification: GateSpecification::Sequence(gate_sequence),
                }),
            }
        }

        fn no_parentheses() -> Self {
            const NO_PARENTHESES: &str = r"seq1 q1 AS SEQUENCE:
    RX(pi/2) q1";
            let gate1 = Gate::new(
                "RX",
                vec![Expression::Infix(InfixExpression {
                    left: ArcIntern::new(Expression::PiConstant()),
                    operator: InfixOperator::Slash,
                    right: ArcIntern::new(Expression::Number(Complex64 { re: 2.0, im: 0.0 })),
                })],
                vec![Qubit::Variable("q1".to_string())],
                vec![],
            )
            .expect("must be valid gate");
            let gate_sequence = DefGateSequence::try_new(vec!["q1".to_string()], vec![gate1])
                .expect("must be valid sequence");
            Self {
                input: NO_PARENTHESES,
                remainder: vec![],
                expected: Ok(GateDefinition {
                    name: "seq1".to_string(),
                    parameters: vec![],
                    specification: GateSpecification::Sequence(gate_sequence),
                }),
            }
        }

        fn unused_argument() -> Self {
            const NO_PARAMETERS: &str = r"seq1(%param01) q1 q2 AS SEQUENCE:
    RX(pi/2) q1";
            let gate1 = Gate::new(
                "RX",
                vec![Expression::Infix(InfixExpression {
                    left: ArcIntern::new(Expression::PiConstant()),
                    operator: InfixOperator::Slash,
                    right: ArcIntern::new(Expression::Number(Complex64 { re: 2.0, im: 0.0 })),
                })],
                vec![Qubit::Variable("q1".to_string())],
                vec![],
            )
            .expect("must be valid gate");
            let gate_sequence =
                DefGateSequence::try_new(vec!["q1".to_string(), "q2".to_string()], vec![gate1])
                    .expect("must be valid sequence");
            Self {
                input: NO_PARAMETERS,
                remainder: vec![],
                expected: Ok(GateDefinition {
                    name: "seq1".to_string(),
                    parameters: vec!["param01".to_string()],
                    specification: GateSpecification::Sequence(gate_sequence),
                }),
            }
        }

        fn error_undefined_gate_sequence_element_qubit() -> Self {
            const UNDEFINED_QUBIT: &str = r"seq1(%param01) q1 AS SEQUENCE:
    RZ(%param01) q1
    ISWAP q1 doesnt_exist_qubit";
            Self {
                input: UNDEFINED_QUBIT,
                remainder: vec![],
                expected: Err(format!(
                    "{:?}",
                    DefGateSequenceError::UndefinedGateSequenceElementQubit {
                        gate_index: 1,
                        qubit_argument_index: 1,
                        argument_name: "doesnt_exist_qubit".to_string(),
                    }
                )),
            }
        }

        fn error_invalid_gate_sequence_element_qubit() -> Self {
            const INVALID_QUBIT: &str = r"seq1(%param01) q1 AS SEQUENCE:
    RZ(%param01) q1
    ISWAP q1 3";
            Self {
                input: INVALID_QUBIT,
                remainder: vec![],
                expected: Err(format!(
                    "{:?}",
                    DefGateSequenceError::InvalidGateSequenceElementQubit {
                        gate_index: 1,
                        qubit_argument_index: 1,
                        qubit: Qubit::Fixed(3)
                    }
                )),
            }
        }

        fn error_at_least_one_qubit() -> Self {
            const AT_LEAST_ONE_QUBIT: &str = r"seq1() AS SEQUENCE:
    RX(pi/2) q1";
            Self {
                input: AT_LEAST_ONE_QUBIT,
                remainder: vec![],
                expected: Err(format!(
                    "{:?}",
                    DefGateSequenceError::AtLeastOneQubitParameterRequired
                )),
            }
        }
    }

    #[rstest]
    #[case::simple_sequence(ParseGateDefinitionTestCase::simple_sequence())]
    #[case::simple_2q(ParseGateDefinitionTestCase::simple_2q())]
    #[case::no_parameters(ParseGateDefinitionTestCase::no_parameters())]
    #[case::no_parentheses(ParseGateDefinitionTestCase::no_parentheses())]
    #[case::unused_argument(ParseGateDefinitionTestCase::unused_argument())]
    #[case::error_undefined_gate_sequence_element_qubit(
        ParseGateDefinitionTestCase::error_undefined_gate_sequence_element_qubit()
    )]
    #[case::error_invalid_gate_sequence_element_qubit(
        ParseGateDefinitionTestCase::error_invalid_gate_sequence_element_qubit()
    )]
    #[case::error_at_least_one_qubit(ParseGateDefinitionTestCase::error_at_least_one_qubit())]
    fn test_parse_gate_definition(#[case] test_case: ParseGateDefinitionTestCase) {
        let input = ::nom_locate::LocatedSpan::new(test_case.input);
        let tokens = lex(input).unwrap();
        match (test_case.expected, super::parse_defgate(&tokens)) {
            (Ok(expected), Ok((remainder, parsed))) => {
                assert_eq!(parsed, Instruction::GateDefinition(expected));
                let remainder: Vec<_> = remainder.iter().map(|t| t.as_token().clone()).collect();
                assert_eq!(remainder, test_case.remainder);
            }
            (Ok(expected), Err(e)) => {
                panic!("Expected {expected:?}, got error: {e:?}");
            }
            (Err(expected), Ok((_, parsed))) => {
                panic!("Expected error: {expected:?}, got {parsed:?}");
            }
            (Err(expected), Err(found)) => {
                let found = format!("{found:?}");
                assert!(found.contains(&expected), "`{expected}` not in `{found}`");
            }
        }
    }
}
