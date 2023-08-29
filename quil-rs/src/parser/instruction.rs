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
    combinator::all_consuming,
    multi::{many0, many1},
    sequence::{delimited, preceded},
};

use crate::parser::{extract_nom_err, InternalParseError, InternalParserResult};
use crate::{
    instruction::{
        ArithmeticOperator, BinaryOperator, ComparisonOperator, Instruction, UnaryOperator,
    },
    token,
};

use super::{
    command, common,
    error::ParserErrorKind,
    gate,
    lexer::{Command, Token},
    ParserInput,
};

/// Parse the next instructon from the input, skipping past leading newlines, comments, and semicolons.
pub(crate) fn parse_instruction(input: ParserInput) -> InternalParserResult<Instruction> {
    let (input, _) = common::skip_newlines_and_comments(input)?;
    match super::split_first_token(input) {
        None => Err(nom::Err::Error(InternalParseError::from_kind(
            input,
            ParserErrorKind::EndOfInput,
        ))),
        Some((Token::Command(command), remainder)) => match command {
            Command::Add => command::parse_arithmetic(ArithmeticOperator::Add, remainder),
            Command::And => command::parse_logical_binary(BinaryOperator::And, remainder),
            Command::Capture => command::parse_capture(remainder, true),
            Command::Convert => command::parse_convert(remainder),
            Command::Declare => command::parse_declare(remainder),
            Command::DefCal => command::parse_defcal(remainder),
            Command::DefCircuit => command::parse_defcircuit(remainder),
            Command::DefFrame => command::parse_defframe(remainder),
            Command::DefGate => command::parse_defgate(remainder),
            Command::DefWaveform => command::parse_defwaveform(remainder),
            Command::Delay => command::parse_delay(remainder),
            Command::Div => command::parse_arithmetic(ArithmeticOperator::Divide, remainder),
            Command::Eq => command::parse_comparison(ComparisonOperator::Equal, remainder),
            Command::GE => {
                command::parse_comparison(ComparisonOperator::GreaterThanOrEqual, remainder)
            }
            Command::GT => command::parse_comparison(ComparisonOperator::GreaterThan, remainder),
            Command::LE => {
                command::parse_comparison(ComparisonOperator::LessThanOrEqual, remainder)
            }
            Command::LT => command::parse_comparison(ComparisonOperator::LessThan, remainder),
            Command::Fence => command::parse_fence(remainder),
            Command::Halt => Ok((remainder, Instruction::Halt)),
            Command::Include => command::parse_include(remainder),
            Command::Ior => command::parse_logical_binary(BinaryOperator::Ior, remainder),
            Command::Jump => command::parse_jump(remainder),
            Command::JumpUnless => command::parse_jump_unless(remainder),
            Command::JumpWhen => command::parse_jump_when(remainder),
            Command::Label => command::parse_label(remainder),
            Command::Load => command::parse_load(remainder),
            Command::Measure => command::parse_measurement(remainder),
            Command::Move => command::parse_move(remainder),
            Command::Exchange => command::parse_exchange(remainder),
            Command::Mul => command::parse_arithmetic(ArithmeticOperator::Multiply, remainder),
            Command::Neg => command::parse_logical_unary(UnaryOperator::Neg, remainder),
            Command::Nop => Ok((remainder, Instruction::Nop)),
            Command::Not => command::parse_logical_unary(UnaryOperator::Not, remainder),
            Command::Pragma => command::parse_pragma(remainder),
            Command::Pulse => command::parse_pulse(remainder, true),
            Command::RawCapture => command::parse_raw_capture(remainder, true),
            Command::Reset => command::parse_reset(remainder),
            Command::SetFrequency => command::parse_set_frequency(remainder),
            Command::SetPhase => command::parse_set_phase(remainder),
            Command::SetScale => command::parse_set_scale(remainder),
            Command::ShiftFrequency => command::parse_shift_frequency(remainder),
            Command::ShiftPhase => command::parse_shift_phase(remainder),
            Command::SwapPhases => command::parse_swap_phases(remainder),
            Command::Store => command::parse_store(remainder),
            Command::Sub => command::parse_arithmetic(ArithmeticOperator::Subtract, remainder),
            Command::Wait => Ok((remainder, Instruction::Wait)),
            Command::Xor => command::parse_logical_binary(BinaryOperator::Xor, remainder),
        }
        .map_err(|err| {
            nom::Err::Failure(
                InternalParseError::from_kind(
                    &input[..1],
                    ParserErrorKind::InvalidCommand { command: *command },
                )
                .with_previous(extract_nom_err(err)),
            )
        }),
        Some((Token::NonBlocking, remainder)) => match super::split_first_token(remainder) {
            Some((Token::Command(command), remainder)) => match command {
                Command::Pulse => command::parse_pulse(remainder, false),
                Command::Capture => command::parse_capture(remainder, false),
                Command::RawCapture => command::parse_raw_capture(remainder, false),
                _ => todo!(),
            },
            _ => todo!(),
        },
        Some((Token::Identifier(_), _)) | Some((Token::Modifier(_), _)) => gate::parse_gate(input),
        Some((_, _)) => Err(nom::Err::Failure(InternalParseError::from_kind(
            &input[..1],
            ParserErrorKind::NotACommandOrGate,
        ))),
    }
}

/// Parse all instructions from the input, trimming leading and trailing newlines and comments.
/// Returns an error if it does not reach the end of input.
pub(crate) fn parse_instructions(input: ParserInput) -> InternalParserResult<Vec<Instruction>> {
    all_consuming(delimited(
        common::skip_newlines_and_comments,
        many0(parse_instruction),
        common::skip_newlines_and_comments,
    ))(input)
}

/// Parse a block of indented "block instructions."
pub(crate) fn parse_block(input: ParserInput) -> InternalParserResult<Vec<Instruction>> {
    many1(parse_block_instruction)(input)
}

/// Parse a single indented "block instruction."
pub(crate) fn parse_block_instruction<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, Instruction> {
    preceded(
        token!(NewLine),
        preceded(token!(Indentation), parse_instruction),
    )(input)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::str::FromStr;

    use nom_locate::LocatedSpan;
    use num_complex::Complex;

    use crate::expression::{
        Expression, ExpressionFunction, FunctionCallExpression, InfixExpression, InfixOperator,
        PrefixExpression, PrefixOperator,
    };
    use crate::instruction::{
        Arithmetic, ArithmeticOperand, ArithmeticOperator, AttributeValue, BinaryLogic,
        BinaryOperand, BinaryOperator, Calibration, Capture, Comparison, ComparisonOperand,
        ComparisonOperator, Convert, FrameDefinition, FrameIdentifier, Gate, GateDefinition,
        GateSpecification, Include, Instruction, Jump, JumpWhen, Label, MemoryReference, Move,
        Pulse, Qubit, RawCapture, Reset, SetFrequency, SetPhase, SetScale, ShiftFrequency,
        ShiftPhase, SwapPhases, Target, UnaryLogic, UnaryOperator, Waveform, WaveformDefinition,
        WaveformInvocation,
    };
    use crate::parser::common::tests::KITCHEN_SINK_QUIL;
    use crate::parser::lexer::lex;
    use crate::quil::Quil;
    use crate::{make_test, real, Program};

    use super::parse_instructions;

    make_test!(
        semicolons_are_newlines,
        parse_instructions,
        "X 0; Y 1\nZ 2",
        vec![
            Instruction::Gate(Gate {
                name: "X".to_owned(),
                parameters: vec![],
                qubits: vec![Qubit::Fixed(0)],
                modifiers: vec![],
            }),
            Instruction::Gate(Gate {
                name: "Y".to_owned(),
                parameters: vec![],
                qubits: vec![Qubit::Fixed(1)],
                modifiers: vec![],
            }),
            Instruction::Gate(Gate {
                name: "Z".to_owned(),
                parameters: vec![],
                qubits: vec![Qubit::Fixed(2)],
                modifiers: vec![],
            }),
        ]
    );

    make_test!(
        arithmetic,
        parse_instructions,
        "ADD ro 2\nMUL ro 1.0\nSUB ro[1] -3\nDIV ro[1] -1.0\nADD ro[1] ro[2]",
        vec![
            Instruction::Arithmetic(Arithmetic {
                operator: ArithmeticOperator::Add,
                destination: ArithmeticOperand::MemoryReference(MemoryReference {
                    name: "ro".to_owned(),
                    index: 0
                }),
                source: ArithmeticOperand::LiteralInteger(2),
            }),
            Instruction::Arithmetic(Arithmetic {
                operator: ArithmeticOperator::Multiply,
                destination: ArithmeticOperand::MemoryReference(MemoryReference {
                    name: "ro".to_owned(),
                    index: 0
                }),
                source: ArithmeticOperand::LiteralReal(1.0),
            }),
            Instruction::Arithmetic(Arithmetic {
                operator: ArithmeticOperator::Subtract,
                destination: ArithmeticOperand::MemoryReference(MemoryReference {
                    name: "ro".to_owned(),
                    index: 1
                }),
                source: ArithmeticOperand::LiteralInteger(-3),
            }),
            Instruction::Arithmetic(Arithmetic {
                operator: ArithmeticOperator::Divide,
                destination: ArithmeticOperand::MemoryReference(MemoryReference {
                    name: "ro".to_owned(),
                    index: 1
                }),
                source: ArithmeticOperand::LiteralReal(-1f64),
            }),
            Instruction::Arithmetic(Arithmetic {
                operator: ArithmeticOperator::Add,
                destination: ArithmeticOperand::MemoryReference(MemoryReference {
                    name: "ro".to_owned(),
                    index: 1
                }),
                source: ArithmeticOperand::MemoryReference(MemoryReference {
                    name: "ro".to_owned(),
                    index: 2
                }),
            })
        ]
    );

    make_test!(
        comparison_logic,
        parse_instructions,
        "EQ dest ro 0\nLT dest ro[1] -1\nLE dest ro 1.2\nGT dest ro[2] 1e-6\nGE dest ro x",
        vec![
            Instruction::Comparison(Comparison {
                operator: ComparisonOperator::Equal,
                operands: (
                    MemoryReference {
                        name: "dest".to_owned(),
                        index: 0
                    },
                    MemoryReference {
                        name: "ro".to_owned(),
                        index: 0
                    },
                    ComparisonOperand::LiteralInteger(0)
                )
            }),
            Instruction::Comparison(Comparison {
                operator: ComparisonOperator::LessThan,
                operands: (
                    MemoryReference {
                        name: "dest".to_owned(),
                        index: 0
                    },
                    MemoryReference {
                        name: "ro".to_owned(),
                        index: 1
                    },
                    ComparisonOperand::LiteralInteger(-1)
                )
            }),
            Instruction::Comparison(Comparison {
                operator: ComparisonOperator::LessThanOrEqual,
                operands: (
                    MemoryReference {
                        name: "dest".to_owned(),
                        index: 0
                    },
                    MemoryReference {
                        name: "ro".to_owned(),
                        index: 0
                    },
                    ComparisonOperand::LiteralReal(1.2)
                )
            }),
            Instruction::Comparison(Comparison {
                operator: ComparisonOperator::GreaterThan,
                operands: (
                    MemoryReference {
                        name: "dest".to_owned(),
                        index: 0
                    },
                    MemoryReference {
                        name: "ro".to_owned(),
                        index: 2
                    },
                    ComparisonOperand::LiteralReal(0.000001)
                )
            }),
            Instruction::Comparison(Comparison {
                operator: ComparisonOperator::GreaterThanOrEqual,
                operands: (
                    MemoryReference {
                        name: "dest".to_owned(),
                        index: 0
                    },
                    MemoryReference {
                        name: "ro".to_owned(),
                        index: 0
                    },
                    ComparisonOperand::MemoryReference(MemoryReference {
                        name: "x".to_owned(),
                        index: 0
                    }),
                )
            })
        ]
    );

    #[test]
    fn test_comparison_logic_error() {
        [
            "EQ ro 1 1",
            "LT 1 1 1",
            "LE 1 x ro",
            "GT 1 ro x",
            "GE dest 0.3 4",
        ]
        .iter()
        .for_each(|input| {
            let input = LocatedSpan::new(*input);
            let tokens = lex(input).unwrap();
            assert!(parse_instructions(&tokens).is_err(), "{}", input);
        })
    }

    make_test!(
        binary_logic,
        parse_instructions,
        "AND ro 1\nIOR ro[1] ro[2]\nXOR ro[1] 0\nAND ro[1] ro[2]",
        vec![
            Instruction::BinaryLogic(BinaryLogic {
                operator: BinaryOperator::And,
                operands: (
                    MemoryReference {
                        name: "ro".to_owned(),
                        index: 0
                    },
                    BinaryOperand::LiteralInteger(1)
                )
            }),
            Instruction::BinaryLogic(BinaryLogic {
                operator: BinaryOperator::Ior,
                operands: (
                    MemoryReference {
                        name: "ro".to_owned(),
                        index: 1
                    },
                    BinaryOperand::MemoryReference(MemoryReference {
                        name: "ro".to_owned(),
                        index: 2
                    })
                )
            }),
            Instruction::BinaryLogic(BinaryLogic {
                operator: BinaryOperator::Xor,
                operands: (
                    MemoryReference {
                        name: "ro".to_owned(),
                        index: 1
                    },
                    BinaryOperand::LiteralInteger(0)
                )
            }),
            Instruction::BinaryLogic(BinaryLogic {
                operator: BinaryOperator::And,
                operands: (
                    MemoryReference {
                        name: "ro".to_owned(),
                        index: 1
                    },
                    BinaryOperand::MemoryReference(MemoryReference {
                        name: "ro".to_owned(),
                        index: 2
                    })
                )
            }),
        ]
    );

    #[test]
    fn test_binary_logic_error() {
        ["AND ro", "XOR 1 1", "IOR 1"].iter().for_each(|input| {
            let input = LocatedSpan::new(*input);
            let tokens = lex(input).unwrap();
            assert!(parse_instructions(&tokens).is_err(), "{}", input);
        })
    }

    make_test!(
        unary_logic,
        parse_instructions,
        "NOT ro\nNEG ro\nNOT ro[1]\nNEG ro[1]",
        vec![
            Instruction::UnaryLogic(UnaryLogic {
                operator: UnaryOperator::Not,
                operand: MemoryReference {
                    name: "ro".to_owned(),
                    index: 0,
                }
            }),
            Instruction::UnaryLogic(UnaryLogic {
                operator: UnaryOperator::Neg,
                operand: MemoryReference {
                    name: "ro".to_owned(),
                    index: 0,
                }
            }),
            Instruction::UnaryLogic(UnaryLogic {
                operator: UnaryOperator::Not,
                operand: MemoryReference {
                    name: "ro".to_owned(),
                    index: 1,
                }
            }),
            Instruction::UnaryLogic(UnaryLogic {
                operator: UnaryOperator::Neg,
                operand: MemoryReference {
                    name: "ro".to_owned(),
                    index: 1,
                }
            }),
        ]
    );

    #[test]
    fn test_unary_logic_error() {
        ["NEG 1", "NOT 1", "NEG 0", "NOT 0"]
            .iter()
            .for_each(|input| {
                let input = LocatedSpan::new(*input);
                let tokens = lex(input).unwrap();
                assert!(parse_instructions(&tokens).is_err(), "{}", input);
            })
    }

    make_test!(
        capture_instructions,
        parse_instructions,
        "CAPTURE 0 \"rx\" my_custom_waveform ro\nRAW-CAPTURE 0 1 \"rx\" 2e9 ro\nNONBLOCKING CAPTURE 0 \"rx\" my_custom_waveform(a: 1.0) ro\nNONBLOCKING RAW-CAPTURE 0 1 \"rx\" 2e9 ro",
        vec![
            Instruction::Capture(Capture {
                blocking: true,
                frame: FrameIdentifier {
                    name: "rx".to_owned(),
                    qubits: vec![Qubit::Fixed(0)]
                },
                waveform: WaveformInvocation {
                    name: "my_custom_waveform".to_owned(),
                    parameters: HashMap::new()
                },
                memory_reference: MemoryReference {
                    name: "ro".to_owned(),
                    index: 0
                }
            }),
            Instruction::RawCapture(RawCapture {
                blocking: true,
                frame: FrameIdentifier {
                    name: "rx".to_owned(),
                    qubits: vec![Qubit::Fixed(0), Qubit::Fixed(1)]
                },
                duration: Expression::Number(real![2e9]),
                memory_reference: MemoryReference {
                    name: "ro".to_owned(),
                    index: 0
                }
            }),
            Instruction::Capture(Capture {
                blocking: false,
                frame: FrameIdentifier {
                    name: "rx".to_owned(),
                    qubits: vec![Qubit::Fixed(0)]
                },
                waveform: WaveformInvocation {
                    name: "my_custom_waveform".to_owned(),
                    parameters: vec![("a".to_owned(), Expression::Number(real!(1f64)))].into_iter().collect()
                },
                memory_reference: MemoryReference {
                    name: "ro".to_owned(),
                    index: 0
                }
            }),
            Instruction::RawCapture(RawCapture {
                blocking: false,
                frame: FrameIdentifier {
                    name: "rx".to_owned(),
                    qubits: vec![Qubit::Fixed(0), Qubit::Fixed(1)]
                },
                duration: Expression::Number(real![2e9]),
                memory_reference: MemoryReference {
                    name: "ro".to_owned(),
                    index: 0
                }
            })
        ]
    );

    make_test!(comment, parse_instructions, "# Questions:\n\n\n", vec![]);

    make_test!(
        comment_and_gate,
        parse_instructions,
        "# Questions:\nX 0",
        vec![Instruction::Gate(Gate {
            name: "X".to_owned(),
            parameters: vec![],
            qubits: vec![Qubit::Fixed(0)],
            modifiers: vec![],
        })]
    );

    make_test!(
        comment_after_block,
        parse_instructions,
        "DEFFRAME 0 \"ro_rx\":\n\tDIRECTION: \"rx\"\n\n# (Pdb) settings.gates[GateID(name=\"x180\", targets=(0,))]\n\n",
        vec![Instruction::FrameDefinition(FrameDefinition {
            identifier: FrameIdentifier { name: "ro_rx".to_owned(), qubits: vec![Qubit::Fixed(0)] },
            attributes: [("DIRECTION".to_owned(), AttributeValue::String("rx".to_owned()))].into_iter().collect()
        })]);

    make_test!(
        simple_gate,
        parse_instructions,
        "RX 0",
        vec![Instruction::Gate(Gate {
            name: "RX".to_owned(),
            parameters: vec![],
            qubits: vec![Qubit::Fixed(0)],
            modifiers: vec![],
        })]
    );

    make_test!(
        parametric_gate,
        parse_instructions,
        "RX(pi) 10",
        vec![Instruction::Gate(Gate {
            name: "RX".to_owned(),
            parameters: vec![Expression::PiConstant],
            qubits: vec![Qubit::Fixed(10)],
            modifiers: vec![],
        })]
    );

    make_test!(
        parametric_calibration,
        parse_instructions,
        "DEFCAL RX(%theta) %qubit:\n\tPULSE 1 \"xy\" custom_waveform(a: 1)",
        vec![Instruction::CalibrationDefinition(Calibration {
            name: "RX".to_owned(),
            parameters: vec![Expression::Variable("theta".to_owned())],
            qubits: vec![Qubit::Variable("qubit".to_owned())],
            modifiers: vec![],
            instructions: vec![Instruction::Pulse(Pulse {
                blocking: true,
                frame: FrameIdentifier {
                    name: "xy".to_owned(),
                    qubits: vec![Qubit::Fixed(1)]
                },
                waveform: WaveformInvocation {
                    name: "custom_waveform".to_owned(),
                    parameters: [("a".to_owned(), Expression::Number(crate::real![1f64]))]
                        .into_iter()
                        .collect()
                }
            })]
        })]
    );

    make_test!(
        frame_definition,
        parse_instructions,
        "DEFFRAME 0 \"rx\":\n\tINITIAL-FREQUENCY: 2e9",
        vec![Instruction::FrameDefinition(FrameDefinition {
            identifier: FrameIdentifier {
                name: "rx".to_owned(),
                qubits: vec![Qubit::Fixed(0)]
            },
            attributes: [(
                "INITIAL-FREQUENCY".to_owned(),
                AttributeValue::Expression(Expression::Number(crate::real![2e9]))
            )]
            .into_iter()
            .collect()
        })]
    );

    make_test!(
        control_flow,
        parse_instructions,
        "LABEL @hello\nJUMP @hello\nJUMP-WHEN @hello ro",
        vec![
            Instruction::Label(Label {
                target: Target::Fixed("hello".to_owned())
            }),
            Instruction::Jump(Jump {
                target: Target::Fixed("hello".to_owned())
            }),
            Instruction::JumpWhen(JumpWhen {
                target: Target::Fixed("hello".to_owned()),
                condition: MemoryReference {
                    name: "ro".to_owned(),
                    index: 0
                }
            })
        ]
    );

    make_test!(
        pulse_instructions,
        parse_instructions,
        "PULSE 0 \"xy\" custom\nNONBLOCKING PULSE 0 \"xy\" custom\nPULSE 0 \"xy\" custom(a: 1.0)",
        vec![
            Instruction::Pulse(Pulse {
                blocking: true,
                frame: FrameIdentifier {
                    name: "xy".to_owned(),
                    qubits: vec![Qubit::Fixed(0)]
                },
                waveform: WaveformInvocation {
                    name: "custom".to_owned(),
                    parameters: HashMap::new()
                }
            }),
            Instruction::Pulse(Pulse {
                blocking: false,
                frame: FrameIdentifier {
                    name: "xy".to_owned(),
                    qubits: vec![Qubit::Fixed(0)]
                },
                waveform: WaveformInvocation {
                    name: "custom".to_owned(),
                    parameters: HashMap::new()
                }
            }),
            Instruction::Pulse(Pulse {
                blocking: true,
                frame: FrameIdentifier {
                    name: "xy".to_owned(),
                    qubits: vec![Qubit::Fixed(0)]
                },
                waveform: WaveformInvocation {
                    name: "custom".to_owned(),
                    parameters: vec![("a".to_owned(), Expression::Number(real!(1f64)))]
                        .into_iter()
                        .collect()
                }
            })
        ]
    );

    make_test!(
        moveit,
        parse_instructions,
        "MOVE a 1.0",
        vec![Instruction::Move(Move {
            destination: MemoryReference {
                name: "a".to_owned(),
                index: 0
            },
            source: ArithmeticOperand::LiteralReal(1.0)
        })]
    );

    make_test!(
        parse_reset,
        parse_instructions,
        "RESET\nRESET 0",
        vec![
            Instruction::Reset(Reset { qubit: None }),
            Instruction::Reset(Reset {
                qubit: Some(Qubit::Fixed(0))
            })
        ]
    );

    make_test!(
        waveform_definition,
        parse_instructions,
        "DEFWAVEFORM q44_q45_cphase/sqrtCPHASE:\n\t0.0, 0.0, 0.00027685415721916584",
        vec![Instruction::WaveformDefinition(WaveformDefinition {
            name: "q44_q45_cphase/sqrtCPHASE".to_owned(),
            definition: Waveform {
                matrix: vec![
                    Expression::Number(real!(0.0)),
                    Expression::Number(real!(0.0)),
                    Expression::Number(real!(0.00027685415721916584))
                ],
                parameters: vec![],
            }
        })]
    );

    make_test!(
        gate_definition,
        parse_instructions,
        "DEFGATE H:\n\t1/sqrt(2), 1/sqrt(2)\n\t1/sqrt(2), -1/sqrt(2)\n",
        vec![Instruction::GateDefinition(GateDefinition {
            name: "H".to_string(),
            parameters: vec![],
            specification: GateSpecification::Matrix(vec![
                vec![
                    Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Number(real!(1.0))),
                        operator: InfixOperator::Slash,
                        right: Box::new(Expression::FunctionCall(FunctionCallExpression {
                            function: crate::expression::ExpressionFunction::SquareRoot,
                            expression: Box::new(Expression::Number(real!(2.0))),
                        })),
                    }),
                    Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Number(real!(1.0))),
                        operator: InfixOperator::Slash,
                        right: Box::new(Expression::FunctionCall(FunctionCallExpression {
                            function: crate::expression::ExpressionFunction::SquareRoot,
                            expression: Box::new(Expression::Number(real!(2.0))),
                        })),
                    }),
                ],
                vec![
                    Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Number(real!(1.0))),
                        operator: InfixOperator::Slash,
                        right: Box::new(Expression::FunctionCall(FunctionCallExpression {
                            function: crate::expression::ExpressionFunction::SquareRoot,
                            expression: Box::new(Expression::Number(real!(2.0))),
                        })),
                    }),
                    Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Prefix(PrefixExpression {
                            operator: PrefixOperator::Minus,
                            expression: Box::new(Expression::Number(real!(1.0))),
                        })),
                        operator: InfixOperator::Slash,
                        right: Box::new(Expression::FunctionCall(FunctionCallExpression {
                            function: crate::expression::ExpressionFunction::SquareRoot,
                            expression: Box::new(Expression::Number(real!(2.0))),
                        })),
                    }),
                ],
            ]),
        })]
    );

    make_test!(
        gate_definition_with_params,
        parse_instructions,
        "DEFGATE RX(%theta):\n\tCOS(%theta/2), -i*SIN(%theta/2)\n\t-i*SIN(%theta/2), COS(%theta/2)\n",
        vec![Instruction::GateDefinition(GateDefinition {
            name: "RX".to_string(),
            parameters: vec!["theta".to_string()],
            specification: GateSpecification::Matrix(vec![
                vec![
                    Expression::FunctionCall(FunctionCallExpression {
                        function: ExpressionFunction::Cosine,
                        expression: Box::new(Expression::Infix(InfixExpression {
                            left: Box::new(Expression::Variable("theta".to_string())),
                            operator: InfixOperator::Slash,
                            right: Box::new(Expression::Number(Complex { re: 2.0, im: 0.0 })),
                        }))
                    }),
                    Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Prefix(PrefixExpression {
                            operator: PrefixOperator::Minus,
                            expression: Box::new(Expression::Number(Complex { re: 0.0, im: 1.0 })),
                        })),
                        operator: InfixOperator::Star,
                        right: Box::new(Expression::FunctionCall(FunctionCallExpression {
                            function: ExpressionFunction::Sine,
                            expression: Box::new(Expression::Infix(InfixExpression {
                                left: Box::new(Expression::Variable("theta".to_string())),
                                operator: InfixOperator::Slash,
                                right: Box::new(Expression::Number(Complex { re: 2.0, im: 0.0 })),
                            }))
                        }))
                    }),
                ],
                vec![
                    Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Prefix(PrefixExpression {
                            operator: PrefixOperator::Minus,
                            expression: Box::new(Expression::Number(Complex { re: 0.0, im: 1.0 }))
                        })),
                        operator: InfixOperator::Star,
                        right: Box::new(Expression::FunctionCall(FunctionCallExpression {
                            function: ExpressionFunction::Sine,
                            expression: Box::new(Expression::Infix(InfixExpression {
                                left: Box::new(Expression::Variable("theta".to_string())),
                                operator: InfixOperator::Slash,
                                right: Box::new(Expression::Number(Complex { re: 2.0, im: 0.0 }))
                            }))
                        }))
                    }),
                    Expression::FunctionCall(FunctionCallExpression {
                        function: ExpressionFunction::Cosine,
                        expression: Box::new(Expression::Infix(InfixExpression {
                            left: Box::new(Expression::Variable("theta".to_string())),
                            operator: InfixOperator::Slash,
                            right: Box::new(Expression::Number(Complex { re: 2.0, im: 0.0 })),
                        }))
                    }),
                ],
            ]),
        })]
    );

    make_test!(
        convert,
        parse_instructions,
        "CONVERT theta unadjusted-theta[1]",
        vec![Instruction::Convert(Convert {
            destination: MemoryReference {
                name: "theta".to_string(),
                index: 0
            },
            source: MemoryReference {
                name: "unadjusted-theta".to_string(),
                index: 1
            },
        })]
    );

    make_test!(
        include,
        parse_instructions,
        r#"INCLUDE "another/quil/file.quil""#,
        vec![Instruction::Include(Include {
            filename: "another/quil/file.quil".to_string()
        })]
    );

    make_test!(nop, parse_instructions, r#"NOP"#, vec![Instruction::Nop]);

    #[test]
    fn parse_set_phase() {
        let input = LocatedSpan::new(r#"SET-PHASE 0 "rf" 1.0; SET-PHASE 0 1 "rf" theta"#);
        let tokens = lex(input).unwrap();
        let (remainder, parsed) = parse_instructions(&tokens).unwrap();
        let expected = vec![
            Instruction::SetPhase(SetPhase {
                frame: FrameIdentifier {
                    name: String::from("rf"),
                    qubits: vec![Qubit::Fixed(0)],
                },
                phase: Expression::Number(real!(1.0)),
            }),
            Instruction::SetPhase(SetPhase {
                frame: FrameIdentifier {
                    name: String::from("rf"),
                    qubits: vec![Qubit::Fixed(0), Qubit::Fixed(1)],
                },
                phase: Expression::Address(MemoryReference {
                    name: String::from("theta"),
                    index: 0,
                }),
            }),
        ];
        assert_eq!(parsed, expected);
        assert_eq!(remainder.len(), 0);
    }

    #[test]
    fn parse_swap_phases() {
        let input =
            LocatedSpan::new(r#"SWAP-PHASES 0 "rf" 1 "rf"; SWAP-PHASES 1 2 3 "rf" 4 5 6 "rf""#);
        let tokens = lex(input).unwrap();
        let (remainder, parsed) = parse_instructions(&tokens).unwrap();
        let expected = vec![
            Instruction::SwapPhases(SwapPhases {
                frame_1: FrameIdentifier {
                    name: String::from("rf"),
                    qubits: vec![Qubit::Fixed(0)],
                },
                frame_2: FrameIdentifier {
                    name: String::from("rf"),
                    qubits: vec![Qubit::Fixed(1)],
                },
            }),
            Instruction::SwapPhases(SwapPhases {
                frame_1: FrameIdentifier {
                    name: String::from("rf"),
                    qubits: vec![Qubit::Fixed(1), Qubit::Fixed(2), Qubit::Fixed(3)],
                },
                frame_2: FrameIdentifier {
                    name: String::from("rf"),
                    qubits: vec![Qubit::Fixed(4), Qubit::Fixed(5), Qubit::Fixed(6)],
                },
            }),
        ];
        assert_eq!(parsed, expected);
        assert_eq!(remainder.len(), 0);
    }

    #[test]
    fn parse_set_scale() {
        let input = LocatedSpan::new(r#"SET-SCALE 0 "rf" 1.0; SET-SCALE 0 1 "rf" theta"#);
        let tokens = lex(input).unwrap();
        let (remainder, parsed) = parse_instructions(&tokens).unwrap();
        let expected = vec![
            Instruction::SetScale(SetScale {
                frame: FrameIdentifier {
                    name: String::from("rf"),
                    qubits: vec![Qubit::Fixed(0)],
                },
                scale: Expression::Number(real!(1.0)),
            }),
            Instruction::SetScale(SetScale {
                frame: FrameIdentifier {
                    name: String::from("rf"),
                    qubits: vec![Qubit::Fixed(0), Qubit::Fixed(1)],
                },
                scale: Expression::Address(MemoryReference {
                    name: String::from("theta"),
                    index: 0,
                }),
            }),
        ];
        assert_eq!(parsed, expected);
        assert_eq!(remainder.len(), 0);
    }

    #[test]
    fn parse_set_frequency() {
        let input = LocatedSpan::new(r#"SET-FREQUENCY 0 "rf" 1.0; SET-FREQUENCY 0 1 "rf" theta"#);
        let tokens = lex(input).unwrap();
        let (remainder, parsed) = parse_instructions(&tokens).unwrap();
        let expected = vec![
            Instruction::SetFrequency(SetFrequency {
                frame: FrameIdentifier {
                    name: String::from("rf"),
                    qubits: vec![Qubit::Fixed(0)],
                },
                frequency: Expression::Number(real!(1.0)),
            }),
            Instruction::SetFrequency(SetFrequency {
                frame: FrameIdentifier {
                    name: String::from("rf"),
                    qubits: vec![Qubit::Fixed(0), Qubit::Fixed(1)],
                },
                frequency: Expression::Address(MemoryReference {
                    name: String::from("theta"),
                    index: 0,
                }),
            }),
        ];
        assert_eq!(parsed, expected);
        assert_eq!(remainder.len(), 0);
    }

    #[test]
    fn parse_shift_frequency() {
        let input =
            LocatedSpan::new(r#"SHIFT-FREQUENCY 0 "rf" 1.0; SHIFT-FREQUENCY 0 1 "rf" theta"#);
        let tokens = lex(input).unwrap();
        let (remainder, parsed) = parse_instructions(&tokens).unwrap();
        let expected = vec![
            Instruction::ShiftFrequency(ShiftFrequency {
                frame: FrameIdentifier {
                    name: String::from("rf"),
                    qubits: vec![Qubit::Fixed(0)],
                },
                frequency: Expression::Number(real!(1.0)),
            }),
            Instruction::ShiftFrequency(ShiftFrequency {
                frame: FrameIdentifier {
                    name: String::from("rf"),
                    qubits: vec![Qubit::Fixed(0), Qubit::Fixed(1)],
                },
                frequency: Expression::Address(MemoryReference {
                    name: String::from("theta"),
                    index: 0,
                }),
            }),
        ];
        assert_eq!(parsed, expected);
        assert_eq!(remainder.len(), 0);
    }

    #[test]
    fn parse_shift_phase() {
        let input = LocatedSpan::new(r#"SHIFT-PHASE 0 "rf" 1.0; SHIFT-PHASE 0 1 "rf" theta"#);
        let tokens = lex(input).unwrap();
        let (remainder, parsed) = parse_instructions(&tokens).unwrap();
        let expected = vec![
            Instruction::ShiftPhase(ShiftPhase {
                frame: FrameIdentifier {
                    name: String::from("rf"),
                    qubits: vec![Qubit::Fixed(0)],
                },
                phase: Expression::Number(real!(1.0)),
            }),
            Instruction::ShiftPhase(ShiftPhase {
                frame: FrameIdentifier {
                    name: String::from("rf"),
                    qubits: vec![Qubit::Fixed(0), Qubit::Fixed(1)],
                },
                phase: Expression::Address(MemoryReference {
                    name: String::from("theta"),
                    index: 0,
                }),
            }),
        ];
        assert_eq!(parsed, expected);
        assert_eq!(remainder.len(), 0);
    }

    /// Test that an entire sample program can be parsed without failure.
    #[test]
    fn kitchen_sink() {
        Program::from_str(KITCHEN_SINK_QUIL).unwrap();
    }

    /// Assert that when a program is converted to a string, the conversion of
    /// that string into a program produces a program identical to the original
    /// program.
    #[test]
    fn parse_roundtrip() {
        let inputs = vec![
            r#"DEFCAL MEASURE 0 dest:
	DECLARE iq REAL[2]
	CAPTURE 0 "out" flat(duration: 1.0, iqs: (2.0+3.0i)) iq[0]"#,
            r#"LABEL @target
JUMP @target"#,
        ];

        for input in inputs {
            let program = Program::from_str(input).unwrap();
            let output = program.to_quil().unwrap();
            let roundtrip = Program::from_str(&output).unwrap();
            assert_eq!(output, roundtrip.to_quil().unwrap());
        }
    }
}
