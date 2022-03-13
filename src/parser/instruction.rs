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
    combinator::all_consuming,
    multi::{many0, many1},
    sequence::{delimited, preceded},
};

use crate::{
    instruction::{ArithmeticOperator, Instruction},
    token,
};

use super::{
    command, common,
    error::{Error, ErrorKind},
    gate,
    lexer::{Command, Token},
    ParserInput, ParserResult,
};

/// Parse the next instructon from the input, skipping past leading newlines, comments, and semicolons.
pub fn parse_instruction(input: ParserInput) -> ParserResult<Instruction> {
    let (input, _) = common::skip_newlines_and_comments(input)?;
    match input.split_first() {
        None => Err(nom::Err::Error(Error {
            input,
            error: ErrorKind::EndOfInput,
        })),
        Some((Token::Command(command), remainder)) => {
            match command {
                Command::Add => command::parse_arithmetic(ArithmeticOperator::Add, remainder),
                // Command::And => {}
                Command::Capture => command::parse_capture(remainder, true),
                // Command::Convert => {}
                Command::Declare => command::parse_declare(remainder),
                Command::DefCal => command::parse_defcal(remainder),
                Command::DefCircuit => command::parse_defcircuit(remainder),
                Command::DefFrame => command::parse_defframe(remainder),
                // Command::DefGate => Ok((remainder, cut(parse_command_defgate))),
                Command::DefWaveform => command::parse_defwaveform(remainder),
                Command::Delay => command::parse_delay(remainder),
                Command::Div => command::parse_arithmetic(ArithmeticOperator::Divide, remainder),
                // Command::Eq => {}
                // Command::Exchange => {}
                Command::Fence => command::parse_fence(remainder),
                // Command::GE => {}
                Command::GT => command::parse_gt(remainder),
                Command::Halt => Ok((remainder, Instruction::Halt)),
                // Command::Include => {}
                // Command::Ior => {}
                Command::Jump => command::parse_jump(remainder),
                Command::JumpUnless => command::parse_jump_unless(remainder),
                Command::JumpWhen => command::parse_jump_when(remainder),
                Command::Label => command::parse_label(remainder),
                // Command::LE => {}
                Command::Load => command::parse_load(remainder),
                // Command::LT => {}
                Command::Measure => command::parse_measurement(remainder),
                Command::Move => command::parse_move(remainder),
                Command::Exchange => command::parse_exchange(remainder),
                Command::Mul => command::parse_arithmetic(ArithmeticOperator::Multiply, remainder),
                // Command::Neg => {}
                // Command::Nop => {}
                // Command::Not => {}
                Command::Pragma => command::parse_pragma(remainder),
                Command::Pulse => command::parse_pulse(remainder, true),
                Command::RawCapture => command::parse_raw_capture(remainder, true),
                Command::Reset => command::parse_reset(remainder),
                Command::SetFrequency => command::parse_set_frequency(remainder),
                Command::SetPhase => command::parse_set_phase(remainder),
                Command::SetScale => command::parse_set_scale(remainder),
                Command::ShiftFrequency => command::parse_shift_frequency(remainder),
                Command::ShiftPhase => command::parse_shift_phase(remainder),
                Command::Store => command::parse_store(remainder),
                Command::Sub => command::parse_arithmetic(ArithmeticOperator::Subtract, remainder),
                // Command::Wait => {}
                // Command::Xor => {}
                _ => Err(nom::Err::Failure(Error {
                    input: &input[..1],
                    error: ErrorKind::UnsupportedInstruction,
                })),
            }
            .map_err(|err| {
                nom::Err::Failure(Error {
                    input: &input[..1],
                    error: ErrorKind::InvalidCommand {
                        command: command.clone(),
                        error: format!("{}", err),
                    },
                })
            })
        }
        Some((Token::NonBlocking, remainder)) => match remainder.split_first() {
            Some((Token::Command(command), remainder)) => match command {
                Command::Pulse => command::parse_pulse(remainder, false),
                Command::Capture => command::parse_capture(remainder, false),
                Command::RawCapture => command::parse_raw_capture(remainder, false),
                _ => todo!(),
            },
            _ => todo!(),
        },
        Some((Token::Identifier(_), _)) | Some((Token::Modifier(_), _)) => gate::parse_gate(input),
        Some((_, _)) => Err(nom::Err::Failure(Error {
            input: &input[..1],
            error: ErrorKind::NotACommandOrGate,
        })),
    }
}

/// Parse all instructions from the input, trimming leading and trailing newlines and comments.
/// Returns an error if it does not reach the end of input.
pub fn parse_instructions(input: ParserInput) -> ParserResult<Vec<Instruction>> {
    all_consuming(delimited(
        common::skip_newlines_and_comments,
        many0(parse_instruction),
        common::skip_newlines_and_comments,
    ))(input)
}

/// Parse a block of indented "block instructions."
pub fn parse_block(input: ParserInput) -> ParserResult<Vec<Instruction>> {
    many1(parse_block_instruction)(input)
}

/// Parse a single indented "block instruction."
pub fn parse_block_instruction<'a>(input: ParserInput<'a>) -> ParserResult<'a, Instruction> {
    preceded(
        token!(NewLine),
        preceded(token!(Indentation), parse_instruction),
    )(input)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::instruction::{
        Label, Reset, SetFrequency, SetPhase, SetScale, ShiftFrequency, ShiftPhase, Waveform,
        WaveformDefinition,
    };
    use crate::parser::lexer::lex;
    use crate::{
        expression::Expression,
        instruction::{
            Arithmetic, ArithmeticOperand, ArithmeticOperator, AttributeValue, Calibration,
            Capture, FrameDefinition, FrameIdentifier, Gate, Instruction, Jump, JumpWhen,
            MemoryReference, Move, Pulse, Qubit, RawCapture, WaveformInvocation,
        },
        make_test, real,
    };

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
            attributes: [("DIRECTION".to_owned(), AttributeValue::String("rx".to_owned()))].iter().cloned().collect()
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
                        .iter()
                        .cloned()
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
            .iter()
            .cloned()
            .collect()
        })]
    );

    make_test!(
        control_flow,
        parse_instructions,
        "LABEL @hello\nJUMP @hello\nJUMP-WHEN @hello ro",
        vec![
            Instruction::Label(Label("hello".to_owned())),
            Instruction::Jump(Jump {
                target: "hello".to_owned()
            }),
            Instruction::JumpWhen(JumpWhen {
                target: "hello".to_owned(),
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
            destination: ArithmeticOperand::MemoryReference(MemoryReference {
                name: "a".to_owned(),
                index: 0
            }),
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

    #[test]
    fn parse_set_phase() {
        let tokens = lex(r#"SET-PHASE 0 "rf" 1.0; SET-PHASE 0 1 "rf" theta"#).unwrap();
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
        assert_eq!(remainder.len(), 0);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn parse_set_scale() {
        let tokens = lex(r#"SET-SCALE 0 "rf" 1.0; SET-SCALE 0 1 "rf" theta"#).unwrap();
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
        assert_eq!(remainder.len(), 0);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn parse_set_frequency() {
        let tokens = lex(r#"SET-FREQUENCY 0 "rf" 1.0; SET-FREQUENCY 0 1 "rf" theta"#).unwrap();
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
        assert_eq!(remainder.len(), 0);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn parse_shift_frequency() {
        let tokens = lex(r#"SHIFT-FREQUENCY 0 "rf" 1.0; SHIFT-FREQUENCY 0 1 "rf" theta"#).unwrap();
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
        assert_eq!(remainder.len(), 0);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn parse_shift_phase() {
        let tokens = lex(r#"SHIFT-PHASE 0 "rf" 1.0; SHIFT-PHASE 0 1 "rf" theta"#).unwrap();
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
        assert_eq!(remainder.len(), 0);
        assert_eq!(parsed, expected);
    }
}
