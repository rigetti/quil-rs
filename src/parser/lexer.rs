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
    bytes::complete::{is_a, is_not, tag, take_until, take_while, take_while1},
    character::complete::{digit1, one_of},
    combinator::{all_consuming, map, recognize, value},
    multi::many0,
    number::complete::double,
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    As,
    Colon,
    Comma,
    Command(Command),
    Comment(String),
    // Constant(Constant),
    DataType(DataType),
    Float(f64),
    Identifier(String),
    Indentation,
    Integer(usize),
    Label(String),
    LBracket,
    LParenthesis,
    NonBlocking,
    Matrix,
    Modifier(Modifier),
    NewLine,
    Operator(Operator),
    Permutation,
    RBracket,
    RParenthesis,
    Semicolon,
    Sharing,
    String(String),
    Variable(String),
}

impl nom::InputLength for Token {
    fn input_len(&self) -> usize {
        // All tokens take up exactly one place in the input token stream
        1
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Command {
    Add,
    And,
    Capture,
    Convert,
    Declare,
    DefCal,
    DefCircuit,
    DefFrame,
    DefGate,
    DefWaveform,
    Delay,
    Div,
    Eq,
    Exchange,
    Fence,
    GE,
    GT,
    Halt,
    Include,
    Ior,
    Jump,
    JumpUnless,
    JumpWhen,
    Label,
    LE,
    Load,
    LT,
    Measure,
    Move,
    Mul,
    Neg,
    Nop,
    Not,
    Pragma,
    Pulse,
    RawCapture,
    Reset,
    SetFrequency,
    SetPhase,
    SetScale,
    ShiftFrequency,
    ShiftPhase,
    Store,
    Sub,
    Wait,
    Xor,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DataType {
    Bit,
    Octet,
    Real,
    Integer,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Modifier {
    Controlled,
    Dagger,
    Forked, // Not in the Quil grammar
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Caret,
    Minus,
    Plus,
    Slash,
    Star,
}

pub type LexResult<'a> = IResult<&'a str, Token>;

/// Completely lex a string, returning the tokens within. Panics if the string cannot be completely read.
pub fn lex(input: &str) -> Vec<Token> {
    let result: Result<(&str, Vec<Token>), String> =
        all_consuming(_lex)(input).map_err(|err| format!("Error remains: {:?}", err));
    result.unwrap().1
}

fn _lex(input: &str) -> IResult<&str, Vec<Token>> {
    terminated(
        many0(alt((
            value(Token::Indentation, tag("    ")),
            preceded(many0(tag(" ")), lex_token),
        ))),
        many0(one_of("\n\t ")),
    )(input)
}

fn lex_token(input: &str) -> LexResult {
    alt((
        lex_comment,
        // Instruction must come before identifier
        lex_instruction,
        lex_data_type,
        lex_modifier,
        lex_punctuation,
        lex_label,
        lex_string,
        // Operator must come before number (or it may be parsed as a prefix)
        lex_operator,
        lex_number,
        lex_variable,
        lex_non_blocking,
        // This should come last because it's sort of a catch all
        lex_identifier,
    ))(input)
}

fn lex_data_type(input: &str) -> LexResult {
    alt((
        value(Token::DataType(DataType::Bit), tag("BIT")),
        value(Token::DataType(DataType::Integer), tag("INTEGER")),
        value(Token::DataType(DataType::Octet), tag("OCTET")),
        value(Token::DataType(DataType::Real), tag("REAL")),
    ))(input)
}

fn lex_comment(input: &str) -> LexResult {
    let (input, _) = tag("#")(input)?;
    let (input, content) = is_not("\n")(input)?;
    Ok((input, Token::Comment(content.to_owned())))
}

fn lex_instruction(input: &str) -> LexResult {
    use Command::*;

    // TODO: Switch these `map`s over to `value`s

    // The `alt`s are nested because there is a limit to how many branches are allowed in one of them
    // https://github.com/Geal/nom/issues/1144
    let result = alt((
        alt((
            map(tag("DEFGATE"), |_| DefGate),
            map(tag("DEFCIRCUIT"), |_| DefCircuit),
            map(tag("MEASURE"), |_| Measure),
            map(tag("HALT"), |_| Halt),
            map(tag("JUMP-WHEN"), |_| JumpWhen),
            map(tag("JUMP-UNLESS"), |_| JumpUnless),
            // Note: this must follow the other jump commands
            map(tag("JUMP"), |_| Jump),
            map(tag("RESET"), |_| Reset),
            map(tag("WAIT"), |_| Wait),
            map(tag("NOP"), |_| Nop),
            map(tag("INCLUDE"), |_| Include),
            map(tag("PRAGMA"), |_| Pragma),
            map(tag("DECLARE"), |_| Declare),
            value(Label, tag("LABEL")),
        )),
        alt((
            map(tag("ADD"), |_| Add),
            map(tag("AND"), |_| And),
            map(tag("CONVERT"), |_| Convert),
            map(tag("DIV"), |_| Div),
            map(tag("EQ"), |_| Eq),
            map(tag("EXCHANGE"), |_| Exchange),
            map(tag("GE"), |_| GE),
            map(tag("GT"), |_| GT),
            map(tag("IOR"), |_| Ior),
            map(tag("LE"), |_| LE),
            map(tag("LOAD"), |_| Load),
            map(tag("LT"), |_| LT),
            map(tag("MOVE"), |_| Move),
            map(tag("MUL"), |_| Mul),
            map(tag("NEG"), |_| Neg),
            map(tag("NOT"), |_| Not),
            map(tag("STORE"), |_| Store),
            map(tag("SUB"), |_| Sub),
            map(tag("XOR"), |_| Xor),
        )),
        alt((
            value(Capture, tag("CAPTURE")),
            value(DefCal, tag("DEFCAL")),
            value(DefFrame, tag("DEFFRAME")),
            value(DefWaveform, tag("DEFWAVEFORM")),
            value(Delay, tag("DELAY")),
            value(Fence, tag("FENCE")),
            value(Pulse, tag("PULSE")),
            value(RawCapture, tag("RAW-CAPTURE")),
            value(SetFrequency, tag("SET-FREQUENCY")),
            value(SetPhase, tag("SET-PHASE")),
            value(SetScale, tag("SET-SCALE")),
            value(ShiftFrequency, tag("SHIFT-FREQUENCY")),
            value(ShiftPhase, tag("SHIFT-PHASE")),
        )),
    ))(input)?;
    Ok((result.0, Token::Command(result.1)))
}

fn is_valid_identifier_character(chr: char) -> bool {
    is_valid_identifier_leading_character(chr) || chr.is_ascii_digit() || chr == '\\' || chr == '-'
}

fn is_valid_identifier_leading_character(chr: char) -> bool {
    chr.is_ascii_alphabetic() || chr == '_'
}

fn lex_identifier_raw(input: &str) -> IResult<&str, String> {
    map(
        tuple((
            take_while1(is_valid_identifier_leading_character),
            take_while(is_valid_identifier_character),
        )),
        |result| [result.0, result.1].concat(),
    )(input)
}

fn lex_identifier(input: &str) -> LexResult {
    lex_identifier_raw(input).map(|(input, ident)| (input, Token::Identifier(ident)))
}

fn lex_label(input: &str) -> LexResult {
    let (input, _) = tag("@")(input)?;
    let (input, label) = lex_identifier_raw(input)?;
    Ok((input, Token::Label(label)))
}

fn lex_non_blocking(input: &str) -> LexResult {
    value(Token::NonBlocking, tag("NONBLOCKING"))(input)
}

fn lex_number(input: &str) -> LexResult {
    let (input, float_string): (&str, &str) = recognize(double)(input)?;
    let integer_parse_result: IResult<&str, _> = all_consuming(digit1)(float_string);
    Ok((
        input,
        match integer_parse_result {
            Ok(_) => Token::Integer(float_string.parse::<usize>().unwrap()),
            Err(_) => Token::Float(double(float_string)?.1 as f64),
        },
    ))
}

fn lex_modifier(input: &str) -> LexResult {
    alt((
        value(Token::As, tag("AS")),
        value(Token::Matrix, tag("MATRIX")),
        value(Token::Modifier(Modifier::Controlled), tag("CONTROLLED")),
        value(Token::Modifier(Modifier::Dagger), tag("DAGGER")),
        value(Token::Modifier(Modifier::Forked), tag("FORKED")),
        value(Token::Permutation, tag("PERMUTATION")),
        value(Token::Sharing, tag("SHARING")),
    ))(input)
}

fn lex_operator(input: &str) -> LexResult {
    use Operator::*;
    map(
        alt((
            value(Caret, tag("^")),
            value(Minus, tag("-")),
            value(Plus, tag("+")),
            value(Slash, tag("/")),
            value(Star, tag("*")),
        )),
        Token::Operator,
    )(input)
}

fn lex_punctuation(input: &str) -> LexResult {
    use Token::*;
    alt((
        value(Colon, tag(":")),
        value(Comma, tag(",")),
        value(Indentation, alt((tag("    "), tag("\t")))),
        value(LBracket, tag("[")),
        value(LParenthesis, tag("(")),
        value(NewLine, alt((is_a("\n"), is_a("\r\n")))),
        value(RBracket, tag("]")),
        value(RParenthesis, tag(")")),
        value(Semicolon, tag(";")),
    ))(input)
}

fn lex_string(input: &str) -> LexResult {
    map(
        delimited(tag("\""), take_until("\""), tag("\"")),
        |v: &str| Token::String(v.to_owned()),
    )(input)
}

fn lex_variable(input: &str) -> LexResult {
    map(preceded(tag("%"), lex_identifier_raw), |ident| {
        Token::Variable(ident)
    })(input)
}

#[cfg(test)]
mod tests {
    use super::{lex, Command, Operator, Token};

    #[test]
    fn comment() {
        let input = "# hello\n#world";
        let tokens = lex(input);
        assert_eq!(
            tokens,
            vec![
                Token::Comment(" hello".to_owned()),
                Token::NewLine,
                Token::Comment("world".to_owned())
            ]
        )
    }

    #[test]
    fn keywords() {
        let input = "DEFGATE DEFCIRCUIT JUMP-WHEN MATRIX";
        let tokens = lex(input);
        assert_eq!(
            tokens,
            vec![
                Token::Command(Command::DefGate),
                Token::Command(Command::DefCircuit),
                Token::Command(Command::JumpWhen),
                Token::Matrix,
            ]
        )
    }

    #[test]
    fn number() {
        let input = "2 2i 2.0 2e3 2.0e3 (1+2i)";
        let tokens = lex(input);
        assert_eq!(
            tokens,
            vec![
                Token::Integer(2),
                Token::Integer(2),
                Token::Identifier("i".to_owned()),
                Token::Float(2.0),
                Token::Float(2000f64),
                Token::Float(2000f64),
                Token::LParenthesis,
                Token::Integer(1),
                Token::Operator(Operator::Plus),
                Token::Integer(2),
                Token::Identifier("i".to_owned()),
                Token::RParenthesis
            ]
        )
    }

    #[test]
    fn string() {
        let input = "\"hello\"\n\"world\"";
        let tokens = lex(input);
        assert_eq!(
            tokens,
            vec![
                Token::String("hello".to_owned()),
                Token::NewLine,
                Token::String("world".to_owned())
            ]
        )
    }

    #[test]
    fn gate_operation() {
        let input = "I 0; RX 1\nCZ 0 1";
        let tokens = lex(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("I".to_owned()),
                Token::Integer(0),
                Token::Semicolon,
                Token::Identifier("RX".to_owned()),
                Token::Integer(1),
                Token::NewLine,
                Token::Identifier("CZ".to_owned()),
                Token::Integer(0),
                Token::Integer(1),
            ]
        )
    }

    #[test]
    fn label() {
        let input = "@hello\n@world";
        let tokens = lex(input);
        assert_eq!(
            tokens,
            vec![
                Token::Label("hello".to_owned()),
                Token::NewLine,
                Token::Label("world".to_owned())
            ]
        )
    }

    #[test]
    fn indentation() {
        let input = "    ";
        let tokens = lex(input);
        assert_eq!(tokens, vec![Token::Indentation,])
    }

    #[test]
    fn indented_block() {
        let input = "DEFGATE Name AS PERMUTATION:\n\t1,0\n    0,1";
        let tokens = lex(input);
        assert_eq!(
            tokens,
            vec![
                Token::Command(Command::DefGate),
                Token::Identifier("Name".to_owned()),
                Token::As,
                Token::Permutation,
                Token::Colon,
                Token::NewLine,
                Token::Indentation,
                Token::Integer(1),
                Token::Comma,
                Token::Integer(0),
                Token::NewLine,
                Token::Indentation,
                Token::Integer(0),
                Token::Comma,
                Token::Integer(1),
            ]
        )
    }

    #[test]
    fn surrounding_whitespace() {
        let input = "\nI 0\n    \n";
        let tokens = lex(input);
        assert_eq!(
            tokens,
            vec![
                Token::NewLine,
                Token::Identifier("I".to_owned()),
                Token::Integer(0),
                Token::NewLine,
                Token::Indentation,
                Token::NewLine
            ]
        )
    }

    /// Test that an entire sample program can be lexed without failure.
    #[test]
    fn whole_program() {
        let input = "DECLARE ro BIT[1]
      DEFGATE HADAMARD AS MATRIX:
      \t(1/sqrt(2)),(1/sqrt(2))
      \t(1/sqrt(2)),((-1)/sqrt(2))
      
      DEFGATE RX(%theta) AS MATRIX:
      \tcos((%theta/2)),((-1i)*sin((%theta/2)))
      \t((-1i)*sin((%theta/2))),cos((%theta/2))
      
      DEFGATE Name AS PERMUTATION:
      \t1,0
      \t0,1
      
      DEFCIRCUIT SIMPLE:
      \tX 0
      \tX 1
      
      RX 0
      CZ 0 1
      MEASURE 0 ro[0]
      RESET 0
      RESET
      CAPTURE 0 \"out\" my_waveform() iq[0]
      DEFCAL X 0:
      \tPULSE 0 \"xy\" my_waveform()
      
      DEFCAL RX(%theta) 0:
      \tPULSE 0 \"xy\" my_waveform()
      
      DEFCAL MEASURE 0 %dest:
      \tDECLARE iq REAL[2]
      \tCAPTURE 0 \"out\" flat(duration: 1000000, iqs: (2+3i)) iq[0]
      
      DEFFRAME 0 \"xy\":
      \tSAMPLE-RATE: 3000
      
      DEFFRAME 0 \"xy\":
      \tDIRECTION: \"rx\"
      \tCENTER-FREQUENCY: 1000
      \tHARDWARE-OBJECT: \"some object\"
      \tINITIAL-FREQUENCY: 2000
      \tSAMPLE-RATE: 3000
      
      DELAY 0 100
      DELAY 0 \"xy\" 100000000
      FENCE 0
      FENCE 0 1
      PULSE 0 \"xy\" my_waveform()
      PULSE 0 1 \"cz\" my_parametrized_waveform(a: 1)
      RAW-CAPTURE 0 \"out\" 200000000 iqs[0]
      SET-FREQUENCY 0 \"xy\" 5400000000
      SET-PHASE 0 \"xy\" pi
      SET-SCALE 0 \"xy\" pi
      SHIFT-FREQUENCY 0 \"ro\" 6100000000
      SHIFT-PHASE 0 \"xy\" (-pi)
      SHIFT-PHASE 0 \"xy\" (%theta*(2/pi))
      SWAP-PHASES 2 3 \"xy\" 3 4 \"xy\"";

        lex(input);
    }
}
