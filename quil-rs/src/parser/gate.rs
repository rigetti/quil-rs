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

use nom::{combinator::opt, multi::many0, multi::separated_list0, sequence::delimited};

use crate::{instruction::Instruction, token};

use super::{
    common::{self, parse_gate_modifier},
    expression::parse_expression,
    ParserInput,
};
use crate::instruction::Gate;
use crate::parser::InternalParserResult;

/// Parse a gate instruction.
pub(crate) fn parse_gate<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, Instruction> {
    let (input, modifiers) = many0(parse_gate_modifier)(input)?;
    let (input, name) = token!(Identifier(v))(input)?;
    let (input, parameters) = opt(delimited(
        token!(LParenthesis),
        separated_list0(token!(Comma), parse_expression),
        token!(RParenthesis),
    ))(input)?;
    let parameters = parameters.unwrap_or_default();
    let (input, qubits) = many0(common::parse_qubit)(input)?;
    Ok((
        input,
        Instruction::Gate(Gate {
            name,
            parameters,
            qubits,
            modifiers,
        }),
    ))
}

#[cfg(test)]
mod test {
    use super::parse_gate;
    use crate::expression::Expression;
    use crate::instruction::{Gate, GateModifier, Instruction, Qubit};
    use crate::make_test;
    use crate::parser::lexer::lex;

    make_test!(
        test_modifiers,
        parse_gate,
        "DAGGER CONTROLLED RX(pi) 0 1",
        Instruction::Gate(Gate {
            name: "RX".to_string(),
            parameters: vec![Expression::PiConstant],
            qubits: vec![Qubit::Fixed(0), Qubit::Fixed(1)],
            modifiers: vec![GateModifier::Dagger, GateModifier::Controlled],
        })
    );
}
