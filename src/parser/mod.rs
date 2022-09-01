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

use nom::IResult;

use error::Error;
pub(crate) use expression::parse_expression;
pub(crate) use instruction::parse_instructions;
pub(crate) use lexer::lex;
use lexer::Token;
use crate::parser::lexer::TokenWithLocation;

mod command;
mod gate;
mod macros;

mod common;
mod error;
mod expression;
pub(crate) mod instruction;
mod lexer;

type ParserInput<'a> = &'a [TokenWithLocation];
type ParserResult<'a, R> = IResult<&'a [TokenWithLocation], R, Error<&'a [TokenWithLocation]>>;

pub(crate) fn split_first_token(input: ParserInput) -> Option<(&Token, &[TokenWithLocation])> {
    input.split_first().map(|(first, rest)| (first.as_token(), rest))
}

pub(crate) fn first_token(input: ParserInput) -> Option<&Token> {
    input.first().map(TokenWithLocation::as_token)
}

pub(crate) fn nom_err_to_string<E: std::error::Error>(err: nom::Err<E>) -> String {
    match &err {
        nom::Err::Incomplete(_) => err.to_string(),
        nom::Err::Error(err) => format!("Parsing error: {}", err),
        nom::Err::Failure(err) => format!("Parsing failure: {}", err),
    }
}