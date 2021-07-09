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
mod command;
mod gate;
mod macros;

mod common;
mod error;
mod expression;
mod instruction;
mod lexer;

pub(crate) use instruction::parse_instructions;
pub(crate) use lexer::lex;

use nom::IResult;

use error::Error;
use lexer::Token;

type ParserInput<'a> = &'a [Token];
type ParserResult<'a, R> = IResult<&'a [Token], R, Error<&'a [Token]>>;
