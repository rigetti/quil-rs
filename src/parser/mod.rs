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

pub mod common;
pub mod error;
pub mod expression;
pub mod instruction;
pub mod lexer;

use nom::IResult;

use error::Error;
use lexer::Token;

pub type ParserInput<'a> = &'a [Token];
pub type ParserResult<'a, R> = IResult<&'a [Token], R, Error<&'a [Token]>>;
