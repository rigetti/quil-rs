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

pub(crate) use expression::parse_expression;
pub(crate) use instruction::parse_instructions;
pub(crate) use lexer::lex;

mod command;
mod gate;
mod macros;

pub(crate) mod common;
mod error;
mod expression;
pub(crate) mod instruction;
mod lexer;
mod token;

pub(crate) use error::{ErrorInput, InternalParseError};
pub use error::{ParseError, ParserErrorKind};
pub use lexer::{LexError, LexErrorKind};
pub use token::{Token, TokenWithLocation};

type ParserInput<'a> = &'a [TokenWithLocation<'a>];
type InternalParserResult<'a, R, E = InternalParseError<'a>> = IResult<ParserInput<'a>, R, E>;

/// Pops the first token off of the `input` and returns it and the remaining input.
///
/// This also converts the first item from [`TokenWithLocation`] to [`Token`], which makes match
/// statements more straightforward.
pub(crate) fn split_first_token(input: ParserInput) -> Option<(&Token, ParserInput)> {
    input
        .split_first()
        .map(|(first, rest)| (first.as_token(), rest))
}

/// Returns the first token of the input as [`Token`] instead of [`TokenWithLocation`].
pub(crate) fn first_token(input: ParserInput) -> Option<&Token> {
    input.first().map(TokenWithLocation::as_token)
}

/// Extracts the actual error from [`nom::Err`].
///
/// Instead of using this with [`Result::map_err`], use [`nom::Finish::finish`].
///
/// # Panics
///
/// Will panic if the error is [`nom::Err::Incomplete`]. This only happens for streaming parsers,
/// which we do not use as of 2022-09-14.
pub(crate) fn extract_nom_err<E>(err: nom::Err<E>) -> E {
    // If this ever panics, switch to returning an Option
    match err {
        nom::Err::Incomplete(_) => {
            unreachable!("can't be incomplete if all parsers are complete variants")
        }
        nom::Err::Error(inner) => inner,
        nom::Err::Failure(inner) => inner,
    }
}
