// Copyright 2022 Rigetti Computing
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

use super::{LexErrorKind, LexInput, LexResult};

use crate::parser::lexer::{InternalLexError, InternalLexResult};
use nom::branch::{alt as nom_alt, Alt};
use nom::bytes::complete::tag as nom_tag;
use nom::error::ParseError;
use nom::Parser;
use std::fmt;

/// Returns a parser that runs the given one and converts its returned error using `mapper`.
pub(crate) fn map_err<'a, P, F, O, E1, E2>(
    mut parser: P,
    mapper: F,
) -> impl FnMut(LexInput<'a>) -> LexResult<'a, O, E2>
where
    P: Parser<LexInput<'a>, O, E1>,
    O: fmt::Debug,
    F: Fn(E1) -> E2,
{
    move |input| match parser.parse(input) {
        Ok(result) => Ok(result),
        Err(err) => match err {
            nom::Err::Incomplete(needed) => Err(nom::Err::Incomplete(needed)),
            nom::Err::Error(err) => Err(nom::Err::Error(mapper(err))),
            nom::Err::Failure(failure) => Err(nom::Err::Failure(mapper(failure))),
        },
    }
}

/// Returns a lexing parser that runs the given one and replaces its error with [`LexErrorKind::ExpectedContext`] with the given string.
pub(crate) fn expecting<'a, O, E, P>(
    context: &'static str,
    mut parser: P,
) -> impl FnMut(LexInput<'a>) -> InternalLexResult<'a, O>
where
    P: Parser<LexInput<'a>, O, E>,
    O: fmt::Debug,
{
    move |input| {
        parser.parse(input).map_err(|err| {
            let new_err =
                InternalLexError::from_kind(input, LexErrorKind::ExpectedContext(context));
            match err {
                nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
                nom::Err::Error(_) => nom::Err::Error(new_err),
                nom::Err::Failure(_) => nom::Err::Failure(new_err),
            }
        })
    }
}

/// A wrapper for [`nom::branch::alt`] that returns [`LexErrorKind::ExpectedContext`] on error.
///
/// This works around the fact that [`nom::branch::alt`] does not return any errors from any of the
/// alternatives, and is more user-friendly than returning all of those errors anyways.
pub(crate) fn alt<'a, O, E, List>(
    context: &'static str,
    alts: List,
) -> impl FnMut(LexInput<'a>) -> InternalLexResult<'a, O>
where
    E: ParseError<LexInput<'a>>,
    List: Alt<LexInput<'a>, O, E>,
    O: fmt::Debug,
{
    let parser = nom_alt::<_, _, E, List>(alts);
    expecting(context, parser)
}

/// A wrapper for [`nom::bytes::complete::tag`] that replaces the error with one that indicates
/// what tag string was expected.
pub(crate) fn tag<'a>(
    lit: &'static str,
) -> impl FnMut(LexInput<'a>) -> InternalLexResult<LexInput<'a>> {
    move |input| {
        map_err(nom_tag(lit), |err: nom::error::Error<LexInput<'a>>| {
            InternalLexError::from_kind(err.input, LexErrorKind::ExpectedString(lit))
        })(input)
    }
}
