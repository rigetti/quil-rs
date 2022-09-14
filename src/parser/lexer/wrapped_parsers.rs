use std::fmt;
use nom::Parser;
use nom::branch::{alt as nom_alt, Alt};
use nom::bytes::complete::tag as nom_tag;
use nom::error::ParseError;
use crate::parser::error::ErrorKind;
use super::{LexError, LexErrorKind, LexInput, LexResult};

pub(crate) fn map_err<'a, P, F, O, E1, E2>(mut parser: P, mapper: F) -> impl FnMut(LexInput<'a>) -> LexResult<'a, O, E2>
    where P: Parser<LexInput<'a>, O, E1>,
          O: fmt::Debug,
          F: Fn(E1) -> E2,
{
    move |input| {
        match parser.parse(input) {
            Ok(result) => Ok(result),
            Err(err) => match err {
                nom::Err::Incomplete(needed) => Err(nom::Err::Incomplete(needed)),
                nom::Err::Error(err) => Err(nom::Err::Error(mapper(err))),
                nom::Err::Failure(failure) => Err(nom::Err::Failure(mapper(failure))),
            }
        }
    }
}

pub(crate) fn expecting<'a, O, E, P>(context: &'static str, mut parser: P) -> impl FnMut(LexInput<'a>) -> LexResult<'a, O>
    where P: Parser<LexInput<'a>, O, E>,
          O: fmt::Debug,
{
    move |input| {
        parser.parse(input).map_err(|err| {
            let new_err = LexError::from_other(input, LexErrorKind::ExpectedContext(context));
            match err {
                nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
                nom::Err::Error(_) => nom::Err::Error(new_err),
                nom::Err::Failure(_) => nom::Err::Failure(new_err),
            }
        })
    }
}

pub(crate) fn alt<'a, O, E, List>(context: &'static str, alts: List) -> impl FnMut(LexInput<'a>) -> LexResult<'a, O>
    where E: ParseError<LexInput<'a>>,
          List: Alt<LexInput<'a>, O, E>,
          O: fmt::Debug,
{
    let parser = nom_alt::<_, _, E, List>(alts);
    expecting(context, parser)
}

pub(crate) fn tag(lit: &'static str) -> impl FnMut(LexInput) -> LexResult<LexInput> {
    move |input| {
        map_err(
            nom_tag(lit),
            |err| LexError::from_nom_err_with_kind(err, ErrorKind::Other(LexErrorKind::ExpectedString(lit)))
        )(input)
    }
}