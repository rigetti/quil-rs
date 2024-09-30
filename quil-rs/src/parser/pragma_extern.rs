use nom::{
    branch::alt,
    combinator::{map, opt},
    multi::separated_list0,
};

use crate::{
    instruction::{ExternParameter, ExternParameterType, ExternSignature, ScalarType},
    token,
};

use super::{
    common::{match_data_type_token, parse_vector_with_brackets},
    InternalParserResult, ParserInput,
};

/// Parse an [`ExternSignature`] from a string. Note, externs are currently defined within a
/// [`crate::instruction::Pragma`] instruction as `PRAGMA EXTERN foo "signature"`; the "signature"
/// currently represents its own mini-language within the Quil specification.
///
/// Signatures are of the form:
///     `@rep[:min 0 :max 1]{@ms{Base Type}} ( @ms{Extern Parameter} @rep[:min 0]{@group{ , @ms{Extern Parameter} }} )`
///
/// For details on the signature format, see the [Quil specification for "Extern Signature"](https://github.com/quil-lang/quil/blob/7f532c7cdde9f51eae6abe7408cc868fba9f91f6/specgen/spec/sec-other.s).
///
/// Note, there are test cases for this parser in [`crate::instruction::extern_call::tests`] (via
/// [`std::str::FromStr`] for [`crate::instruction::ExternSignature`]).
pub(crate) fn parse_extern_signature<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, ExternSignature> {
    let (input, return_type) = opt(token!(DataType(v)))(input)?;
    let (input, lparen) = opt(token!(LParenthesis))(input)?;
    let (input, parameters) = if lparen.is_some() {
        let (input, parameters) =
            opt(separated_list0(token!(Comma), parse_extern_parameter))(input)?;
        let (input, _) = token!(RParenthesis)(input)?;
        (input, parameters.unwrap_or_default())
    } else {
        (input, vec![])
    };

    let signature = ExternSignature {
        return_type: return_type.map(match_data_type_token),
        parameters,
    };

    Ok((input, signature))
}

fn parse_extern_parameter<'a>(input: ParserInput<'a>) -> InternalParserResult<'a, ExternParameter> {
    let (input, name) = token!(Identifier(v))(input)?;
    let (input, _) = token!(Colon)(input)?;
    let (input, mutable) = opt(token!(Mutable))(input)?;
    let (input, data_type) = alt((
        map(
            parse_vector_with_brackets,
            ExternParameterType::FixedLengthVector,
        ),
        map(
            parse_variable_length_vector,
            ExternParameterType::VariableLengthVector,
        ),
        map(token!(DataType(v)), |data_type| {
            ExternParameterType::Scalar(match_data_type_token(data_type))
        }),
    ))(input)?;
    Ok((
        input,
        ExternParameter {
            name,
            mutable: mutable.is_some(),
            data_type,
        },
    ))
}

/// Parse a variable length [`crate::instruction::Vector`], which is represented as [`ScalarType`]
/// followed by empty brackets `[]`.
fn parse_variable_length_vector<'a>(
    input: ParserInput<'a>,
) -> InternalParserResult<'a, ScalarType> {
    let (input, data_type_token) = token!(DataType(v))(input)?;
    let data_type = match_data_type_token(data_type_token);
    let (input, _) = token!(LBracket)(input)?;
    let (input, _) = token!(RBracket)(input)?;

    Ok((input, data_type))
}
