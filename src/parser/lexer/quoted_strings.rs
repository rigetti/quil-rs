use nom::{combinator::map, Slice};

use super::{InternalLexError, InternalLexResult, LexErrorKind, LexInput};

/// Like [`quoted_string`], but unescapes any escaped backslashes or quotes matching those wrapping
/// the string.
///
/// That is, `"\'a\' \"string\""` will become `\'a\' "string"` and `'\'a\' \"string\"'` will become
/// `'a' \"string\"`.
///
/// # Errors
///
/// See [`surrounded`]
pub(crate) fn unescaped_quoted_string(input: LexInput) -> InternalLexResult<String> {
    map(surrounded('"', '"', true), |parsed| {
        parsed.replace("\\\"", "\"").replace("\\\\", "\\")
    })(input)
}

/// A parser that consumes the surrounding characters and returns the inner string.
///
/// If the string contains an escape character and `allow_escaping` is `false`, an error is
/// returned.
///
/// The input string must start with `first`, or an error is returned.
///
/// ```text
/// assert_eq!(
///     surrounded('"', '"', false)(r#""quoted\" ignored""#),
///     (" ignored", r#""quoted\"#),
/// );
/// assert_eq!(
///     surrounded('"', '"', true)(r#""quoted\" ignored""#),
///     ("", r#""quoted\" ignored"#),
/// );
/// assert_eq!(
///     surrounded('(', ')', false)("(contents)other"),
///     ("other", "contents")
/// );
/// ```
fn surrounded(
    first: char,
    last: char,
    allow_escaping: bool,
) -> impl Fn(LexInput) -> InternalLexResult<LexInput> {
    move |input: LexInput| {
        let mut iter = input.char_indices();
        let first_char = iter.next().ok_or_else(|| {
            nom::Err::Error(InternalLexError::from_kind(
                input,
                LexErrorKind::UnexpectedEOF,
            ))
        })?;
        if first_char != (0, first) {
            return Err(nom::Err::Error(InternalLexError::from_kind(
                input,
                LexErrorKind::ExpectedChar(first),
            )));
        }

        let mut is_escaped: bool = false;

        for (i, c) in iter {
            if c == '\\' {
                // Account for escaped escape char
                is_escaped = !is_escaped;
            } else if is_escaped && allow_escaping {
                is_escaped = false;
            } else if c == last {
                return Ok((input.slice(i + 1..), input.slice(1..i)));
            }
        }

        Err(nom::Err::Error(InternalLexError::from_kind(
            input,
            LexErrorKind::UnexpectedEOF,
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::Finish;
    use nom_locate::LocatedSpan;
    use rstest::rstest;

    #[rstest]
    #[case("\"\"", "", "")]
    #[case("\"foo\"", "foo", "")]
    #[case("\"\\\"foo\\\"\" extra", "\"foo\"", " extra")]
    #[case("\"\\\\\"", "\\", "")]
    #[case("\"foo bar (baz) 123\" after", "foo bar (baz) 123", " after")]
    #[case(r#""{\"name\": \"quoted json\"}""#, r#"{"name": "quoted json"}"#, "")]
    #[case(r#""hello"\n"world""#, "hello", "\\n\"world\"")]
    fn test_string_parser(#[case] input: &str, #[case] output: &str, #[case] leftover: &str) {
        let input = LocatedSpan::new(input);
        let (remaining, parsed) = unescaped_quoted_string(input).finish().unwrap();
        assert_eq!(parsed, output);
        assert_eq!(remaining.fragment(), &leftover);
    }
}
