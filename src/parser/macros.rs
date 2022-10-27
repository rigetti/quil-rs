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

#[macro_export]
macro_rules! expected_token {
    ($input: expr, $actual:expr, $expected:expr) => {{
        use $crate::parser::error::{InternalError, ParserErrorKind};
        Err(nom::Err::Error(InternalError::from_kind(
            $input,
            ParserErrorKind::ExpectedToken {
                actual: $actual.clone(),
                expected: $expected,
            },
        )))
    }};
}

#[macro_export]
macro_rules! token {
    ($expected_variant: ident($enm:ident::$variant:ident)) => {{
        use $crate::parser::error::{InternalError, ParserErrorKind};
        use $crate::parser::lexer::$enm;
        use $crate::parser::lexer::Token;
        move |input: ParserInput<'a>| match $crate::parser::split_first_token(input) {
            None => Err(nom::Err::Error(InternalError::from_kind(
                input,
                ParserErrorKind::UnexpectedEOF("something else"),
            ))),
            Some((Token::$expected_variant($enm::$variant), remainder)) => Ok((remainder, ())),
            Some((other_token, _)) => {
                $crate::expected_token!(
                    input,
                    other_token,
                    stringify!($expected_variant).to_owned()
                )
            }
        }
    }};
    ($expected_variant: ident($contents: ident)) => {{
        use $crate::parser::error::{InternalError, ParserErrorKind};
        use $crate::parser::lexer::Token;
        move |input: ParserInput<'a>| match $crate::parser::split_first_token(input) {
            None => Err(nom::Err::Error(InternalError::from_kind(
                input,
                ParserErrorKind::UnexpectedEOF("something else"),
            ))),
            Some((Token::$expected_variant($contents), remainder)) => {
                Ok((remainder, $contents.clone()))
            }
            Some((other_token, _)) => {
                $crate::expected_token!(
                    input,
                    other_token,
                    stringify!($expected_variant).to_owned()
                )
            }
        }
    }};
    ($expected_variant: ident) => {{
        use $crate::parser::error::{InternalError, ParserErrorKind};
        use $crate::parser::lexer::Token;
        move |input: ParserInput<'a>| match $crate::parser::split_first_token(input) {
            None => Err(nom::Err::Error(InternalError::from_kind(
                input,
                ParserErrorKind::UnexpectedEOF("something else"),
            ))),
            Some((Token::$expected_variant, remainder)) => Ok((remainder, ())),
            Some((other_token, _)) => {
                $crate::expected_token!(
                    input,
                    other_token,
                    stringify!($expected_variant).to_owned()
                )
            }
        }
    }};
}

#[macro_export]
macro_rules! unexpected_eof {
    ($input: expr) => {{
        use $crate::parser::error::{InternalError, ParserErrorKind};
        Err(nom::Err::Error(InternalError::from_kind(
            $input,
            ParserErrorKind::UnexpectedEOF("something else"),
        )))
    }};
}

#[macro_export]
macro_rules! make_test {
    ($name: ident, $parser: ident, $input: expr, $expected: expr) => {
        #[test]
        fn $name() {
            let input = ::nom_locate::LocatedSpan::new($input);
            let tokens = lex(input).unwrap();
            let (remainder, parsed) = $parser(&tokens).unwrap();
            assert_eq!(remainder.len(), 0, "tokens left over");
            assert_eq!(parsed, $expected);
        }
    };
}
