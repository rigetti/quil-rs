//! Types and functions related to validating Quil identifiers
use std::str::FromStr;

use once_cell::sync::Lazy;
use regex::Regex;
use thiserror;

use crate::reserved::ReservedToken;

#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum IdentifierValidationError {
    #[error("{0} is a reserved token")]
    Reserved(ReservedToken),

    #[error("{0} is not a valid identifier")]
    Invalid(String),
}

/// A regex that matches only valid Quil identifiers
const IDENTIFIER_REGEX_STRING: &str = r"^([A-Za-z_]|[A-Za-z_][A-Za-z0-9\-_]*[A-Za-z0-9_])$";

static IDENTIFIER_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(IDENTIFIER_REGEX_STRING).expect("regex should be valid"));

/// Returns an error if the given identifier is not a valid Quil Identifier
pub fn validate_identifier(ident: &str) -> Result<(), IdentifierValidationError> {
    match IDENTIFIER_REGEX.is_match(ident) {
        true => Ok(()),
        false => Err(IdentifierValidationError::Invalid(ident.to_string())),
    }
}

/// Returns an error if the given identifier is reserved, or if it is not a valid Quil identifier
pub fn validate_user_identifier(ident: &str) -> Result<(), IdentifierValidationError> {
    ReservedToken::from_str(ident).map_or(validate_identifier(ident), |t| {
        Err(IdentifierValidationError::Reserved(t))
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("Good_Ident1f1er-0", true)]
    #[case("H", true)]
    #[case("-Cant-start-with-dash", false)]
    #[case("Cant-end-with-dash-", false)]
    #[case("1-Cant-start-with-number", false)]
    fn test_validate_identifier(#[case] input: &str, #[case] ok: bool) {
        assert_eq!(validate_identifier(input).is_ok(), ok)
    }

    #[rstest]
    #[case("Good_Ident1f1er-0", true)]
    #[case("DEFGATE", false)]
    #[case("AS", false)]
    #[case("pi", false)]
    #[case("PAULI-SUM", false)]
    #[case("H", false)]
    #[case("G", true)]
    fn test_validate_user_identifier(#[case] input: &str, #[case] ok: bool) {
        assert_eq!(validate_user_identifier(input).is_ok(), ok)
    }
}
