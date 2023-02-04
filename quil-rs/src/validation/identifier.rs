//! Types and functions related to validating Quil identifiers
use std::str::FromStr;

use fancy_regex::Regex;
use thiserror;

use crate::reserved::ReservedToken;

#[derive(Debug, PartialEq, thiserror::Error)]
pub enum IdentifierValidationError {
    #[error("{0} is a reserved token")]
    Reserved(ReservedToken),

    #[error("{0} is not a valid identifier")]
    Invalid(String),
}

/// A regex that matches only valid Quil identifiers
const IDENTIFIER_REGEX: &str = r"^[A-Za-z_]+[A-Za-z0-9\-_]*.*(?<!\-)$";

/// Returns an error if the given identifier is not a valid Quil Identifier
pub fn validate_identifier(ident: &str) -> Result<bool, IdentifierValidationError> {
    let re = Regex::new(IDENTIFIER_REGEX).expect("regex should be valid");

    re.is_match(ident)
        .map_err(|_| IdentifierValidationError::Invalid(ident.to_string()))
}

/// Returns an error if the given identifier is reserved, or if it is not a valid Quil identifier
pub fn validate_user_identifier(ident: &str) -> Result<bool, IdentifierValidationError> {
    ReservedToken::from_str(ident).map_or(validate_identifier(ident), |t| {
        Err(IdentifierValidationError::Reserved(t))
    })
}
