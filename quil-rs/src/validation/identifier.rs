use std::str::FromStr;

use fancy_regex::Regex;
use thiserror;

use crate::reserved::ReservedToken;

#[derive(Debug, thiserror::Error)]
pub enum IdentifierValidationError {
    #[error("{0} is a reserved token")]
    Reserved(ReservedToken),

    #[error("{0} is not a valid identifier")]
    Invalid(String),
}

const IDENTIFIER_REGEX: &str = "";

pub fn validate_identifier(ident: &str) -> Result<bool, IdentifierValidationError> {
    let re = Regex::new(IDENTIFIER_REGEX).expect("regex should be valid");

    re.is_match(ident)
        .map_err(|_| IdentifierValidationError::Invalid(ident.to_string()))
}

pub fn validate_user_identifier(ident: &str) -> Result<bool, IdentifierValidationError> {
    ReservedToken::from_str(ident).map_or(validate_identifier(ident), |t| {
        Err(IdentifierValidationError::Reserved(t))
    })
}
