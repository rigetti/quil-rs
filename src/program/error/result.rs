use crate::parser::{ErrorInput, ParseError};
use crate::program::error::LeftoverError;
use crate::program::ProgramError;
use nom::Finish;

/// If parsing was successful but there is leftover input, returns a [`ProgramError::Leftover`] containing the output.
///
/// See also [`LeftoverError`].
pub fn disallow_leftover<I, O>(result: nom::IResult<I, O, ParseError>) -> Result<O, ProgramError<O>>
where
    I: ErrorInput,
{
    match result.finish() {
        Ok((leftover, parsed)) => {
            if leftover.is_empty() {
                Ok(parsed)
            } else {
                Err(ProgramError::from(LeftoverError::new(leftover, parsed)))
            }
        }
        Err(err) => Err(ProgramError::from(err)),
    }
}

/// Map the parsed output into another value.
///
/// This should be preferred over [`Result::map`], as this will map both the `Ok` case and when
/// there is a [`LeftoverError`].
pub fn map_parsed<O, O2>(
    result: Result<O, ProgramError<O>>,
    map: impl Fn(O) -> O2,
) -> Result<O2, ProgramError<O2>> {
    match result {
        Ok(parsed) => Ok(map(parsed)),
        Err(err) => Err(err.map_parsed(map)),
    }
}

/// If this `result` is `Err(ProgramError::Leftover)`, converts it to `Ok(_)` with the parsed
/// output.
pub fn recover<O>(result: Result<O, ProgramError<O>>) -> Result<O, ProgramError<O>> {
    match result {
        Ok(parsed) => Ok(parsed),
        Err(ProgramError::Leftover(err)) => Ok(err.recover()),
        Err(err) => Err(err),
    }
}

/// Changes the type signature of `result` in cases where the error *is guaranteed not to be*
/// [`ProgramError::Leftover`].
///
/// # Panics
///
/// If the `result` is `Err(ProgramError::Leftover(_))`.
pub fn convert_leftover<O, O2>(result: Result<O, ProgramError<O>>) -> Result<O, ProgramError<O2>> {
    match result {
        Ok(parsed) => Ok(parsed),
        Err(err) => match err {
            ProgramError::InvalidCalibration {
                instruction,
                message,
            } => Err(ProgramError::InvalidCalibration {
                instruction,
                message,
            }),
            ProgramError::RecursiveCalibration(inst) => {
                Err(ProgramError::RecursiveCalibration(inst))
            }
            ProgramError::Syntax(err) => Err(ProgramError::Syntax(err)),
            ProgramError::Leftover(err) => panic!("expected no LeftoverError, got {}", err),
        },
    }
}
