use nom::Finish;
use crate::parser::{ErrorInput, ParseError};
use crate::program::error::LeftoverError;
use crate::program::ProgramError;

pub fn disallow_leftover<I, O>(result: nom::IResult<I, O, ParseError>) -> Result<O, ProgramError<O>>
    where I: ErrorInput,
{
    match result.finish() {
        Ok((leftover, parsed)) => if leftover.is_empty() {
            Ok(parsed)
        } else {
            Err(ProgramError::from(LeftoverError::new(leftover, parsed)))
        },
        Err(err) => Err(ProgramError::from(err))
    }
}

pub fn map_parsed<O, O2>(result: Result<O, ProgramError<O>>, map: impl Fn(O) -> O2) -> Result<O2, ProgramError<O2>> {
    match result {
        Ok(parsed) => Ok(map(parsed)),
        Err(err) => Err(err.map_parsed(map))
    }
}

pub fn recover<O>(result: Result<O, ProgramError<O>>) -> Result<O, ProgramError<O>> {
    match result {
        Ok(parsed) => Ok(parsed),
        Err(ProgramError::Leftover(err)) => Ok(err.recover()),
        Err(err) => Err(err),
    }
}

pub fn convert_leftover<O, O2>(result: Result<O, ProgramError<O>>) -> Result<O, ProgramError<O2>> {
    match result {
        Ok(parsed) => Ok(parsed),
        Err(err) => match err {
            ProgramError::InvalidCalibration { instruction, message } => Err(ProgramError::InvalidCalibration { instruction, message }),
            ProgramError::RecursiveCalibration(inst) => Err(ProgramError::RecursiveCalibration(inst)),
            ProgramError::LexError(err) => Err(ProgramError::LexError(err)),
            ProgramError::ParsingError(err) => Err(ProgramError::ParsingError(err)),
            ProgramError::Leftover(err) => panic!("expected no LeftoverError, got {}", err),
        }
    }
}