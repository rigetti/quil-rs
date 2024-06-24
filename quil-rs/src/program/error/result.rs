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

use crate::parser::{ErrorInput, ParseError};
use crate::program::error::LeftoverError;
use crate::program::ParseProgramError;
use nom::Finish;

use super::SyntaxError;

/// If parsing was successful but there is leftover input, returns a [`SyntaxError::Leftover`] containing the output.
///
/// See also [`LeftoverError`].
pub fn disallow_leftover<I, O, E>(result: nom::IResult<I, O, ParseError>) -> Result<O, E>
where
    I: ErrorInput,
    E: From<SyntaxError<O>>,
{
    match result.finish() {
        Ok((leftover, parsed)) => {
            if leftover.is_empty() {
                Ok(parsed)
            } else {
                Err(E::from(SyntaxError::from(LeftoverError::new(
                    leftover, parsed,
                ))))
            }
        }
        Err(err) => Err(E::from(SyntaxError::from(err))),
    }
}

/// Map the parsed output into another value.
///
/// This should be preferred over [`Result::map`], as this will map both the `Ok` case and when
/// there is a [`LeftoverError`].
pub fn map_parsed<O, O2>(
    result: Result<O, ParseProgramError<O>>,
    map: impl Fn(O) -> O2,
) -> Result<O2, ParseProgramError<O2>> {
    match result {
        Ok(parsed) => Ok(map(parsed)),
        Err(err) => Err(err.map_parsed(map)),
    }
}

/// If this `result` is `Err(ProgramParsingError::Leftover)`, converts it to `Ok(_)` with the parsed
/// output.
pub fn recover<O>(result: Result<O, ParseProgramError<O>>) -> Result<O, ParseProgramError<O>> {
    match result {
        Ok(parsed) => Ok(parsed),
        Err(ParseProgramError::Syntax(err)) => err.recover().map_err(ParseProgramError::from),
        Err(err) => Err(err),
    }
}
