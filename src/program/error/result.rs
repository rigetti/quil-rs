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
        Err(ProgramError::Syntax(err)) => err.recover().map_err(ProgramError::from),
        Err(err) => Err(err),
    }
}
