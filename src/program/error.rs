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

use std::error::Error;
use std::fmt;
use std::fmt::Formatter;
use thiserror::Error;

use crate::instruction::Instruction;
use crate::parser::{LexError, ParseError, Token, TokenWithLocation};
use crate::Program;

#[derive(Debug)]
pub enum ProgramError {
    InvalidCalibration {
        instruction: Instruction,
        message: String,
    },
    RecursiveCalibration(Instruction),
    LexError(LexError<Vec<TokenWithLocation>>),
    ParsingError(ParseError<Program>),
}

impl From<LexError<Vec<TokenWithLocation>>> for ProgramError {
    fn from(e: LexError<Vec<TokenWithLocation>>) -> Self {
        Self::LexError(e)
    }
}

impl From<ParseError<Program>> for ProgramError {
    fn from(e: ParseError<Program>) -> Self {
        Self::ParsingError(e)
    }
}

impl fmt::Display for ProgramError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidCalibration { instruction, message } => write!(f, "invalid calibration `{}`: {}", instruction, message),
            Self::RecursiveCalibration(instruction) => write!(f, "instruction {} expands into itself", instruction),
            Self::LexError(error) => if f.alternate() {
                write!(f, "error while lexing: {:#}", error)
            } else {
                write!(f, "error while lexing: {}", error)
            },
            Self::ParsingError(error) => if f.alternate() {
                write!(f, "error while parsing: {:#}", error)
            } else {
                write!(f, "error while parsing: {}", error)
            },
        }
    }
}

impl std::error::Error for ProgramError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::InvalidCalibration { .. } => None,
            Self::RecursiveCalibration(_) => None,
            Self::LexError(err) => Some(err),
            Self::ParsingError(err) => Some(err),
        }
    }
}

pub type ProgramResult<T> = Result<T, ProgramError>;
