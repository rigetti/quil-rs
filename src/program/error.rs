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

use thiserror::Error;

use crate::instruction::Instruction;

#[derive(Debug, Error, PartialEq)]
pub enum ProgramError {
    #[error("invalid calibration `{0}`: {message}")]
    InvalidCalibration {
        instruction: Instruction,
        message: String,
    },

    #[error("instruction {0} expands into itself")]
    RecursiveCalibration(Instruction),

    #[error("There was leftover input after lexing: {0}")]
    LeftoverInputAfterLexing(String),

    #[error("Incomplete parse: {0}")]
    IncompleteParse(String),

    #[error("Recoverable error while parsing: {0}")]
    RecoverableParsingError(String),

    #[error("Unrecoverable error while parsing: {0}")]
    UnrecoverableParsingError(String),

    #[error("In instruction {instruction}: undefined memory reference {reference}")]
    UndefinedMemoryReference {
        instruction: Instruction,
        reference: String,
    },

    #[error(
        "In instruction {instruction}: data type mismatch; {dst} is of type {dst_type}, while {src} is of type {src_type}"
    )]
    DataTypeMismatch {
        instruction: Instruction,
        dst: String,
        dst_type: String,
        src: String,
        src_type: String,
    },

    #[error(
        "In instruction {instruction}: required a real value, but {value} has type {data_type}"
    )]
    RealValueRequired {
        instruction: Instruction,
        value: String,
        data_type: String,
    },

    #[error(
        "In instruction {instruction}: {operator} can only work with {correct_type} data, but operand {operand} has type {data_type}"
    )]
    OperatorOperandMismatch {
        instruction: Instruction,
        operator: String,
        correct_type: String,
        operand: String,
        data_type: String,
    },
}

pub type ProgramResult<T> = Result<T, ProgramError>;
