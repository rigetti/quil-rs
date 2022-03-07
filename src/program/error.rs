use thiserror::Error;

use crate::instruction::Instruction;

#[derive(Debug, Error)]
pub enum ProgramError {
    #[error("invalid calibration `{0}`: {message}")]
    InvalidCalibration { instruction: Instruction, message: String },

    #[error("instruction {0} expands into itself")]
    RecursiveCalibration(Instruction)
}

pub type ProgramResult<T> = Result<T, ProgramError>;