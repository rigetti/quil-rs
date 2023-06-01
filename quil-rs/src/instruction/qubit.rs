use std::fmt;

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Qubit {
    Fixed(u64),
    Variable(String),
}

impl fmt::Display for Qubit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Qubit::*;
        match self {
            Fixed(value) => write!(f, "{value}"),
            Variable(value) => write!(f, "{value}"),
        }
    }
}
