// come back when:
// [X] Instruction is defined
// GateModifier is defined
// Expression is defined
// [X] Qubit is defined
//

use pyo3::types::PyString;
use quil_rs::instruction::{Calibration, Instruction};
use rigetti_pyo3::{impl_repr, py_wrap_data_struct, py_wrap_type};

use crate::instruction::{expression::Expressions, gate::GateModifiers, qubit::Qubits};

use super::{expression::PyExpressions, gate::PyGateModifiers, qubit::PyQubits, PyInstructions};

py_wrap_data_struct! {
    PyCalibration(Calibration) as "Calibration" {
        instructions: Vec::<Instruction> => PyInstructions,
        modifiers: GateModifiers => PyGateModifiers,
        name: String => PyString,
        parameters: Expressions => PyExpressions,
        qubits: Qubits => PyQubits
    }
}

impl_repr!(PyCalibration);

py_wrap_type! {
    #[derive(Debug)]
    PyCalibrations(Vec<Calibration>) as "Calibrations";
}

impl_repr!(PyCalibrations);
