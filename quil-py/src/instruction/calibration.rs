// come back when:
// [X] Instruction is defined
// GateModifier is defined
// Expression is defined
// [X] Qubit is defined
//

use pyo3::{types::PyString, Py};
use quil_rs::{instruction::{Calibration, Instruction, GateModifier, Qubit}, expression::Expression};
use rigetti_pyo3::{impl_repr, py_wrap_data_struct, py_wrap_type};

use crate::instruction::{expression::PyExpression, qubit::PyQubit};

use super::{gate::PyGateModifier, PyInstructions};

py_wrap_data_struct! {
    PyCalibration(Calibration) as "Calibration" {
        instructions: Vec::<Instruction> => PyInstructions,
        modifiers: Vec<GateModifier> => Vec<PyGateModifier>,
        name: String => Py<PyString>,
        parameters: Vec<Expression> => Vec<PyExpression>,
        qubits: Vec<Qubit> => Vec<PyQubit>
    }
}

impl_repr!(PyCalibration);

py_wrap_type! {
    #[derive(Debug)]
    PyCalibrations(Vec<Calibration>) as "Calibrations";
}

impl_repr!(PyCalibrations);
