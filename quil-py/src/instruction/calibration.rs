use quil_rs::{
    expression::Expression,
    instruction::{Calibration, GateModifier, Instruction, MeasureCalibrationDefinition, Qubit},
};

use rigetti_pyo3::{
    impl_repr, impl_str, py_wrap_data_struct,
    pyo3::{types::PyString, Py},
};

use crate::instruction::{PyExpression, PyGateModifier, PyInstruction, PyQubit};

py_wrap_data_struct! {
    PyCalibration(Calibration) as "Calibration" {
        instructions: Vec<Instruction> => Vec<PyInstruction>,
        modifiers: Vec<GateModifier> => Vec<PyGateModifier>,
        name: String => Py<PyString>,
        parameters: Vec<Expression> => Vec<PyExpression>,
        qubits: Vec<Qubit> => Vec<PyQubit>
    }
}
impl_repr!(PyCalibration);
impl_str!(PyCalibration);

py_wrap_data_struct! {
    PyMeasureCalibrationDefinition(MeasureCalibrationDefinition) as "MeasureCalibrationDefinition" {
        qubit: Option<Qubit> => Option<PyQubit>,
        parameter: String => Py<PyString>,
        instructions: Vec<Instruction> => Vec<PyInstruction>
    }
}
impl_repr!(PyMeasureCalibrationDefinition);
impl_str!(PyMeasureCalibrationDefinition);
