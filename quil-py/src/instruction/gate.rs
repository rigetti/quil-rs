use pyo3::types::PyString;
use quil_rs::instruction::{Gate, GateModifier};

use rigetti_pyo3::{py_wrap_data_struct, py_wrap_type, py_wrap_union_enum};

use crate::instruction::{
    expression::{Expressions, PyExpressions},
    qubit::{PyQubits, Qubits},
};

py_wrap_union_enum! {
    PyGateModifier(GateModifier) as "GateModifier" {
        controlled: Controlled,
        dagger: Dagger,
        forked: Forked
    }
}

pub type GateModifiers = Vec<GateModifier>;
py_wrap_type! {
    #[derive(Debug)]
    PyGateModifiers(GateModifiers) as "GateModifiers";
}

py_wrap_data_struct! {
    PyGate(Gate) as "Gate" {
        name: String => PyString,
        parameters: Expressions => PyExpressions,
        qubits: Qubits => PyQubits,
        modifiers: GateModifiers => PyGateModifiers
    }
}
