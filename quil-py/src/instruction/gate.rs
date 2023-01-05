use pyo3::{types::PyString, Py};
use quil_rs::{instruction::{Gate, GateModifier, Qubit}, expression::Expression};

use rigetti_pyo3::{py_wrap_data_struct, py_wrap_union_enum};

use crate::instruction::{
    expression::PyExpression,
    qubit::PyQubit,
};

py_wrap_union_enum! {
    PyGateModifier(GateModifier) as "GateModifier" {
        controlled: Controlled,
        dagger: Dagger,
        forked: Forked
    }
}

py_wrap_data_struct! {
    PyGate(Gate) as "Gate" {
        name: String => Py<PyString>,
        parameters: Vec<Expression> => Vec<PyExpression>,
        qubits: Vec<Qubit> => Vec<PyQubit>,
        modifiers: Vec<GateModifier> => Vec<PyGateModifier>
    }
}
