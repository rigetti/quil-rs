use pyo3::{
    types::{PyInt, PyString},
    Py,
};
use quil_rs::{
    expression::Expression,
    instruction::{Gate, GateDefinition, GateModifier, GateSpecification, Qubit},
};

use rigetti_pyo3::{impl_repr, impl_str, py_wrap_data_struct, py_wrap_union_enum};

use crate::instruction::{expression::PyExpression, qubit::PyQubit};

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
impl_repr!(PyGate);
impl_str!(PyGate);

/* #[pymethods] */
/* impl PyGate { */
/*     #[new] */
/*     fn new(py: Python<'_>, name, params, qubits) */
/* } */

py_wrap_union_enum! {
    PyGateSpecification(GateSpecification) as "GateSpecification" {
        matrix: Matrix => Vec<Vec<PyExpression>>,
        permutation: Permutation => Vec<Py<PyInt>>
    }
}
impl_repr!(PyGateSpecification);

py_wrap_data_struct! {
    PyGateDefinition(GateDefinition) as "GateDefinition" {
        name: String => Py<PyString>,
        parameters: Vec<String> => Vec<Py<PyString>>,
        specification: GateSpecification => PyGateSpecification
    }
}
impl_repr!(PyGateDefinition);
impl_str!(PyGateDefinition);
