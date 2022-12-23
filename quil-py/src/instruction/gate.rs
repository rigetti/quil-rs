use pyo3::types::PyString;
use quil_rs::instruction::Gate;

use rigetti_pyo3::py_wrap_data_struct;

py_wrap_data_struct! {
    PyGate(Gate) as "Gate" {
        name: String => PyString
        // parameters: Vec<Expression> => PyList
        // qubits: Vec<Qubit> => PyList,
        // modifiers: Vec<GateModifier> => PyList
    }
}
