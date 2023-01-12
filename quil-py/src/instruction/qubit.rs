use pyo3::{
    types::{PyLong, PyString},
    Py,
};
use quil_rs::instruction::Qubit;
use rigetti_pyo3::py_wrap_union_enum;

py_wrap_union_enum! {
    #[derive(Eq, Hash, PartialEq)]
    PyQubit(Qubit) as "Qubit" {
        fixed: Fixed => Py<PyLong>,
        variable: Variable => Py<PyString>
    }
}
