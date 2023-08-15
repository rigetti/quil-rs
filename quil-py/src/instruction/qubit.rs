use quil_rs::instruction::Qubit;

use rigetti_pyo3::{
    impl_hash, impl_repr, py_wrap_union_enum,
    pyo3::{
        pyclass::CompareOp,
        pymethods,
        types::{PyLong, PyString},
        IntoPy, Py, PyObject, Python,
    },
    PyWrapper,
};

use crate::impl_quil;

py_wrap_union_enum! {
    #[derive(Debug, Eq, Hash, PartialEq)]
    PyQubit(Qubit) as "Qubit" {
        fixed: Fixed => Py<PyLong>,
        variable: Variable => Py<PyString>
    }
}
impl_repr!(PyQubit);
impl_quil!(PyQubit);
impl_hash!(PyQubit);

#[pymethods]
impl PyQubit {
    pub fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self.as_inner() == other.as_inner()).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}
