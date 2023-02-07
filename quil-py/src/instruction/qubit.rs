use pyo3::{
    pyclass::CompareOp,
    pymethods,
    types::{PyLong, PyString},
    IntoPy, Py, PyObject, Python,
};
use quil_rs::instruction::Qubit;
use rigetti_pyo3::{impl_hash, py_wrap_union_enum, PyWrapper};

py_wrap_union_enum! {
    #[derive(Eq, Hash, PartialEq)]
    PyQubit(Qubit) as "Qubit" {
        fixed: Fixed => Py<PyLong>,
        variable: Variable => Py<PyString>
    }
}

#[pymethods]
impl PyQubit {
    fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self.as_inner() == other.as_inner()).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}

impl_hash!(PyQubit);
