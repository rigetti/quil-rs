use quil_rs::instruction::{Qubit, QubitPlaceholder};

use rigetti_pyo3::{
    impl_compare, impl_hash, impl_repr, py_wrap_type, py_wrap_union_enum,
    pyo3::{
        pyclass::CompareOp,
        pymethods,
        types::{PyLong, PyString},
        IntoPy, Py, PyObject, Python,
    },
    PyWrapper,
};

use crate::impl_to_quil;

py_wrap_union_enum! {
    #[derive(Debug, Eq, Hash, PartialEq)]
    PyQubit(Qubit) as "Qubit" {
        fixed: Fixed => Py<PyLong>,
        variable: Variable => Py<PyString>,
        placeholder: Placeholder => PyQubitPlaceholder
    }
}
impl_repr!(PyQubit);
impl_to_quil!(PyQubit);
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

py_wrap_type! {
    #[pyo3(subclass)]
    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
    PyQubitPlaceholder(QubitPlaceholder) as "QubitPlaceholder"
}
impl_repr!(PyQubitPlaceholder);
impl_hash!(PyQubitPlaceholder);
impl_compare!(PyQubitPlaceholder);

#[pymethods]
impl PyQubitPlaceholder {
    #[new]
    fn new() -> Self {
        Self(QubitPlaceholder::default())
    }
}
