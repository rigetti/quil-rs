use quil_rs::instruction::{Qubit, QubitPlaceholder};

use rigetti_pyo3::{
    impl_compare, impl_hash, impl_repr, py_wrap_type, py_wrap_union_enum,
    pyo3::{
        pymethods,
        types::{PyLong, PyString},
        Py,
    },
};

use crate::{impl_eq, impl_to_quil};

py_wrap_union_enum! {
    #[derive(Debug, Eq, Hash, PartialEq)]
    #[pyo3(subclass, module = "quil.instructions")]
    PyQubit(Qubit) as "Qubit" {
        fixed: Fixed => Py<PyLong>,
        variable: Variable => Py<PyString>,
        placeholder: Placeholder => PyQubitPlaceholder
    }
}
impl_repr!(PyQubit);
impl_to_quil!(PyQubit);
impl_hash!(PyQubit);
impl_eq!(PyQubit);

py_wrap_type! {
    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
    #[pyo3(subclass, module = "quil.instructions")]
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
