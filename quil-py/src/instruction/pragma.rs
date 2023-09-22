use quil_rs::instruction::{Include, Pragma, PragmaArgument};

use rigetti_pyo3::{
    impl_hash, impl_repr, py_wrap_data_struct, py_wrap_union_enum,
    pyo3::{
        pymethods,
        types::{PyInt, PyString},
        Py, PyResult, Python,
    },
    PyTryFrom,
};

use crate::{impl_copy_for_instruction, impl_eq, impl_to_quil};

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyPragma(Pragma) as "Pragma" {
        name: String => Py<PyString>,
        arguments: Vec<PragmaArgument> => Vec<PyPragmaArgument>,
        data: Option<String> => Option<Py<PyString>>
    }
}
impl_repr!(PyPragma);
impl_to_quil!(PyPragma);
impl_copy_for_instruction!(PyPragma);
impl_hash!(PyPragma);
impl_eq!(PyPragma);

#[pymethods]
impl PyPragma {
    #[new]
    fn new(
        py: Python<'_>,
        name: String,
        arguments: Vec<PyPragmaArgument>,
        data: Option<String>,
    ) -> PyResult<Self> {
        Ok(Self(Pragma::new(
            name,
            Vec::<PragmaArgument>::py_try_from(py, &arguments)?,
            data,
        )))
    }
}

py_wrap_union_enum! {
    #[derive(Debug, PartialEq, Eq)]
    PyPragmaArgument(PragmaArgument) as "PragmaArgument" {
        identifier: Identifier => Py<PyString>,
        integer: Integer => Py<PyInt>
    }
}
impl_repr!(PyPragmaArgument);
impl_to_quil!(PyPragmaArgument);
impl_hash!(PyPragmaArgument);
impl_eq!(PyPragmaArgument);

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyInclude(Include) as "Include" {
        filename: String => Py<PyString>
    }
}
impl_repr!(PyInclude);
impl_to_quil!(PyInclude);
impl_copy_for_instruction!(PyInclude);
impl_hash!(PyInclude);
impl_eq!(PyInclude);

#[pymethods]
impl PyInclude {
    #[new]
    pub fn new(filename: String) -> Self {
        Self(Include::new(filename))
    }
}
