use quil_rs::{
    instruction::{Declaration, MemoryReference, ScalarType, Vector},
    program::SyntaxError,
};

use rigetti_pyo3::{
    impl_from_str, impl_hash, impl_repr, impl_str, py_wrap_data_struct, py_wrap_simple_enum,
    pyo3::{
        pymethods,
        types::{PyInt, PyString},
        Py, PyResult, Python,
    },
    PyTryFrom,
};

py_wrap_simple_enum! {
    PyScalarType(ScalarType) as "ScalarType" {
        Bit,
        Integer,
        Octet,
        Real
    }
}
impl_repr!(PyScalarType);
impl_str!(PyScalarType);

py_wrap_data_struct! {
    #[derive(Debug)]
    #[pyo3(subclass)]
    PyVector(Vector) as "Vector" {
        data_type: ScalarType => PyScalarType,
        length: u64 => Py<PyInt>
    }
}
impl_repr!(PyVector);
impl_str!(PyVector);

#[pymethods]
impl PyVector {
    #[new]
    pub fn new(py: Python<'_>, data_type: PyScalarType, length: u64) -> PyResult<Self> {
        Ok(Self(Vector::new(
            ScalarType::py_try_from(py, &data_type)?,
            length,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug)]
    #[pyo3(subclass)]
    PyDeclaration(Declaration) as "Declaration" {
        name: String => Py<PyString>,
        size: Vector => PyVector,
        sharing: Option<String> => Option<Py<PyString>>
    }
}
impl_repr!(PyDeclaration);
impl_str!(PyDeclaration);

#[pymethods]
impl PyDeclaration {
    #[new]
    pub fn new(
        py: Python<'_>,
        name: String,
        size: PyVector,
        sharing: Option<String>,
    ) -> PyResult<Self> {
        Ok(Self(Declaration::new(
            name,
            Vector::py_try_from(py, &size)?,
            sharing,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug, Hash, PartialEq)]
    #[pyo3(subclass)]
    PyMemoryReference(MemoryReference) as "MemoryReference" {
        name: String => Py<PyString>,
        index: u64 => Py<PyInt>
    }
}
impl_hash!(PyMemoryReference);
impl_repr!(PyMemoryReference);
impl_str!(PyMemoryReference);
impl_from_str!(PyMemoryReference, SyntaxError<MemoryReference>);

#[pymethods]
impl PyMemoryReference {
    #[new]
    pub fn new(name: String, index: u64) -> Self {
        Self(MemoryReference::new(name, index))
    }
}
