use quil_rs::instruction::{
    ArithmeticOperand, Declaration, Load, MemoryReference, Offset, ScalarType, Sharing, Store,
    Vector,
};

use super::PyArithmeticOperand;
use crate::{impl_copy_for_instruction, impl_eq, impl_to_quil};

use rigetti_pyo3::{
    impl_from_str, impl_hash, impl_parse, impl_repr, py_wrap_data_struct, py_wrap_error,
    py_wrap_simple_enum,
    pyo3::{
        exceptions::PyValueError,
        pymethods,
        types::{PyInt, PyString},
        Py, PyResult, Python,
    },
    wrap_error, PyTryFrom,
};

wrap_error!(RustParseMemoryReferenceError(quil_rs::program::SyntaxError<MemoryReference>));
py_wrap_error!(
    quil,
    RustParseMemoryReferenceError,
    ParseMemoryReferenceError,
    PyValueError
);

py_wrap_simple_enum! {
    PyScalarType(ScalarType) as "ScalarType" {
        Bit,
        Integer,
        Octet,
        Real
    }
}
impl_repr!(PyScalarType);
impl_to_quil!(PyScalarType);
impl_hash!(PyScalarType);

py_wrap_data_struct! {
    #[derive(Debug, Hash, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyVector(Vector) as "Vector" {
        data_type: ScalarType => PyScalarType,
        length: u64 => Py<PyInt>
    }
}
impl_repr!(PyVector);
impl_to_quil!(PyVector);
impl_hash!(PyVector);
impl_eq!(PyVector);

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
    #[derive(Debug, PartialEq, Eq, Hash)]
    #[pyo3(subclass)]
    PyOffset(Offset) as "Offset" {
        offset: u64 => Py<PyInt>,
        data_type: ScalarType => PyScalarType
    }
}
impl_repr!(PyOffset);
impl_to_quil!(PyOffset);
impl_hash!(PyOffset);
impl_eq!(PyOffset);

#[pymethods]
impl PyOffset {
    #[new]
    pub fn new(py: Python<'_>, offset: u64, data_type: PyScalarType) -> PyResult<Self> {
        Ok(Self(Offset::new(
            offset,
            ScalarType::py_try_from(py, &data_type)?,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq, Hash)]
    #[pyo3(subclass)]
    PySharing(Sharing) as "Sharing" {
        name: String => Py<PyString>,
        offsets: Vec<Offset> => Vec<PyOffset>
    }
}
impl_repr!(PySharing);
impl_hash!(PySharing);
impl_eq!(PySharing);

#[pymethods]
impl PySharing {
    #[new]
    pub fn new(py: Python<'_>, name: String, offsets: Vec<PyOffset>) -> PyResult<Self> {
        Ok(Self(Sharing::new(
            name,
            Vec::<Offset>::py_try_from(py, &offsets)?,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyDeclaration(Declaration) as "Declaration" {
        name: String => Py<PyString>,
        size: Vector => PyVector,
        sharing: Option<Sharing> => Option<PySharing>
    }
}
impl_repr!(PyDeclaration);
impl_to_quil!(PyDeclaration);
impl_copy_for_instruction!(PyDeclaration);
impl_hash!(PyDeclaration);
impl_eq!(PyDeclaration);

#[pymethods]
impl PyDeclaration {
    #[new]
    pub fn new(
        py: Python<'_>,
        name: String,
        size: PyVector,
        sharing: Option<PySharing>,
    ) -> PyResult<Self> {
        Ok(Self(Declaration::new(
            name,
            Vector::py_try_from(py, &size)?,
            Option::<Sharing>::py_try_from(py, &sharing)?,
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
impl_to_quil!(PyMemoryReference);
impl_from_str!(PyMemoryReference, RustParseMemoryReferenceError);
impl_parse!(PyMemoryReference);
impl_eq!(PyMemoryReference);

#[pymethods]
impl PyMemoryReference {
    #[new]
    pub fn new(name: String, index: u64) -> Self {
        Self(MemoryReference::new(name, index))
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyLoad(Load) as "Load" {
        destination: MemoryReference => PyMemoryReference,
        source: String => Py<PyString>,
        offset: MemoryReference => PyMemoryReference
    }
}
impl_repr!(PyLoad);
impl_to_quil!(PyLoad);
impl_copy_for_instruction!(PyLoad);
impl_hash!(PyLoad);
impl_eq!(PyLoad);

#[pymethods]
impl PyLoad {
    #[new]
    pub fn new(
        py: Python<'_>,
        destination: PyMemoryReference,
        source: String,
        offset: PyMemoryReference,
    ) -> PyResult<Self> {
        Ok(Self(Load::new(
            MemoryReference::py_try_from(py, &destination)?,
            source,
            MemoryReference::py_try_from(py, &offset)?,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass)]
    PyStore(Store) as "Store" {
        destination: String => Py<PyString>,
        offset: MemoryReference => PyMemoryReference,
        source: ArithmeticOperand => PyArithmeticOperand
    }
}
impl_repr!(PyStore);
impl_to_quil!(PyStore);
impl_copy_for_instruction!(PyStore);
impl_hash!(PyStore);
impl_eq!(PyStore);

#[pymethods]
impl PyStore {
    #[new]
    pub fn new(
        py: Python<'_>,
        destination: String,
        offset: PyMemoryReference,
        source: PyArithmeticOperand,
    ) -> PyResult<Self> {
        Ok(Self(Store::new(
            destination,
            MemoryReference::py_try_from(py, &offset)?,
            ArithmeticOperand::py_try_from(py, &source)?,
        )))
    }
}
