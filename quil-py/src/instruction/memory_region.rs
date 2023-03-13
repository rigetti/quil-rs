use quil_rs::{
    instruction::{MemoryReference, ScalarType, Vector},
    program::MemoryRegion,
};

use rigetti_pyo3::{
    impl_repr, impl_str, py_wrap_data_struct, py_wrap_simple_enum,
    pyo3::{
        types::{PyInt, PyString},
        Py,
    },
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
    PyVector(Vector) as "Vector" {
        data_type: ScalarType => PyScalarType,
        length: u64 => Py<PyInt>
    }
}
impl_repr!(PyVector);
impl_str!(PyVector);

py_wrap_data_struct! {
    PyMemoryRegion(MemoryRegion) as "MemoryRegion" {
        size: Vector => PyVector,
        sharing: Option<String> => Option<Py<PyString>>
    }
}
impl_repr!(PyMemoryRegion);

py_wrap_data_struct! {
    #[pyo3(subclass)]
    PyMemoryReference(MemoryReference) as "MemoryReference" {
        name: String => Py<PyString>,
        index: u64 => Py<PyInt>
    }
}
impl_repr!(PyMemoryReference);
impl_str!(PyMemoryReference);
