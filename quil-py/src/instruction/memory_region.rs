use quil_rs::{
    instruction::{MemoryReference, ScalarType, Vector},
    program::MemoryRegion,
};

use rigetti_pyo3::{
    impl_repr, impl_str, py_wrap_data_struct, py_wrap_union_enum,
    pyo3::{
        types::{PyInt, PyString},
        Py,
    },
};

py_wrap_union_enum! {
    PyScalarType(ScalarType) as "ScalarType" {
        bit: Bit,
        integer: Integer,
        octet: Octet,
        real: Real
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
    PyMemoryReference(MemoryReference) as "MemoryReference" {
        name: String => Py<PyString>,
        index: u64 => Py<PyInt>
    }
}
impl_repr!(PyMemoryReference);
impl_str!(PyMemoryReference);
