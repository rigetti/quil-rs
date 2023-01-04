use pyo3::{
    types::{PyInt, PyString},
    Py,
};
use quil_rs::instruction::{Declaration, ScalarType, Vector};
use rigetti_pyo3::{py_wrap_data_struct, py_wrap_type, py_wrap_union_enum};

py_wrap_union_enum! {
    PyScalarType(ScalarType) as "ScalarType" {
        bit: Bit,
        integer: Integer,
        octet: Octet,
        real: Real
    }
}

py_wrap_data_struct! {
    PyVector(Vector) as "Vector" {
        data_type: ScalarType => PyScalarType,
        length: u64 => Py<PyInt>
    }
}

py_wrap_type! {
    #[derive(Debug)]
    PySharing(Option<String>) as "Sharing"
}

py_wrap_data_struct! {
    PyDeclaration(Declaration) as "Declaration" {
        name: String => Py<PyString>,
        size: Vector => PyVector,
        sharing: Option<String> => PySharing
    }
}
