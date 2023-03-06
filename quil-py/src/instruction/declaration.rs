use quil_rs::instruction::{Declaration, ScalarType, Vector};

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
    PyDeclaration(Declaration) as "Declaration" {
        name: String => Py<PyString>,
        size: Vector => PyVector,
        sharing: Option<String> => Option<Py<PyString>>
    }
}
impl_repr!(PyDeclaration);
impl_str!(PyDeclaration);
