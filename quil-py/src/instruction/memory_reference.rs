use pyo3::{
    types::{PyLong, PyString},
    Py,
};
use quil_rs::instruction::MemoryReference;
use rigetti_pyo3::py_wrap_data_struct;

py_wrap_data_struct! {
    PyMemoryReference(MemoryReference) as "MemoryReference" {
        name: String => Py<PyString>,
        index: u64 => Py<PyLong>
    }
}
