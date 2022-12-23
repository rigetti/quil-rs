use pyo3::types::{PyLong, PyString};
use quil_rs::instruction::MemoryReference;
use rigetti_pyo3::py_wrap_data_struct;

py_wrap_data_struct! {
    PyMemoryReference(MemoryReference) as "MemoryReference" {
        name: String => PyString,
        index: u64 => PyLong
    }
}
