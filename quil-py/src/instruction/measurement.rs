use quil_rs::instruction::{Measurement, MemoryReference, Qubit};
use rigetti_pyo3::{
    impl_hash, impl_repr, py_wrap_data_struct,
    pyo3::{pymethods, PyResult, Python},
    PyTryFrom,
};

use crate::{
    impl_copy_for_instruction, impl_eq, impl_to_quil,
    instruction::{PyMemoryReference, PyQubit},
};

py_wrap_data_struct! {
    #[pyo3(subclass)]
    #[derive(Debug, PartialEq, Eq)]
    PyMeasurement(Measurement) as "Measurement" {
        qubit: Qubit => PyQubit,
        target: Option<MemoryReference> => Option<PyMemoryReference>
    }
}
impl_to_quil!(PyMeasurement);
impl_copy_for_instruction!(PyMeasurement);
impl_hash!(PyMeasurement);
impl_repr!(PyMeasurement);
impl_eq!(PyMeasurement);

#[pymethods]
impl PyMeasurement {
    #[new]
    pub fn new(
        py: Python<'_>,
        qubit: PyQubit,
        target: Option<PyMemoryReference>,
    ) -> PyResult<PyMeasurement> {
        Ok(Self(Measurement::new(
            Qubit::py_try_from(py, &qubit)?,
            Option::<MemoryReference>::py_try_from(py, &target)?,
        )))
    }
}
