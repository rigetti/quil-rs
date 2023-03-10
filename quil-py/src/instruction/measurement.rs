use quil_rs::instruction::{Measurement, MemoryReference, Qubit};
use rigetti_pyo3::{
    impl_repr, impl_str, py_wrap_data_struct,
    pyo3::{pymethods, PyResult, Python},
    PyTryFrom,
};

use crate::instruction::{PyMemoryReference, PyQubit};

py_wrap_data_struct! {
    #[pyo3(subclass)]
    PyMeasurement(Measurement) as "Measurement" {
        qubit: Qubit => PyQubit,
        target: Option<MemoryReference> => Option<PyMemoryReference>
    }
}
impl_str!(PyMeasurement);
impl_repr!(PyMeasurement);

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
