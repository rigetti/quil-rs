use quil_rs::instruction::{Qubit, Reset};

use rigetti_pyo3::{
    impl_hash, impl_repr, py_wrap_data_struct,
    pyo3::{pymethods, PyResult, Python},
    PyTryFrom,
};

use crate::{
    impl_copy_for_instruction, impl_eq, impl_pickle_for_instruction, impl_to_quil,
    instruction::PyQubit,
};

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass, module = "quil.instructions")]
    PyReset(Reset) as "Reset" {
        qubit: Option<Qubit> => Option<PyQubit>
    }
}
impl_repr!(PyReset);
impl_copy_for_instruction!(PyReset);
impl_to_quil!(PyReset);
impl_hash!(PyReset);
impl_eq!(PyReset);
impl_pickle_for_instruction!(PyReset);

#[pymethods]
impl PyReset {
    #[new]
    fn new(py: Python<'_>, qubit: Option<PyQubit>) -> PyResult<Self> {
        Ok(Self(Reset::new(Option::<Qubit>::py_try_from(py, &qubit)?)))
    }
}
