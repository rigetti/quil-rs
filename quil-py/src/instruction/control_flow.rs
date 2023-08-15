use quil_rs::instruction::{Jump, JumpUnless, JumpWhen, Label, LabelPlaceholder, MemoryReference};
use rigetti_pyo3::{
    impl_compare, impl_hash, impl_repr, py_wrap_data_struct, py_wrap_type, py_wrap_union_enum,
    pyo3::{pymethods, types::PyString, Py},
    PyWrapper,
};

use crate::{impl_quil, instruction::PyMemoryReference};

py_wrap_union_enum! {
    PyLabel(Label) as "Label" {
        fixed: Fixed => Py<PyString>,
        placeholder: Placeholder => PyLabelPlaceholder
    }
}
impl_repr!(PyLabel);
impl_quil!(PyLabel);

py_wrap_type! {
    #[pyo3(subclass)]
    #[derive(Debug, Hash, PartialOrd, Ord, PartialEq, Eq)]
    PyLabelPlaceholder(LabelPlaceholder) as "LabelPlaceholder"
}
impl_repr!(PyLabelPlaceholder);
impl_hash!(PyLabelPlaceholder);
impl_compare!(PyLabelPlaceholder);

#[pymethods]
impl PyLabelPlaceholder {
    #[new]
    pub fn new(base_label: String) -> Self {
        Self(LabelPlaceholder::new(base_label))
    }

    #[getter]
    pub fn base_label(&self) -> &str {
        PyWrapper::as_inner(self).as_inner()
    }
}

py_wrap_data_struct! {
    #[pyo3(subclass)]
    PyJump(Jump) as "Jump" {
        target: Label => PyLabel
    }
}
impl_repr!(PyJump);
impl_quil!(PyJump);

#[pymethods]
impl PyJump {
    #[new]
    fn new(target: PyLabel) -> Self {
        Self(Jump::new(target.into_inner()))
    }
}

py_wrap_data_struct! {
    #[pyo3(subclass)]
    PyJumpWhen(JumpWhen) as "JumpWhen" {
        target: Label => PyLabel,
        condition: MemoryReference => PyMemoryReference
    }
}
impl_repr!(PyJumpWhen);
impl_quil!(PyJumpWhen);

#[pymethods]
impl PyJumpWhen {
    #[new]
    fn new(target: PyLabel, condition: PyMemoryReference) -> Self {
        Self(JumpWhen::new(target.into_inner(), condition.into_inner()))
    }
}

py_wrap_data_struct! {
    #[pyo3(subclass)]
    PyJumpUnless(JumpUnless) as "JumpUnless" {
        target: Label => PyLabel,
        condition: MemoryReference => PyMemoryReference
    }
}
impl_repr!(PyJumpUnless);
impl_quil!(PyJumpUnless);

#[pymethods]
impl PyJumpUnless {
    #[new]
    fn new(target: PyLabel, condition: PyMemoryReference) -> Self {
        Self(JumpUnless::new(target.into_inner(), condition.into_inner()))
    }
}
