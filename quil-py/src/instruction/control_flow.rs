use quil_rs::instruction::{
    Jump, JumpUnless, JumpWhen, Label, MemoryReference, Target, TargetPlaceholder,
};
use rigetti_pyo3::{
    impl_compare, impl_hash, impl_repr, py_wrap_data_struct, py_wrap_type, py_wrap_union_enum,
    pyo3::{pyclass::CompareOp, pymethods, types::PyString, IntoPy, Py, PyObject, Python},
    PyWrapper,
};

use crate::{impl_copy_for_instruction, impl_to_quil, instruction::PyMemoryReference};

py_wrap_data_struct! {
    #[pyo3(subclass)]
    #[derive(Debug, Hash, PartialEq, Eq)]
    PyLabel(Label) as "Label" {
        target: Target => PyTarget
    }
}
impl_repr!(PyLabel);
impl_hash!(PyLabel);
impl_to_quil!(PyLabel);
impl_copy_for_instruction!(PyLabel);

#[pymethods]
impl PyLabel {
    #[new]
    fn new(target: PyTarget) -> Self {
        PyLabel(Label::new(target.into_inner()))
    }

    fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self == other).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}

py_wrap_union_enum! {
    #[derive(Debug, Hash, PartialEq, Eq)]
    PyTarget(Target) as "Target" {
        fixed: Fixed => Py<PyString>,
        placeholder: Placeholder => PyTargetPlaceholder
    }
}
impl_repr!(PyTarget);
impl_hash!(PyTarget);
impl_to_quil!(PyTarget);

#[pymethods]
impl PyTarget {
    fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self == other).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}

py_wrap_type! {
    #[pyo3(subclass)]
    #[derive(Debug, Hash, PartialOrd, Ord, PartialEq, Eq)]
    PyTargetPlaceholder(TargetPlaceholder) as "TargetPlaceholder"
}
impl_repr!(PyTargetPlaceholder);
impl_hash!(PyTargetPlaceholder);
impl_compare!(PyTargetPlaceholder);

#[pymethods]
impl PyTargetPlaceholder {
    #[new]
    pub fn new(base_label: String) -> Self {
        Self(TargetPlaceholder::new(base_label))
    }

    #[getter]
    pub fn base_label(&self) -> &str {
        PyWrapper::as_inner(self).as_inner()
    }
}

py_wrap_data_struct! {
    #[pyo3(subclass)]
    PyJump(Jump) as "Jump" {
        target: Target => PyTarget
    }
}
impl_repr!(PyJump);
impl_to_quil!(PyJump);
impl_copy_for_instruction!(PyJump);

#[pymethods]
impl PyJump {
    #[new]
    fn new(target: PyTarget) -> Self {
        Self(Jump::new(target.into_inner()))
    }
}

py_wrap_data_struct! {
    #[pyo3(subclass)]
    PyJumpWhen(JumpWhen) as "JumpWhen" {
        target: Target => PyTarget,
        condition: MemoryReference => PyMemoryReference
    }
}
impl_repr!(PyJumpWhen);
impl_to_quil!(PyJumpWhen);
impl_copy_for_instruction!(PyJumpWhen);

#[pymethods]
impl PyJumpWhen {
    #[new]
    fn new(target: PyTarget, condition: PyMemoryReference) -> Self {
        Self(JumpWhen::new(target.into_inner(), condition.into_inner()))
    }
}

py_wrap_data_struct! {
    #[pyo3(subclass)]
    PyJumpUnless(JumpUnless) as "JumpUnless" {
        target: Target => PyTarget,
        condition: MemoryReference => PyMemoryReference
    }
}
impl_repr!(PyJumpUnless);
impl_to_quil!(PyJumpUnless);

#[pymethods]
impl PyJumpUnless {
    #[new]
    fn new(target: PyTarget, condition: PyMemoryReference) -> Self {
        Self(JumpUnless::new(target.into_inner(), condition.into_inner()))
    }
}
