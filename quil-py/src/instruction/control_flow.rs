use quil_rs::instruction::{
    Jump, JumpUnless, JumpWhen, Label, MemoryReference, Target, TargetPlaceholder,
};
use rigetti_pyo3::{
    impl_compare, impl_hash, impl_repr, py_wrap_data_struct, py_wrap_type, py_wrap_union_enum,
    pyo3::{pymethods, types::PyString, Py},
    PyWrapper,
};

use crate::{impl_eq, impl_to_quil, instruction::PyMemoryReference};

/// Implements __copy__ and __deepcopy__ for instructions containing a [`Target`].
///
/// __copy__ implements a shallow copy by returning a reference to the object.
///
/// __deepcopy__ performs a deep copy by cloning the Rust reference, and replacing
/// any [`TargetPlaceholder`]s from the original intruction with new instances so
/// that resolving placeholders on the copy does not affect the original.
macro_rules! impl_copy_for_target_containing_instructions {
    ($name: ident) => {
        #[pyo3::pymethods]
        impl $name {
            pub fn __copy__(&self) -> Self {
                self.clone()
            }

            pub fn __deepcopy__(&self, _memo: &pyo3::types::PyDict) -> Self {
                use quil_rs::instruction::{Target, TargetPlaceholder};
                let mut copy = rigetti_pyo3::PyWrapper::into_inner(self.clone());
                if let Target::Placeholder(placeholder) = copy.target {
                    copy.target = Target::Placeholder(TargetPlaceholder::new(
                        placeholder.as_inner().to_string(),
                    ))
                }

                Self(copy)
            }
        }
    };
}

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
impl_copy_for_target_containing_instructions!(PyLabel);
impl_eq!(PyLabel);

#[pymethods]
impl PyLabel {
    #[new]
    fn new(target: PyTarget) -> Self {
        PyLabel(Label::new(target.into_inner()))
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
impl_eq!(PyTarget);

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
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass)]
    PyJump(Jump) as "Jump" {
        target: Target => PyTarget
    }
}
impl_repr!(PyJump);
impl_to_quil!(PyJump);
impl_copy_for_target_containing_instructions!(PyJump);
impl_eq!(PyJump);

#[pymethods]
impl PyJump {
    #[new]
    fn new(target: PyTarget) -> Self {
        Self(Jump::new(target.into_inner()))
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass)]
    PyJumpWhen(JumpWhen) as "JumpWhen" {
        target: Target => PyTarget,
        condition: MemoryReference => PyMemoryReference
    }
}
impl_repr!(PyJumpWhen);
impl_to_quil!(PyJumpWhen);
impl_copy_for_target_containing_instructions!(PyJumpWhen);
impl_eq!(PyJumpWhen);

#[pymethods]
impl PyJumpWhen {
    #[new]
    fn new(target: PyTarget, condition: PyMemoryReference) -> Self {
        Self(JumpWhen::new(target.into_inner(), condition.into_inner()))
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass)]
    PyJumpUnless(JumpUnless) as "JumpUnless" {
        target: Target => PyTarget,
        condition: MemoryReference => PyMemoryReference
    }
}
impl_repr!(PyJumpUnless);
impl_to_quil!(PyJumpUnless);
impl_copy_for_target_containing_instructions!(PyJumpUnless);
impl_eq!(PyJumpUnless);

#[pymethods]
impl PyJumpUnless {
    #[new]
    fn new(target: PyTarget, condition: PyMemoryReference) -> Self {
        Self(JumpUnless::new(target.into_inner(), condition.into_inner()))
    }
}
