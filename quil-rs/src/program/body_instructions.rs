//! The body instructions of a [`Program`](super::Program).
//!
//! In Rust-only builds, [`BodyInstructions`] is a thin wrapper over a `Vec<Instruction>`.
//! With the `python` feature, it also keeps a Python list of the objects handed out for a
//! prefix of the body, so repeated reads from Python don't re-convert them.

use std::ops::{Deref, DerefMut};

use crate::instruction::Instruction;

#[cfg(feature = "python")]
use pyo3::{
    prelude::*,
    sync::{critical_section::with_critical_section, PyOnceLock},
    types::PyList,
};

pub(crate) struct BodyInstructions {
    rust: Vec<Instruction>,
    /// Python objects for a prefix of `rust`, extended on Python reads.
    ///
    /// Appends to `rust` leave the prefix valid, so they don't touch this;
    /// any other mutable access drops it.
    #[cfg(feature = "python")]
    py_list: PyOnceLock<Py<PyList>>,
}

impl BodyInstructions {
    /// Append an instruction without invalidating the Python cache:
    /// the cached list stays a valid prefix.
    pub(crate) fn push(&mut self, instruction: Instruction) {
        self.rust.push(instruction);
    }

    /// Append instructions without invalidating the Python cache:
    /// the cached list stays a valid prefix.
    pub(crate) fn extend<I: IntoIterator<Item = Instruction>>(&mut self, instructions: I) {
        self.rust.extend(instructions);
    }

    /// Return the cached list, first converting instructions until it holds at least `len`
    /// (capped at the number of instructions).
    #[cfg(feature = "python")]
    pub(crate) fn py_list<'py>(&self, py: Python<'py>, len: usize) -> PyResult<Bound<'py, PyList>> {
        let list = self
            .py_list
            .get_or_init(py, || PyList::empty(py).unbind())
            .bind(py)
            .clone();
        let target = len.min(self.rust.len());
        // Under free-threaded Python, the critical section keeps concurrent readers from
        // appending the same instruction twice. Converting an instruction can suspend the
        // section, so the length is checked again just before each append.
        with_critical_section(list.as_any(), || {
            while list.len() < target {
                let next = list.len();
                let object = self.rust[next].clone().into_pyobject(py)?;
                if list.len() == next {
                    list.append(object)?;
                }
            }
            Ok(list.clone())
        })
    }
}

impl Deref for BodyInstructions {
    type Target = Vec<Instruction>;

    fn deref(&self) -> &Self::Target {
        &self.rust
    }
}

/// Any mutable access may change instructions or their count, so it drops the Python cache.
impl DerefMut for BodyInstructions {
    fn deref_mut(&mut self) -> &mut Self::Target {
        #[cfg(feature = "python")]
        self.py_list.take();
        &mut self.rust
    }
}

impl Default for BodyInstructions {
    fn default() -> Self {
        Vec::new().into()
    }
}

impl Clone for BodyInstructions {
    /// The clone starts with an empty Python cache: it must not share objects with the original.
    fn clone(&self) -> Self {
        self.rust.clone().into()
    }
}

impl PartialEq for BodyInstructions {
    fn eq(&self, other: &Self) -> bool {
        self.rust == other.rust
    }
}

impl PartialEq<Vec<Instruction>> for BodyInstructions {
    fn eq(&self, other: &Vec<Instruction>) -> bool {
        &self.rust == other
    }
}

impl std::fmt::Debug for BodyInstructions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.rust.fmt(f)
    }
}

impl From<Vec<Instruction>> for BodyInstructions {
    fn from(rust: Vec<Instruction>) -> Self {
        Self {
            rust,
            #[cfg(feature = "python")]
            py_list: PyOnceLock::new(),
        }
    }
}

impl IntoIterator for BodyInstructions {
    type Item = Instruction;
    type IntoIter = std::vec::IntoIter<Instruction>;

    fn into_iter(self) -> Self::IntoIter {
        self.rust.into_iter()
    }
}

impl<'a> IntoIterator for &'a BodyInstructions {
    type Item = &'a Instruction;
    type IntoIter = std::slice::Iter<'a, Instruction>;

    fn into_iter(self) -> Self::IntoIter {
        self.rust.iter()
    }
}

impl<'a> IntoIterator for &'a mut BodyInstructions {
    type Item = &'a mut Instruction;
    type IntoIter = std::slice::IterMut<'a, Instruction>;

    fn into_iter(self) -> Self::IntoIter {
        self.deref_mut().iter_mut()
    }
}

#[cfg(feature = "python")]
impl<'py> IntoPyObject<'py> for BodyInstructions {
    type Target = PyAny;
    type Output = Bound<'py, PyAny>;
    type Error = PyErr;

    fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
        self.rust.into_pyobject(py)
    }
}

#[cfg(feature = "python")]
impl<'a, 'py> FromPyObject<'a, 'py> for BodyInstructions {
    type Error = PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        obj.extract::<Vec<Instruction>>().map(Self::from)
    }
}

#[cfg(feature = "stubs")]
impl pyo3_stub_gen::PyStubType for BodyInstructions {
    fn type_output() -> pyo3_stub_gen::TypeInfo {
        Vec::<Instruction>::type_output()
    }

    fn type_input() -> pyo3_stub_gen::TypeInfo {
        Vec::<Instruction>::type_input()
    }
}
