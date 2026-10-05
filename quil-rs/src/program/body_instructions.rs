//! The body instructions of a [`Program`](super::Program).
//!
//! In Rust-only builds, [`BodyInstructions`] is a thin wrapper over a `Vec<Instruction>`.
//! With the `python` feature, it also keeps a lazily built cache of the Python objects
//! handed out for each instruction, so repeated reads from Python don't re-convert them.

use std::ops::{Deref, DerefMut};

use crate::instruction::Instruction;

#[cfg(feature = "python")]
use pyo3::{prelude::*, sync::PyOnceLock};

pub(crate) struct BodyInstructions {
    rust: Vec<Instruction>,
    /// One slot per instruction, built on the first Python read and filled lazily.
    ///
    /// Whenever this is initialized, its length matches `rust`.
    #[cfg(feature = "python")]
    py: PyOnceLock<Vec<PyOnceLock<Py<PyAny>>>>,
}

impl BodyInstructions {
    /// Append an instruction without invalidating the Python cache.
    pub(crate) fn push(&mut self, instruction: Instruction) {
        self.rust.push(instruction);
        #[cfg(feature = "python")]
        if let Some(slots) = self.py.get_mut() {
            slots.push(PyOnceLock::new());
        }
    }

    /// Append instructions without invalidating the Python cache.
    pub(crate) fn extend<I: IntoIterator<Item = Instruction>>(&mut self, instructions: I) {
        self.rust.extend(instructions);
        #[cfg(feature = "python")]
        if let Some(slots) = self.py.get_mut() {
            slots.resize_with(self.rust.len(), PyOnceLock::new);
        }
    }

    /// Return the Python object for the instruction at `index`,
    /// converting and caching it on first access.
    #[cfg(feature = "python")]
    pub(crate) fn py_get<'py>(
        &self,
        py: Python<'py>,
        index: usize,
    ) -> PyResult<Option<Bound<'py, PyAny>>> {
        let Some(instruction) = self.rust.get(index) else {
            return Ok(None);
        };
        let slots = self.py.get_or_init(py, || {
            std::iter::repeat_with(PyOnceLock::new)
                .take(self.rust.len())
                .collect()
        });
        debug_assert_eq!(slots.len(), self.rust.len());
        let object = slots[index].get_or_try_init(py, || {
            instruction.clone().into_pyobject(py).map(Bound::unbind)
        })?;
        Ok(Some(object.bind(py).clone()))
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
        self.py.take();
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
            py: PyOnceLock::new(),
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
