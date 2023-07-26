use std::sync::Arc;

use crate::quil::{Quil, ToQuilError};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Qubit {
    Fixed(u64),
    Placeholder(QubitPlaceholder),
    Variable(String),
}

impl Qubit {
    pub(crate) fn resolve_placeholder<R>(&mut self, resolver: R)
    where
        R: Fn(&QubitPlaceholder) -> Option<u64>,
    {
        if let Qubit::Placeholder(placeholder) = self {
            if let Some(resolved) = resolver(placeholder) {
                *self = Qubit::Fixed(resolved);
            }
        }
    }
}

impl Quil for Qubit {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        use Qubit::*;
        match self {
            Fixed(value) => write!(writer, "{value}").map_err(Into::into),
            Placeholder(_) => Err(ToQuilError::UnresolvedQubitPlaceholder),
            Variable(value) => write!(writer, "{value}").map_err(Into::into),
        }
    }
}

type QubitPlaceholderInner = Arc<()>;

/// An opaque placeholder for a qubit whose index may be assigned
/// at a later time.
#[derive(Clone, Debug, Eq)]
pub struct QubitPlaceholder(QubitPlaceholderInner);

impl QubitPlaceholder {
    fn address(&self) -> usize {
        &*self.0 as *const _ as usize
    }
}

impl Default for QubitPlaceholder {
    fn default() -> Self {
        Self(Arc::new(()))
    }
}

impl std::hash::Hash for QubitPlaceholder {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.address().hash(state);
    }
}

impl PartialEq for QubitPlaceholder {
    #[allow(clippy::ptr_eq)]
    fn eq(&self, other: &Self) -> bool {
        self.address().eq(&other.address())
    }
}

impl PartialOrd for QubitPlaceholder {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.address().partial_cmp(&other.address())
    }
}

impl Ord for QubitPlaceholder {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.address().cmp(&other.address())
    }
}
