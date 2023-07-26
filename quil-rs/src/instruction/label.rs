use std::sync::Arc;

use crate::quil::{Quil, ToQuilError};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Label {
    Fixed(String),
    Placeholder(LabelPlaceholder),
}

impl Label {
    pub(crate) fn resolve_placeholder<R>(&mut self, resolver: R)
    where
        R: Fn(&LabelPlaceholder) -> Option<String>,
    {
        if let Label::Placeholder(placeholder) = self {
            if let Some(resolved) = resolver(placeholder) {
                *self = Label::Fixed(resolved);
            }
        }
    }
}

impl Quil for Label {
    fn write(&self, writer: &mut impl std::fmt::Write) -> crate::quil::ToQuilResult<()> {
        match self {
            Label::Fixed(label) => write!(writer, "LABEL @{}", label).map_err(Into::into),
            Label::Placeholder(_) => Err(ToQuilError::UnresolvedLabelPlaceholder),
        }
    }
}

type LabelPlaceholderInner = Arc<String>;

/// An opaque placeholder for a qubit whose index may be assigned
/// at a later time.
#[derive(Clone, Debug, Eq)]
pub struct LabelPlaceholder(LabelPlaceholderInner);

impl LabelPlaceholder {
    pub fn new(base_label: String) -> Self {
        Self(Arc::new(base_label))
    }

    pub fn as_inner(&self) -> &String {
        &self.0
    }

    fn address(&self) -> usize {
        &*self.0 as *const _ as usize
    }
}

impl std::hash::Hash for LabelPlaceholder {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.address().hash(state);
    }
}

impl PartialOrd for LabelPlaceholder {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.address().partial_cmp(&other.address())
    }
}

impl Ord for LabelPlaceholder {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.address().cmp(&other.address())
    }
}

impl PartialEq for LabelPlaceholder {
    fn eq(&self, other: &Self) -> bool {
        Arc::<std::string::String>::ptr_eq(&self.0, &other.0)
    }
}
