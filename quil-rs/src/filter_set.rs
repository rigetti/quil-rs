use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Filter<T: std::hash::Hash + Eq> {
    Include(HashSet<T>),
    Exclude(HashSet<T>),
}

impl<T: Default + std::hash::Hash + Eq> Default for Filter<T> {
    fn default() -> Self {
        Self::Exclude(HashSet::new())
    }
}

impl<T: std::hash::Hash + Eq> Filter<T> {
    pub(crate) fn include(&self, item: &T) -> bool {
        match self {
            Self::Include(set) => set.contains(item),
            Self::Exclude(set) => !set.contains(item)
        }
    }
}