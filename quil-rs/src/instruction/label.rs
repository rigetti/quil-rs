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
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        match self {
            Label::Fixed(label) => write!(writer, "@{}", label).map_err(Into::into),
            Label::Placeholder(_) => {
                if fall_back_to_debug {
                    write!(writer, "@{:?}", self).map_err(Into::into)
                } else {
                    Err(ToQuilError::UnresolvedLabelPlaceholder)
                }
            }
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

    pub fn as_inner(&self) -> &str {
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

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[test]
    fn resolve_placeholder() {}

    #[rstest]
    #[case(Label::Fixed(String::from("test")), Ok("@test"), "@test")]
    #[case(
        Label::Placeholder(LabelPlaceholder::new(String::from("test-placeholder"))),
        Err(ToQuilError::UnresolvedLabelPlaceholder),
        "@Placeholder(LabelPlaceholder(\"test-placeholder\"))"
    )]
    fn quil_format(
        #[case] input: Label,
        #[case] expected_quil: crate::quil::ToQuilResult<&str>,
        #[case] expected_debug: &str,
    ) {
        assert_eq!(input.to_quil(), expected_quil.map(|s| s.to_string()));
        assert_eq!(input.to_quil_or_debug(), expected_debug);
    }
}
