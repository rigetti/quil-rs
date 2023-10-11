use std::sync::Arc;

use super::MemoryReference;
use crate::quil::{Quil, ToQuilError};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Label {
    pub target: Target,
}

impl Label {
    pub fn new(target: Target) -> Self {
        Label { target }
    }
}

impl Quil for Label {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(writer, "LABEL ")?;
        self.target.write(writer, fall_back_to_debug)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Target {
    Fixed(String),
    Placeholder(TargetPlaceholder),
}

impl Target {
    pub(crate) fn resolve_placeholder<R>(&mut self, resolver: R)
    where
        R: Fn(&TargetPlaceholder) -> Option<String>,
    {
        if let Target::Placeholder(placeholder) = self {
            if let Some(resolved) = resolver(placeholder) {
                *self = Target::Fixed(resolved);
            }
        }
    }
}

impl Quil for Target {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        match self {
            Target::Fixed(label) => write!(writer, "@{}", label).map_err(Into::into),
            Target::Placeholder(_) => {
                if fall_back_to_debug {
                    write!(writer, "@{:?}", self).map_err(Into::into)
                } else {
                    Err(ToQuilError::UnresolvedLabelPlaceholder)
                }
            }
        }
    }
}

type TargetPlaceholderInner = Arc<String>;

/// An opaque placeholder for a label whose index may be assigned
/// at a later time.
#[derive(Clone, Debug, Eq)]
pub struct TargetPlaceholder(TargetPlaceholderInner);

impl TargetPlaceholder {
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

impl std::hash::Hash for TargetPlaceholder {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.address().hash(state);
    }
}

impl PartialOrd for TargetPlaceholder {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TargetPlaceholder {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.address().cmp(&other.address())
    }
}

impl PartialEq for TargetPlaceholder {
    fn eq(&self, other: &Self) -> bool {
        Arc::<std::string::String>::ptr_eq(&self.0, &other.0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Jump {
    pub target: Target,
}

impl Quil for Jump {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> Result<(), crate::quil::ToQuilError> {
        write!(writer, "JUMP ")?;
        self.target.write(writer, fall_back_to_debug)?;
        Ok(())
    }
}

impl Jump {
    pub fn new(target: Target) -> Self {
        Self { target }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct JumpWhen {
    pub target: Target,
    pub condition: MemoryReference,
}

impl JumpWhen {
    pub fn new(target: Target, condition: MemoryReference) -> Self {
        Self { target, condition }
    }
}

impl Quil for JumpWhen {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> Result<(), crate::quil::ToQuilError> {
        write!(writer, "JUMP-WHEN ")?;
        self.target.write(writer, fall_back_to_debug)?;
        write!(writer, " {}", self.condition)?;
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct JumpUnless {
    pub target: Target,
    pub condition: MemoryReference,
}

impl JumpUnless {
    pub fn new(target: Target, condition: MemoryReference) -> Self {
        Self { target, condition }
    }
}

impl Quil for JumpUnless {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> Result<(), crate::quil::ToQuilError> {
        write!(writer, "JUMP-UNLESS ")?;
        self.target.write(writer, fall_back_to_debug)?;
        write!(writer, " {}", self.condition)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[test]
    fn resolve_placeholder() {
        let mut label = Target::Placeholder(TargetPlaceholder::new("base".to_string()));
        label.resolve_placeholder(|_| Some("test".to_string()));
        assert_eq!(label, Target::Fixed("test".to_string()))
    }

    #[rstest]
    #[case(Target::Fixed(String::from("test")), Ok("@test"), "@test")]
    #[case(
        Target::Placeholder(TargetPlaceholder::new(String::from("test-placeholder"))),
        Err(ToQuilError::UnresolvedLabelPlaceholder),
        "@Placeholder(TargetPlaceholder(\"test-placeholder\"))"
    )]
    fn quil_format(
        #[case] input: Target,
        #[case] expected_quil: crate::quil::ToQuilResult<&str>,
        #[case] expected_debug: &str,
    ) {
        assert_eq!(input.to_quil(), expected_quil.map(|s| s.to_string()));
        assert_eq!(input.to_quil_or_debug(), expected_debug);
    }
}
