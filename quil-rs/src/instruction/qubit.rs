use std::sync::Arc;

use crate::quil::{Quil, ToQuilError};

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
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
        fall_back_to_debug: bool,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        use Qubit::*;
        match self {
            Fixed(value) => write!(writer, "{value}").map_err(Into::into),
            Placeholder(_) => {
                if fall_back_to_debug {
                    write!(writer, "{:?}", self).map_err(Into::into)
                } else {
                    Err(ToQuilError::UnresolvedQubitPlaceholder)
                }
            }
            Variable(value) => write!(writer, "{value}").map_err(Into::into),
        }
    }
}

type QubitPlaceholderInner = Arc<()>;

/// An opaque placeholder for a qubit whose index may be assigned
/// at a later time.
#[derive(Clone, Eq)]
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

impl std::fmt::Debug for QubitPlaceholder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "QubitPlaceholder({:#X})", self.address())
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
        Some(self.cmp(other))
    }
}

impl Ord for QubitPlaceholder {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.address().cmp(&other.address())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use regex::Regex;
    use rstest::rstest;

    #[test]
    fn resolve_placeholder() {
        let mut qubit = Qubit::Placeholder(QubitPlaceholder::default());
        qubit.resolve_placeholder(|_| Some(0));
        assert_eq!(qubit, Qubit::Fixed(0));
    }

    #[rstest]
    #[case(Qubit::Fixed(0), Ok("0"), "0")]
    #[case(
        Qubit::Variable("q".to_string()),
        Ok("q"),
        "q"
    )]
    #[case(
        Qubit::Placeholder(QubitPlaceholder::default()),
        Err(ToQuilError::UnresolvedQubitPlaceholder),
        r"Placeholder\(QubitPlaceholder\(0x[0-9,A-Z]+\)\)"
    )]
    fn quil_format(
        #[case] input: Qubit,
        #[case] expected_quil: crate::quil::ToQuilResult<&str>,
        #[case] expected_debug: &str,
    ) {
        assert_eq!(input.to_quil(), expected_quil.map(|s| s.to_string()));
        let re = Regex::new(expected_debug).unwrap();
        assert!(re.is_match(&input.to_quil_or_debug()));
    }
}
