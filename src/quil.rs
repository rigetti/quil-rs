/// A trait to wrap items which represent some construct within the Quil language.
pub trait Quil {
    type Display: std::fmt::Display;

    /// Return a string in valid Quil syntax or an error if the item cannot be represented with valid Quil.
    fn to_quil(&self) -> Result<Self::Display, ToQuilError>;

    /// Return a string in valid Quil syntax if possible or otherwise the debug representation of the item.
    fn to_quil_or_debug(&self) -> String
    where
        Self: std::fmt::Debug,
    {
        self.to_quil()
            .map(|s| s.to_string())
            .unwrap_or_else(|_| format!("{:#?}", self))
    }
}

pub type ToQuilResult<T> = Result<T, ToQuilError>;

/// Errors which can occur when converting a Quil item to a string.
#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum ToQuilError {
    #[error("Label has not yet been resolved")]
    UnresolvedLabelPlaceholder,
    #[error("Qubit has not yet been resolved")]
    UnresolvedQubitPlaceholder,
}
