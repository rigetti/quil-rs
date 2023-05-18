pub trait Quil {
    type Display: std::fmt::Display;

    /// Return a string in valid Quil syntax or an error if the item cannot be represented with valid Quil.
    fn to_quil(&self) -> Result<Self::Display, ToQuilError>;

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

#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum ToQuilError {
    #[error("Label has not yet been resolved")]
    UnresolvedLabelPlaceholder,
    #[error("Qubit has not yet been resolved")]
    UnresolvedQubitPlaceholder,
}
