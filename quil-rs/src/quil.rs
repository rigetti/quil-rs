/// A trait to wrap items which represent some construct within the Quil language.
///
/// If you want to serialize an object to string and fail if it can't be represented as valid Quil, then use
/// `to_quil()`. If you want to serialize an object to string infallibly, and can tolerate invalid Quil, then
/// use `to_quil_or_debug()`.
pub trait Quil: std::fmt::Debug {
    /// Return a string in valid Quil syntax or an error if the item cannot be represented with valid Quil.
    fn to_quil(&self) -> Result<String, ToQuilError> {
        let mut buffer = String::new();
        self.write(&mut buffer, false)?;
        Ok(buffer)
    }

    /// Return a string in valid Quil syntax if possible. Any individual component of this object
    /// which cannot be represented in Quil will be replaced with a `Debug` representation of that
    /// component.
    fn to_quil_or_debug(&self) -> String {
        let mut buffer = String::new();
        let _ = self.write(&mut buffer, true);
        buffer
    }

    /// Write the Quil representation of the item to the given writer. If `fall_back_to_debug`
    /// is `true`, then it must not return an error.
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> Result<(), ToQuilError>;
}

pub type ToQuilResult<T> = Result<T, ToQuilError>;

/// Errors which can occur when converting a Quil item to a string.
#[derive(Debug, thiserror::Error, PartialEq)]
#[non_exhaustive]
pub enum ToQuilError {
    #[error("Failed to write Quil: {0}")]
    FormatError(#[from] std::fmt::Error),
    #[error("Label has not yet been resolved")]
    UnresolvedLabelPlaceholder,
    #[error("Qubit has not yet been resolved")]
    UnresolvedQubitPlaceholder,
}

/// Write an iterator of Quil items to the given writer, joined with the provided `joiner`.
pub(crate) fn write_join_quil<'i, I, T>(
    writer: &mut impl std::fmt::Write,
    fall_back_to_debug: bool,
    values: I,
    joiner: &str,
    prefix: &str,
) -> Result<(), ToQuilError>
where
    I: IntoIterator<Item = &'i T>,
    T: Quil + 'i,
{
    let mut iter = values.into_iter();
    if let Some(first) = iter.next() {
        write!(writer, "{prefix}")?;
        first.write(writer, fall_back_to_debug)?;

        for value in iter {
            write!(writer, "{joiner}{prefix}")?;
            value.write(writer, fall_back_to_debug)?;
        }
    }
    Ok(())
}
