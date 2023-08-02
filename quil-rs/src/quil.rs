/// A trait to wrap items which represent some construct within the Quil language.
pub trait Quil {
    /// Return a string in valid Quil syntax or an error if the item cannot be represented with valid Quil.
    fn to_quil(&self) -> Result<String, ToQuilError> {
        let mut buffer = String::new();
        self.write(&mut buffer, false)?;
        Ok(buffer)
    }

    /// Return a string in valid Quil syntax if possible or otherwise the debug representation of the item.
    fn to_quil_or_debug(&self) -> String
    where
        Self: std::fmt::Debug,
    {
        let mut buffer = String::new();
        self.write(&mut buffer, true).ok();
        buffer
    }

    /// Write the Quil representation of the item to the given writer.
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> Result<(), ToQuilError>;
}

pub type ToQuilResult<T> = Result<T, ToQuilError>;

/// Errors which can occur when converting a Quil item to a string.
#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum ToQuilError {
    #[error("Failed to write Quil: {0}")]
    FormatError(#[from] std::fmt::Error),
    #[error("Label has not yet been resolved")]
    UnresolvedLabelPlaceholder,
    #[error("Qubit has not yet been resolved")]
    UnresolvedQubitPlaceholder,
}

/// Write a sequencer of Quil items to the given writer, joined with the provided `joiner`.
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
        write!(writer, "{}", prefix)?;
        first.write(writer, fall_back_to_debug)?;

        for value in iter {
            write!(writer, "{joiner}{prefix}")?;
            value.write(writer, fall_back_to_debug)?;
        }
    }
    Ok(())
}
