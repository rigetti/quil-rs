/// A trait to wrap items which represent some construct within the Quil language.
pub trait Quil {
    /// Return a string in valid Quil syntax or an error if the item cannot be represented with valid Quil.
    fn to_quil(&self) -> Result<String, ToQuilError> {
        let mut buffer = String::new();
        self.write(&mut buffer)?;
        Ok(buffer)
    }

    /// Write the Quil representation of the item to the given writer.
    fn write(&self, writer: &mut impl std::fmt::Write) -> Result<(), ToQuilError>;

    /// Return a string in valid Quil syntax if possible or otherwise the debug representation of the item.
    fn to_quil_or_debug(&self) -> String
    where
        Self: std::fmt::Debug,
    {
        self.to_quil().unwrap_or_else(|_| format!("{:#?}", self))
    }
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

#[macro_export]
macro_rules! impl_quil_for_ref {
    ($type:ty) => {
        impl $crate::quil::Quil for &$type {
            fn write(&self, writer: &mut impl std::fmt::Write) -> $crate::quil::ToQuilResult<()> {
                (*self).write(writer)
            }
        }
    };
}

/// Implement the Quil trait using the `Display` implementation of the type.
///
/// For types that do not implement `Display`, a manual implementation is needed.
#[macro_export]
macro_rules! impl_quil {
    ($type:ty) => {
        impl $crate::quil::Quil for $type {
            fn write(&self, writer: &mut impl std::fmt::Write) -> $crate::quil::ToQuilResult<()> {
                write!(writer, "{}", self).map_err(Into::into)
            }
        }

        $crate::impl_quil_for_ref!($type);

        // impl $crate::quil::Quil for &$type {
        //     fn write(&self, writer: &mut impl std::fmt::Write) -> $crate::quil::ToQuilResult<()> {
        //         write!(writer, "{}", self).map_err(Into::into)
        //     }
        // }
    };
}

/// Write a sequencer of Quil items to the given writer, joined with the provided `joiner`.
pub(crate) fn write_join_quil<I, T>(
    writer: &mut impl std::fmt::Write,
    values: I,
    joiner: &str,
    prefix: &str,
) -> Result<(), ToQuilError>
where
    I: IntoIterator<Item = T>,
    T: Quil,
{
    let mut iter = values.into_iter();
    if let Some(first) = iter.next() {
        write!(writer, "{}", prefix)?;
        first.write(writer)?;

        for value in iter {
            write!(writer, "{joiner}{prefix}")?;
            value.write(writer)?;
        }
    }
    Ok(())
}
