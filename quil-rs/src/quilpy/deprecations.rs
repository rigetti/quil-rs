//! Helper macros for deprecating items in a crate.

use pyo3::prelude::*;

/// Raise a deprecation warning.
///
/// This expands into [pyo3::PyErr::warn], which may return a [pyo3::PyErr],
/// so when using the macro, you should return a [Result], and likely use `?`.
/// If in the Python interpreter warnings are raised as exceptions,
/// this will return an `Err`, and function execution will stop.
///
/// # Example
///
/// Pass a `Python` token and a `C`-string message:
///
/// ```ignore
/// use pyo3::prelude::*;
/// use quil_rs::quilpy::py_deprecated;
///
/// #[pyclass] struct Foo(Vec<u64>);
///
/// #[pymethods]
/// impl Foo {
///     #[pyo3(signature = (index, *, offset = None))]
///     fn get_at(&self, py: Python<'_>, index: usize, offset: Option<usize>) -> PyResult<u64> {
///         let index = if let Some(offset) = offset {
///             py_deprecated!(py, c"`offset` is deprecated; use `index` instead")?;
///             offset
///         } else {
///             index
///         };
///
///         self.0.get(index).cloned()
///             .ok_or_else(|| pyo3::exceptions::PyIndexError::new_err("index out of bounds"))
///     }
/// }
/// ```
///
/// Optionally, you can set the `level` for [pyo3::PyErr::warn] as a third parameter.
macro_rules! py_deprecated {
    ($py: ident, $message: expr) => {
        $crate::quilpy::deprecations::py_deprecated!($py, $message, 1)
    };

    ($py: ident, $message: expr, $level: expr) => {
        pyo3::PyErr::warn(
            $py,
            &<pyo3::exceptions::PyDeprecationWarning as pyo3::PyTypeInfo>::type_object($py),
            $message,
            $level,
        )
    };
}

/// Generate the `cstr` for a deprecation warning message about parameters.
macro_rules! deprecated_param_cstr {
    (old=$old: ident) => {
        $crate::quilpy::deprecations::_deprecated_param_cstr(concat!(
            "`",
            stringify!($old),
            "` is deprecated and will be removed in the future\0",
        ))
    };

    (new=$new: ident, old=$old: ident) => {
        $crate::quilpy::deprecations::_deprecated_param_cstr(concat!(
            "`",
            stringify!($old),
            "` is deprecated; use `",
            stringify!($new),
            "` instead\0"
        ))
    };

    (old=$old: ident, new=$new: ident) => {
        $crate::quilpy::deprecations::deprecated_param_cstr!(new = $new, old = $old)
    };
}

/// Warn that a parameter is deprecated.
///
/// This macro just formats a standardized warning message and constructs the error type.
/// See [`deprecated_or_new!`] for the common case of choosing between a new or old value
/// and issuing a warning when the old value is used;
/// See [`deprecated_param_cstr!`] for details how the message is formatted.
/// See [`py_deprecated!`] for more details on how the warning is raised.
///
/// # Usage
///
/// Give a `Python` token, the name of the deprecated parameter, and the replacement:
///
/// ```ignore
/// use pyo3::prelude::*;
/// use quil_rs::quilpy::deprecated_param;
///
/// #[pyfunction(signature = (new_name, *, old_name=None))]
/// fn add_one(py: Python<'_>, new_name: i64, old_name: Option<u64>) -> PyResult<i64> {
///     let value = if let Some(old) = old_name {
///         deprecated_param!(py, new=new_name, old=old_name)?;
///         i64::try_from(old)?
///     } else {
///         new_name
///     };
///
///     Ok(value + 1)
/// }
/// ```
macro_rules! deprecated_param {
    ($py: ident, old=$old_param: ident, level=$level: expr) => {
        $crate::quilpy::deprecations::py_deprecated!(
            $py,
            $crate::quilpy::deprecations::deprecated_param_cstr!(old=$old_param),
            $level
        )
    };

    ($py: ident, new=$new_param: ident, old=$old_param: ident, level=$level: expr) => {
        $crate::quilpy::deprecations::py_deprecated!(
            $py,
            $crate::quilpy::deprecated_param_cstr!(new=$new_param, old=$old_param),
            $level
        )
    };

    // The macro is called like a function with keyword-only parameters,
    // Either order of `new` and `old` is fine; `level` is optional (defaults to 1).
    ($py: ident, old=$old_param: ident) => {
        $crate::quilpy::deprecations::deprecated_param!($py, old = $old_param, level = 1)
    };
    ($py: ident, new=$new_param: ident, old=$old_param: ident) => {
        $crate::quilpy::deprecations::deprecated_param!($py, new = $new_param, old = $old_param, level = 1)
    };
    ($py: ident, old=$old_param: ident, new=$new_param: ident $(, level=$level: expr)?) => {
        $crate::quilpy::deprecations::deprecated_param!($py, new = $new_param, old = $old_param $(, level = $level)?)
    };
}

/// Return the new parameter, or if the old parameter is present,
/// warn that it's deprecated and return it instead.
///
/// The `new` parameter must be a `T`, and by default the `old` parameter must be an `Option<T>`,
/// but you can provide a closure `Fn(U) -> PyResult<T>` to convert the `old` parameter to a `T`.
/// If you provide such a closure, you must call this as `(new=$name, old=$old, |$old| { ... })`.
///
/// # Example
///
/// ```ignore
/// use pyo3::prelude::*;
/// use quil_rs::quilpy::deprecated_or_new;
///
/// #[pyfunction(signature = (new_name, *, old_name=None))]
/// fn add_one(py: Python<'_>, new_name: i64, old_name: Option<i64>) -> PyResult<i64> {
///     let value = deprecated_or_new!(py, new=new_name, old=old_name)?;
///     Ok(value + 1)
/// }
/// ```
///
/// # Using a Closure to Convert the Old Parameter
///
/// ```ignore
/// use pyo3::prelude::*;
/// use quil_rs::quilpy::deprecated_or_new;
///
/// #[pyfunction(signature = (new_name, *, old_name=None))]
/// fn add_one(py: Python<'_>, new_name: i64, old_name: Option<u64>) -> PyResult<i64> {
///     let value = deprecated_or_new!(py, new=new_name, old=old_name, |old| { i64::try_into(old) })?;
///     Ok(value + 1)
/// }
/// ```
macro_rules! deprecated_or_new {
    ($py: ident, new=$new_param: ident, old=$old_param: ident, |$old:ident| $if_old:expr) => {
        $crate::quilpy::deprecations::_deprecated_or_new(
            $py,
            $new_param,
            $old_param,
            |$old| $if_old,
            $crate::quilpy::deprecations::deprecated_param_cstr!(
                new = $new_param,
                old = $old_param
            ),
        )
    };

    // Use the identity function for the old parameter if no conversion is provided.
    ($py: ident, new=$new_param: ident, old=$old_param: ident) => {
        $crate::quilpy::deprecations::_deprecated_or_new(
            $py,
            $new_param,
            $old_param,
            |old| Ok(old),
            $crate::quilpy::deprecations::deprecated_param_cstr!(
                new = $new_param,
                old = $old_param
            ),
        )
    };

    // Allow the order of `new` and `old` to be swapped, for convenience.
    ($py: ident, old=$old_param: ident, new=$new_param: ident) => {
        $crate::quilpy::deprecations::deprecated_or_new!($py, new = $new_param, old = $old_param)
    };
}

/// Private const-helper to generate a [`CStr`] for a deprecation warning message about parameters.
///
/// Use the [`deprecated_param_cstr!`] macro instead of this function directly,
/// as it will `stringify!` the parameter names and add a null terminator.
#[doc(hidden)]
pub(crate) const fn _deprecated_param_cstr(s: &str) -> &core::ffi::CStr {
    match core::ffi::CStr::from_bytes_with_nul(s.as_bytes()) {
        Ok(cstr) => cstr,
        Err(_) => panic!("CStr for deprecation warning message must have single null at the end"),
    }
}

#[doc(hidden)]
/// Private helper that returns an old parameter and issues a warning if the old parameter is used;
/// otherwise, returns the new parameter as given.
pub(crate) fn _deprecated_or_new<'py, T, U>(
    py: Python<'py>,
    new: T,
    old: Option<U>,
    convert_old: impl FnOnce(U) -> PyResult<T>,
    message: &core::ffi::CStr,
) -> PyResult<T> {
    if let Some(old) = old {
        py_deprecated!(py, message)?;
        Ok(convert_old(old)?)
    } else {
        Ok(new)
    }
}

pub(crate) use deprecated_or_new;
pub(crate) use deprecated_param;
pub(crate) use deprecated_param_cstr;
pub(crate) use py_deprecated;
