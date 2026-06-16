use pyo3::{prelude::*, PyClass};
use rigetti_pyo3::create_init_submodule;

use crate::expression;
use crate::instruction;
use crate::program;
use crate::validation;
use crate::waveform;

pub(crate) mod errors;

create_init_submodule! {
    errors: [
        errors::QuilError,
        errors::ValueError,
        errors::ToQuilStringError,
        errors::PickleError
    ],
    submodules: [
        "expression": expression::quilpy::init_submodule,
        "instructions": instruction::quilpy::init_submodule,
        "program": program::quilpy::init_submodule,
        "validation": validation::quilpy::init_submodule,
        "waveform": waveform::quilpy::init_submodule
    ],
}

#[pymodule]
#[pyo3(name = "_quil")]
fn init_module(m: &Bound<'_, PyModule>) -> PyResult<()> {
    let py = m.py();
    init_submodule("quil._quil", py, m)?;

    waveform::sampling::quilpy::register_abcs(py)?;

    Ok(())
}

/// Add Python `to_quil` and `to_quil_or_debug` methods for types that implement
/// [`Quil`](crate::quil::Quil).
macro_rules! impl_to_quil {
    ($($name: ident),+ $(,)?) => {
        $(
            #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
            #[pyo3::pymethods]
            impl $name {
                #[pyo3(name = "to_quil")]
                fn py_to_quil(&self) -> pyo3::PyResult<String> {
                    Ok(self.to_quil()?)
                }

                #[pyo3(name = "to_quil_or_debug")]
                fn py_to_quil_or_debug(&self) -> String {
                    self.to_quil_or_debug()
                }
            }
        )*
    };
}

pub(crate) use impl_to_quil;

/// An alternative to [`PyAnyMethods::extract`] that only fails if there are multithreaded mutable
/// borrows; if casting would fail, it returns [`None`] instead.  This allows avoiding [the
/// performance hit due to allocation on `extract`
/// failing](https://pyo3.rs/v0.29.0/performance.html#extract-versus-cast).  It is for use in
/// situations where you want to handle failure internally to the Rust function; if you want a good
/// Python-facing error message, just use `extract`.
pub(crate) fn py_cast_and_borrow<'a, 'py, T: PyClass + FromPyObject<'a, 'py>>(
    obj: &'a Bound<'py, PyAny>,
) -> PyResult<Option<PyRef<'py, T>>> {
    obj.cast::<T>()
        .ok()
        .map(Bound::try_borrow)
        .transpose()
        .map_err(Into::into)
}

/// Like [`py_cast_and_borrow`], but clones the pointed-to value to get an owned version.
pub(crate) fn py_cast_and_clone<'a, 'py, T: PyClass + FromPyObject<'a, 'py> + Clone>(
    obj: &'a Bound<'py, PyAny>,
) -> PyResult<Option<T>> {
    py_cast_and_borrow(obj).map(|obj| obj.as_deref().cloned())
}

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
/// ```
/// #[pyfunction(signature = (index, *, offset = None))]
/// fn get_at(&self, py: Python<'_>, index: u64, offset: Option<u64>) -> PyResult<()> {
///     py_deprecated!(py, c"`index` is deprecated; use `offset` instead")?;
///     Ok(())
/// }
/// ```
///
/// Optionally, you can set the `level` for [pyo3::PyErr::warn] as a third parameter:
///
/// ```
/// #[pyfunction(signature = (index, *, offset=None))]
/// fn wrapper(py: Python<'_>, index: u64, offset: Option<u64>) -> PyResult<()> {
///     wrapped(py, index, offset)
///     Ok(())
/// }
///
/// fn wrapped(&self, py: Python<'_>, index: u64, offset: Option<u64>) -> PyResult<()> {
///     py_deprecated!(py, c"`index` is deprecated; use `offset` instead", 1)?;
///     Ok(())
/// }
/// ```
macro_rules! py_deprecated {
    ($py: ident, $message: expr) => {
        py_deprecated!($py, $message, 1)
    };

    ($py: ident, $message: expr, $level: expr) => {
        pyo3::PyErr::warn(
            $py,
            &<pyo3::exceptions::PyDeprecationWarning as pyo3::PyTypeInfo>::type_object($py),
            $message,
            $level,
        )
    }
}

pub(crate) use py_deprecated;

/// Warn that a parameter is deprecated.
///
/// # Usage
///
/// Give a `Python` token, the name of the deprecated parameter, and the replacement:
///
/// ```
/// #[pyfunction(signature = (new_name, *, old_name=None))]
/// fn add_one(py: Python<'_>, new_name: i64, old_name: Option<u64>) -> PyResult<i64> {
///     let value = if let Some(old) = old_name {
///         deprecated_param!(py, old_name, new_name)?;
///         i64::try_from(old)?
///     } else {
///         new_name
///     };
///
///     Ok(value + 1)
/// }
/// ```
macro_rules! deprecated_param {
    ($py: ident, $old_param: expr, $new_param: expr) => {
        deprecated_param!($py, $old_param, $new_param, 1)
    };

    ($py: ident, $old_param: expr, $new_param: expr, $level: expr) => {
        py_deprecated!(
            $py,
            pyo3::ffi::c_str!(concat!(
                "`", stringify!($old_param), "`",
                " is deprecated; use ",
                "`", stringify!($new_param), "`",
                " instead"
            )),
            $level
        )
    };
}

pub(crate) use deprecated_param;

#[cfg(feature = "stubs")]
pub(crate) mod stubs {
    use pyo3_stub_gen::{module_doc, reexport_module_members};

    // During stub generation, these `quil._quil` modules and contents
    // will be re-exported into the `quil` module namespace.
    reexport_module_members!("quil" from "quil._quil");
    reexport_module_members!("quil.instructions" from "quil._quil.instructions");
    reexport_module_members!("quil.expression" from "quil._quil.expression");
    reexport_module_members!("quil.program" from "quil._quil.program");
    reexport_module_members!("quil.validation" from "quil._quil.validation");
    reexport_module_members!("quil.validation.identifier" from "quil._quil.validation.identifier");
    reexport_module_members!("quil.waveform" from "quil._quil.waveform");
    reexport_module_members!("quil.waveform.sampling" from "quil._quil.waveform.sampling");

    module_doc!(
        "quil._quil",
        r#"
        The `quil` package provides tools for constructing, manipulating,
        parsing, and printing [Quil](https://github.com/quil-lang/quil) programs.

        ⚠️ This package is still in early development
        and breaking changes should be expected between minor versions.
        "#
    );
}

#[cfg(feature = "stubs")]
pyo3_stub_gen::define_stub_info_gatherer!(stub_info);
