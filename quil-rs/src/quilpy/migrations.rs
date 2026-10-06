//! Helpers for migrating Python APIs.

use pyo3::prelude::*;
#[cfg(feature = "stubs")]
use pyo3_stub_gen::PyStubType;

use crate::quilpy::errors::QuilPyValueError;

/// Accept one of two mutually exclusive parameters, converting them to a single type.
///
/// The first two arguments must be `Option` types.
/// If exactly one of them is `Some`, then it is the value that will be returned.
/// If both are `Some`, then this returns a [`QuilPyValueError::MutuallyExclusive`].
/// If both are `None`, then this returns a default value inferred for the return type.
///
/// If you need to convert the parameters, use a closure instead of just the parameter name.
/// If you need to specify a different default value, provide a closure that returns a `PyResult`.
///
/// As a special case, you can set the default argument as `None` to get `PyResult<Option<_>>`,
/// which will be `Ok(None)` if both parameters are `None` or `Ok(Some(_))` if not an `Err`;
/// the other arguments, if not closures, will be wrapped in `Ok(Some(...))` automatically.
/// In other words, these are all equivalent:
///
/// ```rust,ignore
/// let _: Option<_> = aliased!(a, b, None)?;
/// let _: Option<_> = aliased!(a,               |b| Ok(Some(b)), || Ok(None))?;
/// let _: Option<_> = aliased!(|a| Ok(Some(a)),  b,              || Ok(None))?;
/// let _: Option<_> = aliased!(|a| Ok(Some(a)), |b| Ok(Some(b)), || Ok(None))?;
/// ```
///
/// # Examples
///
/// Below is a function that shows the typical expected usage of this macro.
/// It accpets `foo("X")`, `foo(name="X")`, and `foo(nm="X")`,
/// but not `foo(name="X", nm="X")` nor `foo("X", nm="X")`.
/// The old `nm` parameter has been made keyword-only, so `foo("X", "X")` is also rejected.
/// If both are arguments `None`, the returns the default value for `String`, which is `""`.
/// Note that that permits `foo()`, `foo(name=None, nm=None)`, and `foo("X", nm=None)`;
/// use `@overload`s to provide type stubs that indicate the accepted parameter combinations.
///
/// ```rust,ignore
/// #[pyfunction(signature = (name=None, *, nm=None))]
/// fn foo(name: Option<String>, nm: Option<String>) -> PyResult<String> {
///     aliased!(name, nm)
/// }
/// ```
///
/// This is like the above example, but explicitly rejects both parameters being `None`:
///
/// ```rust,ignore
/// #[pyfunction(signature = (name=None, *, nm=None))]
/// fn foo(name: Option<String>, nm: Option<String>) -> PyResult<String> {
///     aliased!(name, nm, || Err(ValueError::new_err("need a `name`")))
/// }
/// ```
///
/// This shows the macro in its most general form:
///
/// ```rust,ignore
/// #[pyo3(signature = (name=None, *, nm=None))]
/// fn foo(&self, name: Option<String>, nm: Option<i64>) -> PyResult<String> {
///     aliased!(
///         |name| if name.is_empty() {
///             Err(errors::ValueError::new_err("`name` must not be empty"))
///         } else {
///             Ok(name)
///         },
///         |nm| { Ok(nm.to_string()) },
///         || Err(errors::ValueError::new_err("missing required parameter `name`"))
///     )
/// }
/// ```
macro_rules! aliased {
    (
        $(|)?$a:ident$(| $a_to_r:expr)?,
        $(|)?$b:ident$(| $b_to_r:expr)?
    $(, $($default:tt)*)?) => {
        $crate::quilpy::migrations::aliased_impl(
            $a,
            $b,
            $crate::quilpy::migrations::aliased!(@conv    [$($($default)*)?] |$a| $($a_to_r)?),
            $crate::quilpy::migrations::aliased!(@conv    [$($($default)*)?] |$b| $($b_to_r)?),
            $crate::quilpy::migrations::aliased!(@default [$($($default)*)?]),
            stringify!($a),
            stringify!($b),
        )
    };

    (@conv [None]        |$x:ident|        ) => { |$x| Ok(Some($x)) };
    (@conv [$($d:expr)?] |$x:ident|        ) => { |$x| Ok($x) };
    (@conv [$($d:expr)?] |$x:ident| $e:expr) => { |$x| $e };
    (@default [|| $default:expr]) => { || $default };
    (@default [   $default:expr]) => { || Ok($default) };
    (@default []                ) => { || Ok(Default::default()) };
}

pub(crate) use aliased;

#[doc(hidden)]
#[inline]
pub(crate) fn aliased_impl<A, B, R>(
    a: Option<A>,
    b: Option<B>,
    a_to_r: impl FnOnce(A) -> PyResult<R>,
    b_to_r: impl FnOnce(B) -> PyResult<R>,
    default: impl FnOnce() -> PyResult<R>,
    name_a: &'static str,
    name_b: &'static str,
) -> PyResult<R> {
    match (a, b) {
        (Some(a), None) => a_to_r(a),
        (None, Some(b)) => b_to_r(b),
        (None, None) => default(),
        (Some(_), Some(_)) => {
            Err(QuilPyValueError::MutuallyExclusive(name_a.to_string(), name_b.to_string()).into())
        }
    }
}

/// A helper enum to migrate parameters from one signature to another.
///
/// Both `New` and `Old` must implement [`FromPyObject`].
/// If they both implement [`PyStubType`], then so does `Migrate<New, Old>`.
///
/// Typically, you'll want to wrap this in [`Option`]
/// and specify `#[pyo3(signature = (...))]` on the function with default `None`s
/// in order to continue to accept callers that used keyword-based arguments;
/// You should also provide type stub `@overload`s for the different accepted signatures,
/// marking the old signatures with `@deprecated` in the docstring.
///
/// For examples, see the types in [`crate::instruction::quilpy`],
/// such as the constructor [`crate::instruction::PauliTerm::__new__`].
#[derive(FromPyObject)]
pub(crate) enum Migrate<New, Old> {
    New(New),
    Old(Old),
}

#[cfg(feature = "stubs")]
impl<New: PyStubType, Old: PyStubType> PyStubType for Migrate<New, Old> {
    fn type_input() -> pyo3_stub_gen::TypeInfo {
        New::type_input() | Old::type_input()
    }

    fn type_output() -> pyo3_stub_gen::TypeInfo {
        New::type_output() | Old::type_output()
    }
}
