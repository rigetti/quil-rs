use std::marker::PhantomData;

use pyo3::types::PyType;
use pyo3::{prelude::*, pyclass::boolean_struct::True, types::PyTuple, PyClass, PyTypeCheck};
use rigetti_pyo3::create_init_submodule;

#[cfg(feature = "stubs")]
use pyo3_stub_gen::PyStubType;

use crate::expression;
use crate::instruction;
use crate::program;
use crate::validation;
use crate::waveform;

pub(crate) mod deprecations;
pub(crate) mod errors;
pub(crate) mod migrations;
pub(crate) mod singleton;

pub(crate) use deprecations::{
    deprecated_or_new, deprecated_param, deprecated_param_cstr, py_deprecated,
};
pub(crate) use migrations::{aliased, Migrate};

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

    instruction::quilpy::post_init(&py.import("quil._quil.instructions")?)?;
    expression::quilpy::post_init(&py.import("quil._quil.expression")?)?;
    program::quilpy::post_init(&py.import("quil._quil.program")?)?;

    Ok(())
}

/// Construct a union of Python types.
///
/// # Example
///
/// To add a runtime type alias to a module, include the following in its initializer:
///
/// ```
/// use pyo3::{prelude::*, types::PyType};
///
/// #[pyclass]
/// struct Type1;
///
/// #[pyclass]
/// struct Type2;
///
/// #[pymodule]
/// fn my_module(m: &Bound<'_, PyModule>) -> PyResult<()> {
///     m.add_class::<Type1>()?;
///     m.add_class::<Type2>()?;
///
///     m.add("SomeUnionType", quil_rs::quilpy::union_type(&[
///         &m.py().get_type::<Type1>(),
///         &m.py().get_type::<Type2>(),
///     ])?)?;
///
///     Ok(())
/// }
/// ```
///
/// This is equivalent to this Python (written in 3.12+ syntax):
///
/// ```python
/// class Type1: pass
/// class Type2: pass
/// type SomeUnionType = Type1 | Type2
/// ```
///
/// # Note
///
/// This panics during compilation if `types` is empty, since a union of no types is not valid.
pub(crate) fn union_type<'py, const N: usize>(
    types: &[&Bound<'py, PyType>; N],
) -> PyResult<Bound<'py, PyAny>>
where
    [u32; N]: Sized,
{
    assert!(
        N > 0,
        "Cannot create a union of zero types; at least one type is required"
    );
    types[1..]
        .iter()
        .try_fold(types[0].clone().into_any(), |u, t| u.bitor(t))
}

/// Construct a union of Python types.
macro_rules! union {
    ($py:ident, $($T:ty),+) => {
        crate::quilpy::union_type(&[ $(&$py.get_type::<$T>()),+ ])
    };
}
pub(crate) use union;

#[cfg(test)]
mod test_union {
    use pyo3::{
        prelude::*,
        types::{PyString, PyTuple},
    };

    use super::union_type;

    #[test]
    fn test_union_type() {
        Python::initialize();
        Python::attach(|py| {
            let type1 = py.get_type::<PyString>();
            let type2 = py.get_type::<PyTuple>();
            let union = union_type(&[&type1, &type2]).unwrap();
            assert!(PyTuple::empty(py).is_instance(&union).unwrap());
            assert!(PyString::new(py, "").is_instance(&union).unwrap());
        });
    }
}

pub(crate) mod nonzero {
    /// A simple wrapper around [`std::num::NonZeroU64`] with [`pyo3_stub_gen::PyStubType`] information.
    #[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Ord, Eq, Hash, pyo3::IntoPyObject)]
    #[cfg_attr(test, derive(proptest_derive::Arbitrary))]
    pub struct NonZeroU64(pub std::num::NonZeroU64);

    impl From<NonZeroU64> for u64 {
        fn from(value: NonZeroU64) -> Self {
            value.0.get()
        }
    }

    impl std::fmt::Display for NonZeroU64 {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            self.0.fmt(f)
        }
    }

    impl NonZeroU64 {
        pub const fn new(value: u64) -> Option<Self> {
            match std::num::NonZeroU64::new(value) {
                Some(nonzero) => Some(Self(nonzero)),
                None => None,
            }
        }

        pub fn get(&self) -> u64 {
            self.0.get()
        }
    }
}

#[cfg(test)]
pub use nonzero::NonZeroU64;
#[cfg(not(test))]
pub(crate) use nonzero::NonZeroU64;

// PyO3 has a conversion we could derive from,
// but it raises a TypeError that says "failed to extract field NonZeroU64.0".
// By implementing it manually, an invalid value instead reads:
// "quil.QuilValueError: expected a positive value".
impl<'a, 'py> pyo3::FromPyObject<'a, 'py> for NonZeroU64 {
    type Error = pyo3::PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        obj.extract::<u64>().and_then(|value| {
            std::num::NonZeroU64::try_from(value)
                .map_err(|_err| errors::ValueError::new_err("expected a positive value"))
                .map(Self)
        })
    }
}

#[cfg(feature = "stubs")]
impl pyo3_stub_gen::PyStubType for NonZeroU64 {
    fn type_output() -> pyo3_stub_gen::TypeInfo {
        pyo3_stub_gen::TypeInfo::builtin("int")
    }
}

/// Wrapper for `Vec<U>` that extracts items using `T` as [`FromPyObject`] and `From<T> for U`.
///
/// # Motivation
///
/// Its main use is with [`pickleable_new!`] and syntax `field: Vec<U> as Seq<T, U>`.
/// Doing so will allow Python users to pass a sequence of things that extract into `T`,
/// _and_ will generate type stubs for `Vec<T>` rather than for `Vec<U>`.
///
/// By way of example, suppose we have a struct with a field typed as `Vec<Expression>`,
/// but we want to let Python users pass in a list containing `Expressions`
/// or anything we know how to convert into an `Expression` (such as `MemoryReference` or `int`s).
/// We have an `ExpressionLike` enum that handles the extraction for us,
/// so we could type the parameter just as `Vec<ExpressionLike>`.
/// That has two downsides, though:
///
/// 1. We'll build up an entire `Vec<ExpressionLike>` just to convert it to a `Vec<Expression>`.
/// 2. We can't use it with `pickleable_new!`, since the parameter is actually `Vec<Expression>`.
///
/// We can work around both these issues by typing the field `Vec<Expression>`
/// and annotating it with `#[pyo3(from_py_with = from_sequence::<ExpressionLike, _>)]`
/// for some function `from_sequence` that does this conversion for us;
/// however, because the parameter type is `Vec<Expression>`, the stubs will be wrong.
///
/// We can't work around that in `pickleable_new!` using `as Vec<ExpressionLike>`
/// because it would require a `From<Vec<ExpressionLike>> for Vec<Expression>` implementation,
/// and even if that implementation existed, we'd be back with downside 1 from above.
///
/// We _could_ use `#[gen_stub(override_type(type_repr = "typing.Sequence[...]", imports=("typing")))],
/// but that's verbose and error-prone (since the `...` must be a string literal).
///
/// This type solves it by extracting (with possible conversions) into `Vec<U>`,
/// provides a `PyStubType` implementation matching `Vec<U>`,
/// and provides an `From<Seq<T, U>> for Vec<T>` implementation:
/// you can use it with `pickleable_new!` by writing `field: Vec<T> as Seq<U, T>`,
/// and you'll get a reasonably efficient extraction with the correct type stubs.
///
/// # Usage
///
/// For real use-cases, look for instances that accept `Vec<ExpressionLike>` or `Vec<QubitLike>`
/// within the [`crate::instructions::quilpy`] module.
///
/// This is most useful with `pickleable_new!` for a field with type `Vec<U>`
/// that should be extracted using the [`FromPyObject`] implementation of `T`:
///
/// ```rust,ignore
/// pickleable_new! {
///     impl MyStruct {
///         fn __new__(foos: Vec<Foo> as Seq<FooLike>);
///     }
/// }
/// ```
#[derive(Debug)]
pub(crate) struct Seq<T, U>(Vec<U>, PhantomData<T>);

impl<'py, T, U> FromPyObject<'_, 'py> for Seq<T, U>
where
    for<'a> T: FromPyObject<'a, 'py> + TryInto<U>,
    U: PyClass + PyTypeCheck + ToOwned<Owned = U>,
    for<'a> <T as TryInto<U>>::Error: Into<PyErr>,
{
    type Error = PyErr;

    fn extract(obj: Borrowed<'_, 'py, PyAny>) -> Result<Self, Self::Error> {
        let mut res = Vec::with_capacity(obj.len().unwrap_or(0));
        for item in obj.try_iter()? {
            let item = item?;
            let item = if let Ok(item) = item.cast::<U>() {
                item.borrow().to_owned()
            } else {
                item.extract::<T>()
                    .map_err(Into::into)?
                    .try_into()
                    .map_err(Into::into)?
            };
            res.push(item);
        }

        Ok(Self(res, PhantomData))
    }
}

impl<T, U> Seq<T, U> {
    pub const fn new() -> Self {
        Self(Vec::new(), PhantomData)
    }
}

impl<T, U> Default for Seq<T, U> {
    fn default() -> Self {
        Self::new()
    }
}

// It's unlikely to be useful to return this type,
// but this implementation is occassionally necessary for the stub generator to work right.
impl<'py, T, U> IntoPyObject<'py> for Seq<T, U>
where
    U: IntoPyObject<'py>,
{
    type Target = PyAny;
    type Output = Bound<'py, Self::Target>;
    type Error = PyErr;

    fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
        self.0.into_pyobject(py)
    }
}

impl<T, U> From<Seq<T, U>> for Vec<U> {
    fn from(val: Seq<T, U>) -> Self {
        val.0
    }
}

#[cfg(feature = "stubs")]
impl<T: PyStubType, U> pyo3_stub_gen::PyStubType for Seq<T, U> {
    fn type_input() -> pyo3_stub_gen::TypeInfo {
        <Vec<T> as PyStubType>::type_input()
    }

    fn type_output() -> pyo3_stub_gen::TypeInfo {
        <Vec<T> as PyStubType>::type_output()
    }
}

/// Extracts as `U`, stubs like `T`.
///
/// When you use `Like<T, U>` as a parameter in a function exposed to Python,
/// the function can accept existing instances of Python objects of (Rust) type `U`,
/// as well as Python data that can extract to `T` and convert into `U`.
#[derive(Debug, Copy, Clone)]
pub(crate) struct Like<T, U>(pub U, PhantomData<T>);

impl<'a, 'py, T, U> FromPyObject<'a, 'py> for Like<T, U>
where
    T: FromPyObject<'a, 'py> + TryInto<U>,
    U: PyClass + PyTypeCheck + ToOwned<Owned = U>,
    <T as TryInto<U>>::Error: Into<PyErr>,
{
    type Error = PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        let val = if let Ok(obj) = obj.cast::<U>() {
            obj.borrow().to_owned()
        } else {
            obj.extract::<T>()
                .map_err(Into::into)?
                .try_into()
                .map_err(Into::into)?
        };
        Ok(Self(val, PhantomData))
    }
}

#[cfg(feature = "stubs")]
impl<T, U: pyo3_stub_gen::runtime::PyRuntimeType> pyo3_stub_gen::runtime::PyRuntimeType
    for Like<T, U>
{
    fn runtime_type_object(py: Python<'_>) -> PyResult<Bound<'_, PyAny>> {
        <U as pyo3_stub_gen::runtime::PyRuntimeType>::runtime_type_object(py)
    }
}

impl<'py, T, U> IntoPyObject<'py> for Like<T, U>
where
    U: IntoPyObject<'py>,
{
    type Target = U::Target;
    type Output = U::Output;
    type Error = U::Error;

    fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
        self.0.into_pyobject(py)
    }
}

#[cfg(feature = "stubs")]
impl<T: PyStubType, U> pyo3_stub_gen::PyStubType for Like<T, U> {
    fn type_input() -> pyo3_stub_gen::TypeInfo {
        <T as PyStubType>::type_input()
    }

    fn type_output() -> pyo3_stub_gen::TypeInfo {
        <T as PyStubType>::type_output()
    }
}

macro_rules! impl_newargs {
    ($name:ident = $($typ:ty)|+) => {
        pub(crate) struct $name;
        #[cfg(feature = "stubs")]
        pyo3_stub_gen::impl_stub_type!($name = $($typ)|+);
    }
}
pub(crate) use impl_newargs;

pub(crate) trait IntoNewArgs<'py, T> {
    fn into_new_args(self, py: Python<'py>) -> PyResult<NewArgs<'py, T>>;
}

impl<'py, T, U> IntoNewArgs<'py, T> for U
where
    U: IntoPyObject<'py>,
{
    fn into_new_args(self, py: Python<'py>) -> PyResult<NewArgs<'py, T>> {
        Ok(NewArgs((self,).into_pyobject(py)?, PhantomData))
    }
}

/// A wrapper around a `Bound<'py, PyTuple>` with a `PyStubType` implementation
/// that returns `tuple[T]` for the type `T` that the tuple is intended to construct.
///
/// When `__getnewargs__` needs to return different types depending on the value of `self`
/// (in particular, when `self` is an enumerated type with multiple variants),
/// set the return type of `__getnewargs__` to `PyResult<NewArgs<'py, T>>`,
/// where `T` is a type that has a `PyStubType` implementation as the union return types;
/// You can use `impl_newargs!(T = A | B | ...)` to generate one as an empty struct type.
///
/// To easily construct the return value, you can use the `IntoNewArgs` trait,
/// which is implemented for any type that implements `IntoPyObject`:
/// simply call `value.into_new_args(py)` on the value you want to return,
/// and it will be converted into a `PyTuple` containing just that value.
pub(crate) struct NewArgs<'py, T>(Bound<'py, PyTuple>, PhantomData<T>);

impl<'py, T> NewArgs<'py, T> {
    pub(crate) fn new<U>(py: Python<'py>, value: U) -> PyResult<Self>
    where
        U: IntoPyObject<'py>,
    {
        Ok(NewArgs((value,).into_pyobject(py)?, PhantomData))
    }
}

impl<'py, T> IntoPyObject<'py> for NewArgs<'py, T> {
    type Target = PyTuple;
    type Output = Bound<'py, Self::Target>;
    type Error = std::convert::Infallible;

    fn into_pyobject(self, _py: Python<'py>) -> Result<Self::Output, Self::Error> {
        Ok(self.0)
    }
}

#[cfg(feature = "stubs")]
impl<'py, T> pyo3_stub_gen::PyStubType for NewArgs<'py, T>
where
    T: pyo3_stub_gen::PyStubType,
{
    fn type_output() -> pyo3_stub_gen::TypeInfo {
        let pyo3_stub_gen::TypeInfo {
            name,
            mut import,
            source_module,
            type_refs,
        } = <T as pyo3_stub_gen::PyStubType>::type_output();

        import.insert("builtins".into());
        pyo3_stub_gen::TypeInfo {
            name: format!("builtins.tuple[{name}]"),
            import,
            source_module,
            type_refs,
        }
    }
}

/// Generate stubs for a wrapper around an enum type so that when used as an input parameter,
/// it shows the enum type and all its variants,
/// but if used as an output type, it just shows the variants.
///
/// The latter is particularly useful for `__getnewargs__`.
///
/// The initial parameter must be the enum type.
macro_rules! impl_stub_enum {
    ($T:ty = $base_ty:ty $(| $variant_ty:ty)*) => {
        #[cfg(feature = "stubs")]
        impl pyo3_stub_gen::PyStubType for $T {
            fn type_input() -> pyo3_stub_gen::TypeInfo {
                <$base_ty as pyo3_stub_gen::PyStubType>::type_input()
                    $(| <$variant_ty as pyo3_stub_gen::PyStubType>::type_input())*
            }

            fn type_output() -> pyo3_stub_gen::TypeInfo {
                $(<$variant_ty as pyo3_stub_gen::PyStubType>::type_output())|*
            }
        }

        #[cfg(feature = "stubs")]
        impl pyo3_stub_gen::runtime::PyRuntimeType for $T {
            fn runtime_type_object(py: Python<'_>) -> PyResult<Bound<'_, PyAny>> {
                <$base_ty as pyo3_stub_gen::runtime::PyRuntimeType>::runtime_type_object(py)
            }
        }
    };
}

pub(crate) use impl_stub_enum;

/// Add Python `to_quil` and `to_quil_or_debug` methods
/// for types that implement [`Quil`](crate::quil::Quil).
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
