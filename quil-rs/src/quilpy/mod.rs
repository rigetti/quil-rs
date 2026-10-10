use std::marker::PhantomData;

use pyo3::exceptions::PyTypeError;
use pyo3::types::{DerefToPyAny, PyString, PyType};
use pyo3::PyTypeInfo;
use pyo3::{prelude::*, types::PyTuple, PyClass, PyTypeCheck};
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
pub(crate) mod memory;
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
/// # Basic Usage
///
/// Here's an example that uses a sequence of a Rust newtype wrapper type exposed as a `#[pyclass]`,
/// but permits Python users to pass a sequence of existing instances or of the base type:
///
/// ```rust,ignore
/// #[pyclass]
/// struct Parameter(String);
///
/// #[derive(FromPyObject, IntoPyObject)]
/// struct ParameterLike(Parameter);
/// impl_stub_type!(ParameterLike = Parameter | String);
/// impl From<ParameterLike> for Parameter {
///    fn from(value: ParameterLike) -> Self {
///        value.0
///    }
/// }
///
/// #[pyfunction]
/// // This is stubbed as `def foo(values: Sequence[Parameter | str]) -> None: ...`
/// fn foo(values: Seq<ParameterLike, Parameter>) {
///     // `Into<Vec<Parameter>>` is equivalent to accessing the inner `values.0`.
///     // No conversion happens here, because `Parameters` are extracted directly
///     // or created from an extracted `str`s at the moment of extraction,
///     // avoiding the intermediary `Vec<ParameterLike>`.
///     let values: Vec<Parameter> = values.into();
/// }
/// ```
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
/// # Other Examples
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

/// This is just like [`Seq`], but does not require `U: PyClass + PyTypeCheck` to extract.
///
/// This allows it to work with types that implement [`FromPyObject`] but are not [`PyClass`]es.
#[derive(Debug)]
pub(crate) struct Seq2<T, U>(Vec<U>, PhantomData<T>);

impl<'py, T, U> FromPyObject<'_, 'py> for Seq<T, U>
where
    for<'a> T: FromPyObject<'a, 'py> + TryInto<U>,
    U: PyClass + PyTypeCheck + ToOwned<Owned = U>,
    for<'a> <T as TryInto<U>>::Error: Into<PyErr>,
{
    type Error = PyErr;

    fn extract(obj: Borrowed<'_, 'py, PyAny>) -> Result<Self, Self::Error> {
        if obj.is_instance_of::<PyString>() {
            return Err(PyTypeError::new_err("expected iterable, but not `str`"));
        }

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

impl<'py, T, U> FromPyObject<'_, 'py> for Seq2<T, U>
where
    for<'a> T: FromPyObject<'a, 'py> + TryInto<U>,
    U: ToOwned<Owned = U>,
    for<'a> <T as TryInto<U>>::Error: Into<PyErr>,
{
    type Error = PyErr;

    fn extract(obj: Borrowed<'_, 'py, PyAny>) -> Result<Self, Self::Error> {
        if obj.is_instance_of::<PyString>() {
            return Err(PyTypeError::new_err("expected iterable, but not `str`"));
        }

        let mut res = Vec::with_capacity(obj.len().unwrap_or(0));
        for item in obj.try_iter()? {
            let item = item?
                .extract::<T>()
                .map_err(Into::into)?
                .try_into()
                .map_err(Into::into)?;
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

impl<T, U> Seq2<T, U> {
    pub const fn new() -> Self {
        Self(Vec::new(), PhantomData)
    }
}

impl<T, U> Default for Seq<T, U> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, U> Default for Seq2<T, U> {
    fn default() -> Self {
        Self::new()
    }
}

// It's unlikely to be useful to return this type,
// but this implementation is necessary for the stub generator to support it as a default argument.
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

impl<'py, T, U> IntoPyObject<'py> for Seq2<T, U>
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

impl<T, U> From<Seq2<T, U>> for Vec<U> {
    fn from(val: Seq2<T, U>) -> Self {
        val.0
    }
}

impl<T, U> IntoIterator for Seq<T, U> {
    type Item = <Vec<U> as IntoIterator>::Item;
    type IntoIter = <Vec<U> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T, U> IntoIterator for Seq2<T, U> {
    type Item = <Vec<U> as IntoIterator>::Item;
    type IntoIter = <Vec<U> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
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

#[cfg(feature = "stubs")]
impl<T: PyStubType, U> pyo3_stub_gen::PyStubType for Seq2<T, U> {
    fn type_input() -> pyo3_stub_gen::TypeInfo {
        <Vec<T> as PyStubType>::type_input()
    }

    fn type_output() -> pyo3_stub_gen::TypeInfo {
        <Vec<T> as PyStubType>::type_output()
    }
}

/// Extractor for Python arguments that may be a single instance or a sequence of instances.
pub(crate) enum OneOrMore<T, U = T> {
    One(U),
    More(Seq2<T, U>),
}

impl<'py, T, U> FromPyObject<'_, 'py> for OneOrMore<T, U>
where
    for<'a> T: FromPyObject<'a, 'py> + TryInto<U>,
    U: ToOwned<Owned = U>,
    for<'a> <T as TryInto<U>>::Error: Into<PyErr>,
{
    type Error = PyErr;

    fn extract(obj: Borrowed<'_, 'py, PyAny>) -> Result<Self, Self::Error> {
        if let Ok(obj) = obj.extract::<T>() {
            let item = obj.try_into().map_err(Into::into)?;
            return Ok(Self::One(item));
        }
        obj.extract::<Seq2<T, U>>().map(Self::More)
    }
}

#[cfg(feature = "stubs")]
impl<T, U> PyStubType for OneOrMore<T, U>
where
    T: PyStubType,
{
    fn type_input() -> pyo3_stub_gen::TypeInfo {
        <T as PyStubType>::type_input() | <Vec<T> as PyStubType>::type_input()
    }

    fn type_output() -> pyo3_stub_gen::TypeInfo {
        <T as PyStubType>::type_output() | <Vec<T> as PyStubType>::type_output()
    }
}

/// An iterator over a `OneOrMore<T, U>`.
pub(crate) enum OneOrMoreIter<U> {
    One(std::iter::Once<U>),
    More(std::vec::IntoIter<U>),
}

impl<T, U> IntoIterator for OneOrMore<T, U> {
    type Item = U;
    type IntoIter = OneOrMoreIter<U>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            OneOrMore::One(item) => OneOrMoreIter::One(std::iter::once(item)),
            OneOrMore::More(Seq2(items, _)) => OneOrMoreIter::More(items.into_iter()),
        }
    }
}

impl<U> Iterator for OneOrMoreIter<U> {
    type Item = U;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            OneOrMoreIter::One(iter) => iter.next(),
            OneOrMoreIter::More(vec) => vec.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            OneOrMoreIter::One(iter) => iter.size_hint(),
            OneOrMoreIter::More(vec) => vec.size_hint(),
        }
    }
}

/// Extracts as `U`, stubs like `T`.
///
/// When you use `Via<T, U>` as a parameter in a function exposed to Python,
/// the function can accept existing instances of Python objects of (Rust) type `U`,
/// as well as Python data that can extract to `T` and convert into `U`.
///
/// Moreover, it generates type stubs for `T` rather than `U`.
/// This leads to a useful idiom: if you internally need Rust type `Foo`,
/// but you want to let Python users pass additional types that can convert into `Foo`,
/// define a Rust type `FooLike` that implements [`FromPyObject`] and [`Into<Foo>`],
/// implement [`PyStubType`] for `FooLike` to generate the input/output type stubs you want,
/// and then you can use `Via<FooLike, Foo>` in your function:
/// the stubs will show `FooLike`'s type representation,
/// but the extraction will result in a `Via<FooLike, Foo>` instance, for which `.0` is a `Foo`.
/// You can get at that directly with `.0`, or take advantage of the [`Into<Foo>`] implementation.
#[derive(Debug, Copy, Clone)]
pub(crate) struct Via<T, U>(pub U, PhantomData<T>);

impl<T, U> Via<T, U> {
    pub(crate) fn into_inner(self) -> U {
        self.0
    }
}

impl<'a, 'py, T, U> FromPyObject<'a, 'py> for Via<T, U>
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
    for Via<T, U>
{
    fn runtime_type_object(py: Python<'_>) -> PyResult<Bound<'_, PyAny>> {
        <U as pyo3_stub_gen::runtime::PyRuntimeType>::runtime_type_object(py)
    }
}

impl<'py, T, U> IntoPyObject<'py> for Via<T, U>
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
impl<T: PyStubType, U> pyo3_stub_gen::PyStubType for Via<T, U> {
    fn type_input() -> pyo3_stub_gen::TypeInfo {
        <T as PyStubType>::type_input()
    }

    fn type_output() -> pyo3_stub_gen::TypeInfo {
        <T as PyStubType>::type_output()
    }
}

/// Generate a new ZST that stubs as the union of the given types.
///
/// # Example
///
/// ```rust,ignore
/// impl_newargs!(MyNewArgs = Type1 | Type2 | Type3);
/// ```
///
/// Expands to:
///
/// ```rust,ignore
/// pub(crate) struct MyNewArgs;
/// #[cfg(feature = "stubs")]
/// pyo3_stub_gen::impl_stub_type!(MyNewArgs = Type1 | Type2 | Type3);
/// ```
macro_rules! impl_newargs {
    ($name:ident = $($typ:ty)|+) => {
        pub(crate) struct $name;
        #[cfg(feature = "stubs")]
        pyo3_stub_gen::impl_stub_type!($name = $($typ)|+);
    }
}
pub(crate) use impl_newargs;

/// A trait that makes it simple to implement `__getnewargs__`.
///
/// Every type that implements [`IntoPyObject`] implements this trait,
/// so you can write a `__getnewargs__` method for an enum that looks like this:
///
/// ```rust,ignore
/// impl_getnewargs!(MyNewArgs = Variant1 | Variant2 | ...);
/// #[pymethods]
/// impl MyEnum {
///     fn __getnewargs__(&self, py: Python<'py>) -> PyResult<NewArgs<'py, MyNewArgs>> {
///         match self {
///             MyEnum::Variant1(v) => v.clone().into_newargs(py),
///             MyEnum::Variant2(v) => v.clone().into_newargs(py),
///             // ...
///         }
///     }
/// }
/// ```
pub(crate) trait IntoNewArgs<'py, T> {
    fn into_newargs(self, py: Python<'py>) -> PyResult<NewArgs<'py, T>>;
}

impl<'py, T, U> IntoNewArgs<'py, T> for U
where
    U: IntoPyObject<'py>,
{
    fn into_newargs(self, py: Python<'py>) -> PyResult<NewArgs<'py, T>> {
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

/// A trait for stating what Python type to check against before trying to extract a Rust type.
///
/// This, together with [`CastWrap`], is mainly to enable some macro magic below.
pub(crate) trait CastAs {
    type PyType: PyTypeCheck;
}

/// Wraps `T` in a type that can implement `FromPyObject`.
pub(crate) struct CastWrap<T>(pub(crate) T);

/// The errors that can be returned when attempting to use `CastWrap`.
pub(crate) enum CastAsError<'a, 'py> {
    Cast(pyo3::CastError<'a, 'py>),
    Extract(PyErr),
}

impl From<CastAsError<'_, '_>> for PyErr {
    fn from(err: CastAsError<'_, '_>) -> Self {
        match err {
            CastAsError::Cast(cast_err) => cast_err.into(),
            CastAsError::Extract(pyerr) => pyerr,
        }
    }
}

/// Blanket implementation of `CastAs` that sets `PyType = T` if `T` is [`IntoPyObject::Target`].
///
/// This avoids [`IntoPyObject::Target`] of [`PyAny`], by using [`DerefToPyAny`] as a bound,
/// since it's specifically NOT implemented for [`PyAny`] itself,
/// but is implemented fo basically every other Python object.
///
/// Note that if [`CastAs::PyType`] is [`PyAny`], then the `cast` will always succeed.
/// Since the point of [`CastAs`] is to avoid the [cost of of an `extract` chain][extract-cost],
/// using it as `PyType` would be counterproductive
/// (though not necessarily incorrect, just as inefficient as not using `CastAs` at all).
///
/// [extract-cost]: https://pyo3.rs/v0.29.3/performance.html#extract-versus-cast
impl<T: for<'py> IntoPyObject<'py, Target = U>, U: DerefToPyAny + PyTypeInfo> CastAs for T {
    type PyType = U;
}

// TODO: remove this dead code if it isn't needed.
/* Alternatively, this macro could be used to implement `CastAs` a bit more explicitly:
macro_rules! impl_cast_as {
    ($($PyType:ty),* $(,)?) => {
        $(
        impl<T: for<'py> IntoPyObject<'py, Target = $PyType>> CastAs for T {
            type PyType = $PyType;
        }
        )*
    };
}

impl_cast_as!(
    pyo3::types::PyInt,
    pyo3::types::PyString,
    pyo3::types::PyFloat,
    pyo3::types::PyComplex,
);
*/

/// Attempts to extract `T` from a Python object.
///
/// Succeeds if the object type checks to `T::PyType` and can be extracted into `T`.
impl<'a, 'py, T: CastAs + FromPyObjectOwned<'py>> TryFrom<Borrowed<'a, 'py, PyAny>> for CastWrap<T>
where
    <T as CastAs>::PyType: 'a,
{
    type Error = CastAsError<'a, 'py>;

    fn try_from(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        obj.cast::<T::PyType>()
            .map_err(CastAsError::Cast)?
            .extract::<T>()
            .map_err(|pyerr| CastAsError::Extract(pyerr.into()))
            .map(CastWrap)
    }
}

/// Generate stubs where the input type is all of `T1 | T2 | ...` but the output type is `T2 | ...`.
///
/// This is particularly useful for typing `__getnewargs__` for `enum`s:
/// the input shows it'll accept the enum type and all its variants,
/// while the output type shows it returns just one of the variants.
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

/// Generate a wrapper type around `T1` that accepts `T1 | T2 | ...` as input.
///
/// The stubs, when used as input type, show `T1 | T2 | ...` while the output shows only `T2 | ...`.
/// This is useful for types that are meant for flexible inputs that coerce to an internal type.
///
/// # Example
///
/// Suppose we have a newtype wrapper around `String`s exposed to Python as a `#[pyclass]`:
///
/// ```rust,ignore
/// #[derive(Debug, Clone, Eq, PartialEq)]
/// #[gen_stub_pyclass]
/// #[pyclass(from_py_object, eq)]
/// struct Parameter(String);
///
/// #[pymethods]
/// impl Parameter {
///     #[new]
///     fn new(value: String) -> Self {
///         Self(value)
///     }
/// }
///
/// #[derive(Debug, Clone)]
/// #[gen_stub_pyclass]
/// #[pyclass(from_py_object)]
/// struct Function {
///     parameters: Vec<Parameter>,
///     body: String,
/// }
///
/// #[pymethods]
/// impl Function {
///     #[new]
///     fn new(parameters: Vec<Parameter>, body: String) -> Self {
///         Self { parameters, body }
///     }
///
///     fn has_parameter(&self, parameter: Parameter) -> bool {
///         self.parameters.iter().any(|p| p.0 == parameter.0)
///     }
///
///     fn first_parameter(&self) -> Option<Parameter> {
///         self.parameters.first().cloned()
///     }
/// }
/// ```
///
/// For Python users, those `Function` methods are kind of annoying to use,
/// since they'll have to wrap every string in a `Parameter` instance:
///
/// ```python
/// func = Function(parameters=[Parameter("x"), Parameter("y")], body="...")
/// assert func.has_parameter(Parameter("x"))
/// assert not func.has_parameter(Parameter("z"))
/// assert func.first_parameter() == Parameter("x")
/// ```
///
/// We can make it easier on them by writing a [`FromPyObject`] implementation for `Parameter`
/// that accepts both `Parameter` instances and `str`s:
///
/// ```rust,ignore
/// impl<'a, 'py> FromPyObject<'a, 'py> for Parameter {
///     type Error = PyErr;
///     fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
///         if let Ok(param) = obj.cast::<Parameter>() {
///             Ok(param.borrow().to_owned())
///         } else {
///             obj.extract::<String>().map_err(Into::into).map(Parameter)
///         }
///     }
/// }
/// ```
///
/// Now this is accepted:
///
/// ```python
/// func = Function(parameters=["x", "y"], body="...")
/// assert func.has_parameter("x")
/// assert not func.has_parameter("z")
/// assert func.first_parameter() == Parameter("x")
/// assert func.first_parameter() != "x"  # The return type is intentionally `Parameter`.
/// ```
///
/// However, in order for the above code NOT to cause type errors,
/// we need the type stubs to show that _both_ `Parameter` and `str` are accepted,
/// yet we still want the return type of `first_parameter` to be just `Parameter`:
///
/// ```python
/// class Function:
///     def __new__(self, parameters: Sequence[Parameter | str], body: str) -> None: ...
///     def has_parameter(self, parameter: Parameter | str) -> bool: ...
///     def first_parameter(self) -> Parameter: ...
/// ```
///
/// Ideally, we'd be able to just implement the [`PyStubType`] for `Parameter` ourselves,
/// but unfortunately, we need to tell the stub generator about the `Parameter` type
/// in order to for it to correctly add stubs for its methods
/// (we did so with `#[gen_stub_pyclass]`, but we'd have the same problem if we did it manually),
/// and by doing so, the it'll end up with a blanket implementation of [`PyStubType`].
///
/// Moreover, the [`FromPyObject`] implementation for `Parameter` is pretty verbose,
/// and by virtue of having it, we _can't_ type a function in a way
/// that explicitly requires a `Parameter` instance but does not auto-convert `str`s.
///
/// To work around these issues, instead of implementing [`FromPyObject`] for `Parameter`,
/// we can instead define a new type that explicitly states what acts "like" a `Parameter`,
/// and then use that type in the function signatures that are meant to be more flexible.
///
/// We use this macro call:
///
/// ```rust,ignore
/// impl_stub_like!(ParameterLike = Parameter | String);
/// ```
///
/// And now our methods can be typed as follows:
///
/// ```rust,ignore
/// #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
/// #[pymethods]
/// impl Function {
///     // Bonus: use `Seq` to extract directly into `Vec<Parameter>` while using ParameterLike!
///     #[new]
///     fn new(parameters: Seq<ParameterLike, Parameter>, body: String) -> Self {
///         Self { parameters: parameters.into(), body }
///     }
///
///     fn has_parameter(&self, parameter: ParameterLike) -> bool {
///         let parameter: Parameter = parameter.into();
///         self.parameters.iter().any(|p| p == parameter)
///     }
///
///     fn first_parameter(&self) -> Option<Parameter> {
///         self.parameters.first().cloned()
///     }
/// }
/// ```
///
/// And we'll get the stubs we wanted from above.
macro_rules! impl_stub_like {
    (
        $(#[$meta:meta])*
        $vis:vis $T:ident = enum $base_ty:ty $(| $variant_ty:ty $(as $variant_name:ident)? )*) => {
        $crate::quilpy::impl_stub_enum!($T = $base_ty $(| $variant_ty)*);
        $crate::quilpy::impl_stub_like!(@initial $(#[$meta])* $vis $T = $base_ty $(| $variant_ty $(as $variant_name)? )*);
        $crate::quilpy::impl_stub_like!(@as_enum $vis $T = $base_ty $(| $variant_ty $(as $variant_name)? )*);
    };

    (
        $(#[$meta:meta])*
        $vis:vis $T:ident = $base_ty:ty $(| $variant_ty:ty $(as $variant_name:ident)? )*) => {
        $crate::quilpy::impl_stub_enum!($T = $base_ty $(| $variant_ty)*);
        $crate::quilpy::impl_stub_like!(@initial $(#[$meta])* $vis $T = $base_ty $(| $variant_ty $(as $variant_name)? )*);
        $crate::quilpy::impl_stub_like!(@no_enum $vis $T = $base_ty $(| $variant_ty $(as $variant_name)? )*);
    };

    (@initial $(#[$meta:meta])* $vis:vis $T:ident = $base_ty:ty $(| $variant_ty:ty $(as $variant_name:ident)? )*) => {
        $(#[$meta])*
        $vis struct $T($base_ty);

        impl From<$T> for $base_ty {
            fn from(value: $T) -> Self {
                value.0
            }
        }
    };

    (@as_enum $vis:vis $T:ident = $base_ty:ty $(| $variant_ty:ty $(as $variant_name:ident)? )*) => {
        impl<'a, 'py> FromPyObject<'a, 'py> for $T {
            type Error = PyErr;

            fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
                if let Ok(obj) = obj.cast::<$base_ty>() {
                    Ok(Self(obj.extract()?))
                }$( else if let Ok(obj) = $crate::quilpy::CastWrap::<$variant_ty>::try_from(obj) {
                    Ok(Self(
                        $crate::quilpy::impl_stub_like!(@extract_name $base_ty, $variant_ty $(, $variant_name)? )(obj.0)
                    ))
                })* else {
                    Err(CastError::new(obj, <$base_ty as pyo3::type_object::PyTypeCheck>::classinfo_object(obj.py())))?
                }
            }
        }
    };

    (@no_enum $vis:vis $T:ident = $base_ty:ty $(| $variant_ty:ty )*) => {
        impl<'a, 'py> FromPyObject<'a, 'py> for $T {
            type Error = PyErr;

            fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
                if let Ok(obj) = obj.cast::<$base_ty>() {
                    Ok(Self(obj.extract()?))
                }$( else if let Ok(obj) = $crate::quilpy::CastWrap::<$variant_ty>::try_from(obj) {
                    Ok(Self(
                        <$variant_ty as std::convert::Into<$base_ty>>::into(obj.0)
                    ))
                })* else {
                    Err(CastError::new(obj, <$base_ty as pyo3::type_object::PyTypeCheck>::classinfo_object(obj.py())))?
                }
            }
        }
    };

    // Form the variant name; allows it to be different from the type used for extraction.
    (@extract_name $base_ty:ty, $variant_ty:ty, $variant_name:ident) => { <$base_ty>::$variant_name };
    (@extract_name $base_ty:ty, $variant_ty:ty) => { <$base_ty>::$variant_ty };

}

pub(crate) use impl_stub_like;

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
