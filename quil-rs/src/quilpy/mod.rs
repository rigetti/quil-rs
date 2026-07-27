use std::marker::PhantomData;

use pyo3::types::PyType;
use pyo3::{PyClass, PyTypeCheck, prelude::*, pyclass::boolean_struct::True, types::PyTuple};
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

    instruction::quilpy::post_init(m)?;
    expression::quilpy::post_init(m)?;
    program::quilpy::post_init(m)?;

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
pub(crate) fn union_type<'py, const N: usize>(types: &[&Bound<'py, PyType>; N]) -> PyResult<Bound<'py, PyAny>>
where [u32; N]: Sized
{
    assert!(N > 0, "Cannot create a union of zero types; at least one type is required");
    types[1..].iter().try_fold(types[0].clone().into_any(), |u, t| { u.bitor(t) })
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
    use pyo3::{prelude::*, types::{PyString, PyTuple}};

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

/// Convert a Python sequence to a Rust `Vec`, converting each item to type `T` and then to `U`.
///
/// Use this when you want to use a `Vec<U>` on the Rust side
/// but want to accept `Iterable[T]` on the Python side.
/// This is particularly useful when using `pickleable_new!` when a field is a `Vec<U>`,
/// as `pickleable_new!` expects the parameter types of `__new__` to match the fields types.
///
/// Note that `T` must implement `FromPyObject`, and `U` must implement `From<T>`.
///
/// # Example
///
/// As a motivating example, suppose you have a complex enum `T` you wish to expose to Python,
/// but you'd like to let Python users call methods both with an instance of the class,
/// or any of the subclass variants directly, without having to wrap them in the enum themselves.
/// You can define another enum `U` for the parameter type without exposing it to Python,
/// with the same variants as the original, plus a variant for `T` itself,
/// and then derive or implement `FromPyObject`.
/// Now you can use `U` as the parameter type for your methods,
/// and Python users can call those methods with any of the variants of `U`.
/// By adding a `From<U> for T` implementation, you can easily expose Python functions like this:
///
/// ```ignore
/// #[pymethods]
/// impl T {
///     #[pyo3(name = "foo")]
///     fn py_foo(&self, item: U) -> S {
///         self.foo(item.into())
///     }
/// }
/// ```
///
/// That's great, but it means writing explicit wrapper methods for each method you expose.
/// If you want to share implementations with existing Rust methods
/// or otherwise have functions that take `T` rather than `U`,
/// you'll need to tell PyO3 how to make the conversion,
/// which you can do with the `#[pyo3(from_py_with = ...)]` attribute on the parameter.
/// For a single instance of `T`, you can simply use `T::from`
///
/// ```ignore
/// use pyo3::{prelude::*, py_run, types::PyTuple};
/// use rigetti_pyo3::fix_complex_enums;
/// use quil_rs::{pickleable_new, quilpy::from_sequence};
///
/// #[derive(Clone, PartialEq, Eq)]
/// #[pyclass(module = "my_module")]
/// enum Item {
///     Int(i64),
///     Str(String),
/// }
///
/// #[pymethods]
/// impl Item {
///     // For `pickle` support.
///     fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
///         match self {
///             Item::Int(value) => (value,).into_pyobject(py),
///             Item::Str(value) => (value.clone(),).into_pyobject(py),
///         }
///     }
/// }
///
/// /// Parameter type for functions that take an `Item` or something that can be converted to one.
/// #[derive(Clone, pyo3::FromPyObject)]
/// enum ItemLike {
///     Item(Item),
///     Int(i64),
///     Str(String),
/// }
///
/// impl From<ItemLike> for Item {
///     fn from(item_like: ItemLike) -> Self {
///         match item_like {
///             ItemLike::Item(item) => item,
///             ItemLike::Int(value) => Item::Int(value),
///             ItemLike::Str(value) => Item::Str(value),
///         }
///     }
/// }
///
/// /// Return `True` if the `item` is equal to 42 or "42".
/// #[pyfunction]
/// fn is_42ish(item: ItemLike) -> bool {
///     match item.into() {
///         Item::Int(value) => value == 42,
///         Item::Str(ref value) => value == "42",
///     }
/// }
///
/// #[pyclass(module = "my_module")]
/// struct MyClass {
///     name: String,
///     items: Vec<Item>,
/// }
///
/// pickleable_new! {
///     impl MyClass {
///         fn __new__(
///             name: String,
///             #[pyo3(from_py_with = from_sequence::<ItemLike, _>)]
///             items: Vec<Item>,
///         ) -> MyClass {
///             Self { name, items }
///         }
///     }
/// }
///
/// #[pymethods]
/// impl MyClass {
///     fn has_item(&self, item: ItemLike) -> bool {
///         self.items.contains(&Item::from(item))
///     }
/// }
///
/// #[pymodule]
/// fn my_module(m: &Bound<'_, PyModule>) -> PyResult<()> {
///     m.add_class::<Item>()?;
///     m.add_class::<MyClass>()?;
///     m.add_function(pyo3::wrap_pyfunction!(is_42ish, m)?)?;
///     fix_complex_enums!(m.py(), Item);
///     Ok(())
/// }
///
/// pyo3::append_to_inittab!(my_module);
/// Python::initialize();
///
/// Python::attach(|py| {
///    let my_module = PyModule::import(py, "my_module").unwrap();
///    py_run!(py, my_module, r#"
///        from my_module import Item, MyClass, is_42ish
///        import pickle
///
///        item1 = Item.Int(42)
///        item2 = Item.Str("42")
///        assert is_42ish(item1)
///        assert is_42ish(item2)
///        assert not is_42ish(Item.Int(43))
///
///        my_class = MyClass("example", [item1, item2])
///        assert my_class.has_item(item1)
///        assert my_class.has_item(item2)
///
///        pickled = pickle.dumps(my_class)
///        unpickled = pickle.loads(pickled)
///        assert unpickled.has_item(item1)
///        assert unpickled.has_item(item2)
///    "#);
/// });
/// ```
pub(crate) fn from_sequence<'it, 'py, T, U>(sequence: &'it Bound<'py, PyAny>) -> PyResult<Vec<U>>
    where
        U: PyClass + PyTypeCheck + From<T> + ToOwned<Owned = U>,
        for<'a> T: FromPyObject<'a, 'py>,
        for<'a> PyErr: From<<T as FromPyObject<'a, 'py>>::Error>,
{
        let capacity = sequence.len()?;
        let mut res = Vec::with_capacity(capacity);
        for item in sequence.try_iter()? {
            res.push(from_like(&item?)?);
        }
        Ok(res)
}

/// Extract a Python object as a [`PyClass`] `U`, or if that fails,
/// route through [`Bound::extract`] and `U`.
///
/// In either case, the value is a cloned instance of the Python object.
pub(crate) fn from_like<'a, 'py, T, U>(obj: &'a Bound<'py, PyAny>) -> PyResult<U>
where
    T: FromPyObject<'a, 'py>,
    U: PyClass + PyTypeCheck + From<T> + ToOwned<Owned = U>,
    PyErr: From<<T as FromPyObject<'a, 'py>>::Error>,
{
    if let Ok(obj) = obj.cast::<U>() {
        Ok(obj.borrow().to_owned())
    } else {
        obj.extract::<T>().map(U::from).map_err(Into::into)
    }
}


#[cfg(test)]
mod wrapper_tests {
    use pyo3::prelude::*;
    use pyo3::types::{PyString, PyInt};

    use super::*;

    #[derive(Debug, Clone, Eq, PartialEq, FromPyObject)]
    #[pyclass(skip_from_py_object)]
    enum MyClass {
        Int(i64),
        Str(String),
    }

    #[pyfunction]
    fn foo(#[pyo3(from_py_with = any_variant::<MyClass>)] inst: MyClass) -> String {
        match inst {
            MyClass::Int(value) => format!("Int({value})"),
            MyClass::Str(value) => format!("Str({value})"),
        }
    }

    #[pyfunction]
    fn foo2(inst: Like<MyClass>) -> String {
        match inst.get() {
            MyClass::Int(value) => format!("Int({value})"),
            MyClass::Str(value) => format!("Str({value})"),
        }
    }

    #[test]
    fn test_any_variant_python_fn() {
        Python::initialize();
        Python::attach(|py| {
            #[allow(non_snake_case)]
            let MyClass = py.get_type::<MyClass>();
            let foo = pyo3::wrap_pyfunction!(foo2, py).unwrap();
            pyo3::py_run!(py, foo MyClass, r#"
                values = [foo("foo"), foo(MyClass.Str("foo")), "Str(foo)"]
                assert values[0] == values[1] == values[2], f"values: {values}"
                values = [foo(42), foo(MyClass.Int(42)), "Int(42)"]
                assert values[0] == values[1] == values[2], f"values: {values}"
            "#);
        });
    }

    #[test]
    fn test_any_variant_class() {
        Python::initialize();
        Python::attach(|py| {
            let py_str = PyString::new(py, "foo").into_any();
            let py_int = PyInt::new(py, 42).into_any();

            let my_str = MyClass::Str("foo".to_string());
            let my_int = MyClass::Int(42);
            assert_eq!(my_str, any_variant::<MyClass>(&py_str).unwrap());
            assert_eq!(my_int, any_variant::<MyClass>(&py_int).unwrap());

            let inst_str = MyClass::Str("foo".to_string()).into_pyobject(py).unwrap().into_any();
            let inst_int = MyClass::Int(42).into_pyobject(py).unwrap().into_any();
            assert_eq!(my_str, any_variant::<MyClass>(&inst_str).unwrap());
            assert_eq!(my_int, any_variant::<MyClass>(&inst_int).unwrap());
        });
    }
}

/// Extract a value of type `T` from a Python object,
/// or try to convert it from a variant that `T` supports.
///
/// PyO3's `FromPyObject` trait can automatically convert from a Python type
/// to the first variant of an enum that matches the Python type,
/// but when accepting `T` as a parameter, the input must be an instance of the enum.
///
/// If you want to be able to accept any of the variants of `T` directly as well,
/// you can use this function in a `#[pyo3(from_py_with = ...)]` attribute on the parameter,
/// and it will first try to extract `T` directly, and if that fails,
/// it will try to extract any of the variants of `T` and convert it to `T` with
///
/// This is useful when you have a complex enum `T`
/// that you want to be able to accept as a parameter in Python,
/// but you also want to allow users to pass in any of the variants of `T`
/// directly without having to wrap them in the enum themselves.
pub(crate) fn any_variant<'a, 'py, T>(obj: &'a Bound<'py, PyAny>) -> PyResult<T>
where
    T: FromPyObject<'a, 'py> + PyTypeCheck + PyClass + Clone,
    PyErr: From<<T as FromPyObject<'a, 'py>>::Error>
{
    if let Ok(obj) = obj.cast::<T>() {
        Ok(obj.borrow().to_owned())
    } else {
        obj.extract().map_err(Into::into)
    }
}


#[derive(Debug, Copy, Clone)]
pub(crate) enum Like<'a, 'py, T> {
    Borrowed(Borrowed<'a, 'py, T>),
    Extracted(T),
}

impl <'a, 'py, T> FromPyObject<'a, 'py> for Like<'a, 'py, T>
where
    T: PyClass + FromPyObject<'a, 'py>,
{
    type Error = <T as FromPyObject<'a, 'py>>::Error;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        obj.cast::<T>().map(Self::Borrowed).or_else(|_| obj.extract::<T>().map(Self::Extracted))
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

impl <'py, T, U> IntoNewArgs<'py, T> for U
where
    U: IntoPyObject<'py>,
{
    fn into_new_args(self, py: Python<'py>) -> PyResult<NewArgs<'py, T>> {
        Ok(NewArgs((self,).into_pyobject(py)?, PhantomData))
    }
}

pub(crate) struct NewArgs<'py, T>(Bound<'py, PyTuple>, PhantomData<T>);

impl<'py, T> NewArgs<'py, T> {
    pub(crate) fn new<U>(py: Python<'py>, value: U) -> PyResult<Self>
        where U: IntoPyObject<'py>
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
    where T: pyo3_stub_gen::PyStubType,
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
            name: format!("tuple[{name}]"),
            import,
            source_module,
            type_refs,
        }
    }
}

macro_rules! py_friendly_enum {
    (for $T:ty = $first_ty:ty $(| $variant_ty:ty)*) => {
        impl<'a, 'py> From<Like<'a, 'py, $T>> for $T {
            fn from(like: Like<'a, 'py, $T>) -> Self {
                like.into_inner()
            }
        }

        // Generate stubs so that, when `Like<_, _, T>` is used as an (input) parameter,
        // the stubs will show the parameter as `T | Variant1 | Variant2 | ...`
        // (or rather, as the union of their input stub types).
        //
        // The `type_output` is just a tuple of the variants' output stub types,
        // which is useful for `__getnewargs__`.
        #[cfg(feature = "stubs")]
        impl pyo3_stub_gen::PyStubType for Like<'_, '_, $T> {
            fn type_input() -> pyo3_stub_gen::TypeInfo {
                <$T as pyo3_stub_gen::PyStubType>::type_input()
                      | <$first_ty   as pyo3_stub_gen::PyStubType>::type_input()
                    $(| <$variant_ty as pyo3_stub_gen::PyStubType>::type_input())*
            }

            fn type_output() -> pyo3_stub_gen::TypeInfo {
                    <$first_ty   as pyo3_stub_gen::PyStubType>::type_output()
                $(| <$variant_ty as pyo3_stub_gen::PyStubType>::type_output())*
            }
        }

        // Implement `__new__` such that Python users can construct `T` directly from its variants.
        #[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
        #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
        #[pymethods]
        impl $T {
            #[new]
            #[pyo3(signature = (arg, /))]
            fn __new__<'py>(py: Python<'py>, arg: Like<$T>) -> PyResult<Bound<'py, $T>> {
                arg.into_inner().into_pyobject(py)
            }
        }
    };
}

pub(crate) use py_friendly_enum;

impl<'a, 'py, T> Like<'a, 'py, T> {
    pub(crate) fn into_inner(self) -> T
    where
        T: PyClass + std::borrow::ToOwned<Owned = T>,
    {
        match self {
            Self::Borrowed(b) => b.borrow().to_owned(),
            Self::Extracted(extracted) => extracted,
        }
    }

    pub(crate) fn extract<O>(self) -> Result<O, O::Error>
        where O: FromPyObject<'a, 'py> + From<T>,
    {
        match self {
            Self::Borrowed(b) => b.extract::<O>(),
            Self::Extracted(extracted) => Ok(extracted.into()),
        }
    }

    pub(crate) fn get(&self) -> &T
    where
        T: PyClass<Frozen = True> + Sync,
    {
        match self {
            Self::Borrowed(bound) => bound.get(),
            Self::Extracted(extracted) => extracted,
        }
    }
}

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
        $crate::quilpy::py_deprecated!($py, $message, 1)
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

/// Return the new parameter, or if the old parameter is present,
/// warn that it's deprecated and return it instead.
///
/// # Example
///
/// ```ignore
/// use pyo3::prelude::*;
/// use quil_rs::quilpy::deprecated_or_new;
///
/// #[pyfunction(signature = (new_name, *, old_name=None))]
/// fn add_one(py: Python<'_>, new_name: i64, old_name: Option<u64>) -> PyResult<i64> {
///     let value = deprecated_or_new!(py, old=old_name, new=new_name)?;
///     Ok(value + 1)
/// }
/// ```
macro_rules! deprecated_or_new {
    ($py: ident, old=$old_param: ident, new=$new_param: ident) => {
        deprecated_or_new!($py, new=$new_param, old=$old_param)
    };

    ($py: ident, new=$new_param: ident, old=$old_param: ident) => {
        $old_param.map_or(Ok($new_param), |old| {
            $crate::quilpy::deprecated_param!($py, new=$new_param, old=$old_param)?;
            Ok::<_, ::pyo3::PyErr>(old)
        })
    };
}

/// Warn that a parameter is deprecated.
///
/// See [`deprecated_or_new!`] for the common case of choosing between a new or old value.
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
///         deprecated_param!(py, new=old_name, old=new_name)?;
///         i64::try_from(old)?
///     } else {
///         new_name
///     };
///
///     Ok(value + 1)
/// }
/// ```
macro_rules! deprecated_param {
    ($py: ident, new=$new_param: ident, old=$old_param: ident) => {
        $crate::quilpy::deprecated_param!($py, new=$new_param, old=$old_param, level=1)
    };

    ($py: ident, new=$new_param: ident, old=$old_param: ident, level=$level: expr) => {
        $crate::quilpy::py_deprecated!(
            $py,
            ::pyo3::ffi::c_str!(concat!(
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
pub(crate) use deprecated_or_new;

// When building with the `stubs` feature,
// this generates the entrypoint used by our `stub_gen` binary.
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
