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

mod nonzero {
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

    fn extract(ob: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        ob.extract::<u64>().and_then(|value| {
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
pub(crate) fn from_sequence<T, U>(values: &Bound<'_, PyAny>) -> PyResult<Vec<U>>
    where
        for<'a, 'py> T: FromPyObject<'a, 'py>,
        U: From<T>
{
    values.try_iter()?
        .map(|i|
            i.and_then(|i|
                i.extract::<T>()
                .map_err(Into::into)
                .map(U::from))
                )
        .collect()
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
