//! Traits and macros for defining singleton types in Python.

use pyo3::{prelude::*, PyClass};

/// A trait for types that should be used as singletons in Python.
///
/// For example, in Python there's a single `None` value of type `NoneType`.
/// You could model that in Rust as an empty struct type like so:
///
/// ```rust,ignore
/// #[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq)]
/// #[pyclass(frozen)]
/// struct NoneType;
///
/// impl PySingleton for NoneType {
///     const NAME: &'static str = "None";
///     fn get(py: Python<'_>) -> PyResult<&Bound<'_, Self>> {
///         static CELL: PyOnceLock<Py<NoneType>> = PyOnceLock::new();
///         CELL.get_or_try_init(py, || Py::new(py, NoneType))
///     }
/// }
///
/// #[pymodule]
/// fn builtins(m: &Bound<'_, PyModule>) -> PyResult<()> {
///     m.add_class::<NoneType>()?;
///     m.add(NoneType::NAME, NoneType::get(m.py())?)?;
///     Ok(())
/// }
/// ```
pub trait PySingleton: PyClass {
    /// The Python name of the singleton value that inhabits this type.
    const NAME: &'static str;

    /// Get the singleton instance of the type.
    fn get(py: Python<'_>) -> PyResult<&Bound<'_, Self>>;
}

pub trait PyModuleSingletonExt: private::Sealed {
    /// Add the class type and singleton instance of an instruction to a module.
    fn add_singleton<T: PySingleton>(&self) -> PyResult<()>;
}

impl PyModuleSingletonExt for Bound<'_, PyModule> {
    /// Add the class type and singleton instance of an instruction to a module.
    fn add_singleton<T: PySingleton>(&self) -> PyResult<()> {
        self.add_class::<T>()?;
        self.add(
            <T as PySingleton>::NAME,
            <T as PySingleton>::get(self.py())?,
        )
    }
}

/// Implement [`PySingleton`] for a `#[pyclass]`.
///
/// # Usage
///
/// ```rust,ignore
/// /// The type of the single `MySingleton` instance.
/// #[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq)]
/// #[pyclass(frozen, eq, hash, from_py_object)]
/// struct MySingletonType;
///
/// py_singleton!(MySingletonType, "MySingleton", |py| MySingletonType);
/// ```
///
/// The arguments are the type, the Python name for the singleton instance,
/// and a "closure" that takes a [`Python<'py>`] token and creates the singleton value.
/// The value is likely the type name again, if the type is just an empty struct,
/// but it can be more complex if, for example, the type is a subclass of another type
/// and so needs to use [`PyClassInitializer`] during construction.
/// Implementation-wise, that code is executed within a [`PyOnceLock`]
/// to ensure that the singleton instance is only created once.
/// To actually create that instance and add it to the module,
/// use [`PyModuleSingletonExt::add_singleton`] during module initialization.
macro_rules! py_singleton {
    ($T:ty, $name:expr, |$py:ident| $value:expr $(,)?) => {
        impl $crate::quilpy::singleton::PySingleton for $T {
            const NAME: &'static str = $name;

            /// Get the singleton instance of the type, creating it if necessary.
            fn get($py: Python<'_>) -> PyResult<&Bound<'_, Self>> {
                static CELL: ::pyo3::sync::PyOnceLock<Py<$T>> = ::pyo3::sync::PyOnceLock::new();
                CELL.get_or_try_init($py, || {
                    let value = $value;
                    Py::new($py, value)
                })
                .map(|inst| inst.bind($py))
            }
        }

        impl<'py> IntoPyObject<'py> for $T {
            type Target = Self;
            type Output = Bound<'py, Self::Target>;
            type Error = PyErr;

            fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
                Ok(<$T as $crate::quilpy::singleton::PySingleton>::get(py)?.to_owned())
            }
        }

        #[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
        #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
        #[pymethods]
        impl $T {
            // It might seems strange to have a constructor at all,
            // but it matches how Python handles singletons.
            // For example, `types.NoneType() is None`.
            /// Get a reference to the singleton instance of this type.
            #[new]
            fn new(py: Python<'_>) -> PyResult<&Bound<'_, Self>> {
                <Self as $crate::quilpy::singleton::PySingleton>::get(py)
            }

            /// Returns the name of the singleton instance relative its module.
            ///
            /// Enables [`pickling`][] of singleton instances.
            ///
            /// [`pickling`]: https://docs.python.org/3/library/pickle.html#object.__reduce__
            fn __reduce__<'py>(&self, py: Python<'py>) -> &::pyo3::Bound<'py, ::pyo3::types::PyString> {
                ::pyo3::intern!(py, <$T as $crate::quilpy::singleton::PySingleton>::NAME)
            }
        }
    };
}
pub(crate) use py_singleton;

// Prevent external code from implementing `PyModuleSingletonExt`.
#[doc(hidden)]
mod private {
    pub trait Sealed {}

    impl Sealed for pyo3::Bound<'_, pyo3::types::PyModule> {}
}
