use pyo3::{
    prelude::*,
    types::{PyDict, PyList, PyTuple, PyType, PyTypeMethods},
    wrap_pymodule,
};

#[cfg(feature = "stubs")]
use pyo3_stub_gen::define_stub_info_gatherer;

use crate::expression;
use crate::instruction;
use crate::program;
use crate::validation;
use crate::waveform;

pub(crate) mod errors;

#[pymodule]
#[pyo3(name = "_quil")]
fn init_module(m: &Bound<'_, PyModule>) -> PyResult<()> {
    use crate::quilpy::errors;

    let py = m.py();

    m.add_wrapped(wrap_pymodule!(expression::quilpy::init_submodule))?;
    m.add_wrapped(wrap_pymodule!(instruction::quilpy::init_submodule))?;
    m.add_wrapped(wrap_pymodule!(program::quilpy::init_submodule))?;
    m.add_wrapped(wrap_pymodule!(validation::quilpy::init_submodule))?;
    m.add_wrapped(wrap_pymodule!(waveform::quilpy::init_submodule))?;

    m.add("QuilError", py.get_type::<errors::QuilError>())?;
    m.add("ValueError", py.get_type::<errors::ValueError>())?;
    m.add(
        "ToQuilStringError",
        py.get_type::<errors::ToQuilStringError>(),
    )?;
    m.add("PickleError", py.get_type::<errors::PickleError>())?;

    let sys = PyModule::import(py, "sys")?;
    let sys_modules: Bound<'_, PyDict> = sys.getattr("modules")?.downcast_into()?;
    sys_modules.set_item("quil.expression", m.getattr("expression")?)?;
    sys_modules.set_item("quil.instructions", m.getattr("instructions")?)?;
    sys_modules.set_item("quil.program", m.getattr("program")?)?;
    // validation is added below
    sys_modules.set_item("quil.waveforms", m.getattr("waveforms")?)?;

    let validation_module = m.getattr("validation")?;
    sys_modules.set_item(
        "quil.validation.identifier",
        validation_module.getattr("identifier")?,
    )?;
    sys_modules.set_item("quil.validation", validation_module)?;
    Ok(())
}

/// Fix the `__qualname__` on PyO3's "complex enums" so that they can be pickled.
///
/// Essentially, this runs the following Python code:
///
/// ```python
/// import inspect
/// issubclass = lambda cls: inspect.isclass(cls) and issubclass(cls, typ)
/// for name, cls in inspect.getmembers(typ, issubclass):
///     cls.__qualname__ = f"{prefix}.{name}"
/// ```
///
/// # In a Pickle
///
/// PyO3 processes `enum`s with non-unit variants by creating a Python class for the enum,
/// then creating a class for each variant, subclassed from the main enum class.
/// The subclasses end up as attributes on the main enum class,
/// which enables syntax like `q = Qubit.Fixed(0)`;
/// however, they're given qualified names that use `_` as a seperator instead of `.`,
/// e.g. we get `Qubit.Fixed(0).__qualname__ == "Qubit_Fixed"`
/// rather than `Qubit.Fixed`, as we would if we had written the inner class ourselves.
/// As a consequence, attempting to `pickle` an instance of it
/// will raise an error complaining that `quil.instructions.Qubit_Fixed` can't be found.
///
/// There are a handful of ways of making this work,
/// but modifying the `__qualname__` seems not only simple, but correct.
///
/// # See Also
///
/// - PyO3's Complex Enums: https://pyo3.rs/v0.25.1/class#complex-enums
/// - Issue regarding `__qualname__`: https://github.com/PyO3/pyo3/issues/5270
/// - Python's `inspect`: https://docs.python.org/3/library/inspect.html#inspect.getmembers
pub(crate) fn fix_enum_qual_names<'py>(typ: &Bound<'py, PyType>) -> PyResult<()> {
    let py = typ.py();
    let inspect = PyModule::import(py, "inspect")?;
    let isclass = inspect.getattr("isclass")?;
    let get_members = inspect.getattr("getmembers")?;

    let prefix = typ.qualname()?;
    let prefix = prefix.as_borrowed();
    let prefix = prefix.to_str()?;

    let inner: Bound<'_, PyList> = get_members.call((typ, isclass), None)?.downcast_into()?;
    for item in &inner {
        let item = item.downcast::<PyTuple>()?;

        let cls = item.get_borrowed_item(1)?;
        if cls.downcast()?.is_subclass(typ)? {
            // See https://pyo3.rs/v0.25.1/types#borroweda-py-t for info on `get_borrowed_item`.
            let name = item.get_borrowed_item(0)?;
            let fixed_name = format!("{prefix}.{}", name.downcast()?.to_str()?);
            cls.setattr(pyo3::intern!(py, "__qualname__"), fixed_name)?;
        }
    }

    Ok(())
}

/// Fix the `__qualname__` on a list of complex enums so that they can be pickled.
/// See [`fix_enum_qual_names`] for more information.
///
/// The first argument should be a `Python<'py>` instance;
/// all others should be names of `#[pyclass]`-annotated `enum`s with non-unit variants.
///
/// # Example
///
/// ```ignore
/// use pyo3;
/// use pyo3_stub_gen::derive::gen_stub_pyclass_complex_enum;
///
/// #[pyo3::pymodule(name = "place", module = "some", submodule)]
/// fn init_some_module(m: &Bound<'_, PyModule>) -> PyResult<()> {
///   let py = m.py();
///
///   m.add_class::<Foo>()?;
///   m.add_class::<Bar>()?;
///
///   fix_complex_enums!(py, Foo, Bar);
/// }
///
/// #[gen_stub_pyclass_complex_enum]
/// #[pyo3::pyclass(module = "some.place", eq, frozen, hash, get_all)]
/// pub enum Foo {
///     Integer(i64),
///     Real(f64),
/// }
///
/// #[gen_stub_pyclass_complex_enum]
/// #[pyo3::pyclass(module = "some.place", eq, frozen, hash, get_all)]
/// pub enum Bar {
///     Integer(i64),
///     Real(f64),
/// }
/// ```
macro_rules! fix_complex_enums {
    ($py:expr, $($name:ident),* $(,)?) => {
        {
            let py = $py;
            $($crate::quilpy::fix_enum_qual_names(&py.get_type::<$name>())?;)*
        }
    };
}

/// Add Python `to_quil` and `to_quil_or_debug` methods
/// for types that implements [`Quil`](quil_rs::quil::Quil).
macro_rules! impl_to_quil {
    ($name: ident) => {
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
    };
}

/// Add a `__repr__` method that returns the Rust type's `Debug` string.
macro_rules! impl_repr {
    ($name: ident) => {
        #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
        #[pyo3::pymethods]
        impl $name {
            fn __repr__(&self) -> String {
                format!("{self:?}")
            }
        }
    };
}

pub(crate) use fix_complex_enums;
pub(crate) use impl_repr;
pub(crate) use impl_to_quil;

#[cfg(feature = "stubs")]
define_stub_info_gatherer!(stub_info);
