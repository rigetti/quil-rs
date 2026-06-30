use std::convert::Infallible;

use pyo3::{
    prelude::*,
    types::{IntoPyDict as _, PyDict},
    IntoPyObjectExt as _,
};
use rigetti_pyo3::create_init_submodule;

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pymethods};

use crate::{instruction::WaveformInvocation, quilpy::errors};

use super::{
    builtin::{quilpy::*, BoxcarKernel, ExplicitCommonBuiltinParameters},
    Waveform, WaveformData,
};

mod sampling {
    use rigetti_pyo3::create_init_submodule;

    use crate::quilpy::errors;

    use super::super::sampling::quilpy::*;

    create_init_submodule! {
        classes: [
            PyIqSamplesIter,
            PyIqSamplesRevIter,
        ],
        complex_enums: [
            PyIqSamples
        ],
        errors: [
            errors::SamplingError,
        ],
    }
}

macro_rules! just_for_the_linter {
    ($($tt:tt)*) => {
        // Our linter needs to see all the Python types defined explicitly.
    };
}

just_for_the_linter! {
    #[gen_stub_pyclass]
    #[pyclass(module = "quil.waveform", name = "Flat")]
    struct PyFlat;

    #[gen_stub_pyclass]
    #[pyclass(module = "quil.waveform", name = "Gaussian")]
    struct PyGaussian;

    #[gen_stub_pyclass]
    #[pyclass(module = "quil.waveform", name = "DragGaussian")]
    struct PyDragGaussian;

    #[gen_stub_pyclass]
    #[pyclass(module = "quil.waveform", name = "ErfSquare")]
    struct PyErfSquare;

    #[gen_stub_pyclass]
    #[pyclass(module = "quil.waveform", name = "HermiteGaussian")]
    struct PyHermiteGaussian;

    #[gen_stub_pyclass]
    #[pyclass(module = "quil.waveform", name = "BoxcarKernel")]
    struct BoxcarKernel;
}

create_init_submodule! {
    classes: [
        PyWaveform,
        PyBuiltinWaveform,
        PyCommonBuiltinParameters,
        ExplicitCommonBuiltinParameters,
        PyFlat,
        PyGaussian,
        PyDragGaussian,
        PyErfSquare,
        PyHermiteGaussian,
        BoxcarKernel,
    ],
    errors: [
         errors::WaveformParameterError,
         errors::WaveformInvocationError,
    ],
    funcs: [ py_apply_phase_and_detuning ],
    submodules: [
        "sampling": sampling::init_submodule,
    ],
}

/// A wrapper around <code>[Py]&lt;[PyAny]&gt;</code> that exposes the simplest useful Rust traits:
///
/// - [`std::fmt::Debug`], which tries to use the Python `repr` if it can and prints out the
///   underlying pointer if it can't.
/// - [`PartialEq`] and [`Eq`], which compare object identity
/// - [`Clone`], which attaches to the Python interpreter and calls [`Py::clone_ref`].
#[derive(FromPyObject, IntoPyObject)]
pub struct PyAnyRust(pub Py<PyAny>);

impl std::fmt::Debug for PyAnyRust {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self(any) = self;
        Python::try_attach(|py| any.bind(py).repr().ok().map(|repr| write!(f, "{repr}")))
            .flatten()
            .unwrap_or_else(|| f.debug_tuple("PyAnyRust").field(any).finish())
    }
}

impl PartialEq for PyAnyRust {
    fn eq(&self, other: &Self) -> bool {
        self.0.is(&other.0)
    }
}

impl Eq for PyAnyRust {}

impl Clone for PyAnyRust {
    fn clone(&self) -> Self {
        Python::attach(|py| self.clone_ref(py))
    }
}

impl PyAnyRust {
    pub fn clone_ref<'py>(&self, py: Python<'py>) -> Self {
        Self(self.0.clone_ref(py))
    }

    pub fn clone_ref_ok<'py>(&self, py: Python<'py>) -> Result<Self, Infallible> {
        Ok(self.clone_ref(py))
    }
}

/// A [`WaveformData`] implementor for use from Python, where the individual complex and real fields
/// can be any Python object.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Pythonic {}

impl WaveformData for Pythonic {
    type Real = PyAnyRust;
    type Complex = PyAnyRust;
}

#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.waveform", name = "Waveform", subclass, eq)
)]
pub struct PyWaveform(pub Waveform<Pythonic>);

fn call_on_py_any_rust<'py>(f: &Bound<'py, PyAny>, x: &PyAnyRust) -> PyResult<PyAnyRust> {
    let PyAnyRust(x) = x;
    Ok(PyAnyRust(f.call1((x,))?.unbind()))
}

/// Wrap the `try_evaluate` methods on the various waveform types to expose them to Python as
/// `evaluate`.
macro_rules! define_py_evaluate {
    ($($name:ident),* $(,)?) => {
        $(
            paste::paste! {
                #[pymethods]
                impl [<Py$name>] {
                    fn evaluate<'py>(
                        &self,
                        real: &Bound<'py, PyAny>,
                        complex: &Bound<'py, PyAny>,
                    ) -> PyResult<[<Py$name>]> {
                        Ok([<Py$name>](self.0.as_ref().try_evaluate(
                            |r| call_on_py_any_rust(real, r),
                            |c| call_on_py_any_rust(complex, c),
                        )?))
                    }
                }

                #[cfg(feature = "stubs")]
                pyo3::inventory::submit! {
                    py_evaluate_method_info::<[<Py$name>]>()
                }
            }
        )*
    };
}

#[cfg(feature = "stubs")]
const fn py_evaluate_method_info<T: std::any::Any + pyo3::type_object::PyTypeInfo>(
) -> pyo3_stub_gen::type_info::PyMethodsInfo {
    use pyo3_stub_gen::{type_info::*, TypeInfo};

    PyMethodsInfo {
        struct_id: std::any::TypeId::of::<T>,
        attrs: &[],
        getters: &[],
        setters: &[],
        methods: &[MethodInfo {
            name: "evaluate",
            parameters: &[
                ParameterInfo {
                    name: "real",
                    kind: ParameterKind::PositionalOrKeyword,
                    type_info: || {
                        TypeInfo::with_module(
                            "collections.abc.Callable[[Real], OtherReal]",
                            "collections.abc".into(),
                        )
                    },
                    default: ParameterDefault::None,
                },
                ParameterInfo {
                    name: "complex",
                    kind: ParameterKind::PositionalOrKeyword,
                    type_info: || {
                        TypeInfo::with_module(
                            "collections.abc.Callable[[Complex], OtherComplex]",
                            "collections.abc".into(),
                        )
                    },
                    default: ParameterDefault::None,
                },
            ],
            r#return: || TypeInfo {
                name: format!("{}[OtherReal, OtherComplex]", T::NAME),
                import: Default::default(),
            },
            doc: "",
            r#type: MethodType::Instance,
            is_async: false,
            deprecated: None,
            type_ignored: None,
            is_overload: false,
        }],
        file: file!(),
        line: line!(),
        column: column!(),
    }
}

define_py_evaluate! {
    Waveform,
    BuiltinWaveform,
    CommonBuiltinParameters,
    Flat,
    Gaussian,
    DragGaussian,
    ErfSquare,
    HermiteGaussian,
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PyWaveform {
    #[staticmethod]
    #[gen_stub(override_return_type(
        type_repr = "Waveform[quil.expression.Expression]",
        imports = ("quil.expression")
    ))]
    fn from_quil<'py>(py: Python<'py>, invocation: WaveformInvocation) -> PyResult<Self> {
        let into_py_any = |expr: crate::expression::Expression| expr.into_py_any(py).map(PyAnyRust);
        Waveform::new(invocation)?
            .try_evaluate(into_py_any, into_py_any)
            .map(Self)
    }

    #[gen_stub(override_return_type(type_repr = "Waveform[Real, Complex]"))]
    #[staticmethod]
    fn builtin(
        #[gen_stub(override_type(type_repr = "BuiltinWaveform[Real, Complex]"))]
        waveform: PyBuiltinWaveform,
        #[gen_stub(override_type(type_repr = "CommonBuiltinParameters[Real, Complex]"))]
        common_parameters: PyCommonBuiltinParameters,
    ) -> PyResult<Self> {
        Ok(Self(Waveform::Builtin {
            waveform: waveform.0,
            common_parameters: common_parameters.0,
        }))
    }

    #[staticmethod]
    fn custom(
        name: String,
        #[gen_stub(override_type(
            type_repr = "builtins.dict[builtins.str, Complex]",
            imports = ("builtins")
        ))]
        parameters: Bound<'_, PyDict>,
    ) -> PyResult<Self> {
        Ok(Self(Waveform::Custom {
            name,
            parameters: parameters
                .iter()
                .map(|(name, value)| Ok((name.extract::<String>()?, PyAnyRust(value.unbind()))))
                .collect::<PyResult<_>>()?,
        }))
    }

    #[gen_stub(override_return_type(
        type_repr = "typing.Optional[builtins.tuple[\
                         BuiltinWaveform[Real, Complex], \
                         CommonBuiltinParameters[Real, Complex]\
                     ]]",
        imports = ("builtins", "typing")
    ))]
    fn as_builtin<'py>(
        &self,
        py: Python<'py>,
    ) -> Option<(PyBuiltinWaveform, PyCommonBuiltinParameters)> {
        match &self.0 {
            Waveform::Builtin {
                waveform,
                common_parameters,
            } => Some((
                PyBuiltinWaveform(
                    waveform
                        .as_ref()
                        .try_evaluate(|r| r.clone_ref_ok(py), |c| c.clone_ref_ok(py))
                        .unwrap_or_else(|never| match never {}),
                ),
                PyCommonBuiltinParameters(
                    common_parameters
                        .as_ref()
                        .try_evaluate(|r| r.clone_ref_ok(py), |c| c.clone_ref_ok(py))
                        .unwrap_or_else(|never| match never {}),
                ),
            )),
            Waveform::Custom { .. } => None,
        }
    }

    #[gen_stub(override_return_type(
        type_repr = "typing.Optional[builtins.tuple[\
                         builtins.str, \
                         builtins.dict[builtins.str, Complex]]\
                     ]",
        imports = ("builtins", "typing")
    ))]
    fn as_custom<'py>(&self, py: Python<'py>) -> PyResult<Option<(String, Bound<'py, PyDict>)>> {
        Ok(match &self.0 {
            Waveform::Custom { name, parameters } => Some((
                name.clone(),
                parameters
                    .iter()
                    .map(|(pname, PyAnyRust(pvalue))| (pname.clone(), pvalue.bind(py)))
                    .into_py_dict(py)?,
            )),
            Waveform::Builtin { .. } => None,
        })
    }

    fn __repr__<'py>(&self, py: Python<'py>) -> PyResult<String> {
        match &self.0 {
            Waveform::Builtin {
                waveform,
                common_parameters,
            } => Ok(format!(
                "waveform.builtin(waveform={}, common_parameters={})",
                waveform.py_repr(py)?,
                common_parameters.py_repr(py)?
            )),
            Waveform::Custom { name, parameters } => Ok(format!(
                "waveform.custom(name={name:?}, parameters={parameters:?})"
            )),
        }
    }
}
