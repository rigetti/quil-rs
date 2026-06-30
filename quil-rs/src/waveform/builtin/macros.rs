/// Macros for generating the built-in waveform types
///
/// We have to declare a bunch of waveforms which all look very similar but have slightly different
/// contents.  This is done in the `define_waveforms!` macro block below, but to get there, we have
/// to define a number of helper macros.
///
/// For details on usage, see the parent module.
///
/// The generated Python API is as follows:
///
/// 1. A module `quilpy_waveforms` exporting, for every waveform that has fields, a `Py$waveform`
///    type that wraps `$waveform<Pythonic>`; if the waveform does not have fields, then `$waveform`
///    is exposed directly to Python.
///
/// 2. A Python keyword-only constructor, a Python wrapper for `iq_values_at_sample_rate`, and a
///    custom `__repr__` that produces output that looks like Python, all defined on `Py$waveform`
///    for normal waveforms and `$waveform` for fieldless waveforms.
///
/// 3. Getters and setters for all the fields of the `Py$waveform` type.
///
/// 4. A way to get the Python repr from Rust, defined on the underlying `$waveform` types:
///
///    - A `pub(crate) fn __repr__(&self) -> &'static str` function `py_repr` function on any
///      fieldless `$waveform`
///
///    - A `pub(crate) fn py_repr<'py>(&self, py: Python<'py>) -> PyResult<String>` function on all
///      other `$waveform<Pythonic>`s.

/// Return the appropriate parser for one of the field pseudo-types.  These are for use in
/// [`parse::Extractable`] implementations.
macro_rules! field_parser {
    (ConcreteReal) => {
        parse::concrete_real
    };
    (Real) => {
        parse::mandatory
    };
    (Complex) => {
        parse::mandatory
    };
}

/// Given a [`WaveformData`] marker, convert one of the field pseudo-types into the corresponding
/// actual Rust type.  Non-identifier types must be wrapped in parentheses because macros can't
/// match balanced angle brackets.
macro_rules! field_type {
    ($waveform_type:ident, $field_type:ident) => {
        field_type!(($waveform_type), $field_type)
    };
    (($($waveform_type:tt)+), ConcreteReal) => {
        f64
    };
    (($($waveform_type:tt)+), Real) => {
        $($waveform_type)+::Real
    };
    (($($waveform_type:tt)+), Complex) => {
        $($waveform_type)+::Complex
    };
}

/// `extract_type_if_generic_field!($pseudo_type; ...)` evaluates to `T::$pseudo_type` for the
/// `$pseudo_type`s `Real` and `Complex` if they occur in the type list `...`, and to `()`
/// otherwise.
///
/// This lets us e.g. get the appropriate [`Copy`] bounds on our types with [`derive_where`], since
/// `()` trivially satisfies the bounds.
macro_rules! extract_type_if_generic_field {
    (Real; Real $($rest:tt)*) => {
        T::Real
    };
    (Complex; Complex $($rest:tt)*) => {
        T::Complex
    };
    ($ty:ident; $other:tt $($rest:tt)*) => {
        extract_type_if_generic_field!($ty; $($rest)*)
    };
    ($ty:ident;) => {
        ()
    };
}

/// Part of the implementation of the `as_ref` function for waveforms; given a reference to a
/// pseudo-typed field `$field_ref: $pseudo_type`, either dereference it (if it's concrete) or leave
/// it alone (if it isn't concrete).
macro_rules! field_referencer {
    (ConcreteReal, $field_ref:expr) => {
        *$field_ref
    };
    (Real, $field_ref:expr) => {
        $field_ref
    };
    (Complex, $field_ref:expr) => {
        $field_ref
    };
}

/// Part of the implementation of the `try_evaluate` function for waveforms; given a pseudo-typed
/// field `$field: $pseudo_type` and the `$real` and `$complex` evaluator parameters to
/// `try_evaluate`, `field_evaluator!($pseudo_type, real = $real, complex = $complex, field)` will
/// transform that `field` from one [`WaveformData`] to another.
macro_rules! field_evaluator {
    (ConcreteReal, real = $real:ident, complex = $complex:ident, $field:expr) => {
        $field
    };
    (Real, real = $real:ident, complex = $complex:ident, $field:expr) => {
        $real($field)?
    };
    (Complex, real = $real:ident, complex = $complex:ident, $field:expr) => {
        $complex($field)?
    };
}

/// Constant strings documenting the origin of waveforms, powering the `#[waveform_source(...)]`
/// attribute within [`define_waveforms!`].
macro_rules! waveform_source {
    (QuilT) => {
        r#"This waveform is part of the [Quil-T][] spec ([§12.2, Waveforms][]).

[Quil-T]: https://quil-lang.github.io/#12Annex-T--Pulse-Level-Control
[§12.2, Waveforms]: https://quil-lang.github.io/#12-2Waveforms"#
    };
    (Rigetti) => {
        r#"This waveform is a Rigetti extension to Quil-T."#
    };
}

/// `concrete_waveform!($ty, ...fields of $ty...)` is effectively the same as `$ty<Concrete>`,
/// except (a) it uses `super` because it's intended for use within a submodule, and (b) it just
/// emits `$ty` if there are no fields, since fieldless waveforms aren't generic.
macro_rules! concrete_waveform {
    ($name:ident, $($field:ident)+) => {
        super::$name<super::Concrete>
    };
    ($name:ident,) => {
        super::$name
    };
}

/// ASZ docs
///
/// 1. We have to define the getter all at once because using a macro in the return type fails to be
///    able to refer to `'py` for the purposes of the `#[pyo3::pymethods]` macro.
///
/// 2. We have to define the methods in consecutive `impl` blocks because otherwise we'd have to
///    define just the method with a macro, and you can't use macros inside `#[pyo3::pymethods]`
///    blocks.
///
/// Setters don't *need* the same treatment, since they don't need to refer to any lifetime
/// parameters, but we define them along with the getters for simplicity.
macro_rules! python_get_set {
    ($ty_name:ident, $field:ident, ConcreteReal) => {
        paste::paste! {
            #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
            #[pyo3::pymethods]
            impl $ty_name {
                #[getter($field)]
                fn [<py_get_$field>](&self) -> f64 {
                    self.0.$field
                }

                #[setter($field)]
                fn [<py_set_$field>](&mut self, $field: f64) {
                    self.0.$field = $field;
                }
            }
        }
    };

    ($ty_name:ident, $field:ident, Real) => {
        python_get_set!($ty_name, $field, PyAny("Real"));
    };

    ($ty_name:ident, $field:ident, Complex) => {
        python_get_set!($ty_name, $field, PyAny("Complex"));
    };

    ($ty_name:ident, $field:ident, PyAny($type_name:literal)) => {
        paste::paste! {
            #[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
            #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
            #[pyo3::pymethods]
            impl $ty_name {
                #[getter($field)]
                #[gen_stub(override_return_type(type_repr = $type_name))]
                fn [<py_get_$field>]<'py>(&self, py: Python<'py>) -> &Bound<'py, PyAny> {
                    self.0.$field.0.bind(py)
                }

                #[setter($field)]
                fn [<py_set_$field>](
                    &mut self,
                    #[gen_stub(override_type(type_repr = $type_name))]
                    $field: Py<PyAny>
                ) {
                    self.0.$field.0 = $field;
                }
            }
        }
    };
}

/// Clone this value if it's not a `ConcreteReal`, otherwise copy it.
macro_rules! maybe_clone_ref {
    ($field:expr, ConcreteReal, $py:ident) => {
        *$field
    };

    ($field:expr, Real, $py:ident) => {
        $field.clone_ref($py)
    };

    ($field:expr, Complex, $py:ident) => {
        $field.clone_ref($py)
    };
}

/// Push Python `repr` of this field onto `$output`
macro_rules! push_field_repr {
    ($output:ident, $field:expr, ConcreteReal, $py:ident) => {{
        use std::fmt::Write as _;
        write!(&mut $output, "{}", $field).unwrap(); // Writing to a string can't fail
    }};

    ($output:ident, $field:expr, Real, $py:ident) => {
        $output.push_str($field.0.bind($py).repr()?.to_str()?);
    };

    ($output:ident, $field:expr, Complex, $py:ident) => {
        $output.push_str($field.0.bind($py).repr()?.to_str()?);
    };
}

/// A token-muncher to associate the `Real` and `Complex` type names with the corresponding string
/// literal, so that the `define_python_waveform!` and
/// `add_python_waveform_convenience_constructor!` macros can use them to override type signatures.
macro_rules! define_python_interop {
    ($name:ident $({ $($field:ident: $ty:ident),+ })?) => {
        define_python_interop! {
            @parse
                $name
            |
                $({ $($field: $ty),+ })?
        }
    };

    (@parse
         $name:ident $({ $($pfield:ident: $pty:ident $(($pty_str:literal))?,)+ })?
     |
         $({})?
    ) => {
        define_python_waveform! {
            $name $({ $($pfield: $pty $(($pty_str))?),+ })?
        }
        add_python_waveform_convenience_constructor! {
            $name $({ $($pfield: $pty $(($pty_str))?),+ })?
        }
    };

    (@parse
         $name:ident $({ $($pfield:ident: $pty:ident $(($pty_str:literal))?,)+ })?
     |
         { $field1:ident: ConcreteReal $(, $field:ident: $ty:ident)* }
    ) => {
        define_python_interop! {
            @parse
                $name { $($($pfield: $pty $(($pty_str))?,)+)? $field1: ConcreteReal, }
            |
                { $($field: $ty),* }
        }
    };

    (@parse
         $name:ident $({ $($pfield:ident: $pty:ident $(($pty_str:literal))?,)+ })?
     |
         { $field1:ident: Real $(, $field:ident: $ty:ident)* }
    ) => {
        define_python_interop! {
            @parse
                $name { $($($pfield: $pty $(($pty_str))?,)+)? $field1: Real ("Real"), }
            |
                { $($field: $ty),* }
        }
    };

    (@parse
         $name:ident $({ $($pfield:ident: $pty:ident $(($pty_str:literal))?,)+ })?
     |
         { $field1:ident: Complex $(, $field:ident: $ty:ident)* }
    ) => {
        define_python_interop! {
            @parse
                $name { $($($pfield: $pty $(($pty_str))?,)+)? $field1: Complex ("Complex"), }
            |
                { $($field: $ty),* }
        }
    };
}

/// ASZ docs
///
/// Generates either `Syntactic$name` or `Concrete$name` Python wrapper types, depending on what's
/// provided as `$waveform_type`.  These wrapper types are thin public wrappers around
/// `$name<$waveform_type>` and provide Python setters and getters for the fields.
///
/// Generate Python syntactic and concrete waveforms.
macro_rules! define_python_waveform {
    ($name:ident) => {
        // We can reuse the empty struct as the type that's exposed to Python, but even if we do we
        // still need to give it its methods.
        #[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
        #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
        #[pyo3::pymethods]
        impl super::$name {
            #[new]
            fn __new__() -> Self {
                Self
            }

            #[pyo3(name = "iq_values_at_sample_rate")]
            fn py_iq_values_at_sample_rate<'py>(
                &self,
                py: Python<'py>,
                #[gen_stub(override_type(
                    type_repr = "CommonBuiltinParameters[builtins.float, _T]",
                    imports = ("builtins"))
                )]
                common: PyCommonBuiltinParameters,
                sample_rate: f64,
            ) -> PyResult<PyIqSamples> {
                Ok(self
                   .iq_values_at_sample_rate(
                       common
                           .0
                           .as_ref()
                           .try_evaluate(
                               |PyAnyRust(r)| r.extract(py),
                               |PyAnyRust(c)| c.extract(py),
                           )?,
                       sample_rate,
                   )?
                   .into())
            }

            pub(crate) fn __repr__(&self) -> &'static str {
                concat!(stringify!($name), "()")
            }
        }
    };

    ($name:ident { $($field:ident: $ty:ident $(($ty_str:literal))?),+ }) => {
        paste::paste! {
            #[derive(Clone, PartialEq, Debug)]
            #[cfg_attr(feature = "stubs", gen_stub_pyclass)]
            #[pyo3::pyclass(module = "quil.waveform", subclass, eq)]
            pub struct $name(pub super::$name<Pythonic>);

            #[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
            #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
            #[pyo3::pymethods]
            impl $name {
                #[pyo3(signature = (* $(, $field)+))]
                // ASZ #[gen_stub(override_return_type(
                //     type_repr = "$name[Real, Complex]",
                // ))]
                #[new]
                fn __new__(
                    $(
                        $(#[gen_stub(override_type(type_repr = $ty_str))])?
                        $field: field_type!((<Pythonic as WaveformData>), $ty)
                    ),+
                ) -> Self {
                    Self(super::$name { $($field),+ })
                }

                fn __getnewargs_ex__<'py>(
                    &self,
                    py: Python<'py>
                ) -> PyResult<Bound<'py, PyTuple>> {
                    let Self(super::$name { $($field),+ }) = self;
                    let arguments: [(&'static str, Bound<'py, PyAny>); _] = [
                        $((
                            stringify!($field),
                            maybe_clone_ref!($field, $ty, py).into_bound_py_any(py)?,
                        )),+
                    ];
                    (PyTuple::empty(py), arguments.into_py_dict(py)?).into_pyobject(py)
                }

                #[pyo3(name = "iq_values_at_sample_rate")]
                fn py_iq_values_at_sample_rate<'py>(
                    // ASZ: expose a way to do this in the stub_gen binary
                    // #[gen_stub(override_type(
                    //     type_repr = "$name[builtins.float, builtins.complex]",
                    //     imports = ("builtins"))
                    // )]
                    &self,
                    py: Python<'py>,
                    #[gen_stub(override_type(
                        type_repr = "CommonBuiltinParameters[builtins.float, Complex]",
                        imports = ("builtins"))
                    )]
                    common: PyCommonBuiltinParameters,
                    sample_rate: f64,
                ) -> PyResult<PyIqSamples> {
                    Ok(self
                       .0
                       .as_ref()
                       .try_evaluate::<crate::waveform::Concrete, _>(
                           |PyAnyRust(r)| r.extract(py),
                           |PyAnyRust(c)| c.extract(py),
                       )?
                       .iq_values_at_sample_rate(
                           common
                               .0
                               .as_ref()
                               .try_evaluate(
                                   |PyAnyRust(r)| r.extract(py),
                                   |PyAnyRust(c)| c.extract(py),
                               )?,
                           sample_rate,
                       )?
                       .into())
                }

                fn __repr__<'py>(&self, py: Python<'py>) -> PyResult<String> {
                    self.0.py_repr(py)
                }
            }

            $(python_get_set!($name, $field, $ty);)*

            impl super::$name<Pythonic> {
                pub(crate) fn py_repr<'py>(&self, py: Python<'py>) -> PyResult<String> {
                    let Self { $($field),+ } = self;
                    let mut output = stringify!($name).to_owned();
                    let mut sep = "(";
                    $(
                        output.push_str(sep);
                        output.push_str(concat!(stringify!($field), "="));
                        push_field_repr!(output, $field, $ty, py);
                        #[allow(unused_assignments)]
                        { sep = ", "; }
                    )+
                    output.push(')');
                    Ok(output)
                }
            }
        }
    }
}

macro_rules! add_python_waveform_convenience_constructor {
    ($name:ident $({ $($field:ident: $ty:ident $(($ty_str:literal))?),+ })?) => {
        paste::paste! {
            #[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
            #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
            #[pyo3::pymethods]
            impl crate::waveform::quilpy::PyWaveform {
                #[pyo3(
                    name = $name:snake,
                    signature = (
                        *,
                        duration, scale = None, phase = None, detuning = None,
                        $($($field),+)?
                    )
                )]
                // ASZ #[gen_stub(override_return_type(
                //     type_repr = "$name[Real, Complex]",
                // ))]
                // (but only if there are fields)
                #[staticmethod]
                #[allow(
                    clippy::too_many_arguments,
                    reason = "a many-keyword-argument function is genuinely a nice interface"
                )]
                fn [<py_$name:snake>]<'py>(
                    py: Python<'py>,
                    duration: f64,
                    #[gen_stub(override_type(
                        type_repr = "typing.Optional[Real]",
                        imports = ("typing"))
                    )]
                    #[gen_stub(override_type(
                        type_repr = "typing.Optional[Real]",
                        imports = ("typing"))
                    )]
                    scale: Option<&Bound<'py, PyAny>>,
                    #[gen_stub(override_type(
                        type_repr = "typing.Optional[Real]",
                        imports = ("typing"))
                    )]
                    phase: Option<&Bound<'py, PyAny>>,
                    #[gen_stub(override_type(
                        type_repr = "typing.Optional[Real]",
                        imports = ("typing"))
                    )]
                    detuning: Option<&Bound<'py, PyAny>>,
                    $($(
                        $(#[gen_stub(override_type(type_repr = $ty_str))])?
                        $field: field_type!((<Pythonic as WaveformData>), $ty)
                    ),+)?
                ) -> Self {
                    Self(crate::waveform::Waveform::Builtin {
                        common_parameters: super::CommonBuiltinParameters {
                            duration,
                            scale: scale.map(|scale| {
                                PyAnyRust(scale.as_unbound().clone_ref(py))
                            }),
                            phase: phase.map(|phase| {
                                Cycles(PyAnyRust(phase.as_unbound().clone_ref(py)))
                            }),
                            detuning: detuning.map(|detuning| {
                                PyAnyRust(detuning.as_unbound().clone_ref(py))
                            }),
                        },
                        waveform: super::BuiltinWaveform::$name(super::$name $({
                            $($field),+
                        })?),
                    })
                }
            }
        }
    }
}

/// ASZ docs
macro_rules! reexport_python_waveform {
    ($submodule:ident::$name:ident as $py_name:ident) => {};

    ($submodule:ident::$name:ident { $($field:ident: $ty:ident),+ } as $py_name:ident) => {
        pub use $submodule::$name as $py_name;
    };
}

/// Define a single waveform, as described in the introduction to this section.  Note that we handle
/// fieldless waveforms specially; these get to omit a lot of work because they aren't generic.
/// Documentation is handled in `define_waveforms!`.
macro_rules! define_waveform {
    {
        $(#[$struct_meta:meta])*
        pub struct $name:ident
    } => {
        $(#[$struct_meta])*
        #[derive(Clone, PartialEq, Debug, Copy, Serialize, Deserialize)]
        #[cfg_attr(feature = "stubs", gen_stub_pyclass)]
        #[cfg_attr(feature = "python", pyo3::pyclass(module = "quil.waveform", subclass, eq))]
        pub struct $name;

        #[automatically_derived]
        impl parse::Extractable for $name {
            fn extract_from(
                parameters: &mut crate::instruction::WaveformParameters
            ) -> Result<Self, WaveformParameterError> {
                Ok(Self)
            }
        }
    };

    {
        $(#[$struct_meta:meta])*
        pub struct $name:ident {
            $(
                $(#[$field_meta:meta])*
                pub $field:ident: $ty:ident
            ),+
            $(,)?
        }
    } => {
        $(#[$struct_meta])*
        #[derive_where(Clone, PartialEq, Debug)]
        #[derive_where(
            Copy, Serialize, Deserialize;
            extract_type_if_generic_field!(Real; $($ty),+),
            extract_type_if_generic_field!(Complex; $($ty),+)
        )]
        pub struct $name<T: WaveformData> {
            $(
                $(#[$field_meta])*
                pub $field: field_type!(T, $ty)
            ),+
        }

        #[automatically_derived]
        impl parse::Extractable for $name<Syntactic> {
            fn extract_from(
                parameters: &mut crate::instruction::WaveformParameters,
            ) -> Result<Self, WaveformParameterError> {
                $(
                    let $field = field_parser!($ty)(parameters, stringify!($field))?;
                )+
                Ok(Self { $($field),+ })
            }
        }

        impl<S: WaveformData> $name<S> {
            #[doc = concat!(
                "Convert an owned [`", stringify!($name), "`] into an equivalent one ",
                "whose (non-concrete) parameters are all references."
            )]
            pub fn as_ref(&self) -> $name<Reference<'_, S>> {
                let Self { $($field),+ } = self;
                $name {
                    $($field: field_referencer!($ty, $field)),+
                }
            }


            #[doc = concat!(
                "Convert one [`", stringify!($name), "`] into another ",
                "by replacing its associated data."
            )]
            ///
            /// Given two forms of waveform data, `S` and `T`, the user specifies how to evaluate
            /// `S`'s real numbers into `T`'s real numbers and how to evaluate `S`'s complex numbers
            /// to `T`'s complex numbers.  For example, to convert parsed ([`Syntactic`]) parameters
            /// into sampleable ([`Concrete`]) parameters, you can pass
            /// [`Expression::evaluate`][crate::expression::Expression::evaluate] to this function.
            ///
            /// For a more detailed example, see the documentation for
            /// [`Waveform::try_evaluate`][crate::waveform::Waveform::try_evaluate], which has the
            /// same structure as this function.
            #[allow(unused_variables, reason = "macro-generated code")]
            pub fn try_evaluate<T: WaveformData, E>(
                self,
                real: impl Fn(S::Real) -> Result<T::Real, E>,
                complex: impl Fn(S::Complex) -> Result<T::Complex, E>,
            ) -> Result<$name<T>, E> {
                let Self { $($field),+ } = self;
                Ok($name {
                    $($field: field_evaluator!($ty, real = real, complex = complex, $field)),+
                })
            }
        }
    }
}

/// Define a collection of waveforms, as described in the introduction to this section.  Also
/// generates Python wrappers in a submodule and creates the `Sealed` trait used for
/// [`BuiltinWaveformParameters`].
macro_rules! define_waveforms {
    (
        $(
            $(#[doc = $struct_doc:literal])*
            #[waveform_source($waveform_source:ident)]
            $(#[$struct_meta:meta])*
            pub struct $name:ident $({
                $(
                    $(#[$field_meta:meta])*
                    pub $field:ident: $ty:ident
                ),+
                $(,)?
            })?
            $(;)?
        )+
    ) => {
        $(
            define_waveform! {
                $(#[doc = $struct_doc])*
                ///
                #[doc = waveform_source!($waveform_source)]
                $(#[$struct_meta])*
                pub struct $name $({
                    $(
                        $(#[$field_meta])*
                        pub $field: $ty
                    ),+
                })?
            }
        )+

        // Exported from the `quilpy` submodule
        #[cfg(feature = "python")]
        mod quilpy_waveforms {
            use super::*;

            // We reexport the Python waveforms from this submodule so we can give them
            // `Py`-prefixed names; we can't do that directly because then we'd have to rename them,
            // and we can only rename them to string literals which aren't available inside this
            // macro.
            mod waveform_types {
                use super::macros::*;

                #[cfg(feature = "stubs")]
                use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pymethods};

                use pyo3::{
                    marker::Python,
                    types::{
                        IntoPyDict as _, PyAny, PyAnyMethods as _, PyStringMethods as _, PyTuple,
                    },
                    Bound, IntoPyObject as _, IntoPyObjectExt as _, Py, PyResult,
                };

                use crate::{
                    waveform::{
                        quilpy::{PyAnyRust, Pythonic},
                        sampling::quilpy::PyIqSamples,
                        WaveformData,
                    },
                    units::Cycles,
                };

                use super::{
                    quilpy::PyCommonBuiltinParameters,
                    BuiltinWaveformParameters as _,
                };

                $(define_python_interop!($name $({ $($field: $ty),+ })?);)*
            }

            paste::paste! {
                $(reexport_python_waveform! {
                    waveform_types::$name $({ $($field: $ty),+ })? as [<Py$name>]
                })*
            }
        }

        mod private {
            use super::macros::*;

            pub trait Sealed {}
            impl Sealed for super::BuiltinWaveform<super::Concrete> {}
            $(
                #[automatically_derived]
                impl Sealed for concrete_waveform!($name, $($($field)+)?) {}
            )*
        }
    }
}

pub(crate) use {
    add_python_waveform_convenience_constructor, concrete_waveform, define_python_interop,
    define_python_waveform, define_waveform, define_waveforms, extract_type_if_generic_field,
    field_evaluator, field_parser, field_referencer, field_type, maybe_clone_ref, push_field_repr,
    python_get_set, reexport_python_waveform, waveform_source,
};
