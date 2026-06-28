use std::collections::HashMap;

use num_complex::Complex64;
use numpy::{IntoPyArray as _, PyArray1, PyArrayMethods as _};
use pyo3::{
    exceptions::{PyIndexError, PyStopIteration},
    prelude::*,
    types::{IntoPyDict as _, PyDict, PySlice, PySliceIndices, PyTuple},
};
use rigetti_pyo3::{create_init_submodule, impl_repr};

#[cfg(feature = "stubs")]
use pyo3_stub_gen::{
    derive::{
        gen_methods_from_python, gen_stub_pyclass, gen_stub_pyclass_complex_enum,
        gen_stub_pyfunction, gen_stub_pymethods,
    },
    PyStubType, TypeInfo,
};

use crate::{
    quilpy::errors,
    units::Cycles,
    waveform::{
        builtin::{quilpy::*, *},
        sampling::IqSamples,
        *,
    },
};

create_init_submodule! {
    classes: [
        PyIqSamplesIter,
        PyIqSamplesRevIter,
        SyntacticWaveform,
        ConcreteWaveform,
        SyntacticBuiltinWaveform,
        ConcreteBuiltinWaveform,
        SyntacticCommonBuiltinParameters,
        ConcreteCommonBuiltinParameters,
        ExplicitCommonBuiltinParameters,
        SyntacticFlat,
        ConcreteFlat,
        SyntacticGaussian,
        ConcreteGaussian,
        SyntacticDragGaussian,
        ConcreteDragGaussian,
        SyntacticErfSquare,
        ConcreteErfSquare,
        SyntacticHermiteGaussian,
        ConcreteHermiteGaussian,
        BoxcarKernel,
    ],
    complex_enums: [ PyIqSamples ],
    errors: [
         errors::WaveformParameterError,
         errors::WaveformInvocationError,
         errors::SamplingError,
    ],
    funcs: [ py_apply_phase_and_detuning ],
}

impl_repr! {
    PyIqSamples,
    PyIqSamplesIter,
    PyIqSamplesRevIter,
    SyntacticWaveform,
    ConcreteWaveform,
    SyntacticBuiltinWaveform,
    ConcreteBuiltinWaveform,
    SyntacticCommonBuiltinParameters,
    ConcreteCommonBuiltinParameters,
    ExplicitCommonBuiltinParameters,
    SyntacticFlat,
    ConcreteFlat,
    SyntacticGaussian,
    ConcreteGaussian,
    SyntacticDragGaussian,
    ConcreteDragGaussian,
    SyntacticErfSquare,
    ConcreteErfSquare,
    SyntacticHermiteGaussian,
    ConcreteHermiteGaussian,
    BoxcarKernel,
}

// A duplication of [`crate::waveform::sampling::IqSamples<Complex64>`], but nongeneric so it can be
// exposed to Python.  It also uses a named argument for the `Samples` constructor because that
// produces a nicer Python interface and avoids conflicting `__getitem__` definitions.
/// The result of sampling a waveform, representing a sequence of IQ value samples.
#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass_complex_enum)]
#[pyclass(module = "quil.waveform", name = "IqSamples", eq, frozen)]
pub enum PyIqSamples {
    /// A flat waveform, consisting of a single IQ value repeated some number of times.
    ///
    /// This is a more optimizable special-case representation for [`IqSamples::Samples(vec![iq;
    /// sample_count])`][Self::Samples], but is otherwise equivalent.
    Flat { iq: Complex64, sample_count: usize },

    /// A literal sequence of IQ samples.
    Samples { samples: Vec<Complex64> },
}

#[derive(Clone, Debug, derive_more::From, derive_more::Into)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(module = "quil.waveform", name = "IqSamplesIter")]
pub struct PyIqSamplesIter(pub sampling::IntoIter<Complex64>);

#[derive(Clone, Debug, derive_more::From, derive_more::Into)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(module = "quil.waveform", name = "IqSamplesRevIter")]
pub struct PyIqSamplesRevIter(pub std::iter::Rev<sampling::IntoIter<Complex64>>);

impl From<IqSamples<Complex64>> for PyIqSamples {
    fn from(value: IqSamples<Complex64>) -> Self {
        match value {
            IqSamples::Flat { iq, sample_count } => Self::Flat { iq, sample_count },
            IqSamples::Samples(samples) => Self::Samples { samples },
        }
    }
}

impl From<PyIqSamples> for IqSamples<Complex64> {
    fn from(value: PyIqSamples) -> Self {
        match value {
            PyIqSamples::Flat { iq, sample_count } => Self::Flat { iq, sample_count },
            PyIqSamples::Samples { samples } => Self::Samples(samples),
        }
    }
}

#[derive(Debug, FromPyObject)]
pub enum IqSamplesIndex<'py> {
    Int(isize),
    Slice(Bound<'py, PySlice>),
}

#[derive(Debug, IntoPyObject)]
pub enum IqSamplesIndexed {
    One(Complex64),
    Many(Vec<Complex64>),
}

#[cfg(feature = "stubs")]
impl PyStubType for IqSamplesIndex<'_> {
    fn type_output() -> TypeInfo {
        isize::type_output() | TypeInfo::builtin("slice[int,int,int]")
    }
}

#[cfg(feature = "stubs")]
impl PyStubType for IqSamplesIndexed {
    fn type_output() -> TypeInfo {
        Complex64::type_output() | TypeInfo::list_of::<Complex64>()
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PyIqSamples {
    /// The number of samples.  The same as `len`.
    // This is a reimplementation of [`IqSamples::sample_count`]
    #[getter(sample_count)]
    pub fn sample_count(&self) -> usize {
        match self {
            Self::Flat {
                sample_count,
                iq: _,
            } => *sample_count,

            Self::Samples { samples } => samples.len(),
        }
    }

    /// The number of samples.  The same as `sample_count`.
    #[pyo3(name = "__len__")]
    pub fn len(&self) -> usize {
        self.sample_count()
    }

    #[pyo3(name = "__length_hint__")]
    pub fn len_hint(&self) -> usize {
        self.sample_count()
    }

    /// Get the nth sample.  The same as indexing, but returns `None` instead of raising an error if
    /// the index is out of range.
    // This contains a reimplementation of [`IqSamples::get`], but handles Pythonic conventions as
    // well.
    #[pyo3(name = "get")]
    #[gen_stub(skip)] // Overloads handled below
    pub fn get(&self, index: IqSamplesIndex<'_>) -> PyResult<Option<IqSamplesIndexed>> {
        #[inline]
        fn get_one(this: &PyIqSamples, mut index: isize) -> Option<Complex64> {
            if index < 0 {
                index = index.checked_add_unsigned(this.sample_count())?;
            }
            let index = usize::try_from(index).ok()?;
            match this {
                PyIqSamples::Flat { iq, sample_count } => (index < *sample_count).then_some(*iq),
                PyIqSamples::Samples { samples } => samples.get(index).copied(),
            }
        }

        match index {
            IqSamplesIndex::Int(index) => Ok(get_one(self, index).map(IqSamplesIndexed::One)),

            IqSamplesIndex::Slice(indices) => {
                let PySliceIndices {
                    start,
                    stop,
                    step,
                    slicelength,
                } = indices.indices(self.sample_count() as isize)?;
                debug_assert!(step != 0);
                debug_assert!(slicelength <= self.sample_count());
                debug_assert!(start >= 0 || (start == -1 && step < 0));
                debug_assert!(stop >= 0 || (stop == -1 && step < 0));

                let sliced_samples = match self {
                    Self::Flat {
                        iq,
                        sample_count: _,
                    } => vec![*iq; slicelength],

                    Self::Samples { samples } => {
                        if step > 0 {
                            let start = start as usize; // Guaranteed by documentation
                            let stop = stop as usize; // Guaranteed by documentation
                            let step = step as usize;
                            if step == 1 {
                                samples[start..stop].to_owned()
                            } else {
                                let mut result = Vec::with_capacity(slicelength);
                                let mut i = start;
                                while i < stop {
                                    result.push(samples[i]);
                                    i += step;
                                }
                                result
                            }
                        } else {
                            let mut result = Vec::with_capacity(slicelength);
                            // If `start` is `-1`, this loop will never iterate, as `stop` is at
                            // least `-1`.  Moreover, `i` will never reach `-1` at all, for the same
                            // reason.  Thus, `i` will always be an in-range `usize`.
                            let mut i = start;
                            while i > stop {
                                result.push(samples[i as usize]);
                                i += step; // Counts down, since `step < 0`
                            }
                            result
                        }
                    }
                };

                Ok(Some(IqSamplesIndexed::Many(sliced_samples)))
            }
        }
    }

    /// Get the nth sample.
    #[pyo3(name = "__getitem__")]
    #[gen_stub(skip)] // Overloads handled below
    pub fn getitem(&self, index: IqSamplesIndex<'_>) -> PyResult<IqSamplesIndexed> {
        self.get(index)?
            .ok_or_else(|| PyIndexError::new_err("sample index out of range"))
    }

    #[pyo3(name = "__iter__")]
    pub fn iter(&self) -> PyIqSamplesIter {
        PyIqSamplesIter(IqSamples::from(self.clone()).into_iter())
    }

    #[pyo3(name = "__reversed__")]
    pub fn reversed(&self) -> PyIqSamplesRevIter {
        PyIqSamplesRevIter(self.iter().0.rev())
    }

    #[pyo3(name = "__contains__")]
    pub fn contains(&self, item: Bound<'_, PyAny>) -> bool {
        let Ok(item) = item.extract::<Complex64>() else {
            return false;
        };
        match self {
            Self::Flat { iq, sample_count } => *sample_count > 0 && *iq == item,
            Self::Samples { samples } => samples.contains(&item),
        }
    }

    /// Convert this sequence of samples into an explicit numpy array.
    ///
    /// The length of this array is [`self.sample_count()`][Self::sample_count], and its values are
    /// given by [`self[_]`][Self::get].
    pub fn iq_values<'py>(&self, py: Python<'py>) -> Bound<'py, PyArray1<Complex64>> {
        IqSamples::from(self.clone())
            .into_iq_values()
            .into_pyarray(py)
    }
}

#[cfg(feature = "stubs")]
pyo3::inventory::submit! {
    gen_methods_from_python! {r#"
        class PyIqSamples:
            @overload
            def get(self, index: int) -> typing.Optional[complex]: ...
            @overload
            def get(self, index: slice[int,int,int]) -> list[complex]: ...
            @overload
            def get(self, index: pyo3_stub_gen.RustType["IqSamplesIndex<'_>"]) -> pyo3_stub_gen.RustType["Option<IqSamplesIndexed>"]: ...

            @overload
            def __getitem__(self, index: int) -> complex: ...
            @overload
            def __getitem__(self, index: slice[int,int,int]) -> list[complex]: ...
            @overload
            def __getitem__(self, index: pyo3_stub_gen.RustType["IqSamplesIndex<'_>"]) -> pyo3_stub_gen.RustType["IqSamplesIndexed"]: ...
    "#}
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PyIqSamplesIter {
    #[pyo3(name = "__next__")]
    pub fn next(&mut self) -> PyResult<Complex64> {
        self.0.next().ok_or_else(|| PyStopIteration::new_err(()))
    }

    #[pyo3(name = "__iter__")]
    pub fn iter(slf: PyRef<'_, Self>) -> PyRef<'_, Self> {
        slf
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PyIqSamplesRevIter {
    #[pyo3(name = "__next__")]
    pub fn next(&mut self) -> PyResult<Complex64> {
        self.0.next().ok_or_else(|| PyStopIteration::new_err(()))
    }

    #[pyo3(name = "__iter__")]
    pub fn iter(slf: PyRef<'_, Self>) -> PyRef<'_, Self> {
        slf
    }
}

#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.waveform", subclass, eq)
)]
pub struct SyntacticWaveform(pub Waveform<Syntactic>);

#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.waveform", subclass, eq)
)]
pub struct ConcreteWaveform(pub Waveform<Concrete>);

/// Wrap the `try_evaluate` methods on the various waveform types to expose them to Python.  This
/// `try_evaluate` is more restricted than the Rust version, as it only supports going from
/// [`Syntactic`] to [`Concrete`] via [`Expression::evaluate`].
macro_rules! define_py_try_evaluate {
    ($($name:ident),* $(,)?) => {
        $(
            paste::paste! {
                #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
                #[pymethods]
                impl [<Syntactic$name>] {
                    #[pyo3(name = "try_evaluate")]
                    fn py_try_evaluate(
                        &self,
                        variables: HashMap<String, Complex64>,
                        memory_references: HashMap<String, Vec<f64>>,
                    ) -> PyResult<[<Concrete$name>]> {
                        Ok([<Concrete$name>](self.0.clone().try_evaluate(
                            |real| match real.evaluate(&variables, &memory_references)? {
                                Complex64 { re, im: 0.0 } => Ok(re),
                                _ => Err(EvaluationError::NumberNotReal),
                            },
                            |complex| complex.evaluate(&variables, &memory_references),
                        )?))
                    }
                }
            }
        )*
    }
}

define_py_try_evaluate! {
    Waveform,
    BuiltinWaveform,
    CommonBuiltinParameters,
    Flat,
    Gaussian,
    DragGaussian,
    ErfSquare,
    HermiteGaussian,
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl SyntacticWaveform {
    #[new]
    fn __new__(invocation: WaveformInvocation) -> Result<Self, WaveformInvocationError> {
        Waveform::new(invocation).map(Self)
    }
}

fn py_custom<'py, T: WaveformData>(
    name: String,
    parameters: Bound<'py, PyDict>,
) -> PyResult<Waveform<T>>
where
    T::Complex: for<'a> FromPyObject<'a, 'py>,
{
    let parameters = parameters
        .iter()
        .map(|(name, value)| {
            Ok((
                name.extract::<String>()?,
                value.extract::<T::Complex>().map_err(Into::into)?,
            ))
        })
        .collect::<PyResult<_>>()?;
    Ok(Waveform::Custom { name, parameters })
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl SyntacticWaveform {
    #[staticmethod]
    fn custom(name: String, parameters: Bound<'_, PyDict>) -> PyResult<Self> {
        py_custom(name, parameters).map(Self)
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl ConcreteWaveform {
    #[staticmethod]
    fn custom(name: String, parameters: Bound<'_, PyDict>) -> PyResult<Self> {
        py_custom(name, parameters).map(Self)
    }

    fn as_builtin(&self) -> Option<(ConcreteBuiltinWaveform, ConcreteCommonBuiltinParameters)> {
        match self.0 {
            Waveform::Builtin {
                waveform,
                common_parameters,
            } => Some((
                ConcreteBuiltinWaveform(waveform),
                ConcreteCommonBuiltinParameters(common_parameters),
            )),
            Waveform::Custom { .. } => None,
        }
    }

    #[gen_stub(override_return_type(
        type_repr = "tuple[builtins.str, builtins.dict[builtins.str, builtins.complex]] | None",
        imports = ("builtins")
    ))]
    fn as_custom<'py>(&self, py: Python<'py>) -> PyResult<Option<(String, Bound<'py, PyDict>)>> {
        Ok(match &self.0 {
            Waveform::Custom { name, parameters } => {
                Some((name.clone(), parameters.into_py_dict(py)?))
            }
            Waveform::Builtin { .. } => None,
        })
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl ConcreteBuiltinWaveform {
    #[gen_stub(override_return_type(
        type_repr = "typing.Union[\
                       ConcreteFlat, \
                       ConcreteGaussian, \
                       ConcreteDragGaussian, \
                       ConcreteErfSquare, \
                       ConcreteHermiteGaussian, \
                       BoxcarKernel\
                     ]",
        imports = ("typing")
      ))]
    fn as_inner<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        Ok(match self.0 {
            BuiltinWaveform::Flat(flat) => ConcreteFlat(flat).into_pyobject(py)?.into_any(),
            BuiltinWaveform::Gaussian(gaussian) => {
                ConcreteGaussian(gaussian).into_pyobject(py)?.into_any()
            }
            BuiltinWaveform::DragGaussian(drag_gaussian) => ConcreteDragGaussian(drag_gaussian)
                .into_pyobject(py)?
                .into_any(),
            BuiltinWaveform::ErfSquare(erf_square) => {
                ConcreteErfSquare(erf_square).into_pyobject(py)?.into_any()
            }
            BuiltinWaveform::HermiteGaussian(hermite_gaussian) => {
                ConcreteHermiteGaussian(hermite_gaussian)
                    .into_pyobject(py)?
                    .into_any()
            }
            BuiltinWaveform::BoxcarKernel(boxcar_kernel) => {
                boxcar_kernel.into_pyobject(py)?.into_any()
            }
        })
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl SyntacticCommonBuiltinParameters {
    #[pyo3(signature = (*, duration, scale = None, phase = None, detuning = None))]
    #[new]
    fn __new__(
        duration: f64,
        scale: Option<crate::expression::Expression>,
        phase: Option<crate::expression::Expression>,
        detuning: Option<crate::expression::Expression>,
    ) -> Self {
        Self(CommonBuiltinParameters {
            duration,
            scale,
            phase: phase.map(Cycles),
            detuning,
        })
    }

    fn __getnewargs_ex__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        let Self(CommonBuiltinParameters {
            duration,
            scale,
            phase,
            detuning,
        }) = self;
        let arguments = [
            ("duration", duration.into_pyobject(py)?.into_any()),
            ("scale", scale.clone().into_pyobject(py)?.into_any()),
            (
                "phase",
                phase.clone().map(|p| p.0).into_pyobject(py)?.into_any(),
            ),
            ("detuning", detuning.clone().into_pyobject(py)?.into_any()),
        ]
        .into_py_dict(py)?;
        (PyTuple::empty(py), arguments).into_pyobject(py)
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl ConcreteCommonBuiltinParameters {
    #[pyo3(signature = (*, duration, scale = None, phase = None, detuning = None))]
    #[new]
    fn __new__(
        duration: f64,
        scale: Option<f64>,
        phase: Option<f64>,
        detuning: Option<f64>,
    ) -> Self {
        Self(CommonBuiltinParameters {
            duration,
            scale,
            phase: phase.map(Cycles),
            detuning,
        })
    }

    fn __getnewargs_ex__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        let Self(CommonBuiltinParameters {
            duration,
            scale,
            phase,
            detuning,
        }) = *self;
        let arguments = [
            ("duration", Some(duration)),
            ("scale", scale),
            ("phase", phase.map(|p| p.0)),
            ("detuning", detuning),
        ]
        .into_py_dict(py)?;
        (PyTuple::empty(py), arguments).into_pyobject(py)
    }

    #[pyo3(name = "resolve_with_sample_rate")]
    fn py_resolve_with_sample_rate(
        &self,
        sample_rate: f64,
    ) -> PyResult<ExplicitCommonBuiltinParameters> {
        Ok(self.0.resolve_with_sample_rate(sample_rate)?)
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl ExplicitCommonBuiltinParameters {
    #[pyo3(signature = (*, sample_count, scale, phase, detuning))]
    #[new]
    fn __new__(sample_count: u32, scale: f64, phase: f64, detuning: f64) -> Self {
        Self {
            sample_count,
            scale,
            phase: Cycles(phase),
            detuning,
        }
    }

    fn __getnewargs_ex__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        let Self {
            sample_count,
            scale,
            phase,
            detuning,
        } = *self;
        let arguments = [
            ("sample_count", sample_count.into_pyobject(py)?.into_any()),
            ("scale", scale.into_pyobject(py)?.into_any()),
            ("phase", phase.0.into_pyobject(py)?.into_any()),
            ("detuning", detuning.into_pyobject(py)?.into_any()),
        ]
        .into_py_dict(py)?;
        (PyTuple::empty(py), arguments).into_pyobject(py)
    }
}

/// Modulate and phase shift waveform IQ data in place.
#[cfg_attr(feature = "stubs", gen_stub_pyfunction(module = "quil.waveform"))]
#[pyfunction(name = "apply_phase_and_detuning")]
fn py_apply_phase_and_detuning(
    iq_values: &Bound<'_, PyArray1<Complex64>>,
    phase: f64,
    detuning: f64,
    sample_rate: f64,
) -> PyResult<()> {
    // Though we could call the Rust version of this function and then modify the Python list,
    // this version allows us to iterate only once and avoid allocating a new `Vec<Complex64>`.
    for (index, value) in iq_values
        .try_readwrite()?
        .as_array_mut()
        .into_iter()
        .enumerate()
    {
        *value =
            apply_phase_and_detuning_at_index(*value, Cycles(phase), detuning, sample_rate, index);
    }

    Ok(())
}

/// `python_access_inner!(self, $(Concrete)?)` returns `self.0` if `Concrete` is present and `self`
/// if not; this is used to abstract over field-bearing waveforms (which are wrapper types) and
/// fieldless ones (which are not).
macro_rules! python_access_inner {
    ($item:expr,) => {
        $item
    };

    ($item:expr, Concrete) => {
        $item.0
    };
}

/// A "function" mapping one of the pseudo-types `Complex`, `Real`, or `ConcreteReal` to the
/// corresponding type for [`Concrete`] waveforms.  The former two become the corresponding
/// associated type and the latter becomes [`f64`].
macro_rules! concrete_type {
    (Complex) => {
        Complex64
    };

    (Real) => {
        f64
    };

    (ConcreteReal) => {
        f64
    };
}

/// A "function" mapping one of the pseudo-types `Complex`, `Real`, or `ConcreteReal` to the
/// corresponding type for [`Syntactic`] waveforms.  The former two become the corresponding
/// associated type and the latter becomes [`f64`].
macro_rules! syntactic_type {
    (Complex) => {
        Expression
    };

    (Real) => {
        Expression
    };

    (ConcreteReal) => {
        f64
    };
}

/// Given a sequence of waveform declarations with their concrete types, generate simple Python
/// wrappers for the concrete versions of those types.  The form of the Python waveforms is (1)
///
/// ```text
/// $waveform {
///     $field: $ty,
///     ...
/// }
/// ```
///
/// for the Rust generic waveform `$waveform<Concrete>` and its Python wrapper `ConcreteWaveform`,
/// or (2)
///
/// ```text
/// $waveform
/// ```
///
/// for the Rust fieldless waveform `$waveform` which is exposed to Python directly.
///
/// The field types are given as the pseudo-types `Complex` (for `$waveform_data::Complex`), `Real`
/// (for `$waveform_data::Real`), and `ConcreteReal` (which is always `f64`).
macro_rules! python_waveforms {
    // Input: Handles the user input, which is of the form given above.
    //
    // Next step: Immediately enter `@collect` mode, passing on a semicolon-terminated form of the
    // original input.
    //
    // `@collect` mode is where the bulk of this macro's work happens.  It will scan over the input,
    // decide what needs to be generated from it, and place the template information that it
    // generates in `{}`.  This is necessary to build up macro output piece by piece.
    ($($waveform:ident $({ $($field:ident: $ty:ty),+ $(,)? })?)*) => {
        python_waveforms! {
            @collect {}
            $($waveform $({ $($field: $ty),+ })?;)*
        }
    };

    // Input: In `@collect` mode, input of the form `Waveform { ... }; ...`
    //
    // Next step: Register two output templates, one for syntactic waveforms and one for concrete
    // waveforms.
    //
    // Note that we replace the input pseudo-types with the appropriate output types.
    (
        @collect { $($output:tt)* }
        $waveform:ident { $($field:ident: $ty:ty),+ $(,)? };
        $($rest:tt)*
    ) => {
        python_waveforms! {
            @collect
            {
                $($output)*
                $waveform (specific = Syntactic, general = Syntactic, sampling = false) {
                    $($field: syntactic_type!($ty)),+
                }
                $waveform (specific = Concrete, general = Concrete, sampling = true) {
                    $($field: concrete_type!($ty)),+
                }
            }
            $($rest)*
        }
    };

    // Input: In `@collect` mode, input of the form `Waveform; ...`
    //
    // Next step: Register one and a half output templates, one for syntactic waveforms and one for
    // concrete waveforms.  The former one is the "half" template (signified by `specific = false`),
    // because it will not be emitted for the underlying waveform as this waveform is not generic.
    (
        @collect { $($output:tt)* }
        $waveform:ident;
        $($rest:tt)*
    ) => {
        python_waveforms! {
            @collect {
                $($output)*
                $waveform (specific = @false, general = Syntactic, sampling = false)
                $waveform (general = Concrete, sampling = true)
            }
            $($rest)*
        }
    };

    // Input: The end of input in `@collect` mode.
    //
    // Next step: Immediately enter `@normalize` mode, passing the output generated from `@collect`
    // back in.
    (@collect { $($output:tt)* }) => {
        python_waveforms! {
            @resolve
            $($output)*
        }
    };

    // Input: `@resolve` mode, taking all the waveform templates.
    //
    // Next step: Expand to the final output.
    //
    // The three new template parameters `specific`, `general`, and `sampling` determine what code
    // we emit.  The `specific` parameter specifies the [`WaveformData`] for the waveform in
    // question; if its associated boolean is `false`, then this is a non-generic waveform and so we
    // skip this step.  The `general` parameter specifies the [`WaveformData`] for the constructor
    // we're putting on [`Waveform`].  The `sampling` boolean indicates whether we should generate
    // the `iq_values_at_sampling_rate` wrapper.
    (
        @resolve $(
            $waveform:ident
            (
                $(specific = $($specific_data:ident)? $(@ $omit_specific:ident)?,)?
                general = $general_data:ident,
                sampling = $include_sampling:ident
            )
            $({ $($field:ident: $ty:ty),+ })?
        )*

    ) => {
        paste::paste! {
            $(
                #[cfg(all($($($omit_specific)?)?))]
                #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
                #[pymethods]
                impl [<$($($specific_data)?)? $waveform>] {
                    #[pyo3(signature = ($(* $(, $field)+)?))]
                    #[new]
                    fn __new__($($($field: $ty),+)?) -> Self {
                        Self$(($waveform { $($field),* }))?
                    }

                    fn __getnewargs_ex__<'py>(
                        &self,
                        py: Python<'py>
                    ) -> PyResult<Bound<'py, PyTuple>> {
                        let Self$(($waveform { $($field),+ }))? = self.clone();
                        let arguments: [(&'static str, Bound<'py, PyAny>); _] =
                            [$($((stringify!($field), $field.into_pyobject(py)?.into_any())),+)?];
                        (PyTuple::empty(py), arguments.into_py_dict(py)?).into_pyobject(py)
                    }
                }

                #[cfg(all($include_sampling, $($($omit_specific)?)?))]
                #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
                #[pymethods]
                impl [<$($($specific_data)?)? $waveform>] {
                    #[pyo3(name = "iq_values_at_sample_rate")]
                    fn py_iq_values_at_sample_rate(
                        &self,
                        common: ConcreteCommonBuiltinParameters,
                        sample_rate: f64,
                    ) -> PyResult<PyIqSamples> {
                        Ok(
                            python_access_inner!(self, $($($specific_data)?)?)
                                .iq_values_at_sample_rate(common.0, sample_rate)?
                                .into()
                        )
                    }
                }

                #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
                #[pymethods]
                impl [<$general_data Waveform>] {
                    #[pyo3(
                        name = $waveform:snake,
                        signature = (
                            *,
                            duration, scale = None, phase = None, detuning = None
                            $($(, $field)+)?
                        )
                    )]
                    #[staticmethod]
                    #[allow(
                        clippy::too_many_arguments,
                        reason = "a many-keyword-argument function is genuinely a nice interface"
                    )]
                    fn [<py_$waveform:snake>](
                        duration: f64,
                        scale: Option<<$general_data as WaveformData>::Real>,
                        phase: Option<<$general_data as WaveformData>::Real>,
                        detuning: Option<<$general_data as WaveformData>::Real>,
                        $($($field: $ty),+)?
                    ) -> Self {
                        Self(Waveform::Builtin {
                            common_parameters: CommonBuiltinParameters {
                                duration,
                                scale,
                                phase: phase.map(Cycles),
                                detuning,
                            },
                            waveform: BuiltinWaveform::$waveform(
                                $waveform $({ $($field),+ })?,
                            ),
                        })
                    }
                }
            )*
        }
    }
}

python_waveforms! {
    Flat {
        iq: Complex,
    }

    Gaussian {
        fwhm: Real,
        t0: Real,
    }

    DragGaussian {
        fwhm: Real,
        t0: Real,
        anh: Real,
        alpha: Real,
    }

    ErfSquare {
        risetime: Real,
        pad_left: ConcreteReal,
        pad_right: ConcreteReal,
    }

    HermiteGaussian {
        fwhm: Real,
        t0: Real,
        anh: Real,
        alpha: Real,
        second_order_hrm_coeff: Real,
    }

    BoxcarKernel
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl ConcreteBuiltinWaveform {
    #[pyo3(name = "iq_values_at_sample_rate")]
    fn py_iq_values_at_sample_rate(
        &self,
        common: ConcreteCommonBuiltinParameters,
        sample_rate: f64,
    ) -> PyResult<PyIqSamples> {
        Ok(self
            .0
            .iq_values_at_sample_rate(common.0, sample_rate)?
            .into())
    }
}
