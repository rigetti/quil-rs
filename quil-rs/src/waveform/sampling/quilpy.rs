use num_complex::Complex64;
use numpy::{IntoPyArray as _, PyArray1};
use pyo3::{
    exceptions::{PyIndexError, PyStopIteration, PyValueError},
    prelude::*,
    types::{PySlice, PySliceIndices},
};

#[cfg(feature = "stubs")]
use pyo3_stub_gen::{
    derive::{
        gen_methods_from_python, gen_stub_pyclass, gen_stub_pyclass_complex_enum,
        gen_stub_pymethods,
    },
    PyStubType, TypeInfo,
};

use crate::waveform::sampling::*;

// The `init_submodule` occurs directly in `waveform/quilpy.rs`, as otherwise our linter is unhappy.

/// Register the relevant classes as subclasses of `collections.abc.*`.  Must be called exactly one;
/// ideally would be called from `init_submodule`, but we can't hook into that, so we expose this as
/// a `pub(crate)` function (to get warnings if it's unused) and then call it from the root
/// `#[pymodule]` function.
pub(crate) fn register_abcs<'py>(py: Python<'py>) -> PyResult<()> {
    pyo3::types::PySequence::register::<PyIqSamples>(py)?;
    Ok(())
}

// A duplication of [`IqSamples<Complex64>`], but nongeneric so it can be exposed to Python.  It
// also uses a named argument for the `Samples` constructor because that produces a nicer Python
// interface and avoids conflicting `__getitem__` definitions.  We don't mark this type as
// `#[pyclass(…, sequence)]`, because [`sequence` types have special
// behavior](https://pyo3.rs/main/class/protocols#mapping--sequence-types); this could be changed in
// the future if we're willing to handle that.
/// The result of sampling a waveform, representing a sequence of IQ value samples.
#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass_complex_enum)]
#[pyclass(module = "quil.waveform.sampling", name = "IqSamples", eq, frozen)]
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
#[pyclass(module = "quil.waveform.sampling", name = "IqSamplesIter")]
pub struct PyIqSamplesIter(pub IntoIter<Complex64>);

#[derive(Clone, Debug, derive_more::From, derive_more::Into)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(module = "quil.waveform.sampling", name = "IqSamplesRevIter")]
pub struct PyIqSamplesRevIter(pub std::iter::Rev<IntoIter<Complex64>>);

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
        isize::type_output() | TypeInfo::builtin("slice[int | None]")
    }
}

#[cfg(feature = "stubs")]
impl PyStubType for IqSamplesIndexed {
    fn type_output() -> TypeInfo {
        Complex64::type_output() | TypeInfo::list_of::<Complex64>()
    }
}

#[cfg(feature = "stubs")]
pyo3::inventory::submit! {
    gen_methods_from_python! {r#"
        class PyIqSamples:
            @overload
            def get(self, index: int) -> typing.Optional[complex]: ...
            @overload
            def get(self, index: slice[typing.Optional[int]]) -> list[complex]: ...

            @overload
            def __getitem__(self, index: int) -> complex: ...
            @overload
            def __getitem__(self, index: slice[typing.Optional[int]]) -> list[complex]: ...
    "#}
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
    pub fn length_hint(&self) -> usize {
        self.sample_count()
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
    pub fn contains(&self, value: Bound<'_, PyAny>) -> bool {
        let Ok(value) = value.extract::<Complex64>() else {
            return false;
        };
        match self {
            Self::Flat { iq, sample_count } => *sample_count > 0 && *iq == value,
            Self::Samples { samples } => samples.contains(&value),
        }
    }

    /// Get the nth sample.  The same as indexing, but returns `None` instead of raising an error if
    /// the index is out of range.
    // This contains a reimplementation of [`IqSamples::get`], but handles Pythonic conventions as
    // well.
    #[pyo3(name = "get")]
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
                let sample_count = self.sample_count();
                let Ok(signed_sample_count) = isize::try_from(sample_count) else {
                    // Rust guarantees that allocated objects have size at most `isize::MAX`, so we
                    // must be in the `Flat` case and have more than `isize::MAX` samples.  However,
                    // `PyO3` only supports `isize` indices in `slice` (as of 0.29), and doesn't
                    // offer a way to get the raw values out, so we have to error here.
                    // Fortunately, this should happen approximately never.
                    //
                    // PyO3 docs: <https://pyo3.rs/main/doc/pyo3/types/struct.pyslice>

                    return Err(PyIndexError::new_err(format!(
                        "cannot index into an IqSamples.Flat object with >= isize::MAX ({}) \
                         samples using a slice",
                        isize::MAX
                    )));
                };

                let PySliceIndices {
                    start,
                    stop,
                    step,
                    slicelength,
                } = indices.indices(signed_sample_count)?;
                debug_assert!(step != 0);
                debug_assert!(slicelength <= sample_count);
                debug_assert!(start >= 0 || (start == -1 && step < 0));
                debug_assert!(stop >= 0 || (stop == -1 && step < 0));

                let sliced_samples = match self {
                    Self::Flat {
                        iq,
                        sample_count: _,
                    } => vec![*iq; slicelength],

                    Self::Samples { samples } => {
                        if let Ok(step) = usize::try_from(step) {
                            // Per the documentation of `PySliceIndices`
                            // (https://pyo3.rs/main/doc/pyo3/types/struct.pysliceindices), `start`
                            // and `stop` are nonnegative as long as the `step` is nonnegative.
                            let start = start as usize; // Guaranteed by documentation
                            let stop = stop as usize; // Guaranteed by documentation

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
                            // Per the documentation of `PySliceIndices`
                            // (https://pyo3.rs/main/doc/pyo3/types/struct.pysliceindices), `start`
                            // and `stop` are no smaller than `-1`.  If `start` is `-1`, this loop
                            // will never iterate, as `stop` is also at least `-1`.  Moreover, `i`
                            // will never reach `-1` at all, for the same reason.  Thus, `i` will
                            // always be an in-range `usize`.
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
    pub fn getitem(&self, index: IqSamplesIndex<'_>) -> PyResult<IqSamplesIndexed> {
        self.get(index)?
            .ok_or_else(|| PyIndexError::new_err("sample index out of range"))
    }

    /// Return the number of occurrences of the specified sample.
    ///
    /// Part of the `collections.abc.Sequence` interface.
    pub fn count(&self, value: Bound<'_, PyAny>) -> usize {
        let Ok(value) = value.extract::<Complex64>() else {
            return 0;
        };
        match self {
            Self::Flat { iq, sample_count } => {
                if *iq == value {
                    *sample_count
                } else {
                    0
                }
            }
            Self::Samples { samples } => samples.iter().filter(|sample| **sample == value).count(),
        }
    }

    /// Return the first occurrences of the specified sample.  Raises `ValueError` if the value is
    /// not present.
    ///
    /// Part of the `collections.abc.Sequence` interface.
    #[pyo3(signature = (value, start = 0, stop = None))]
    pub fn index(
        &self,
        value: Bound<'_, PyAny>,
        start: isize,
        stop: Option<isize>,
    ) -> PyResult<usize> {
        let error = || PyValueError::new_err(format!("{value:?} is not in samples"));

        let Ok(value) = value.extract::<Complex64>() else {
            return Err(error());
        };

        let sample_count = self.sample_count();

        // Indices that are "before the beginning" (more negative than `-sample_count`) or "after
        // the end" (greater than `sample_count`) are allowed, they're just equivalent to the
        // beginning/end, respectively.
        let (start, stop) = if let Ok(signed_sample_count) = isize::try_from(sample_count) {
            // It's panic-safe to negate `signed_sample_count` here, as it must be nonnegative.
            let signed_index = |i: isize| i.clamp(-signed_sample_count, signed_sample_count);
            (signed_index(start), stop.map(signed_index))
        } else {
            // We're in the `Flat` case and `sample_count > isize::MAX`, so every `isize` index is
            // valid.
            (start, stop)
        };

        // We have guaranteed that `-sample_count <= start, stop <= sample_count`, so this
        // arithmetic will always succeed at fixing our negative indices.
        let unsigned_index =
            |i: isize| usize::try_from(i).unwrap_or(sample_count.strict_add_signed(i));
        let start = unsigned_index(start);
        let stop = stop.map(unsigned_index).unwrap_or(sample_count);

        if start >= stop {
            // The range of indices is empty
            return Err(error());
        }

        // Since `start < stop`, and since we clamped `stop` to be at most `sample_count`, `start`
        // is a legal index and `start..stop` is a legal range of indices.

        match self {
            Self::Flat {
                iq,
                sample_count: _,
            } => {
                if *iq == value {
                    Ok(start)
                } else {
                    Err(error())
                }
            }
            Self::Samples { samples } => samples[start..stop]
                .iter()
                .position(|sample| *sample == value)
                .map(|i| i + start)
                .ok_or_else(error),
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

    fn __repr__<'py>(&self, py: Python<'py>) -> PyResult<String> {
        // To get Python-like debug output, we have to ask Python to format our complex numbers
        let complex_repr = |c: &Complex64| c.into_pyobject(py)?.repr();

        match self {
            Self::Flat { iq, sample_count } => Ok(format!(
                "IqSamples.Flat(iq={iq}, sample_count={sample_count})",
                iq = complex_repr(iq)?.to_str()?,
            )),
            Self::Samples { samples } => {
                let mut output = "IqSamples.Samples([".to_owned();
                let mut first = true;
                for sample in samples {
                    if first {
                        first = false;
                    } else {
                        output.push_str(", ");
                    }
                    output.push_str(complex_repr(sample)?.to_str()?);
                }
                output.push_str("])");
                Ok(output)
            }
        }
    }
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

    fn __repr__(slf: PyRef<'_, Self>) -> String {
        format!("<IqSamplesIter object at {:?}>", slf.as_ptr())
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

    fn __repr__(slf: PyRef<'_, Self>) -> String {
        format!("<IqSamplesRevIter object at {:?}>", slf.as_ptr())
    }
}
