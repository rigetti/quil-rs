use num_complex::Complex64;
use numpy::{IntoPyArray as _, PyArray1};
use pyo3::{
    exceptions::{PyIndexError, PyStopIteration, PyValueError},
    prelude::*,
    types::{PySlice, PySliceIndices},
};
use rigetti_pyo3::impl_repr;

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

impl_repr! {
    PyIqSamples,
    PyIqSamplesIter,
    PyIqSamplesRevIter,
}

// A duplication of [`IqSamples<Complex64>`], but nongeneric so it can be exposed to Python.  It
// also uses a named argument for the `Samples` constructor because that produces a nicer Python
// interface and avoids conflicting `__getitem__` definitions.
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

        // If this is a positive index or a legal negative index, returns `Ok(Some(_))` of the
        // corresponding positive index.  If this is an illegal negative index, returns `Ok(None)`.
        // If there's arithmetic overflow, returns an error.
        let fix_negative_index = |i: isize| -> PyResult<Option<usize>> {
            if i < 0 {
                let i = i.checked_add_unsigned(sample_count).ok_or_else(error)?;
                Ok(usize::try_from(i).ok())
            } else {
                // Positive `isize` fits in `usize`
                Ok(Some(i as usize))
            }
        };

        // Starting "before the beginning" is allowed, and is the same as starting at the beginning.
        let start = fix_negative_index(start)?.unwrap_or(0);
        let stop = match stop {
            Some(stop) => fix_negative_index(stop)?
                .ok_or_else(error)? // We can't *stop* before the beginning, …
                .min(sample_count), // … but we *can* stop "after the end", so we clamp to the count.
            None => sample_count,
        };

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
}

// Pymethods that need manual stub generation for overloads (handled below).  Our linter doesn't
// know about `#[gen_stub(skip)]` and gets concerned when we have a `pymethods` block without any
// stub generation, so we have to move this to a different impl block and then lie to the linter
// about generating stubs.
#[cfg_attr(false, gen_stub_pymethods)]
#[pymethods]
impl PyIqSamples {
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
    pub fn getitem(&self, index: IqSamplesIndex<'_>) -> PyResult<IqSamplesIndexed> {
        self.get(index)?
            .ok_or_else(|| PyIndexError::new_err("sample index out of range"))
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
            def get(self, index: pyo3_stub_gen.RustType["IqSamplesIndex<'_>"]) -> pyo3_stub_gen.RustType["Option<IqSamplesIndexed>"]: ...

            @overload
            def __getitem__(self, index: int) -> complex: ...
            @overload
            def __getitem__(self, index: slice[typing.Optional[int]]) -> list[complex]: ...
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
