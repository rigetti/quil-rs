//! Create and manipulate `collections.abc.Sequence`-compatible views exposed to Python.
//!
//! The core struct here is [`SequenceOf<R>`], which wraps a closure
//! that fetches an item at a given index from some Python-based data structure.
//! This struct (together with a reference to the Python object that owns the data)
//! can be provided to Python users via the [`Sequence`] Python class,
//! which implements the `Sequence` protocol, so from a Python user's perspective
//! it behaves much like a `list`, except that it does not provide mutation operations.

use std::marker::PhantomData;
use std::sync::Arc;

use pyo3::{
    exceptions::{PyIndexError, PyStopIteration, PyValueError},
    prelude::*,
    pyclass::boolean_struct::{False, True},
    types::{PySlice, PySliceIndices},
    IntoPyObjectExt, PyClass,
};

#[cfg(feature = "stubs")]
use pyo3::types::PyIterator;

#[cfg(feature = "stubs")]
use pyo3_stub_gen::{
    derive::{gen_stub_pyclass, gen_stub_pymethods},
    runtime::PyRuntimeType,
    PyStubType, TypeInfo,
};

/// A function that returns an item at a given index.
type GetItemFn = dyn for<'py> Fn(Python<'py>, usize) -> PyResult<Py<PyAny>> + Send + Sync;

pub(crate) fn register_abcs(py: Python<'_>) -> PyResult<()> {
    pyo3::types::PySequence::register::<Sequence>(py)?;
    Ok(())
}

/// An internal Rust type that provides access to sequences of Python items owned by a Rust type.
pub(crate) struct SequenceOf<R> {
    /// The underlying Sequence.
    sequence: Sequence,
    /// The result type of the Sequence's getitem function, used for type stub generation.
    result_type: PhantomData<R>,
}

impl<R> SequenceOf<R>
where
    R: for<'py> IntoPyObject<'py> + Send,
{
    /// Create a new `SequenceOf` that provides data owned by an immutable `obj` using a closure.
    pub(crate) fn new<T, F>(obj: Bound<'_, T>, len: usize, getitem: F) -> Self
    where
        T: PyClass<Frozen = True> + Sync,
        F: Fn(&T, usize) -> PyResult<Option<R>> + Send + Sync + 'static,
    {
        let inner = obj.clone().unbind().into_any();

        let obj = obj.unbind();
        let getitem: Arc<GetItemFn> = Arc::new(move |py: Python<'_>, index| {
            getitem(obj.get(), index)?
                .ok_or_else(|| PyIndexError::new_err("index inconsistent with sequence length"))
                .and_then(|r| r.into_py_any(py))
        });

        let sequence = Sequence {
            inner,
            getitem,
            start: 0,
            step: 1,
            len,
        };

        Self {
            sequence,
            result_type: PhantomData,
        }
    }

    /// Create a new `SequenceOf` that provides data owned by a mutable `obj` using a closure.
    pub(crate) fn new_mut<T, F>(obj: Bound<'_, T>, len: usize, getitem: F) -> Self
    where
        T: PyClass<Frozen = False> + Sync,
        F: Fn(&T, usize) -> PyResult<Option<R>> + Send + Sync + 'static,
    {
        let inner = obj.clone().unbind().into_any();

        let obj = obj.unbind();
        let getitem: Arc<GetItemFn> = Arc::new(move |py: Python<'_>, index| {
            let obj = obj.bind(py).try_borrow()?;
            getitem(&obj, index)?
                .ok_or_else(|| PyIndexError::new_err("index inconsistent with sequence length"))
                .and_then(|r| r.into_py_any(py))
        });

        let sequence = Sequence {
            inner,
            getitem,
            start: 0,
            step: 1,
            len,
        };

        Self {
            sequence,
            result_type: PhantomData,
        }
    }

    /// Convert this `SequenceOf` into a standalone, single-pass [`SequenceIterator`].
    ///
    /// Used by [`IteratorOf`] to hand back an iterator
    /// simplifying the ability to return instances from other classes' `__iter__`s.
    fn into_iterator(self) -> SequenceIterator {
        SequenceIterator {
            sequence: self.sequence,
            position: 0,
            reverse: false,
        }
    }
}

/// Send [`SequenceOf`] to Python by dropping the generic parameters.
impl<'py, R> IntoPyObject<'py> for SequenceOf<R>
where
    R: for<'py2> IntoPyObject<'py2> + Send,
{
    type Target = Sequence;
    type Output = Bound<'py, Self::Target>;
    type Error = PyErr;

    fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
        self.sequence.into_pyobject(py)
    }
}

#[cfg(feature = "stubs")]
impl<R: PyStubType> PyStubType for SequenceOf<R> {
    fn type_output() -> TypeInfo {
        // Use the logic of `list_of`, but replace it with `collections.abc.Sequence`.
        let TypeInfo {
            name: _,
            source_module,
            mut import,
            type_refs,
        } = TypeInfo::list_of::<R>();

        let inner = <R as PyStubType>::type_output();

        // `list_of` automatically adds `builtins`, so we should remove it,
        // unless it were already in the original `inner` imports.
        let builtins = "builtins".into();
        if !inner.import.contains(&builtins) {
            import.remove(&builtins);
        }
        import.insert("collections.abc".into());

        TypeInfo {
            name: format!("collections.abc.Sequence[{}]", inner.name),
            source_module,
            import,
            type_refs,
        }
    }
}

#[cfg(feature = "stubs")]
impl<R: PyRuntimeType> PyRuntimeType for SequenceOf<R> {
    fn runtime_type_object(py: Python<'_>) -> PyResult<Bound<'_, PyAny>> {
        Ok(py.get_type::<PyIterator>().into_any())
    }
}

/// An index or slice passed to `Sequence.__getitem__`.
#[derive(Debug, FromPyObject)]
enum GetItemIndex<'py> {
    Int(isize),
    Slice(Bound<'py, PySlice>),
}

/// The result of `Sequence.__getitem__`: a single item or another lazy sub-view.
///
/// The sub-view is itself a [`Sequence`], sharing the root's `getitem` with a composed
/// `start`/`step` (see [`Sequence::slice_view`]).
#[derive(IntoPyObject)]
enum GetItemValue<T> {
    One(T),
    Many(Sequence),
}

#[cfg(feature = "stubs")]
impl<T: PyStubType> PyStubType for GetItemValue<T> {
    fn type_output() -> TypeInfo {
        <T as PyStubType>::type_output() | <SequenceOf<T> as PyStubType>::type_output()
    }
}

#[cfg(feature = "stubs")]
pyo3_stub_gen::impl_stub_type!(GetItemIndex<'_> = isize | PySlice);

/// Resolve a (possibly negative) Python index against a sequence of length `len`,
/// following the usual Python convention negative indices count from the end.
fn resolve_index(mut index: isize, len: usize) -> PyResult<usize> {
    if index < 0 {
        index = index
            .checked_add_unsigned(len)
            .ok_or_else(|| PyIndexError::new_err("index out of range"))?;
    }
    usize::try_from(index)
        .ok()
        .filter(|&index| index < len)
        .ok_or_else(|| PyIndexError::new_err("index out of range"))
}

/// Resolve a (possibly negative or out-of-range) `(start, stop)` pair
/// against a sequence of length `len`,
/// following the clamping convention used by `list.index`/`list.count`
/// (as opposed to the error-on-out-of-range convention used by plain indexing).
fn resolve_range(start: isize, stop: Option<isize>, len: usize) -> (usize, usize) {
    let signed_len = isize::try_from(len).unwrap_or(isize::MAX);
    let clamp = |index: isize| index.clamp(-signed_len, signed_len);
    let to_unsigned = |index: isize| {
        if index < 0 {
            len - usize::try_from(-index).unwrap_or(len).min(len)
        } else {
            usize::try_from(index).unwrap_or(len)
        }
    };

    let start = to_unsigned(clamp(start));
    let stop = stop.map(clamp).map(to_unsigned).unwrap_or(len);
    (start, stop.clamp(start, len))
}

/// Resolve a logical index `i` to an index into a view's root sequence,
/// via the affine map `start + i * step`.
fn resolve_affine(start: usize, step: isize, i: usize) -> PyResult<usize> {
    isize::try_from(start)
        .ok()
        .and_then(|start| {
            let offset = isize::try_from(i).ok()?.checked_mul(step)?;
            offset.checked_add(start)
        })
        .and_then(|index| usize::try_from(index).ok())
        .ok_or_else(|| PyIndexError::new_err("index inconsistent with sequence length"))
}

/// A Python class that supports the `Sequence` protocol over data owned by a Rust type.
///
/// This is an internal class not intended for direct construction by Python users.
/// Some classes in `quil` return this type to provide idiomatic and optimized data access.
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyo3::pyclass(module = "quil._quil", name = "_Sequence", frozen)]
pub(crate) struct Sequence {
    /// A reference to a Python object that owns sequence data.
    ///
    /// We need to keep a reference to integrate with Python's garbage collector,
    /// but we erase the type here so that we don't require it as a generic parameter.
    inner: Py<PyAny>,
    /// A function to access data from the Python object given an index.
    getitem: Arc<GetItemFn>,
    /// This view's offset into the root sequence, i.e. `self[0]` is `getitem(start)`.
    start: usize,
    /// This view's stride over the root sequence, i.e. `self[i]` is `getitem(start + i * step)`.
    step: isize,
    /// The length of the underlying sequence.
    ///
    /// Note that this implies the underlying sequence length should not change
    /// while the `SequenceOf` is alive; doing so is not unsafe, but it is a logic error.
    len: usize,
}

impl Sequence {
    /// Return a new `Sequence` with the same underlying data and `getitem` function,
    fn clone_ref(&self, py: Python<'_>) -> Self {
        Sequence {
            inner: self.inner.clone_ref(py),
            getitem: Arc::clone(&self.getitem),
            start: self.start,
            step: self.step,
            len: self.len,
        }
    }

    /// Get the item at logical index `i` (`0 <= i < self.len`)
    /// via the root sequence's `getitem` via this view's `start`/`step` affine map.
    fn getitem_at(&self, py: Python<'_>, i: usize) -> PyResult<Py<PyAny>> {
        let parent_index = resolve_affine(self.start, self.step, i)?;
        (self.getitem)(py, parent_index)
    }

    /// Build a new, independent `Sequence` re-indexed over this one
    /// by `start + i * step` for `i` in `0..len`.
    ///
    /// The affine maps are composed directly:
    /// `self[new_start + i * new_step]` is `root[self.start + (new_start + i * new_step) * self.step]`,
    /// i.e. `root[(self.start + new_start * self.step) + i * (new_step * self.step)]`.
    fn slice_view(
        &self,
        py: Python<'_>,
        start: usize,
        step: isize,
        len: usize,
    ) -> PyResult<Sequence> {
        let composed_start = resolve_affine(self.start, self.step, start)?;
        let composed_step = self
            .step
            .checked_mul(step)
            .ok_or_else(|| PyIndexError::new_err("index inconsistent with sequence length"))?;

        Ok(Sequence {
            inner: self.inner.clone_ref(py),
            getitem: Arc::clone(&self.getitem),
            start: composed_start,
            step: composed_step,
            len,
        })
    }

    fn iterator(&self, py: Python<'_>, reverse: bool) -> SequenceIterator {
        SequenceIterator {
            sequence: self.clone_ref(py),
            position: 0,
            reverse,
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Sequence {
    fn __len__(&self) -> usize {
        self.len
    }

    fn __length_hint__(&self) -> usize {
        self.len
    }

    fn __getitem__<'py>(
        &self,
        py: Python<'py>,
        index: GetItemIndex<'py>,
    ) -> PyResult<GetItemValue<Py<PyAny>>> {
        match index {
            GetItemIndex::Int(index) => {
                let index = resolve_index(index, self.len)?;
                self.getitem_at(py, index).map(GetItemValue::One)
            }

            GetItemIndex::Slice(slice) => {
                let len = isize::try_from(self.len)
                    .map_err(|_| PyValueError::new_err("sequence too long to slice"))?;
                let PySliceIndices {
                    start,
                    step,
                    slicelength,
                    ..
                } = slice.indices(len)?;

                let view = self.slice_view(py, start as usize, step, slicelength)?;
                Ok(GetItemValue::Many(view))
            }
        }
    }

    fn __contains__(&self, py: Python<'_>, value: Bound<'_, PyAny>) -> PyResult<bool> {
        for index in 0..self.len {
            if self.getitem_at(py, index)?.bind(py).eq(&value)? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    /// Return the index of the first occurrence of `value`.
    ///
    /// Raises `ValueError` if `value` is not present.
    ///
    /// Part of the `collections.abc.Sequence` interface.
    #[pyo3(signature = (value, start = 0, stop = None))]
    fn index(
        &self,
        py: Python<'_>,
        value: Bound<'_, PyAny>,
        start: isize,
        stop: Option<isize>,
    ) -> PyResult<usize> {
        let (start, stop) = resolve_range(start, stop, self.len);
        for index in start..stop {
            if self.getitem_at(py, index)?.bind(py).eq(&value)? {
                return Ok(index);
            }
        }
        Err(PyValueError::new_err(format!("{value} is not in sequence")))
    }

    /// Return the number of occurrences of `value`.
    ///
    /// Part of the `collections.abc.Sequence` interface.
    fn count(&self, py: Python<'_>, value: Bound<'_, PyAny>) -> PyResult<usize> {
        let mut count = 0;
        for index in 0..self.len {
            if self.getitem_at(py, index)?.bind(py).eq(&value)? {
                count += 1;
            }
        }
        Ok(count)
    }

    fn __eq__(&self, py: Python<'_>, other: Bound<'_, PyAny>) -> PyResult<bool> {
        let Ok(other_len) = other.len() else {
            return Ok(false);
        };
        if other_len != self.len {
            return Ok(false);
        }
        for index in 0..self.len {
            let Ok(other_item) = other.get_item(index) else {
                return Ok(false);
            };
            if !self.getitem_at(py, index)?.bind(py).eq(&other_item)? {
                return Ok(false);
            }
        }
        Ok(true)
    }

    fn __repr__(&self, py: Python<'_>) -> PyResult<String> {
        let mut items = Vec::with_capacity(self.len);
        for index in 0..self.len {
            items.push(self.getitem_at(py, index)?.bind(py).repr()?.to_string());
        }
        Ok(format!("[{}]", items.join(", ")))
    }

    fn __iter__<'py>(slf: &Bound<'py, Self>, py: Python<'py>) -> SequenceIterator {
        slf.get().iterator(py, false)
    }

    fn __reversed__<'py>(slf: &Bound<'py, Self>, py: Python<'py>) -> SequenceIterator {
        slf.get().iterator(py, true)
    }

    #[gen_stub(skip)]
    fn __traverse__(&self, visit: ::pyo3::PyVisit<'_>) -> Result<(), ::pyo3::PyTraverseError> {
        visit.call(&self.inner)
    }
}

/// An internal Rust type that provides iteration over Python items owned by a Rust type.
///
/// This is used to automate type stub generation.
pub(crate) struct IteratorOf<R> {
    inner: SequenceIterator,
    return_type: PhantomData<R>,
}

impl<R> IteratorOf<R>
where
    R: for<'py> IntoPyObject<'py> + Send,
{
    /// Create a new `IteratorOf` over data owned by an immutable `obj` using a closure.
    pub(crate) fn new<T, F>(obj: Bound<'_, T>, len: usize, getitem: F) -> Self
    where
        T: PyClass<Frozen = True> + Sync,
        F: Fn(&T, usize) -> PyResult<Option<R>> + Send + Sync + 'static,
    {
        Self {
            inner: SequenceOf::new(obj, len, getitem).into_iterator(),
            return_type: PhantomData,
        }
    }
}

impl<'py, R> IntoPyObject<'py> for IteratorOf<R>
where
    R: for<'py2> IntoPyObject<'py2> + Send,
{
    type Target = SequenceIterator;
    type Output = Bound<'py, Self::Target>;
    type Error = PyErr;

    fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
        self.inner.into_pyobject(py)
    }
}

#[cfg(feature = "stubs")]
impl<R: PyStubType> PyStubType for IteratorOf<R> {
    fn type_output() -> TypeInfo {
        // Use the logic of `list_of`, but replace it with `collections.abc.Iterator`.
        let TypeInfo {
            name: _,
            source_module,
            mut import,
            type_refs,
        } = TypeInfo::list_of::<R>();

        let inner = <R as PyStubType>::type_output();

        // `list_of` automatically adds `builtins`, so we should remove it,
        // unless it were already in the original `inner` imports.
        let builtins = "builtins".into();
        if !inner.import.contains(&builtins) {
            import.remove(&builtins);
        }
        import.insert("collections.abc".into());

        TypeInfo {
            name: format!("collections.abc.Iterator[{}]", inner.name),
            source_module,
            import,
            type_refs,
        }
    }
}

#[cfg(feature = "stubs")]
impl<R: PyRuntimeType> PyRuntimeType for IteratorOf<R> {
    fn runtime_type_object(py: Python<'_>) -> PyResult<Bound<'_, PyAny>> {
        Ok(py.get_type::<PyIterator>().into_any())
    }
}

/// A standalone, single-pass iterator handed back by `Sequence.__iter__`/`Sequence.__reversed__`.
///
/// This is an internal class not intended for direct construction by Python users.
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyo3::pyclass(module = "quil._quil")]
pub(crate) struct SequenceIterator {
    sequence: Sequence,
    position: usize,
    reverse: bool,
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl SequenceIterator {
    fn __next__(&mut self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        if self.position >= self.sequence.len {
            return Err(PyStopIteration::new_err(()));
        }
        let index = if self.reverse {
            self.sequence.len - 1 - self.position
        } else {
            self.position
        };
        self.position += 1;
        let parent_index = resolve_affine(self.sequence.start, self.sequence.step, index)?;
        (self.sequence.getitem)(py, parent_index)
    }

    fn __iter__(slf: PyRef<'_, Self>) -> PyRef<'_, Self> {
        slf
    }

    fn __length_hint__(&self) -> usize {
        self.sequence.len.saturating_sub(self.position)
    }

    #[gen_stub(skip)]
    fn __traverse__(&self, visit: ::pyo3::PyVisit<'_>) -> Result<(), ::pyo3::PyTraverseError> {
        visit.call(&self.sequence.inner)
    }
}

#[cfg(test)]
mod tests {
    use pyo3::types::PySlice;
    use rstest::rstest;

    use super::*;

    #[rstest]
    #[case::first(0, 5, 0)]
    #[case::last(4, 5, 4)]
    #[case::negative_last(-1, 5, 4)]
    #[case::negative_first(-5, 5, 0)]
    fn resolve_index_valid(#[case] index: isize, #[case] len: usize, #[case] expected: usize) {
        assert_eq!(resolve_index(index, len).unwrap(), expected);
    }

    #[rstest]
    #[case::positive_out_of_range(5, 5)]
    #[case::negative_out_of_range(-6, 5)]
    #[case::empty(0, 0)]
    fn resolve_index_out_of_range(#[case] index: isize, #[case] len: usize) {
        assert!(resolve_index(index, len).is_err());
    }

    #[rstest]
    // Plain in-range start/stop.
    #[case::plain(1, Some(3), 5, (1, 3))]
    // Negative indices count from the end.
    #[case::negative_start(-3, Some(4), 5, (2, 4))]
    #[case::negative_stop(1, Some(-1), 5, (1, 4))]
    // Out-of-range values clamp rather than error, matching `list.index`/`list.count`.
    #[case::start_too_large(10, None, 5, (5, 5))]
    #[case::stop_too_large(0, Some(10), 5, (0, 5))]
    #[case::start_too_negative(-10, None, 5, (0, 5))]
    // `stop` defaults to `len`.
    #[case::no_stop(2, None, 5, (2, 5))]
    // A `stop` before `start` clamps up to `start`, yielding an empty range rather than panicking.
    #[case::stop_before_start(3, Some(1), 5, (3, 3))]
    #[case::empty(0, None, 0, (0, 0))]
    fn resolve_range_cases(
        #[case] start: isize,
        #[case] stop: Option<isize>,
        #[case] len: usize,
        #[case] expected: (usize, usize),
    ) {
        assert_eq!(resolve_range(start, stop, len), expected);
    }

    #[rstest]
    #[case::zero(0, 1, 0, 0)]
    #[case::offset_only(3, 1, 0, 3)]
    #[case::positive_step(0, 2, 5, 10)]
    #[case::negative_step(10, -2, 3, 4)]
    fn resolve_affine_valid(
        #[case] start: usize,
        #[case] step: isize,
        #[case] i: usize,
        #[case] expected: usize,
    ) {
        assert_eq!(resolve_affine(start, step, i).unwrap(), expected);
    }

    #[test]
    fn resolve_affine_overflow_errors_instead_of_panicking() {
        // `i * step` overflows `isize` here; this must raise rather than wrap or panic
        // (in a debug build, the unchecked equivalent would panic).
        assert!(resolve_affine(0, isize::MAX, 2).is_err());
        assert!(resolve_affine(usize::MAX, 1, 1).is_err());
    }

    /// A frozen owner pyclass, used to test the `SequenceOf::new`/`IteratorOf::new` paths.
    #[pyo3::pyclass(frozen)]
    struct FrozenOwner {
        data: Vec<i64>,
    }

    /// A non-frozen owner pyclass, used to test the
    /// `SequenceOf::new_mut`/`IteratorOf::new_mut` paths.
    #[pyo3::pyclass]
    struct MutOwner {
        data: Vec<i64>,
    }

    /// Build a `Sequence` (bound to Python) over `data`, via `SequenceOf::new`.
    fn make_sequence(py: Python<'_>, data: Vec<i64>) -> Bound<'_, Sequence> {
        let len = data.len();
        let owner = Bound::new(py, FrozenOwner { data }).unwrap();
        SequenceOf::new(owner, len, |owner: &FrozenOwner, index| {
            Ok(owner.data.get(index).copied())
        })
        .into_pyobject(py)
        .unwrap()
    }

    /// Extract a `Py<PyAny>` returned by `Sequence`/`SequenceIterator` machinery back to `i64`.
    fn as_i64(py: Python<'_>, value: &Py<PyAny>) -> i64 {
        value.bind(py).extract().unwrap()
    }

    /// Collect a (possibly sliced) `Sequence` by indexing `0..len`, for comparison against
    /// the equivalent plain `Vec` slice.
    fn collect(py: Python<'_>, seq: &Bound<'_, Sequence>) -> Vec<i64> {
        let len = seq.borrow().len;
        (0..len)
            .map(|i| {
                let GetItemValue::One(value) = seq
                    .borrow()
                    .__getitem__(py, GetItemIndex::Int(i as isize))
                    .unwrap()
                else {
                    panic!("indexing with an int must return `GetItemValue::One`");
                };
                as_i64(py, &value)
            })
            .collect()
    }

    /// Slice `seq` using Python's own `slice.indices()` semantics, by building a real
    /// `PySlice` and passing it through `Sequence::__getitem__`.
    fn slice<'py>(
        py: Python<'py>,
        seq: &Bound<'py, Sequence>,
        start: isize,
        stop: isize,
        step: isize,
    ) -> Bound<'py, Sequence> {
        let index = GetItemIndex::Slice(PySlice::new(py, start, stop, step));
        match seq.borrow().__getitem__(py, index).unwrap() {
            GetItemValue::Many(sub) => sub.into_pyobject(py).unwrap(),
            GetItemValue::One(_) => panic!("slicing must return `GetItemValue::Many`"),
        }
    }

    #[test]
    fn len_and_plain_indexing() {
        Python::initialize();
        Python::attach(|py| {
            let seq = make_sequence(py, vec![10, 20, 30, 40, 50]);
            assert_eq!(seq.borrow().__len__(), 5);
            assert_eq!(collect(py, &seq), vec![10, 20, 30, 40, 50]);

            let GetItemValue::One(last) =
                seq.borrow().__getitem__(py, GetItemIndex::Int(-1)).unwrap()
            else {
                panic!("expected `GetItemValue::One`");
            };
            assert_eq!(as_i64(py, &last), 50);

            assert!(seq.borrow().__getitem__(py, GetItemIndex::Int(5)).is_err());
            assert!(seq.borrow().__getitem__(py, GetItemIndex::Int(-6)).is_err());
        });
    }

    #[test]
    fn slicing_matches_vec_slicing() {
        Python::initialize();
        Python::attach(|py| {
            let data = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
            let seq = make_sequence(py, data.clone());

            let cases: &[(isize, isize, isize)] = &[
                (0, 10, 1),
                (2, 8, 1),
                (0, 10, 2),
                (1, 10, 3),
                (9, isize::MIN, -1),
                (9, -1, -2),
                (3, 3, 1),
                (0, isize::MAX, 1),
            ];
            for &(start, stop, step) in cases {
                let view = slice(py, &seq, start, stop, step);
                let expected: Vec<i64> = {
                    let py_slice = PySlice::new(py, start, stop, step);
                    let PySliceIndices {
                        start, stop, step, ..
                    } = py_slice.indices(data.len() as isize).unwrap();
                    let mut out = Vec::new();
                    let mut i = start;
                    if step > 0 {
                        while i < stop {
                            out.push(data[i as usize]);
                            i += step;
                        }
                    } else {
                        while i > stop {
                            out.push(data[i as usize]);
                            i += step;
                        }
                    }
                    out
                };
                assert_eq!(
                    collect(py, &view),
                    expected,
                    "slice({start}, {stop}, {step}) mismatch"
                );
            }
        });
    }

    #[test]
    fn chained_slicing_composes_without_revisiting_skipped_items() {
        Python::initialize();
        Python::attach(|py| {
            let data: Vec<i64> = (0..30).collect();
            let seq = make_sequence(py, data);

            // `x[::2][::3]` should behave exactly like `x[::6]`.
            let double_sliced = slice(py, &slice(py, &seq, 0, 30, 2), 0, isize::MAX, 3);
            let direct = slice(py, &seq, 0, 30, 6);
            assert_eq!(collect(py, &double_sliced), collect(py, &direct));
            assert_eq!(collect(py, &direct), vec![0, 6, 12, 18, 24]);

            // Slicing a negative-step view with another negative step composes to a
            // positive step, matching plain `list` semantics.
            let reversed = slice(py, &seq, 29, isize::MIN, -1);
            let re_reversed = slice(py, &reversed, 29, isize::MIN, -1);
            assert_eq!(collect(py, &re_reversed), collect(py, &seq));
        });
    }

    #[test]
    fn deeply_chained_slicing_does_not_panic_at_realistic_depths() {
        Python::initialize();
        Python::attach(|py| {
            let data: Vec<i64> = (0..1_000_000).collect();
            let seq = make_sequence(py, data);

            // Chain 20 levels of `[::2]`: the effective composed step doubles each time
            // (up to `2^20`), well within `isize`, and must resolve in `O(1)` per access
            // regardless of depth (no nested closures to walk through).
            let mut view = seq;
            for _ in 0..20 {
                view = slice(py, &view, 0, isize::MAX, 2);
            }
            assert_eq!(collect(py, &view), vec![0]);
        });
    }

    #[test]
    fn composed_step_overflow_errors_instead_of_panicking() {
        Python::initialize();
        Python::attach(|py| {
            let seq = make_sequence(py, vec![1, 2, 3]);
            let once = slice(py, &seq, 0, isize::MAX, isize::MAX);
            // Composing another non-trivial step must overflow `isize::MAX * 2`
            // and raise `IndexError` rather than panicking.
            let index = GetItemIndex::Slice(PySlice::new(py, 0, isize::MAX, 2));
            assert!(once.borrow().__getitem__(py, index).is_err());
        });
    }

    #[test]
    fn contains_index_and_count() {
        Python::initialize();
        Python::attach(|py| {
            let seq = make_sequence(py, vec![1, 2, 2, 3]);
            let value = |v: i64| v.into_pyobject(py).unwrap().into_any();

            assert!(seq.borrow().__contains__(py, value(2)).unwrap());
            assert!(!seq.borrow().__contains__(py, value(99)).unwrap());

            assert_eq!(seq.borrow().index(py, value(2), 0, None).unwrap(), 1);
            assert_eq!(seq.borrow().index(py, value(2), 2, None).unwrap(), 2);
            assert!(seq.borrow().index(py, value(99), 0, None).is_err());

            assert_eq!(seq.borrow().count(py, value(2)).unwrap(), 2);
            assert_eq!(seq.borrow().count(py, value(99)).unwrap(), 0);
        });
    }

    #[test]
    fn eq_and_repr() {
        Python::initialize();
        Python::attach(|py| {
            let a = make_sequence(py, vec![1, 2, 3]);
            let b = make_sequence(py, vec![1, 2, 3]);
            let c = make_sequence(py, vec![1, 2]);

            assert!(a.borrow().__eq__(py, b.clone().into_any()).unwrap());
            assert!(!a.borrow().__eq__(py, c.into_any()).unwrap());

            let list = pyo3::types::PyList::new(py, [1, 2, 3]).unwrap();
            assert!(a.borrow().__eq__(py, list.into_any()).unwrap());

            assert_eq!(a.borrow().__repr__(py).unwrap(), "[1, 2, 3]");
        });
    }

    #[test]
    fn iter_is_single_pass_and_reiterable() {
        Python::initialize();
        Python::attach(|py| {
            let seq = make_sequence(py, vec![1, 2, 3]);

            let mut iterator = Sequence::__iter__(&seq, py);
            let mut collected = Vec::new();
            while let Ok(value) = iterator.__next__(py) {
                collected.push(as_i64(py, &value));
            }
            assert_eq!(collected, vec![1, 2, 3]);
            // Exhausted iterators keep raising `StopIteration`, matching real iterators.
            assert!(iterator.__next__(py).is_err());

            // A fresh call to `__iter__` must produce an independent, fully-reset iterator.
            let mut second = Sequence::__iter__(&seq, py);
            assert_eq!(as_i64(py, &second.__next__(py).unwrap()), 1);
        });
    }

    #[test]
    fn reversed_iterates_back_to_front() {
        Python::initialize();
        Python::attach(|py| {
            let seq = make_sequence(py, vec![1, 2, 3]);
            let mut iterator = Sequence::__reversed__(&seq, py);
            let mut collected = Vec::new();
            while let Ok(value) = iterator.__next__(py) {
                collected.push(as_i64(py, &value));
            }
            assert_eq!(collected, vec![3, 2, 1]);
        });
    }

    #[test]
    fn iterator_of_yields_a_single_pass_sequence_iterator() {
        Python::initialize();
        Python::attach(|py| {
            let data = vec![4, 5, 6];
            let len = data.len();
            let owner = Bound::new(py, FrozenOwner { data }).unwrap();
            let iterator_of = IteratorOf::new(owner, len, |owner: &FrozenOwner, index| {
                Ok(owner.data.get(index).copied())
            });

            let bound = iterator_of.into_pyobject(py).unwrap();
            let mut collected = Vec::new();
            loop {
                let mut this = bound.borrow_mut();
                match this.__next__(py) {
                    Ok(value) => collected.push(as_i64(py, &value)),
                    Err(_) => break,
                }
            }
            assert_eq!(collected, vec![4, 5, 6]);
        });
    }

    #[test]
    fn new_mut_reads_through_a_mutable_owner() {
        Python::initialize();
        Python::attach(|py| {
            let data = vec![7, 8, 9];
            let len = data.len();
            let owner = Bound::new(py, MutOwner { data }).unwrap();
            let seq: Bound<'_, Sequence> =
                SequenceOf::new_mut(owner, len, |owner: &MutOwner, index| {
                    Ok(owner.data.get(index).copied())
                })
                .into_pyobject(py)
                .unwrap();

            assert_eq!(collect(py, &seq), vec![7, 8, 9]);
        });
    }
}
