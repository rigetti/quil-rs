//! Types and helpers for working with [`MemoryReference`] and [`Delcaration`] from Python.

use pyo3::{
    exceptions::PyValueError,
    prelude::*,
    types::{PyAny, PyList, PyTuple},
    CastError, PyTraverseError, PyTypeCheck, PyVisit,
};

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pymethods};

use crate::{
    instruction::{
        quilpy::{ArithmeticOperandLike, BinaryOperandLike, ComparisonOperandLike},
        Arithmetic, ArithmeticOperand, ArithmeticOperator, BinaryLogic, BinaryOperator, Comparison,
        ComparisonOperator, Convert, Declaration, Exchange, Load, MemoryReference, Move,
        ScalarType, Store, UnaryLogic, UnaryOperator,
    },
    quilpy::errors,
};

// TODO: This should probably be in a different module.
/// A trait for types that might be able to provide a [`Borrowed`] to Python data.
pub(crate) trait TryAsBorrowed<'py, T> {
    fn try_borrow<'a>(&'a self) -> Option<Borrowed<'a, 'py, T>>;
}

#[derive(FromPyObject)]
pub(crate) struct MemoryReferencePair {
    #[pyo3(item(0))]
    name: String,
    #[pyo3(item(1))]
    index: u64,
}

/// An object in the Python heap that can be converted into a [`MemoryReference`].
///
/// If a Python user derives values from [`Declaration`]s to use as [`MemoryReference`]s,
/// we can provide additional validation when they use them to create instructions.
/// See [`DeclarationAt`] for examples how these fit together for program building.
///
/// Prefer this to `MemoryReferenceLike` if there would be value (e.g., for validation)
/// in knowing the underlying [`Declaration`] it is meant to refer to,
#[derive(FromPyObject)]
pub(crate) enum PyMemRef<'py> {
    DeclarationAt(Bound<'py, DeclarationAt>),
    Declaration(Bound<'py, Declaration>),
    MemoryReference(Bound<'py, MemoryReference>),
}

// Generally we don't want to return this as a Python type,
// but the stub generator needs this when constructing default values in signatures.
#[cfg_attr(feature = "stubs", derive(IntoPyObject))]
pub(crate) struct MemoryReferenceLike(MemoryReference);

impl From<MemoryReferenceLike> for MemoryReference {
    fn from(value: MemoryReferenceLike) -> Self {
        value.0
    }
}

impl Declaration {
    /// Create a `MemoryReference` from `self`, copying the underlying `Declaration`'s name.
    pub(crate) fn to_memory_reference(&self, index: u64) -> MemoryReference {
        MemoryReference::new(self.name.clone(), index)
    }
}

// This is essentially what a "memory reference" really is:
// a reference at a specific index to a declared region of memory.
/// A wrapper around a [`Declaration`] for use in places we'd normally need a `MemoryReference`.
///
/// You can get an instance of `DeclarationAt` by indexing a `Declaration`,
/// and you can then use it in places where a `MemoryReference` is expected.
///
/// # Example
///
/// With use of the warlus operator, you can often write a list of instructions
/// without needing to explicitly declare `MemoryReference` objects:
///
/// ```python
/// instructions = [
///     counter := Declaration("counter", ScalarType.INTEGER),      # DECLARE counter INTEGER
///     counter[0].move(10),                                        # MOVE counter[0] 10
///     top := Label("top"),                                        # LABEL @top
///     # additional instructions...
///     counter[0].sub(1),                                          # SUB counter[0] 1
///     JumpWhen(top, counter[0]),                                  # JUMP-WHEN @top counter[0]
/// ]
/// ```
#[derive(Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(module = "quil._quil.instructions", frozen, skip_from_py_object)]
pub(crate) struct DeclarationAt {
    // TODO: instead of (or in addition to) the Py<Declaration>,
    // we should store the name as a PyString and use it to generate PyMemRefs instead,
    // so that they can all share the same underlying string object;
    // that will take some reworking of the PyMemRef to use Py<_> fields instead,
    // and likely will need some work for proper interop.
    // The point is the same: we can do better.
    declaration: Py<Declaration>,
    index: u64,
}

impl<'a, 'py> FromPyObject<'a, 'py> for DeclarationAt {
    type Error = PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        // This "extraction" actually just increases the refcount.
        let decl_at = if let Ok(decl) = obj.cast::<DeclarationAt>() {
            DeclarationAt {
                declaration: decl.get().declaration.clone_ref(obj.py()),
                index: decl.get().index,
            }
        } else if let Ok(decl) = obj.cast::<Declaration>() {
            DeclarationAt {
                declaration: decl.to_owned().unbind(), // This just increases the refcount.
                index: 0,
            }
        } else {
            return Err(CastError::new(obj, Declaration::classinfo_object(obj.py())))?;
        };

        Ok(decl_at)
    }
}

impl DeclarationAt {
    pub fn new(declaration: Py<Declaration>, index: u64) -> Self {
        Self { declaration, index }
    }

    /// Return a `MemoryReference` to the underlying `Declaration` at the given index.
    ///
    /// This makes a clone the `Declaration`'s name.
    pub fn memref<'py>(&self, py: Python<'py>) -> MemoryReference {
        self.declaration
            .bind(py)
            .get()
            .to_memory_reference(self.index)
    }

    /// Return an error if a comparison instruction for `self := a <op> b`
    /// is known to produce invalid Quil.
    pub fn check_comparison<'py>(
        &self,
        a: &PyMemRef<'py>,
        b: &ComparisonOperandLike<'py>,
    ) -> PyResult<()> {
        if self.declaration.get().size.data_type != ScalarType::Bit {
            // TODO: make a custom error type for this
            return Err(errors::ValueError::new_err(
                "the destination of a comparison must be BIT-typed",
            ));
        }

        if matches!(same_type(a, b), Some(false)) {
            return Err(errors::ValueError::new_err(
                "the left-hand side and right-hand side of a comparison must have the same type",
            ));
        }

        Ok(())
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl DeclarationAt {
    /// Return a new `Move` instruction representing `self = value`.
    ///
    /// # Example
    ///
    /// ```python
    /// from quil.instructions import Declaration, ScalarType, Move
    ///
    /// x = Declaration("x", ScalarType.INTEGER, 3)
    /// mv = x[2].move(5)
    /// assert isinstance(mv, Move)
    /// assert mv.to_quil() == "MOVE x[2] 5"  # x[2] := 5
    /// ```
    #[pyo3(name = "move", signature = (value, /))]
    fn py_move<'py>(&self, py: Python<'py>, value: ArithmeticOperandLike) -> Move {
        Move::new(self.memref(py), value.into())
    }

    /// Return a new `Exchange` instruction representing the exchange of `self` and `other`.
    ///
    /// # Example
    ///
    /// ```Python
    /// from quil.instructions import Declaration, ScalarType, Exchange
    ///
    /// x = Declaration("x", ScalarType.INTEGER, 3)
    /// y = Declaration("y", ScalarType.INTEGER, 3)
    /// ex = x[2].exchange(y[1])
    /// assert isinstance(ex, Exchange)
    /// assert ex.to_quil() == "EXCHANGE x[2] y[1]"  # x[2] <-> y[1]
    /// ```
    #[pyo3(signature = (other, /))]
    fn exchange<'py>(&self, py: Python<'py>, other: MemoryReferenceLike) -> Exchange {
        Exchange::new(self.memref(py), other.into())
    }

    /// Return a new `Load` instruction representing the indirect load `self = source[offset]`.
    ///
    /// # Example
    ///
    /// ```python
    /// from quil.instructions import Declaration, ScalarType, Load
    ///
    /// x = Declaration("x", ScalarType.INTEGER, 3)
    /// y = Declaration("y", ScalarType.INTEGER, 3)
    /// z = Declaration("z", ScalarType.INTEGER, 3)
    /// ld = x[2].load_from(y, z[1])
    /// assert isinstance(ld, Load)
    /// assert ld.to_quil() == "LOAD x[2] y z[1]"  # x[2] := y[z[1]]
    /// ```
    #[pyo3(signature = (source, offset, /))]
    fn load_from<'py>(
        &self,
        py: Python<'py>,
        source: &Declaration,
        offset: MemoryReferenceLike,
    ) -> Load {
        Load::new(self.memref(py), source.name.clone(), offset.into())
    }

    /// Return a new `Store` instruction representing the indirect store `destination[offset] = self`.
    ///
    /// # Example
    ///
    /// ```python
    /// from quil.instructions import Declaration, ScalarType, Store
    ///
    /// x = Declaration("x", ScalarType.INTEGER, 3)
    /// y = Declaration("y", ScalarType.INTEGER, 3)
    /// z = Declaration("z", ScalarType.INTEGER, 3)
    /// st = x[2].store_to(y, z[1])
    /// assert isinstance(st, Store)
    /// assert st.to_quil() == "STORE y z[1] x[2]"  # y[z[1]] := x[2]
    /// ```
    #[pyo3(signature = (destination, offset, /))]
    fn store_to<'py>(
        &self,
        py: Python<'py>,
        destination: &Declaration,
        offset: MemoryReferenceLike,
    ) -> Store {
        Store::new(
            destination.name.clone(),
            offset.into(),
            ArithmeticOperand::MemoryReference(self.memref(py)),
        )
    }

    /// Return a new `Convert` instruction representing `self = (T)source`.
    ///
    /// This is like a `Move` instruction with a cast from the source type to the destination type.
    /// Note that in Quil, the source of a `CONVERT` instruction MUST be a `MemoryReference`,
    /// so this method only accepts `MemoryReference`s and things that can be converted into them,
    /// not arbitrary numeric literals.
    ///
    /// # Example
    ///
    /// ```python
    /// from quil.instructions import Declaration, ScalarType, Convert
    ///
    /// x = Declaration("x", ScalarType.INTEGER, 3)
    /// y = Declaration("y", ScalarType.REAL, 3)
    /// cvt = x[2].convert(y[1])
    /// assert isinstance(cvt, Convert)
    /// assert cvt.to_quil() == "CONVERT x[2] y[1]"  # x[2] := (INTEGER) y[1]
    /// ```
    #[pyo3(signature = (source, /))]
    fn convert<'py>(&self, py: Python<'py>, source: MemoryReferenceLike) -> Convert {
        Convert::new(self.memref(py), source.into())
    }

    // Note: These are not implemented as Python dunder arithmetic methods,
    // because the point is to return an `Arithmetic` instruction.
    //
    // What might be nice in the future is to implement something that supports chained operations.
    // For example, a user could write `x[2] + 5 - 3`, and we'd know it represents
    // a sequence of instructions (`ADD x[2] 5` followed by `SUB x[2] 3`);
    // but that would require a fair bit of implementation work we're not interested in right now,
    // and we don't want to block ourselves from doing it in the future.

    /// Return a new instruction representing a logical `NOT` of this memory reference.
    ///
    /// # Example
    ///
    /// ```python
    /// from quil.instructions import Declaration, ScalarType, UnaryLogic
    ///
    /// x = Declaration("x", ScalarType.INTEGER, 3)
    /// n = x[2].not()
    /// assert isinstance(n, UnaryLogic)
    /// assert n.to_quil() == "NOT x[2]"  # x[2] := ~x[2]
    /// ```
    fn not_<'py>(&self, py: Python<'py>) -> UnaryLogic {
        UnaryLogic::new(UnaryOperator::Not, self.memref(py))
    }

    fn neg<'py>(&self, py: Python<'py>) -> UnaryLogic {
        UnaryLogic::new(UnaryOperator::Neg, self.memref(py))
    }

    #[pyo3(signature = (other, /))]
    fn and_<'py>(&self, py: Python<'py>, other: BinaryOperandLike) -> BinaryLogic {
        BinaryLogic::new(BinaryOperator::And, self.memref(py), other.into())
    }

    #[pyo3(signature = (other, /))]
    fn ior<'py>(&self, py: Python<'py>, other: BinaryOperandLike) -> BinaryLogic {
        BinaryLogic::new(BinaryOperator::And, self.memref(py), other.into())
    }

    #[pyo3(signature = (other, /))]
    fn xor<'py>(&self, py: Python<'py>, other: BinaryOperandLike) -> BinaryLogic {
        BinaryLogic::new(BinaryOperator::And, self.memref(py), other.into())
    }

    /// Return a new `Arithmetic` instruction representing `ADD self other`.
    ///
    /// # Example
    ///
    /// ```python
    /// from quil.instructions import Declaration, ScalarType
    ///
    /// x = Declaration("x", ScalarType.INTEGER, 3)
    /// arith = x[2].add(5)
    /// assert isinstance(arith, Arithmetic)
    /// assert arith.to_quil() == "ADD x[2] 5"
    /// ```
    #[pyo3(signature = (other, /))]
    fn add<'py>(&self, py: Python<'py>, other: ArithmeticOperandLike) -> Arithmetic {
        Arithmetic::new(ArithmeticOperator::Add, self.memref(py), other.into())
    }

    #[pyo3(signature = (other, /))]
    fn sub<'py>(&self, py: Python<'py>, other: ArithmeticOperandLike) -> Arithmetic {
        Arithmetic::new(ArithmeticOperator::Subtract, self.memref(py), other.into())
    }

    #[pyo3(signature = (other, /))]
    fn div<'py>(&self, py: Python<'py>, other: ArithmeticOperandLike) -> Arithmetic {
        Arithmetic::new(ArithmeticOperator::Divide, self.memref(py), other.into())
    }

    #[pyo3(signature = (other, /))]
    fn mul<'py>(&self, py: Python<'py>, other: ArithmeticOperandLike) -> Arithmetic {
        Arithmetic::new(ArithmeticOperator::Multiply, self.memref(py), other.into())
    }

    /// Return a new `Comparison` instruction representing `self = (a == b)`.
    ///
    /// Note that `self` must represent a `MemoryReference` of type `BIT`,
    /// the left-hand side of the comparison must be a `MemoryReference`-like object,
    /// and the right-hand side must have a type compatible with the left-hand side.
    /// See the Quil specification for more details on valid comparisons.
    #[pyo3(signature = (a, b, /))]
    fn store_eq<'py>(
        &self,
        py: Python<'py>,
        a: PyMemRef<'py>,
        b: ComparisonOperandLike<'py>,
    ) -> PyResult<Comparison> {
        self.check_comparison(&a, &b)?;
        Ok(Comparison::new(
            ComparisonOperator::Equal,
            self.memref(py),
            a.into(),
            b.into(),
        ))
    }

    #[pyo3(signature = (a, b, /))]
    fn store_gt<'py>(
        &self,
        py: Python<'py>,
        a: PyMemRef<'py>,
        b: ComparisonOperandLike<'py>,
    ) -> PyResult<Comparison> {
        self.check_comparison(&a, &b)?;
        Ok(Comparison::new(
            ComparisonOperator::GreaterThan,
            self.memref(py),
            a.into(),
            b.into(),
        ))
    }

    #[pyo3(signature = (a, b, /))]
    fn store_ge<'py>(
        &self,
        py: Python<'py>,
        a: PyMemRef<'py>,
        b: ComparisonOperandLike<'py>,
    ) -> PyResult<Comparison> {
        self.check_comparison(&a, &b)?;
        Ok(Comparison::new(
            ComparisonOperator::GreaterThanOrEqual,
            self.memref(py),
            a.into(),
            b.into(),
        ))
    }

    #[pyo3(signature = (a, b, /))]
    fn store_lt<'py>(
        &self,
        py: Python<'py>,
        a: PyMemRef<'py>,
        b: ComparisonOperandLike<'py>,
    ) -> PyResult<Comparison> {
        self.check_comparison(&a, &b)?;
        Ok(Comparison::new(
            ComparisonOperator::LessThan,
            self.memref(py),
            a.into(),
            b.into(),
        ))
    }

    #[pyo3(signature = (a, b, /))]
    fn store_le<'py>(
        &self,
        py: Python<'py>,
        a: PyMemRef<'py>,
        b: ComparisonOperandLike<'py>,
    ) -> PyResult<Comparison> {
        self.check_comparison(&a, &b)?;
        Ok(Comparison::new(
            ComparisonOperator::LessThanOrEqual,
            self.memref(py),
            a.into(),
            b.into(),
        ))
    }

    // Garbage collection integration. For more information, see:
    // https://pyo3.rs/v0.29.0/class/protocols.html#garbage-collector-integration
    #[gen_stub(skip)]
    fn __traverse__(&self, visit: PyVisit<'_>) -> Result<(), PyTraverseError> {
        visit.call(&self.declaration)?;
        Ok(())
    }
}

impl<'a, 'py> FromPyObject<'a, 'py> for MemoryReferenceLike {
    type Error = PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        let mem_ref = if let Ok(mem_ref) = obj.cast::<MemoryReference>() {
            mem_ref.borrow().clone()
        } else if let Ok(decl) = obj.cast::<DeclarationAt>() {
            // Create a new `MemoryReference` from an underlying `Declaration` and index.
            decl.get().memref(obj.py())
        } else if let Ok(decl) = obj.cast::<Declaration>() {
            // Create a new `MemoryReference` from a `Declaration` assuming an index of 0.
            decl.get().to_memory_reference(0)
        } else if let Ok(s) = obj.cast::<PyTuple>() {
            // Create a new `MemoryReference` from a tuple of `(str, int)` pair.
            let (name, index) = s.extract()?;
            MemoryReference::new(name, index)
        } else if let Ok(s) = obj.cast::<PyList>() {
            // As above, but from a list of `[str, int]` pair.
            let len = obj.len()?;
            if len != 2 {
                return Err(PyValueError::new_err(
                    "expected list of length 2, but got list of length {len}",
                ));
            }
            let MemoryReferencePair { name, index } = s.extract()?;
            MemoryReference::new(name, index)
        } else {
            return Err(CastError::new(
                obj,
                MemoryReference::classinfo_object(obj.py()),
            ))?;
        };

        Ok(Self(mem_ref))
    }
}

impl From<PyMemRef<'_>> for MemoryReference {
    fn from(value: PyMemRef<'_>) -> Self {
        match value {
            PyMemRef::DeclarationAt(decl_at) => decl_at.get().memref(decl_at.py()),
            PyMemRef::Declaration(decl) => decl.get().to_memory_reference(0),
            PyMemRef::MemoryReference(mem_ref) => mem_ref.borrow().clone(),
        }
    }
}

/// Check if two values derived from [`Declaration`]s have the same data types.
///
/// If the underlying [`Declaration`]s are known, this returns `Some(b)`,
/// where `b` is true if the data types match and false otherwise.
/// If we can't get the underlying [`Declaration`]s, this returns `None`.
pub(crate) fn same_type<'py, T, U>(lhs: &T, rhs: &U) -> Option<bool>
where
    T: TryAsBorrowed<'py, Declaration>,
    U: TryAsBorrowed<'py, Declaration>,
{
    lhs.try_borrow().and_then(|l| {
        rhs.try_borrow()
            .map(|r| l.get().size.data_type == r.get().size.data_type)
    })
}

impl<'py> TryAsBorrowed<'py, Declaration> for ComparisonOperandLike<'py> {
    fn try_borrow<'a>(&'a self) -> Option<Borrowed<'a, 'py, Declaration>> {
        match self {
            ComparisonOperandLike::Declaration(decl) => Some(decl.as_borrowed()),
            ComparisonOperandLike::DeclarationAt(decl_at) => {
                Some(decl_at.get().declaration.bind_borrowed(decl_at.py()))
            }
            _ => None,
        }
    }
}

impl<'py> TryAsBorrowed<'py, Declaration> for PyMemRef<'py> {
    fn try_borrow<'a>(&'a self) -> Option<Borrowed<'a, 'py, Declaration>> {
        match self {
            PyMemRef::Declaration(decl) => Some(decl.as_borrowed()),
            PyMemRef::DeclarationAt(decl_at) => {
                Some(decl_at.get().declaration.bind_borrowed(decl_at.py()))
            }
            _ => None,
        }
    }
}

impl<'py> TryAsBorrowed<'py, MemoryReference> for PyMemRef<'py> {
    fn try_borrow<'a>(&'a self) -> Option<Borrowed<'a, 'py, MemoryReference>> {
        match self {
            PyMemRef::MemoryReference(mem_ref) => Some(mem_ref.as_borrowed()),
            PyMemRef::DeclarationAt(_) | PyMemRef::Declaration(_) => None,
        }
    }
}
