use quil_rs::instruction::{
    Arithmetic, ArithmeticOperand, ArithmeticOperator, BinaryLogic, BinaryOperand, BinaryOperands,
    BinaryOperator, Comparison, ComparisonOperand, ComparisonOperator, Convert, Exchange,
    MemoryReference, Move, UnaryLogic, UnaryOperator,
};

use rigetti_pyo3::{
    impl_as_mut_for_wrapper, impl_hash, impl_repr, py_wrap_data_struct, py_wrap_simple_enum,
    py_wrap_type, py_wrap_union_enum,
    pyo3::{
        pymethods,
        types::{PyFloat, PyInt},
        Py, PyResult, Python,
    },
    PyTryFrom, PyWrapper, PyWrapperMut, ToPython,
};

use super::PyMemoryReference;
use crate::{impl_copy_for_instruction, impl_eq, impl_to_quil};

py_wrap_data_struct! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass)]
    PyArithmetic(Arithmetic) as "Arithmetic" {
        operator: ArithmeticOperator => PyArithmeticOperator,
        destination: ArithmeticOperand => PyArithmeticOperand,
        source: ArithmeticOperand => PyArithmeticOperand
    }
}
impl_repr!(PyArithmetic);
impl_to_quil!(PyArithmetic);
impl_copy_for_instruction!(PyArithmetic);
impl_hash!(PyArithmetic);
impl_eq!(PyArithmetic);

#[pymethods]
impl PyArithmetic {
    #[new]
    pub fn new(
        py: Python<'_>,
        operator: PyArithmeticOperator,
        destination: PyArithmeticOperand,
        source: PyArithmeticOperand,
    ) -> PyResult<Self> {
        Ok(PyArithmetic(Arithmetic::new(
            ArithmeticOperator::py_try_from(py, &operator)?,
            ArithmeticOperand::py_try_from(py, &destination)?,
            ArithmeticOperand::py_try_from(py, &source)?,
        )))
    }
}

py_wrap_union_enum! {
    #[derive(Debug, PartialEq)]
    PyArithmeticOperand(ArithmeticOperand) as "ArithmeticOperand" {
        literal_integer: LiteralInteger => Py<PyInt>,
        literal_real: LiteralReal => Py<PyFloat>,
        memory_reference: MemoryReference => PyMemoryReference
    }
}
impl_repr!(PyArithmeticOperand);
impl_to_quil!(PyArithmeticOperand);
impl_hash!(PyArithmeticOperand);
impl_eq!(PyArithmeticOperand);

py_wrap_simple_enum! {
    #[derive(Debug, PartialEq)]
    PyArithmeticOperator(ArithmeticOperator) as "ArithmeticOperator" {
        Add,
        Subtract,
        Divide,
        Multiply
    }
}
impl_repr!(PyArithmeticOperator);
impl_to_quil!(PyArithmeticOperator);
impl_hash!(PyArithmeticOperator);
impl_eq!(PyArithmeticOperator);

py_wrap_union_enum! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyBinaryOperand(BinaryOperand) as "BinaryOperand" {
        literal_integer: LiteralInteger => Py<PyInt>,
        memory_reference: MemoryReference => PyMemoryReference
    }
}
impl_repr!(PyBinaryOperand);
impl_to_quil!(PyBinaryOperand);
impl_hash!(PyBinaryOperand);
impl_eq!(PyBinaryOperand);

py_wrap_type! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyBinaryOperands(BinaryOperands) as "BinaryOperands"
}
impl_repr!(PyBinaryOperands);
impl_hash!(PyBinaryOperands);
impl_as_mut_for_wrapper!(PyBinaryOperands);
impl_eq!(PyBinaryOperands);

#[pymethods]
impl PyBinaryOperands {
    #[new]
    pub fn new(
        py: Python<'_>,
        memory_reference: PyMemoryReference,
        operand: PyBinaryOperand,
    ) -> PyResult<Self> {
        Ok(Self((
            MemoryReference::py_try_from(py, &memory_reference)?,
            BinaryOperand::py_try_from(py, &operand)?,
        )))
    }

    #[getter]
    pub fn get_memory_reference(&self, py: Python<'_>) -> PyResult<PyMemoryReference> {
        self.as_inner().0.to_python(py)
    }

    #[setter]
    pub fn set_memory_reference(
        &mut self,
        py: Python<'_>,
        memory_reference: PyMemoryReference,
    ) -> PyResult<()> {
        self.as_inner_mut().0 = MemoryReference::py_try_from(py, &memory_reference)?;
        Ok(())
    }

    #[getter]
    pub fn get_operand(&self, py: Python<'_>) -> PyResult<PyBinaryOperand> {
        self.as_inner().1.to_python(py)
    }

    #[setter]
    pub fn set_operand(&mut self, py: Python<'_>, binary_operand: PyBinaryOperand) -> PyResult<()> {
        self.as_inner_mut().1 = BinaryOperand::py_try_from(py, &binary_operand)?;
        Ok(())
    }
}

py_wrap_simple_enum! {
    #[derive(Debug, PartialEq, Eq)]
    PyBinaryOperator(BinaryOperator) as "BinaryOperator" {
        And,
        Ior,
        Xor
    }
}
impl_repr!(PyBinaryOperator);
impl_to_quil!(PyBinaryOperator);
impl_hash!(PyBinaryOperator);
impl_eq!(PyBinaryOperator);

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyBinaryLogic(BinaryLogic) as "BinaryLogic" {
        operator: BinaryOperator => PyBinaryOperator,
        operands: BinaryOperands => PyBinaryOperands
    }
}
impl_repr!(PyBinaryLogic);
impl_to_quil!(PyBinaryLogic);
impl_copy_for_instruction!(PyBinaryLogic);
impl_eq!(PyBinaryLogic);

#[pymethods]
impl PyBinaryLogic {
    #[new]
    pub fn new(
        py: Python<'_>,
        operator: PyBinaryOperator,
        operands: PyBinaryOperands,
    ) -> PyResult<Self> {
        Ok(PyBinaryLogic(BinaryLogic::new(
            BinaryOperator::py_try_from(py, &operator)?,
            BinaryOperands::py_try_from(py, &operands)?,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyConvert(Convert) as "Convert" {
        destination: MemoryReference => PyMemoryReference,
        source: MemoryReference => PyMemoryReference
    }
}
impl_repr!(PyConvert);
impl_to_quil!(PyConvert);
impl_copy_for_instruction!(PyConvert);
impl_hash!(PyConvert);
impl_eq!(PyConvert);

#[pymethods]
impl PyConvert {
    #[new]
    fn new(
        py: Python<'_>,
        destination: PyMemoryReference,
        source: PyMemoryReference,
    ) -> PyResult<Self> {
        Ok(Self(Convert::new(
            MemoryReference::py_try_from(py, &destination)?,
            MemoryReference::py_try_from(py, &source)?,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass)]
    PyMove(Move) as "Move" {
        destination: MemoryReference => PyMemoryReference,
        source: ArithmeticOperand => PyArithmeticOperand
    }
}
impl_repr!(PyMove);
impl_to_quil!(PyMove);
impl_copy_for_instruction!(PyMove);
impl_hash!(PyMove);
impl_eq!(PyMove);

#[pymethods]
impl PyMove {
    #[new]
    fn new(
        py: Python<'_>,
        destination: PyMemoryReference,
        source: PyArithmeticOperand,
    ) -> PyResult<Self> {
        Ok(Self(Move::new(
            MemoryReference::py_try_from(py, &destination)?,
            ArithmeticOperand::py_try_from(py, &source)?,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass)]
    PyExchange(Exchange) as "Exchange" {
        left: MemoryReference => PyMemoryReference,
        right: MemoryReference => PyMemoryReference
    }
}
impl_repr!(PyExchange);
impl_to_quil!(PyExchange);
impl_copy_for_instruction!(PyExchange);
impl_hash!(PyExchange);
impl_eq!(PyExchange);

#[pymethods]
impl PyExchange {
    #[new]
    pub fn new(
        py: Python<'_>,
        left: PyMemoryReference,
        right: PyMemoryReference,
    ) -> PyResult<Self> {
        Ok(Self(Exchange::new(
            MemoryReference::py_try_from(py, &left)?,
            MemoryReference::py_try_from(py, &right)?,
        )))
    }
}

py_wrap_union_enum! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass)]
    PyComparisonOperand(ComparisonOperand) as "ComparisonOperand" {
        literal_integer: LiteralInteger => Py<PyInt>,
        literal_real: LiteralReal => Py<PyFloat>,
        memory_reference: MemoryReference => PyMemoryReference
    }
}
impl_repr!(PyComparisonOperand);
impl_to_quil!(PyComparisonOperand);
impl_hash!(PyComparisonOperand);

py_wrap_simple_enum! {
    #[derive(Debug, PartialEq, Eq)]
    PyComparisonOperator(ComparisonOperator) as "ComparisonOperator" {
        Equal,
        GreaterThanOrEqual,
        GreaterThan,
        LessThanOrEqual,
        LessThan
    }
}

type RustComparisonOperands = (MemoryReference, MemoryReference, ComparisonOperand);

// This is a helper type to manage easy conversion of the inner tuple
// with the macros. It should not be exposed directly.
py_wrap_type! {
    PyComparisonOperands(RustComparisonOperands)
}

impl PyComparisonOperands {
    pub(crate) fn from_py_tuple(
        py: Python<'_>,
        tuple: (PyMemoryReference, PyMemoryReference, PyComparisonOperand),
    ) -> PyResult<Self> {
        Ok(Self((
            MemoryReference::py_try_from(py, &tuple.0)?,
            MemoryReference::py_try_from(py, &tuple.1)?,
            ComparisonOperand::py_try_from(py, &tuple.2)?,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass)]
    PyComparison(Comparison) as "Comparison" {
        operator: ComparisonOperator => PyComparisonOperator,
        operands: RustComparisonOperands => PyComparisonOperands
    }
}
impl_repr!(PyComparison);
impl_to_quil!(PyComparison);
impl_copy_for_instruction!(PyComparison);
impl_hash!(PyComparison);
impl_eq!(PyComparison);

#[pymethods]
impl PyComparison {
    #[new]
    pub fn new(
        py: Python<'_>,
        operator: PyComparisonOperator,
        operands: (PyMemoryReference, PyMemoryReference, PyComparisonOperand),
    ) -> PyResult<Self> {
        Ok(Self(Comparison::new(
            ComparisonOperator::py_try_from(py, &operator)?,
            RustComparisonOperands::py_try_from(
                py,
                &PyComparisonOperands::from_py_tuple(py, operands)?,
            )?,
        )))
    }

    // Override the getters/setters generated by [`py_wrap_data_struct!`] so that they
    // return/take tuples instead of the wrapping [`PyComparisonOperands`] type.
    #[getter(operands)]
    fn get_operands_as_tuple(
        &self,
        py: Python<'_>,
    ) -> PyResult<(PyMemoryReference, PyMemoryReference, PyComparisonOperand)> {
        let operands = &self.as_inner().operands;
        Ok((
            operands.0.to_python(py)?,
            operands.1.to_python(py)?,
            operands.2.to_python(py)?,
        ))
    }

    #[setter(operands)]
    fn set_operands_from_tuple(
        &mut self,
        py: Python<'_>,
        operands: (PyMemoryReference, PyMemoryReference, PyComparisonOperand),
    ) -> PyResult<()> {
        self.as_inner_mut().operands = RustComparisonOperands::py_try_from(
            py,
            &PyComparisonOperands::from_py_tuple(py, operands)?,
        )?;
        Ok(())
    }
}

py_wrap_simple_enum! {
    #[derive(Debug, PartialEq, Eq)]
    PyUnaryOperator(UnaryOperator) as "UnaryOperator" {
        Neg,
        Not
    }
}
impl_repr!(PyUnaryOperator);
impl_to_quil!(PyUnaryOperator);
impl_hash!(PyUnaryOperator);

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyUnaryLogic(UnaryLogic) as "UnaryLogic" {
        operator: UnaryOperator => PyUnaryOperator,
        operand: MemoryReference => PyMemoryReference
    }
}
impl_repr!(PyUnaryLogic);
impl_to_quil!(PyUnaryLogic);
impl_copy_for_instruction!(PyUnaryLogic);
impl_hash!(PyUnaryLogic);
impl_eq!(PyUnaryLogic);

#[pymethods]
impl PyUnaryLogic {
    #[new]
    pub fn new(
        py: Python<'_>,
        operator: PyUnaryOperator,
        operand: PyMemoryReference,
    ) -> PyResult<Self> {
        Ok(Self(UnaryLogic::new(
            UnaryOperator::py_try_from(py, &operator)?,
            MemoryReference::py_try_from(py, &operand)?,
        )))
    }
}
