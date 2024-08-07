use quil_rs::instruction::{
    Arithmetic, ArithmeticOperand, ArithmeticOperator, BinaryLogic, BinaryOperand, BinaryOperator,
    Comparison, ComparisonOperand, ComparisonOperator, Convert, Exchange, MemoryReference, Move,
    UnaryLogic, UnaryOperator,
};

use rigetti_pyo3::{
    impl_hash, impl_repr, py_wrap_data_struct, py_wrap_simple_enum, py_wrap_union_enum,
    pyo3::{
        pymethods,
        types::{PyFloat, PyInt},
        Py, PyResult, Python,
    },
    PyTryFrom,
};

use super::PyMemoryReference;
use crate::{impl_copy_for_instruction, impl_eq, impl_pickle_for_instruction, impl_to_quil};

py_wrap_data_struct! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass, module = "quil.instructions")]
    PyArithmetic(Arithmetic) as "Arithmetic" {
        operator: ArithmeticOperator => PyArithmeticOperator,
        destination: MemoryReference => PyMemoryReference,
        source: ArithmeticOperand => PyArithmeticOperand
    }
}
impl_repr!(PyArithmetic);
impl_to_quil!(PyArithmetic);
impl_copy_for_instruction!(PyArithmetic);
impl_hash!(PyArithmetic);
impl_eq!(PyArithmetic);
impl_pickle_for_instruction!(PyArithmetic);

#[pymethods]
impl PyArithmetic {
    #[new]
    pub fn new(
        py: Python<'_>,
        operator: PyArithmeticOperator,
        destination: PyMemoryReference,
        source: PyArithmeticOperand,
    ) -> PyResult<Self> {
        Ok(PyArithmetic(Arithmetic::new(
            ArithmeticOperator::py_try_from(py, &operator)?,
            MemoryReference::py_try_from(py, &destination)?,
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
    #[pyo3(subclass, module = "quil.instructions")]
    PyBinaryLogic(BinaryLogic) as "BinaryLogic" {
        operator: BinaryOperator => PyBinaryOperator,
        destination: MemoryReference => PyMemoryReference,
        source: BinaryOperand => PyBinaryOperand
    }
}
impl_repr!(PyBinaryLogic);
impl_to_quil!(PyBinaryLogic);
impl_copy_for_instruction!(PyBinaryLogic);
impl_eq!(PyBinaryLogic);
impl_pickle_for_instruction!(PyBinaryLogic);

#[pymethods]
impl PyBinaryLogic {
    #[new]
    pub fn new(
        py: Python<'_>,
        operator: PyBinaryOperator,
        destination: PyMemoryReference,
        source: PyBinaryOperand,
    ) -> PyResult<Self> {
        Ok(PyBinaryLogic(BinaryLogic::new(
            BinaryOperator::py_try_from(py, &operator)?,
            MemoryReference::py_try_from(py, &destination)?,
            BinaryOperand::py_try_from(py, &source)?,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass, module = "quil.instructions")]
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
impl_pickle_for_instruction!(PyConvert);

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
    #[pyo3(subclass, module = "quil.instructions")]
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
impl_pickle_for_instruction!(PyMove);

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
    #[pyo3(subclass, module = "quil.instructions")]
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
impl_pickle_for_instruction!(PyExchange);

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

py_wrap_data_struct! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass, module = "quil.instructions")]
    PyComparison(Comparison) as "Comparison" {
        operator: ComparisonOperator => PyComparisonOperator,
        destination: MemoryReference => PyMemoryReference,
        lhs: MemoryReference => PyMemoryReference,
        rhs: ComparisonOperand => PyComparisonOperand
    }
}
impl_repr!(PyComparison);
impl_to_quil!(PyComparison);
impl_copy_for_instruction!(PyComparison);
impl_hash!(PyComparison);
impl_eq!(PyComparison);
impl_pickle_for_instruction!(PyComparison);

#[pymethods]
impl PyComparison {
    #[new]
    pub fn new(
        py: Python<'_>,
        operator: PyComparisonOperator,
        destination: PyMemoryReference,
        lhs: PyMemoryReference,
        rhs: PyComparisonOperand,
    ) -> PyResult<Self> {
        Ok(Self(Comparison::new(
            ComparisonOperator::py_try_from(py, &operator)?,
            MemoryReference::py_try_from(py, &destination)?,
            MemoryReference::py_try_from(py, &lhs)?,
            ComparisonOperand::py_try_from(py, &rhs)?,
        )))
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
    #[pyo3(subclass, module = "quil.instructions")]
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
impl_pickle_for_instruction!(PyUnaryLogic);

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
