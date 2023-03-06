use quil_rs::instruction::{
    Arithmetic, ArithmeticOperand, ArithmeticOperator, BinaryLogic, BinaryOperand, BinaryOperands,
    BinaryOperator,
};

use rigetti_pyo3::{
    impl_repr, impl_str, py_wrap_data_struct, py_wrap_simple_enum, py_wrap_type,
    py_wrap_union_enum,
    pyo3::{
        types::{PyFloat, PyInt},
        Py,
    },
};

use super::PyMemoryReference;

py_wrap_simple_enum! {
    PyArithmeticOperator(ArithmeticOperator) as "ArithmeticOperator" {
        Add,
        Subtract,
        Divide,
        Multiply
    }
}
impl_repr!(PyArithmeticOperator);
impl_str!(PyArithmeticOperator);

py_wrap_union_enum! {
    PyArithmeticOperand(ArithmeticOperand) as "ArithmeticOperand" {
        literal_integer: LiteralInteger => Py<PyInt>,
        literal_real: LiteralReal => Py<PyFloat>,
        memory_reference: MemoryReference => PyMemoryReference
    }
}
impl_repr!(PyArithmeticOperand);
impl_str!(PyArithmeticOperand);

py_wrap_data_struct! {
    PyArithmetic(Arithmetic) as "Arithmetic" {
        operator: ArithmeticOperator => PyArithmeticOperator,
        destination: ArithmeticOperand => PyArithmeticOperand,
        source: ArithmeticOperand => PyArithmeticOperand
    }
}
impl_repr!(PyArithmetic);

py_wrap_union_enum! {
    PyBinaryOperator(BinaryOperator) as "BinaryOperator" {
        and: And,
        ior: Ior,
        xor: Xor
    }
}
impl_repr!(PyBinaryOperator);
impl_str!(PyBinaryOperator);

py_wrap_union_enum! {
    PyBinaryOperand(BinaryOperand) as "BinaryOperand" {
        literal_integer: LiteralInteger => Py<PyInt>,
        memory_reference: MemoryReference => PyMemoryReference
    }
}
impl_repr!(PyBinaryOperand);
impl_str!(PyBinaryOperand);

py_wrap_type! {
    #[derive(Debug)]
    PyBinaryOperands(BinaryOperands) as "BinaryOperands";
}

py_wrap_data_struct! {
    PyBinaryLogic(BinaryLogic) as "BinaryLogic" {
        operator: BinaryOperator => PyBinaryOperator,
        operands: BinaryOperands => PyBinaryOperands
    }
}
impl_repr!(PyBinaryLogic);
