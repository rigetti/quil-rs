use pyo3::types::PyLong;
use quil_rs::instruction::{BinaryLogic, BinaryOperand, BinaryOperands, BinaryOperator};
use rigetti_pyo3::{py_wrap_data_struct, py_wrap_type, py_wrap_union_enum};

use super::memory_reference::PyMemoryReference;

py_wrap_union_enum! {
    PyBinaryOperator(BinaryOperator) as "BinaryOperator" {
        and: And,
        ior: Ior,
        xor: Xor
    }
}

py_wrap_union_enum! {
    PyBinaryOperand(BinaryOperand) as "BinaryOperand" {
        literal_integer: LiteralInteger => PyLong,
        memory_reference: MemoryReference => PyMemoryReference
    }
}

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
