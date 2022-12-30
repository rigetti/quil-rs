use pyo3::types::{PyFloat, PyLong};
use quil_rs::instruction::{Arithmetic, ArithmeticOperand, ArithmeticOperator};
use rigetti_pyo3::{py_wrap_data_struct, py_wrap_union_enum};

use super::memory_reference::PyMemoryReference;

py_wrap_union_enum! {
    PyArithmeticOperator(ArithmeticOperator) as "ArithmeticOperator" {
        add: Add,
        subtract: Subtract,
        divide: Divide,
        multiply: Multiply
    }
}

py_wrap_union_enum! {
    PyArithmeticOperand(ArithmeticOperand) as "ArithmeticOperand" {
        literal_integer: LiteralInteger => PyLong,
        literal_real: LiteralReal => PyFloat,
        memory_reference: MemoryReference => PyMemoryReference
    }
}

py_wrap_data_struct! {
    PyArithmetic(Arithmetic) as "Arithmetic" {
        operator: ArithmeticOperator => PyArithmeticOperator,
        destination: ArithmeticOperand => PyArithmeticOperand,
        source: ArithmeticOperand => PyArithmeticOperand
    }
}
