use quil_rs::instruction::Instruction;
use rigetti_pyo3::{create_init_submodule, impl_repr, impl_str, py_wrap_type, py_wrap_union_enum};

use self::{
    arithmetic::PyArithmetic,
    declaration::{PyDeclaration, PyScalarType, PyVector},
    expression::{PyExpression, PyExpressionFunction, PyFunctionCallExpression},
    gate::PyGate,
    memory_reference::PyMemoryReference,
    qubit::PyQubit,
};

pub mod arithmetic;
pub mod binary_logic;
pub mod calibration;
pub mod declaration;
pub mod expression;
pub mod frame;
pub mod gate;
pub mod memory_reference;
pub mod memory_region;
pub mod qubit;
pub mod waveform;

py_wrap_type! {
    #[derive(Debug)]
    PyInstructions(Vec<Instruction>) as "Instructions";
}

py_wrap_union_enum! {
    PyInstruction(Instruction) as "Instruction" {
        halt: Halt,
        nop: Nop,
        arithmetic: Arithmetic => PyArithmetic,
        gate: Gate => PyGate,
        declaration: Declaration => PyDeclaration
    }
}

impl_repr!(PyInstruction);
impl_str!(PyInstruction);

create_init_submodule! {
    classes: [PyInstruction, PyGate, PyDeclaration, PyScalarType, PyVector, PyExpression, PyExpressionFunction, PyFunctionCallExpression, PyQubit, PyMemoryReference],
}
