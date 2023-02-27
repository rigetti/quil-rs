use quil_rs::instruction::Instruction;
use rigetti_pyo3::{create_init_submodule, impl_repr, impl_str, py_wrap_union_enum};

pub use self::{
    arithmetic::{
        PyArithmetic, PyArithmeticOperand, PyArithmeticOperator, PyBinaryLogic, PyBinaryOperand,
        PyBinaryOperands, PyBinaryOperator,
    },
    calibration::{PyCalibration, PyMeasureCalibrationDefinition},
    declaration::{PyDeclaration, PyScalarType, PyVector},
    expression::{
        PyExpression, PyExpressionFunction, PyFunctionCallExpression, PyInfixExpression,
        PyInfixOperator,
    },
    frame::{PyAttributeValue, PyFrameAttributes, PyFrameDefinition, PyFrameIdentifier},
    gate::{PyGate, PyGateDefinition, PyGateError, PyGateModifier, PyGateSpecification},
    memory_region::{PyMemoryReference, PyMemoryRegion},
    qubit::PyQubit,
    waveform::{PyWaveform, PyWaveformDefinition},
};

mod arithmetic;
mod calibration;
mod declaration;
mod expression;
mod frame;
mod gate;
mod memory_region;
mod qubit;
mod waveform;

py_wrap_union_enum! {
    #[derive(Debug)]
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
    classes: [
        PyInstruction,
        PyArithmetic,
        PyArithmeticOperand,
        PyArithmeticOperator,
        PyBinaryLogic,
        PyBinaryOperand,
        PyBinaryOperands,
        PyBinaryOperator,
        PyCalibration,
        PyMeasureCalibrationDefinition,
        PyDeclaration,
        PyScalarType,
        PyVector,
        PyExpression,
        PyExpressionFunction,
        PyFunctionCallExpression,
        PyInfixExpression,
        PyInfixOperator,
        PyAttributeValue,
        PyFrameDefinition,
        PyFrameIdentifier,
        PyGate,
        PyGateDefinition,
        PyGateModifier,
        PyGateSpecification,
        PyMemoryReference,
        PyMemoryRegion,
        PyQubit,
        PyWaveform,
        PyWaveformDefinition
    ],
    errors: [ PyGateError ],
}
