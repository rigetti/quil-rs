use quil_rs::instruction::Instruction;
use rigetti_pyo3::{create_init_submodule, impl_repr, impl_str, py_wrap_union_enum};

pub use self::{
    arithmetic::{
        PyArithmetic, PyArithmeticOperand, PyArithmeticOperator, PyBinaryLogic, PyBinaryOperand,
        PyBinaryOperands, PyBinaryOperator,
    },
    calibration::{PyCalibration, PyMeasureCalibrationDefinition},
    declaration::{PyDeclaration, PyMemoryReference, PyScalarType, PyVector},
    frame::{PyAttributeValue, PyFrameAttributes, PyFrameDefinition, PyFrameIdentifier},
    gate::{GateError, PyGate, PyGateDefinition, PyGateModifier, PyGateSpecification},
    measurement::PyMeasurement,
    qubit::PyQubit,
    waveform::{PyWaveform, PyWaveformDefinition, PyWaveformInvocation},
};

mod arithmetic;
mod calibration;
mod declaration;
mod frame;
mod gate;
mod measurement;
mod qubit;
mod waveform;

py_wrap_union_enum! {
    #[derive(Debug)]
    PyInstruction(Instruction) as "Instruction" {
        arithmetic: Arithmetic => PyArithmetic,
        binary_logic: BinaryLogic => PyBinaryLogic,
        calibration_definition: CalibrationDefinition => PyCalibration,
        declaration: Declaration => PyDeclaration,
        frame_definition: FrameDefinition => PyFrameDefinition,
        gate: Gate => PyGate,
        halt: Halt,
        measure_calibration_definition: MeasureCalibrationDefinition => PyMeasureCalibrationDefinition,
        measurement: Measurement => PyMeasurement,
        nop: Nop
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
        PyMeasurement,
        PyDeclaration,
        PyScalarType,
        PyVector,
        PyAttributeValue,
        PyFrameDefinition,
        PyFrameIdentifier,
        PyGate,
        PyGateDefinition,
        PyGateModifier,
        PyGateSpecification,
        PyMeasurement,
        PyMemoryReference,
        PyQubit,
        PyWaveform,
        PyWaveformDefinition,
        PyWaveformInvocation
    ],
    errors: [ GateError ],
}
