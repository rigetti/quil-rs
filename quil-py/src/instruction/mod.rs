use quil_rs::instruction::Instruction;
use rigetti_pyo3::{
    create_init_submodule, impl_repr, impl_str, py_wrap_union_enum,
    pyo3::{pyclass::CompareOp, pymethods, IntoPy, PyObject, Python},
    PyWrapper,
};

pub use self::{
    arithmetic::{
        PyArithmetic, PyArithmeticOperand, PyArithmeticOperator, PyBinaryLogic, PyBinaryOperand,
        PyBinaryOperands, PyBinaryOperator,
    },
    calibration::{PyCalibration, PyMeasureCalibrationDefinition},
    circuit::PyCircuitDefinition,
    declaration::{
        ParseMemoryReferenceError, PyDeclaration, PyMemoryReference, PyOffset, PyScalarType,
        PySharing, PyVector,
    },
    frame::{
        PyAttributeValue, PyCapture, PyFrameAttributes, PyFrameDefinition, PyFrameIdentifier,
        PyPulse, PyRawCapture, PySetFrequency, PySetPhase, PySetScale, PyShiftFrequency,
        PyShiftPhase, PySwapPhases,
    },
    gate::{
        GateError, PyGate, PyGateDefinition, PyGateModifier, PyGateSpecification, PyPauliGate,
        PyPauliSum, PyPauliTerm,
    },
    measurement::PyMeasurement,
    pragma::{PyPragma, PyPragmaArgument},
    qubit::PyQubit,
    reset::PyReset,
    timing::{PyDelay, PyFence},
    waveform::{PyWaveform, PyWaveformDefinition, PyWaveformInvocation},
};

mod arithmetic;
mod calibration;
mod circuit;
mod declaration;
mod frame;
mod gate;
mod measurement;
mod pragma;
mod qubit;
mod reset;
mod timing;
mod waveform;

py_wrap_union_enum! {
    #[derive(Debug, PartialEq)]
    PyInstruction(Instruction) as "Instruction" {
        arithmetic: Arithmetic => PyArithmetic,
        binary_logic: BinaryLogic => PyBinaryLogic,
        calibration_definition: CalibrationDefinition => PyCalibration,
        capture: Capture => PyCapture,
        circuit_definition: CircuitDefinition => PyCircuitDefinition,
        declaration: Declaration => PyDeclaration,
        delay: Delay => PyDelay,
        fence: Fence => PyFence,
        frame_definition: FrameDefinition => PyFrameDefinition,
        gate: Gate => PyGate,
        gate_definition: GateDefinition => PyGateDefinition,
        halt: Halt,
        measure_calibration_definition: MeasureCalibrationDefinition => PyMeasureCalibrationDefinition,
        measurement: Measurement => PyMeasurement,
        nop: Nop,
        pragma: Pragma => PyPragma,
        pulse: Pulse => PyPulse,
        raw_capture: RawCapture => PyRawCapture,
        reset: Reset => PyReset,
        set_frequency: SetFrequency => PySetFrequency,
        set_phase: SetPhase => PySetPhase,
        set_scale: SetScale => PySetScale,
        shift_frequency: ShiftFrequency => PyShiftFrequency,
        shift_phase: ShiftPhase => PyShiftPhase,
        swap_phases: SwapPhases => PySwapPhases,
        waveform_definition: WaveformDefinition => PyWaveformDefinition
    }
}
impl_repr!(PyInstruction);
impl_str!(PyInstruction);

#[pymethods]
impl PyInstruction {
    pub fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self.as_inner() == other.as_inner()).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}

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
        PyCircuitDefinition,
        PyMeasureCalibrationDefinition,
        PyDeclaration,
        PyOffset,
        PySharing,
        PyScalarType,
        PyVector,
        PyMeasurement,
        PyPragma,
        PyPragmaArgument,
        PyDeclaration,
        PyScalarType,
        PyVector,
        PyAttributeValue,
        PyCapture,
        PyFrameDefinition,
        PyFrameIdentifier,
        PyPulse,
        PyRawCapture,
        PySetFrequency,
        PySetPhase,
        PySetScale,
        PyShiftFrequency,
        PyShiftPhase,
        PySwapPhases,
        PyGate,
        PyGateDefinition,
        PyGateModifier,
        PyGateSpecification,
        PyPauliGate,
        PyPauliTerm,
        PyPauliSum,
        PyMeasurement,
        PyMemoryReference,
        PyQubit,
        PyReset,
        PyDelay,
        PyFence,
        PyWaveform,
        PyWaveformDefinition,
        PyWaveformInvocation
    ],
    errors: [ GateError, ParseMemoryReferenceError ],
}
