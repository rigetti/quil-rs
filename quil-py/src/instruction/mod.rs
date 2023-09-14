use quil_rs::instruction::Instruction;
use rigetti_pyo3::{
    create_init_submodule, impl_repr, py_wrap_union_enum,
    pyo3::{pyclass::CompareOp, pymethods, types::PyDict, IntoPy, PyObject, PyResult, Python},
    PyWrapper,
};

use crate::impl_to_quil;

pub use self::{
    calibration::{PyCalibration, PyMeasureCalibrationDefinition},
    circuit::PyCircuitDefinition,
    classical::{
        PyArithmetic, PyArithmeticOperand, PyArithmeticOperator, PyBinaryLogic, PyBinaryOperand,
        PyBinaryOperands, PyBinaryOperator, PyComparison, PyComparisonOperand,
        PyComparisonOperator, PyConvert, PyExchange, PyMove, PyUnaryLogic, PyUnaryOperator,
    },
    control_flow::{PyJump, PyJumpUnless, PyJumpWhen, PyLabel, PyTarget, PyTargetPlaceholder},
    declaration::{
        ParseMemoryReferenceError, PyDeclaration, PyLoad, PyMemoryReference, PyOffset,
        PyScalarType, PySharing, PyStore, PyVector,
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
    pragma::{PyInclude, PyPragma, PyPragmaArgument},
    qubit::{PyQubit, PyQubitPlaceholder},
    reset::PyReset,
    timing::{PyDelay, PyFence},
    waveform::{PyWaveform, PyWaveformDefinition, PyWaveformInvocation},
};

mod calibration;
mod circuit;
mod classical;
mod control_flow;
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
        convert: Convert => PyConvert,
        comparison: Comparison => PyComparison,
        declaration: Declaration => PyDeclaration,
        delay: Delay => PyDelay,
        exchange: Exchange => PyExchange,
        fence: Fence => PyFence,
        frame_definition: FrameDefinition => PyFrameDefinition,
        gate: Gate => PyGate,
        gate_definition: GateDefinition => PyGateDefinition,
        halt: Halt,
        include: Include => PyInclude,
        jump: Jump => PyJump,
        jump_when: JumpWhen => PyJumpWhen,
        jump_unless: JumpUnless => PyJumpUnless,
        label: Label => PyLabel,
        load: Load => PyLoad,
        measure_calibration_definition: MeasureCalibrationDefinition => PyMeasureCalibrationDefinition,
        measurement: Measurement => PyMeasurement,
        move: Move => PyMove,
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
        store: Store => PyStore,
        swap_phases: SwapPhases => PySwapPhases,
        unary_logic: UnaryLogic => PyUnaryLogic,
        waveform_definition: WaveformDefinition => PyWaveformDefinition,
        wait: Wait
    }
}
impl_repr!(PyInstruction);
impl_to_quil!(PyInstruction);

#[pymethods]
impl PyInstruction {
    pub fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self.as_inner() == other.as_inner()).into_py(py),
            _ => py.NotImplemented(),
        }
    }

    pub fn copy(&self, py: Python<'_>) -> PyResult<Self> {
        match self.inner(py) {
            Ok(inner) => Ok(PyInstruction::new(
                py,
                inner
                    .call_method1(py, "__deepcopy__", (PyDict::new(py),))?
                    .as_ref(py),
            )?),
            Err(_) => Ok(self.clone()),
        }
    }

    pub fn __copy__(&self, py: Python<'_>) -> PyResult<Self> {
        self.copy(py)
    }

    pub fn __deepcopy__(&self, py: Python<'_>, memo: &PyDict) -> PyResult<Self> {
        match self.inner(py) {
            Ok(inner) => Ok(PyInstruction::new(
                py,
                inner.call_method1(py, "__deepcopy__", (memo,))?.as_ref(py),
            )?),
            Err(_) => Ok(self.clone()),
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
        PyComparison,
        PyComparisonOperand,
        PyComparisonOperator,
        PyConvert,
        PyExchange,
        PyMove,
        PyUnaryLogic,
        PyUnaryOperator,
        PyCalibration,
        PyCircuitDefinition,
        PyMeasureCalibrationDefinition,
        PyDeclaration,
        PyLoad,
        PyOffset,
        PySharing,
        PyStore,
        PyScalarType,
        PyVector,
        PyMeasurement,
        PyInclude,
        PyPragma,
        PyPragmaArgument,
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
        PyJump,
        PyJumpWhen,
        PyJumpUnless,
        PyLabel,
        PyTarget,
        PyTargetPlaceholder,
        PyMeasurement,
        PyMemoryReference,
        PyQubit,
        PyQubitPlaceholder,
        PyReset,
        PyDelay,
        PyFence,
        PyWaveform,
        PyWaveformDefinition,
        PyWaveformInvocation
    ],
    errors: [ GateError, ParseMemoryReferenceError ],
}

/// Implements __copy__ and __deepcopy__ on any variant of the [`PyInstruction`] class, making
/// them compatible with Python's `copy` module.
///
/// The `__copy__` method returns a reference to the instruction, making it shallow: any changes
/// to the copy will update the original.
///
/// The `__deepcopy__` method returns a deep copy. Because [`QubitPlaceholder`]s are implemented
/// with [`Arc`], the implementation will find and replace any [`QubitPlaceholder`]s with new ones.
#[macro_export]
macro_rules! impl_copy_for_instruction {
    ($py_name: ident) => {
        #[pyo3::pymethods]
        impl $py_name {
            pub fn __deepcopy__(
                &self,
                py: Python<'_>,
                _memo: &pyo3::types::PyDict,
            ) -> pyo3::PyResult<Self> {
                let mut instruction = $crate::instruction::PyInstruction::new(
                    py,
                    pyo3::ToPyObject::to_object(&self, py).as_ref(py),
                )?;

                use quil_rs::instruction::{Qubit, QubitPlaceholder};
                use std::collections::HashMap;
                let mut placeholders: HashMap<QubitPlaceholder, QubitPlaceholder> = HashMap::new();

                for qubit in
                    rigetti_pyo3::PyWrapperMut::as_inner_mut(&mut instruction).get_qubits_mut()
                {
                    match qubit {
                        Qubit::Fixed(_) | Qubit::Variable(_) => *qubit = qubit.clone(),
                        Qubit::Placeholder(placeholder) => {
                            *qubit = Qubit::Placeholder(
                                placeholders.entry(placeholder.clone()).or_default().clone(),
                            )
                        }
                    }
                }

                Ok(instruction
                    .inner(py)
                    .unwrap()
                    .extract::<$py_name>(py)
                    .expect("a copy of a type should extract to the same type"))
            }

            pub fn __copy__(&self, py: Python<'_>) -> pyo3::PyObject {
                pyo3::ToPyObject::to_object(&self, py).clone_ref(py)
            }
        }
    };
}
