use quil_rs::instruction::Instruction;
use rigetti_pyo3::{
    create_init_submodule, impl_repr, py_wrap_union_enum,
    pyo3::{pymethods, types::PyDict, PyResult, Python},
};

use crate::{impl_eq, impl_to_quil};

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
impl_eq!(PyInstruction);

#[pymethods]
impl PyInstruction {
    // Implement the __copy__ and __deepcopy__ dunder methods, which are used by Python's
    // `copy` module.
    //
    // If the instruction contains some inner data, then the implementation for __deepcopy__
    // is delegated to that inner type so that each type can define its own copy behavior.
    // This comes with the caveat that this implementation will error if the inner type doesn't
    // implement __deepcopy__ itself. See [`impl_copy_for_instruction!`] for an easy way to
    // implement these methods on any variant of [`PyInstruction`].
    pub fn __copy__(&self) -> Self {
        self.clone()
    }

    pub fn __deepcopy__(&self, py: Python<'_>, memo: &PyDict) -> PyResult<Self> {
        match self.inner(py) {
            Ok(inner) => Ok(PyInstruction::new(
                py,
                inner.call_method1(py, "__deepcopy__", (memo,))?.as_ref(py),
            )?),
            Err(_) => Ok(self.clone()), // No inner data implies this is a simple instruction, safe to
                                        // just clone.
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
/// The `__deepcopy__` method creates a deep copy by cloning the inner instruction, querying its
/// qubits, and replacing any [`quil_rs::instruction::QubitPlaceholder`]s with new instances so
/// that resolving them in one copy doesn't affect the other. Duplicates of the same instruction in
/// the original instruction will be replaced with the same copy in the new instruction.
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

            pub fn __copy__(&self) -> Self {
                self.clone()
            }
        }
    };
}
