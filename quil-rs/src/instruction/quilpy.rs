use num_complex::Complex64;
use numpy::{PyArray2, ToPyArray};
use pyo3::prelude::*;

use super::*;
use crate::{
    impl_repr, impl_to_quil, pickleable_new, validation::identifier::IdentifierValidationError,
};

#[pymodule]
#[pyo3(name = "instructions", module = "quil", submodule)]
pub(crate) fn init_submodule(m: &Bound<'_, PyModule>) -> PyResult<()> {
    use crate::quilpy::errors;

    let py = m.py();

    m.add("CallError", py.get_type::<errors::CallError>())?;
    m.add("ExternError", py.get_type::<errors::ExternError>())?;
    m.add("GateError", py.get_type::<errors::GateError>())?;
    m.add(
        "ParseInstructquilpyr",
        py.get_type::<errors::ParseInstructionError>(),
    )?;
    m.add(
        "ParseMemoryRequilpyError",
        py.get_type::<errors::ParseMemoryReferenceError>(),
    )?;

    m.add_class::<Arithmetic>()?;
    m.add_class::<ArithmeticOperand>()?;
    m.add_class::<ArithmeticOperator>()?;
    m.add_class::<AttributeValue>()?;
    m.add_class::<BinaryLogic>()?;
    m.add_class::<BinaryOperand>()?;
    m.add_class::<BinaryOperator>()?;
    m.add_class::<Calibration>()?;
    m.add_class::<CalibrationIdentifier>()?;
    m.add_class::<Call>()?;
    m.add_class::<UnresolvedCallArgument>()?; // Python name: CallArgument
    m.add_class::<Capture>()?;
    m.add_class::<CircuitDefinition>()?;
    m.add_class::<Comparison>()?;
    m.add_class::<ComparisonOperand>()?;
    m.add_class::<ComparisonOperator>()?;
    m.add_class::<Convert>()?;
    m.add_class::<Declaration>()?;
    m.add_class::<Delay>()?;
    m.add_class::<Exchange>()?;
    m.add_class::<ExternParameter>()?;
    m.add_class::<ExternParameterType>()?;
    m.add_class::<ExternSignature>()?;
    m.add_class::<Fence>()?;
    m.add_class::<FrameDefinition>()?;
    m.add_class::<FrameIdentifier>()?;
    m.add_class::<Gate>()?;
    m.add_class::<GateDefinition>()?;
    m.add_class::<GateModifier>()?;
    m.add_class::<GateSpecification>()?;
    m.add_class::<Include>()?;
    m.add_class::<Instruction>()?;
    m.add_class::<Jump>()?;
    m.add_class::<JumpUnless>()?;
    m.add_class::<JumpWhen>()?;
    m.add_class::<Label>()?;
    m.add_class::<Load>()?;
    m.add_class::<MeasureCalibrationDefinition>()?;
    m.add_class::<MeasureCalibrationIdentifier>()?;
    m.add_class::<Measurement>()?;
    m.add_class::<MemoryReference>()?;
    m.add_class::<Move>()?;
    m.add_class::<Offset>()?;
    m.add_class::<PauliGate>()?;
    m.add_class::<PauliSum>()?;
    m.add_class::<PauliTerm>()?;
    m.add_class::<Pragma>()?;
    m.add_class::<PragmaArgument>()?;
    m.add_class::<Pulse>()?;
    m.add_class::<Qubit>()?;
    m.add_class::<QubitPlaceholder>()?;
    m.add_class::<RawCapture>()?;
    m.add_class::<Reset>()?;
    m.add_class::<ScalarType>()?;
    m.add_class::<SetFrequency>()?;
    m.add_class::<SetPhase>()?;
    m.add_class::<SetScale>()?;
    m.add_class::<Sharing>()?;
    m.add_class::<ShiftFrequency>()?;
    m.add_class::<ShiftPhase>()?;
    m.add_class::<Store>()?;
    m.add_class::<SwapPhases>()?;
    m.add_class::<Target>()?;
    m.add_class::<TargetPlaceholder>()?;
    m.add_class::<UnaryLogic>()?;
    m.add_class::<UnaryOperator>()?;
    m.add_class::<Vector>()?;
    m.add_class::<Waveform>()?;
    m.add_class::<WaveformDefinition>()?;
    m.add_class::<WaveformInvocation>()?;

    Ok(())
}

/// Add a `parse` implementation to a `#[pyclass]` to uses the type's `from_str` implementation.
macro_rules! impl_parse {
    ($name: ident) => {
        #[pyo3::pymethods]
        impl $name {
            #[staticmethod]
            #[pyo3(name = "parse")]
            fn py_parse(string: &str) -> PyResult<Self> {
                Ok(Self::from_str(string)?)
            }
        }
    };
}

/// Implement expected methods on each of the instruction-related types, given as a list.
/// This makes it easy to see which classes make up the `instructions` module,
/// to verify that those classes have necessary `#[pymethods]` implemented,
/// and to see at a glance what differences they do have in their implementations.
///
/// Types are supplied in a list. Those which deviate from the default implementations
/// can give the items they should implement as a sublist, as in this example:
///
/// ```
/// impl_instruction!([
///     A, // By default, [repr + quil]
///     B [repr + quil],
///     C [quil + parse],
/// ]);
/// ```
macro_rules! impl_instruction {
    // Terminal rule -- an empty list expands to nothing.
    ([]) => {};

    // Implements default methods for an instruction, then recursively expands the rest of the list.
    ([$name: ident, $($tail: tt)*]) => {
        impl_instruction!(@one $name [+ repr + quil]);
        impl_instruction!([$($tail)*]);
    };

    // Implements specific methods for an instruction, then recursively expands the rest of the list.
    ([$name: ident [$($args: tt)+], $($tail: tt)*]) => {
        impl_instruction!(@one $name [+ $($args)*]);
        impl_instruction!([$($tail)*]);
    };

    // All the `@one` rules expand a single `$name` and its list of required methods.

    // Terminal rule -- an empty list expands to nothing.
    (@one $name: ident []) => {};

    (@one $name: ident [+ repr $($tail: tt)*]) => {
        impl_repr!($name);
        impl_instruction!(@one $name [$($tail)*]);
    };

    (@one $name: ident [+ quil $($tail: tt)*]) => {
        impl_to_quil!($name);
        impl_instruction!(@one $name [$($tail)*]);
    };

    (@one $name: ident [+ parse $($tail: tt)*]) => {
        impl_parse!($name);
        impl_instruction!(@one $name [$($tail)*]);
    };
}

impl_instruction!([
    Arithmetic,
    ArithmeticOperand,
    ArithmeticOperator,
    AttributeValue,
    BinaryLogic,
    BinaryOperand,
    BinaryOperator,
    Calibration,
    CalibrationIdentifier,
    Call,
    Capture,
    CircuitDefinition,
    Comparison,
    ComparisonOperand,
    Convert,
    Declaration,
    Delay,
    Exchange,
    ExternParameter,
    ExternParameterType,
    ExternSignature,
    Fence,
    FrameDefinition,
    FrameIdentifier,
    Gate,
    GateDefinition,
    GateModifier,
    GateSpecification,
    Include,
    Instruction[repr + quil + parse],
    Jump,
    JumpUnless,
    JumpWhen,
    Label,
    Load,
    MeasureCalibrationDefinition,
    MeasureCalibrationIdentifier,
    Measurement,
    MemoryReference[repr + quil + parse],
    Move,
    Offset,
    PauliGate[repr],
    PauliSum[repr],
    Pragma,
    PragmaArgument,
    Pulse,
    Qubit,
    QubitPlaceholder[repr],
    RawCapture,
    Reset,
    ScalarType,
    SetFrequency,
    SetPhase,
    SetScale,
    Sharing[repr],
    ShiftFrequency,
    ShiftPhase,
    Store,
    SwapPhases,
    Target,
    TargetPlaceholder[repr],
    UnaryLogic,
    UnaryOperator,
    UnresolvedCallArgument,
    Vector,
    Waveform[repr],
    WaveformDefinition,
    WaveformInvocation,
]);

#[cfg(feature = "stubs")]
impl pyo3_stub_gen::PyStubType for ExternPragmaMap {
    fn type_output() -> pyo3_stub_gen::TypeInfo {
        pyo3_stub_gen::TypeInfo::dict_of::<Option<String>, Pragma>()
    }
}

#[pymethods]
impl Calibration {
    #[getter]
    fn name(&self) -> &str {
        &self.identifier.name
    }

    #[getter]
    fn parameters(&self) -> Vec<Expression> {
        self.identifier.parameters.clone()
    }

    #[getter]
    fn qubits(&self) -> Vec<Qubit> {
        self.identifier.qubits.clone()
    }

    #[getter]
    fn modifiers(&self) -> Vec<GateModifier> {
        self.identifier.modifiers.clone()
    }
}

pickleable_new! {
    impl CalibrationIdentifier {
        /// Builds a new calibration identifier.
        ///
        /// Raises an error if the given name isn't a valid Quil identifier.
        fn __new__(
            name: String,
            parameters: Vec<Expression>,
            qubits: Vec<Qubit>,
            modifiers: Vec<GateModifier>,
        ) -> Result<Self, IdentifierValidationError> {
            // Note that  the parameter order is different for the Python version :(
            Self::new(name, modifiers, parameters, qubits)
        }
    }
}

#[pymethods]
impl Call {
    fn py_arguments(&self) -> Vec<UnresolvedCallArgument> {
        self.arguments.clone()
    }
}

pickleable_new! {
    impl Gate {
        fn __new__(
            name: String,
            parameters: Vec<Expression>,
            qubits: Vec<Qubit>,
            modifiers: Vec<GateModifier>,
        ) -> Result<Self, GateError> {
            Self::new(&name, parameters, qubits, modifiers)
        }
    }
}

#[pymethods]
impl Gate {
    #[pyo3(name = "dagger")]
    #[must_use]
    fn py_dagger(&self) -> Self {
        self.clone().dagger()
    }

    #[pyo3(name = "controlled")]
    #[must_use]
    fn py_controlled(&self, control_qubit: Qubit) -> Self {
        self.clone().controlled(control_qubit)
    }

    #[pyo3(name = "forked")]
    fn py_forked(&self, fork_qubit: Qubit, alt_params: Vec<Expression>) -> Result<Self, GateError> {
        self.clone().forked(fork_qubit, alt_params)
    }

    // TODO: `Gate` is now immutable for Python users because it uses the Rust hash impl.
    // Consequently, this method now makes a clone of the `Gate` before calculating the unitary.
    // It probably makes sense to change the method name to reflect this.
    #[pyo3(name = "to_unitary_mut")]
    fn py_to_unitary_mut<'py>(
        slf: &Bound<'py, Self>,
        n_qubits: u64,
    ) -> PyResult<Bound<'py, PyArray2<Complex64>>> {
        let py = slf.py();
        let mut slf = { slf.get().clone() };
        Ok(slf.to_unitary(n_qubits)?.to_pyarray(py))
    }
}

#[pymethods]
impl MeasureCalibrationDefinition {
    #[getter]
    fn qubit(&self) -> Option<Qubit> {
        self.identifier.qubit.clone()
    }

    #[getter]
    fn parameter(&self) -> &str {
        &self.identifier.parameter
    }
}

#[pymethods]
impl Sharing {
    #[getter]
    fn name(&self) -> String {
        self.name.clone()
    }

    #[getter]
    fn offsets(&self) -> Vec<Offset> {
        self.offsets.clone()
    }
}

#[pymethods]
impl Offset {
    #[getter]
    fn offset(&self) -> u64 {
        self.offset
    }

    #[getter]
    fn data_type(&self) -> ScalarType {
        self.data_type
    }
}

#[pymethods]
impl PauliGate {
    /// Parse a ``PauliGate`` from a string.
    ///
    /// Raises a ``ParseExpressionError`` error if the string isn't a valid Quil expression.
    #[staticmethod]
    fn parse(input: &str) -> Result<Self, ParseInstructionError> {
        <Self as std::str::FromStr>::from_str(input)
            .map_err(|err| ParseInstructionError::Parse(err.to_string()))
    }
}

#[pymethods]
impl QubitPlaceholder {
    #[new]
    fn new() -> Self {
        Self::default()
    }
}
