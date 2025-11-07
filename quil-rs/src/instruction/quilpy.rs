use num_complex::Complex64;
use numpy::{PyArray2, ToPyArray};
use paste::paste;
use pyo3::{
    prelude::*,
    types::{IntoPyDict as _, PyDict, PyTuple},
};

use super::*;
use crate::{
    instruction::gate::GateSignature,
    pickleable_new,
    quilpy::{errors::PickleError, fix_complex_enums, impl_repr, impl_to_quil},
    validation::identifier::IdentifierValidationError,
};

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pymethods};

#[pymodule]
#[pyo3(name = "instructions", module = "quil", submodule)]
pub(crate) fn init_submodule(m: &Bound<'_, PyModule>) -> PyResult<()> {
    use crate::quilpy::errors;

    let py = m.py();

    m.add(
        "InstructionError",
        py.get_type::<errors::InstructionError>(),
    )?;
    m.add("CallError", py.get_type::<errors::CallError>())?;
    m.add("ExternError", py.get_type::<errors::ExternError>())?;
    m.add("GateError", py.get_type::<errors::GateError>())?;
    m.add(
        "DefGateSequenceError",
        py.get_type::<errors::DefGateSequenceError>(),
    )?;
    m.add(
        "ParseInstructionError",
        py.get_type::<errors::ParseInstructionError>(),
    )?;
    m.add(
        "ParseMemoryReferenceError",
        py.get_type::<errors::ParseMemoryReferenceError>(),
    )?;

    add_instruction_classes(m)?;

    fix_complex_enums!(
        py,
        ArithmeticOperand,
        AttributeValue,
        BinaryOperand,
        ComparisonOperand,
        ExternParameterType,
        GateSpecification,
        Instruction,
        PragmaArgument,
        Qubit,
        Target,
        UnresolvedCallArgument,
    );

    Ok(())
}

/// Add a `parse` implementation to a `#[pyclass]` to use the type's `from_str` implementation.
macro_rules! impl_parse {
    ($name: ident) => {
        #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
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
/// ```ignore
/// impl_instruction!([
///     A, // By default, [repr + quil]
///     B [repr + quil],
///     C [quil + parse],
/// ]);
/// ```
macro_rules! impl_instruction {
    // Initial capture: this lets us grab all the names in one go,
    // which we can then use to generate parts of the module initializer.
    // After we generate that, the entire input is passed on to the @list rule,
    // which will chew through the tokens recursively.
    ([$( $name:ident $([$($args: tt)*])? ),* ,]) => {

        /// Adds instruction classes to the given module, assumed to be `quil.instructions`.
        fn add_instruction_classes(m: &Bound<'_, PyModule>) -> PyResult<()> {
            $(m.add_class::<$name>()?;)*
            Ok(())
        }

        impl_instruction!(@list [$($name $([$($args)*])? ,)*]);
    };

    // Terminal rule -- an empty list expands to nothing.
    (@list []) => {};

    // Implements default methods for an instruction, then recursively expands the rest of the list.
    (@list [$name:ident, $($tail: tt)*]) => {
        impl_instruction!(@one $name [+ repr + quil]);
        impl_instruction!(@list [$($tail)*]);
    };

    // Implements specific methods for an instruction, then recursively expands the rest of the list.
    (@list [$name: ident [$($args: tt)+], $($tail: tt)*]) => {
        impl_instruction!(@one $name [+ $($args)*]);
        impl_instruction!(@list [$($tail)*]);
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
    CalibrationDefinition,
    CalibrationIdentifier,
    Call,
    Capture,
    CircuitDefinition,
    Comparison,
    ComparisonOperand,
    ComparisonOperator,
    Convert,
    Declaration,
    Delay,
    DefGateSequence[repr],
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
    GateType,
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
    OwnedGateSignature[repr],
    PauliGate[repr],
    PauliTerm[repr],
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
    UnresolvedCallArgument, // Python name: CallArgument
    Vector,
    Waveform[repr],
    WaveformDefinition,
    WaveformInvocation,
]);

/// This macro expands to the `__getnewargs__` definition for the `Instruction` enum,
/// making it compatible with the `copy` and `pickle` modules,
/// (provided the variant itself supports it).
///
/// Note that we have to manually handle `Halt`, `Nop`, and `Wait`, since they don't actually have
/// an inner value.
///
/// Finally, this macro makes use of `paste` to generate the `pyo3_stub_gen` return type,
/// since it's a union of an empty tuple and a tuple of variant's inner type.
/// The `type_repr` requires a string literal, so we can't just use `concat!(stringify!`.
/// This is also why the macro operates on the full list of variants,
/// rather than just generating a single match arm.
///
// Developer note: with a little effort, this could be made to operate on any "complex" enum,
// but it would likely best suited as a `#[derive(...)]` macro.
// Since we currently have only about a dozen of them, most with only a couple variants,
// the reason to do so would mostly be potential future reuse.
//
// In particular, unlike struct-based `#[pyclass]`es,
// there's little risk a handwritten `__getnewargs__` could be incompatible with `__new__`,
// since most changes to the variants would either work fine or would cause a compiler error.
// As an exception to that rule, though, if you change the variant's type,
// the methods will still work as intended, but the type stub annotation will be wrong.
//
// It might still make sense to create a derive macro that works for all `#[pyclass]`es
// and generates the `__new__` and/or `__getnewargs__` appropriate for the type,
// which could then replace most uses of `pickleable_new!` as well.
macro_rules! instruction_getnewargs {
    ($($kind:ty),* $(,)?) => { paste! {
        #[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
        #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
        #[pymethods]
        impl Instruction {
            #[gen_stub(override_return_type(
                type_repr = "tuple[()] | tuple[" $($kind)" | "* "]"
            ))]
            fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
                match self {
                    Instruction::Halt() | Instruction::Nop() | Instruction::Wait() => {
                        Ok(PyTuple::empty(py))
                    },
                    $(Instruction::$kind(instr) => (instr.clone(),).into_pyobject(py),)*
                }
            }
        }
    }};
}

// Note that these are just the [`Instruction`] variants that take parameters.
instruction_getnewargs!(
    Arithmetic,
    BinaryLogic,
    Call,
    Capture,
    CalibrationDefinition,
    CircuitDefinition,
    Comparison,
    Convert,
    Declaration,
    Delay,
    Exchange,
    Fence,
    FrameDefinition,
    Gate,
    GateDefinition,
    Include,
    Jump,
    JumpUnless,
    JumpWhen,
    Label,
    Load,
    MeasureCalibrationDefinition,
    Measurement,
    Move,
    Pragma,
    Pulse,
    RawCapture,
    Reset,
    SetFrequency,
    SetPhase,
    SetScale,
    ShiftFrequency,
    ShiftPhase,
    Store,
    SwapPhases,
    UnaryLogic,
    WaveformDefinition,
);

// The following types implement `__getnewargs__` manually because,
// as (complex-)enums, they get their `__new__` methods from PyO3 directly,
// so we can't wrap them in the `pickleable_new!` macro.
// In any case, this lets us correctly set the type stubs' return types,
// which would otherwise require either creating our own derive macro,
// or using `paste!` (as is done in the macro version for `Instruction`).

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl ArithmeticOperand {
    #[gen_stub(override_return_type(type_repr = "tuple[int | float | MemoryReference]"))]
    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        match self {
            Self::LiteralInteger(value) => (value,).into_pyobject(py),
            Self::LiteralReal(value) => (value,).into_pyobject(py),
            Self::MemoryReference(value) => (value.clone(),).into_pyobject(py),
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl AttributeValue {
    #[gen_stub(override_return_type(type_repr = "tuple[str | Expression]"))]
    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        match self {
            Self::String(value) => (value.clone(),).into_pyobject(py),
            Self::Expression(value) => (value.clone(),).into_pyobject(py),
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl BinaryOperand {
    #[gen_stub(override_return_type(type_repr = "tuple[int | MemoryReference]"))]
    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        match self {
            Self::LiteralInteger(value) => (value,).into_pyobject(py),
            Self::MemoryReference(value) => (value.clone(),).into_pyobject(py),
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl ComparisonOperand {
    #[gen_stub(override_return_type(type_repr = "tuple[int | float | MemoryReference]"))]
    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        match self {
            Self::LiteralInteger(value) => (value,).into_pyobject(py),
            Self::LiteralReal(value) => (value,).into_pyobject(py),
            Self::MemoryReference(value) => (value.clone(),).into_pyobject(py),
        }
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl CalibrationDefinition {
    /// The gate name that this calibration definition is for.
    #[getter]
    fn name(&self) -> &str {
        &self.identifier.name
    }

    /// The list of parameters that this calibration definition is for.
    #[getter]
    fn parameters(&self) -> Vec<Expression> {
        self.identifier.parameters.clone()
    }

    /// The list of [`Qubit`]s that this calibration definition is for.
    #[getter]
    fn qubits(&self) -> Vec<Qubit> {
        self.identifier.qubits.clone()
    }

    /// The list of [`GateModifier`]s that this calibration definition is for.
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
        ) -> Result<CalibrationIdentifier, IdentifierValidationError> {
            // Note that  the parameter order is different for the Python version :(
            Self::new(name, modifiers, parameters, qubits)
        }
    }
}

#[cfg(feature = "stubs")]
impl pyo3_stub_gen::PyStubType for ExternPragmaMap {
    fn type_output() -> pyo3_stub_gen::TypeInfo {
        pyo3_stub_gen::TypeInfo::dict_of::<Option<String>, Pragma>()
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl ExternParameterType {
    #[gen_stub(override_return_type(type_repr = "tuple[ScalarType | Vector]"))]
    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        match self {
            Self::Scalar(value) | Self::VariableLengthVector(value) => (*value,).into_pyobject(py),
            Self::FixedLengthVector(value) => (value.clone(),).into_pyobject(py),
        }
    }
}

pickleable_new! {
    impl Gate {
        fn __new__(
            name: String,
            parameters: Vec<Expression>,
            qubits: Vec<Qubit>,
            modifiers: Vec<GateModifier>,
        ) -> Result<Gate, GateError> {
            Self::new(&name, parameters, qubits, modifiers)
        }
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Gate {
    /// Return a copy of the ``Gate`` with the ``DAGGER`` modifier added to it.
    #[pyo3(name = "dagger")]
    #[must_use]
    fn py_dagger(&self) -> Self {
        self.clone().dagger()
    }

    /// Return a copy of the ``Gate`` with the ``CONTROLLED`` modifier added to it.
    #[pyo3(name = "controlled")]
    #[must_use]
    fn py_controlled(&self, control_qubit: Qubit) -> Self {
        self.clone().controlled(control_qubit)
    }

    /// Return a copy of the ``Gate`` with the ``FORKED`` modifier added to it.
    ///
    /// Raises a ``GateError`` if the number of provided alternate parameters
    /// don't equal the number of existing parameters.
    #[pyo3(name = "forked")]
    fn py_forked(&self, fork_qubit: Qubit, alt_params: Vec<Expression>) -> Result<Self, GateError> {
        self.clone().forked(fork_qubit, alt_params)
    }

    /// Get the matrix resulting from lifting this ``Gate``
    /// to the full `n_qubits`-qubit Hilbert space.
    ///
    /// Raises a ``GateError`` if any of the parameters of this ``Gate`` are non-constant,
    /// if any of the ``Qubit``s are variable,
    /// if the name of this ``Gate`` is unknown,
    /// or if there are an unexpected number of parameters.
    ///
    /// # Notes
    ///
    /// A previous version of this library called this `to_unitary_mut`,
    /// and modified the ``Gate`` when called.
    /// This is no longer possible, as it would modify the ``Gate``'s hash,
    /// leading to confusing bugs.
    /// ``Gate``s, as well as all other hashable classes, are immutable from Python.
    ///
    /// # Bugs
    ///
    /// Supplying `n_qubits` as `0` will raise an unspecified exception;
    /// other invalid input parameters may silently return an invalid result.
    ///
    #[pyo3(name = "to_unitary")]
    fn py_to_unitary<'py>(
        &self,
        n_qubits: u64,
        py: Python<'py>,
    ) -> PyResult<Bound<'py, PyArray2<Complex64>>> {
        Ok(self.clone().to_unitary(n_qubits)?.to_pyarray(py))
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl GateDefinition {
    #[getter(signature)]
    fn py_signature(&self) -> OwnedGateSignature {
        self.signature().into()
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl GateSpecification {
    #[gen_stub(override_return_type(
        type_repr = "tuple[list[list[Expression]] | list[int] | PauliSum | DefGateSequence]"
    ))]
    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        match self {
            Self::Matrix(value) => (value.clone(),).into_pyobject(py),
            Self::Permutation(value) => (value.clone(),).into_pyobject(py),
            Self::PauliSum(value) => (value.clone(),).into_pyobject(py),
            Self::Sequence(value) => (value.clone(),).into_pyobject(py),
        }
    }
}

/// A signature for a gate definition; this does not include the gate definition content.
/// To get a signature from a definition, use `GateDefinition.signature`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyo3::pyclass(
    module = "quil.instructions",
    name = "GateSignature",
    eq,
    frozen,
    hash,
    get_all,
    subclass
)]
pub struct OwnedGateSignature {
    name: String,
    gate_parameters: Vec<String>,
    qubit_parameters: Vec<String>,
    gate_type: GateType,
}

pickleable_new! {
    impl OwnedGateSignature {
        fn new(name: String, gate_parameters: Vec<String>, qubit_parameters: Vec<String>, gate_type: GateType);
    }
}

impl From<GateSignature<'_>> for OwnedGateSignature {
    fn from(signature: GateSignature) -> Self {
        OwnedGateSignature {
            name: signature.name().to_string(),
            gate_parameters: signature.gate_parameters().to_vec(),
            qubit_parameters: signature.qubit_parameters().to_vec(),
            gate_type: signature.gate_type(),
        }
    }
}

impl<'a> TryFrom<&'a OwnedGateSignature> for GateSignature<'a> {
    type Error = GateError;
    fn try_from(signature: &'a OwnedGateSignature) -> Result<Self, Self::Error> {
        GateSignature::try_new(
            &signature.name,
            signature.gate_parameters.as_slice(),
            signature.qubit_parameters.as_slice(),
            signature.gate_type,
        )
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl MeasureCalibrationDefinition {
    /// The Quil-T name of the measurement that this measure calibration definition is for, if any.
    #[getter]
    fn name(&self) -> Option<&str> {
        self.identifier.name.as_deref()
    }

    /// The qubit that this measure calibration definition is for.
    #[getter]
    fn qubit(&self) -> Qubit {
        self.identifier.qubit.clone()
    }

    /// The name the measurement calibration uses for the variable it will write the measurement
    /// result to, if this is a measurement for record.
    #[getter]
    fn target(&self) -> Option<&str> {
        self.identifier.target.as_deref()
    }
}

// We don't use [`pickleable_new!`] here because we're separating Rust's
// [`MeasureCalibrationIdentifier::new`] and Python's `MeasureCalibrationIdentifier.new`.
#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl MeasureCalibrationIdentifier {
    // Note that the Python argument order is not the same as the Rust argument order for
    // [`Self::new`], and that this function requires keywords on the Python side!  Make sure
    // `__getnewargs_ex__` is consistent with `__new__`!
    #[pyo3(signature = (qubit, target, *, name = None))]
    #[new]
    fn __new__(qubit: Qubit, target: Option<String>, name: Option<String>) -> Self {
        Self::new(name, qubit, target)
    }

    #[gen_stub(override_return_type(
        type_repr = "tuple[tuple[Qubit, str | None], dict[str, str | None]]"
    ))]
    fn __getnewargs_ex__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        let Self {
            name,
            qubit,
            target,
        } = self;
        let positional: Bound<'py, PyTuple> = (qubit.clone(), target.clone()).into_pyobject(py)?;
        let keyword: Bound<'py, PyDict> = [("name", name)].into_py_dict(py)?;
        (positional, keyword).into_pyobject(py)
    }
}

// We don't use [`pickleable_new!`] here because we're separating Rust's [`Measurement::new`] and
// Python's `Measurement.new`.
#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Measurement {
    // Note that the Python argument order is not the same as the Rust argument order for
    // [`Self::new`], and that this function requires keywords on the Python side!  Make sure
    // `__getnewargs_ex__` is consistent with `__new__`!
    #[pyo3(signature = (qubit, target, *, name = None))]
    #[new]
    fn __new__(qubit: Qubit, target: Option<MemoryReference>, name: Option<String>) -> Self {
        Self::new(name, qubit, target)
    }

    #[gen_stub(override_return_type(
        type_repr = "tuple[tuple[Qubit, MemoryReference | None], dict[str, str | None]]"
    ))]
    fn __getnewargs_ex__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        let Self {
            name,
            qubit,
            target,
        } = self;
        let positional: Bound<'py, PyTuple> = (qubit.clone(), target.clone()).into_pyobject(py)?;
        let keyword: Bound<'py, PyDict> = [("name", name)].into_py_dict(py)?;
        (positional, keyword).into_pyobject(py)
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
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

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
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

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
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

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PragmaArgument {
    #[gen_stub(override_return_type(type_repr = "tuple[int | str]"))]
    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        match self {
            Self::Identifier(value) => (value.clone(),).into_pyobject(py),
            Self::Integer(value) => (*value,).into_pyobject(py),
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Qubit {
    #[gen_stub(override_return_type(type_repr = "tuple[int | str | QubitPlaceholder]"))]
    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        match self {
            Self::Fixed(value) => (value,).into_pyobject(py),
            Self::Variable(value) => (value,).into_pyobject(py),
            Self::Placeholder(value) => (value.clone(),).into_pyobject(py),
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl QubitPlaceholder {
    #[new]
    fn new() -> Self {
        Self::default()
    }

    /// `QubitPlaceholder`s do not support `pickle` or `deepcopy`.
    /// Calling this method will raise an error.
    #[gen_stub(override_return_type(type_repr = "typing.NoReturn", imports = ("typing")))]
    fn __getnewargs__(&self) -> PyResult<()> {
        Err(PickleError::new_err(
            "Unable to pickle or deepcopy a QubitPlaceholder.",
        ))
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Target {
    #[gen_stub(override_return_type(type_repr = "tuple[str | TargetPlaceholder]"))]
    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        match self {
            Self::Fixed(value) => (value,).into_pyobject(py),
            Self::Placeholder(value) => (value.clone(),).into_pyobject(py),
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl UnresolvedCallArgument {
    #[gen_stub(override_return_type(type_repr = "tuple[str | MemoryReference | complex]"))]
    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        match self {
            Self::Identifier(value) => (value.clone(),).into_pyobject(py),
            Self::MemoryReference(value) => (value.clone(),).into_pyobject(py),
            Self::Immediate(value) => (*value,).into_pyobject(py),
        }
    }
}
