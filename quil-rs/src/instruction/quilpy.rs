use indexmap::IndexMap;
use num_complex::Complex64;
use numpy::{PyArray2, ToPyArray};
use paste::paste;
use pyo3::{
    exceptions::PyValueError,
    prelude::*,
    types::{IntoPyDict as _, PyDict, PyTuple},
    IntoPyObjectExt,
};
use rigetti_pyo3::{create_init_submodule, impl_repr};

#[cfg(feature = "stubs")]
use pyo3_stub_gen::{
    derive::{gen_methods_from_python, gen_stub_pyclass, gen_stub_pymethods},
    inventory::submit,
};

use super::*;
use crate::{
    expression::quilpy::ExpressionLike,
    instruction::gate::GateSignature,
    pickleable_new,
    quilpy::{
        deprecated_param,
        errors::{self, PickleError},
        impl_to_quil, py_deprecated,
    },
    validation::identifier::IdentifierValidationError,
};

create_init_submodule! {
    classes: [
        Arithmetic,
        ArithmeticOperator,
        AttributeValue,
        BinaryLogic,
        BinaryOperator,
        CalibrationDefinition,
        CalibrationIdentifier,
        Call,
        Capture,
        CircuitDefinition,
        Comparison,
        ComparisonOperator,
        Convert,
        Declaration,
        Delay,
        DefGateSequence,
        Exchange,
        ExternParameter,
        ExternSignature,
        Fence,
        FrameDefinition,
        FrameIdentifier,
        Gate,
        GateDefinition,
        GateModifier,
        GateType,
        Include,
        Jump,
        JumpUnless,
        JumpWhen,
        Label,
        Load,
        MeasureCalibrationDefinition,
        MeasureCalibrationIdentifier,
        Measurement,
        MemoryReference,
        Move,
        Offset,
        OwnedGateSignature,
        PauliGate,
        PauliTerm,
        PauliSum,
        Pragma,
        Pulse,
        QubitPlaceholder,
        RawCapture,
        Reset,
        ScalarType,
        SetFrequency,
        SetPhase,
        SetScale,
        Sharing,
        ShiftFrequency,
        ShiftPhase,
        Store,
        SwapPhases,
        TargetPlaceholder,
        UnaryLogic,
        UnaryOperator,
        Vector,
        Waveform,
        WaveformDefinition,
        WaveformInvocation
    ],

    complex_enums: [
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
        UnresolvedCallArgument
    ],

    errors: [
        errors::InstructionError,
        errors::CallError,
        errors::DefGateSequenceError,
        errors::ExternError,
        errors::GateError,
        errors::ParseInstructionError,
        errors::ParseMemoryReferenceError
    ],
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

/// Add an `out` implementation to a `#[pyclass]` to use the type's `Quil` implementation.
///
/// The method reports that it is deprecated and recommends using `to_quil` instead.
/// It is added to maintain backwards compatibility with `QuilAtom` types.
macro_rules! impl_out {
    ($($name: ty),*) => {
        $(
        #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
        #[pyo3::pymethods]
        impl $name {
            /// Get a Quil representation as a string.
            ///
            /// This method is deprecated; you should use `to_quil` instead.
            fn out(&self, py: Python<'_>) -> PyResult<String> {
                py_deprecated!(py, c"`out` is deprecated; use `to_quil` instead")?;
                self.py_to_quil()
            }

            /// Get a Quil-like representation as a string.
            fn __str__(&self) -> String {
                self.py_to_quil_or_debug()
            }
        }
        )*
    };
}

impl_out!(FrameIdentifier, Label, MemoryReference, WaveformInvocation);

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
                type_repr = "builtins.tuple[()] | builtins.tuple[" $($kind)" | "* "]",
                imports = ("builtins")
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
    #[gen_stub(override_return_type(type_repr = "builtins.tuple[builtins.int | builtins.float | MemoryReference]", imports = ("builtins")))]
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
    #[gen_stub(override_return_type(type_repr = "builtins.tuple[builtins.str | Expression]", imports = ("builtins")))]
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
    #[gen_stub(override_return_type(type_repr = "builtins.tuple[builtins.int | MemoryReference]", imports = ("builtins")))]
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
    #[gen_stub(override_return_type(type_repr = "builtins.tuple[builtins.int | builtins.float | MemoryReference]", imports = ("builtins")))]
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
    #[gen_stub(override_return_type(type_repr = "builtins.tuple[ScalarType | Vector]", imports = ("builtins")))]
    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        match self {
            Self::Scalar(value) | Self::VariableLengthVector(value) => (*value,).into_pyobject(py),
            Self::FixedLengthVector(value) => (value.clone(),).into_pyobject(py),
        }
    }
}

/// A designator for a gate modifier, which can be either a string or an actual `GateModifier`.
///
/// Used to allow functions exposed via Python bindings to accept either type.
#[derive(FromPyObject)]
enum GateModifierDesignator {
    String(String),
    GateModifier(GateModifier),
}

impl TryFrom<GateModifierDesignator> for GateModifier {
    type Error = PyErr;
    fn try_from(value: GateModifierDesignator) -> PyResult<Self> {
        match value {
            GateModifierDesignator::GateModifier(m) => Ok(m),
            GateModifierDesignator::String(s) => GateModifier::try_from_str(&s),
        }
    }
}

// Override the type signature for `Gate.__new__` to hide the `params` parameter,
// which is only used for backwards compatibility and is not intended for users.
#[cfg(feature = "stubs")]
submit! {
    gen_methods_from_python! {
        r#"
        import quil.expression

        class Gate:
            def __new__(
                cls,
                name: builtins.str,
                parameters: Sequence[quil.expression.ParameterLike],
                qubits: Sequence[Qubit],
                modifiers: Sequence[GateModifierDesignator] = None,
            ) -> Self:
                """Create a new ``Gate``."""
        "#
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Gate {
    #[new]
    #[gen_stub(skip)]
    #[pyo3(signature = (name, parameters, qubits, modifiers = None, *, params = None))]
    fn __new__(
        py: Python<'_>,
        name: String,
        parameters: Vec<ExpressionLike>,
        qubits: Vec<QubitDesignator>,
        modifiers: Option<Vec<GateModifierDesignator>>,
        // `params` is for backwards compatibility and will raise a deprecation warning if used.
        params: Option<Vec<ExpressionLike>>,
    ) -> PyResult<Gate> {
        let qubits = qubits.into_iter().map(|q| Qubit::__new__(q)).collect();

        let parameters = if let Some(params) = params {
            deprecated_param!(py, param, parameters)?;
            params
        } else {
            parameters
        }
        .into_iter()
        .map(|p| p.into())
        .collect();

        let modifiers = modifiers
            .unwrap_or_default()
            .into_iter()
            .map(|m| TryInto::<GateModifier>::try_into(m))
            .collect::<Result<Vec<GateModifier>, PyErr>>()?;

        Ok(Self::new(&name, parameters, qubits, modifiers)?)
    }

    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        Ok((
            self.name.clone(),
            self.parameters.clone(),
            self.qubits.clone(),
            self.modifiers.clone(),
        )
            .into_pyobject_or_pyerr(py)?)
    }

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

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl GateModifier {
    #[staticmethod]
    #[pyo3(name = "from_str")]
    fn try_from_str(modifier: &str) -> PyResult<Self> {
        match modifier.to_ascii_uppercase().as_str() {
            "CONTROLLED" => Ok(GateModifier::Controlled),
            "DAGGER" => Ok(GateModifier::Dagger),
            "FORKED" => Ok(GateModifier::Forked),
            _ => Err(PyValueError::new_err("unknown gate modifier")),
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl GateSpecification {
    #[gen_stub(override_return_type(
        type_repr = "builtins.tuple[builtins.list[builtins.list[expression.Expression]] | builtins.list[builtins.int] | PauliSum | DefGateSequence]",
        imports = ("quil._quil.expression")
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
    module = "quil._quil.instructions",
    name = "GateSignature",
    eq,
    frozen,
    hash,
    get_all,
    subclass,
    from_py_object
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

// TODO(migration-guide): The `FrameIdentifier` corresponds to PyQuil's `quilatom.Frame`.
// Since this is easy to find (`rg Frame\(`), easily caught by static type checkers,
// and will fail unambiguously at runtime, we'll just point this out in the migration guide.

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl FrameIdentifier {
    // Note: the parameter order here is swapped around
    // for backwards compatibility with PyQuil's `quilatom.Frame`.
    #[new]
    fn __new__(qubits: Vec<QubitDesignator>, name: String) -> Self {
        let qubits = qubits.into_iter().map(|q| q.into()).collect();
        Self::new(name, qubits)
    }

    fn __getnewargs__(&self) -> (Vec<Qubit>, String) {
        (self.qubits.clone(), self.name.clone())
    }
}

/// Used to create `Label`s from existing `Target`s as well as Python `str` instances.
#[derive(FromPyObject)]
enum LabelTargetParameter {
    Fixed(String),
    Target(Target),
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Label {
    // TODO(migration-guide):
    //   Quil has `LABEL @jump-target` instructions,
    //   but it's handy for programmatic `Program` construction
    //   to not need to give an explicit `jump-target` name,
    //   but instead just to correlate via `Label` instances.
    //
    //   In `quil-rs`, the `Label` struct represents a Quil `LABEL` instruction,
    //   but the real meat of its behavior comes from the `Target` enum,
    //   which can be a `Fixed(String)` or a `Placeholder(TargetPlaceholder)`.
    //   A Quil `LABEL` is `Label { target: Target::Fixed("jump-target") }`,
    //   and hence `Label` is essentially just a wrapper around a `Target`.
    //   The `Placeholder` is for the benefit of `Program` construction,
    //   particularly loops.
    //
    //   The old PyQuil v4 had `QuilAtom`s named `Label` and `LabelPlaceholder`,
    //   which each had a `target` parameter of type `Target`.
    //   The former was always `Target::Fixed` while the latter was `Target::Placeholder`.
    //   That is, neither was actually a `quil-rs` `Label`!.
    //
    //   So, in v5, `Label` and `LabelPlaceholder` are merged into simply `Label`,
    //   which can be constructed with a string or an actual `Target`.
    //   To specify that the `target` should be a placeholder,
    //   users can pass the argument `placeholder=True`.
    //
    //   Backwards compatibility with the old `LabelPlaceholder` class is maintained
    //   via a `partial` that sets this value for the user;
    //   however, that specific class accepted a parameter by that name,
    //   which was expected to be an existing `TargetPlaceholder` instance,
    //   so this constructor handles that instance specifically.
    //
    //   Since much of this constructor trickery exists just for backwards compatibility,
    //   it'll raise deprecation warnings, and can (and should) be removed in the future.
    //
    //   The old uses can be updated as follows:
    //
    //   |  Deprecated Usage                 | Updated Usage                         |
    //   | --------------------------------- | ------------------------------------- |
    //   | `Label("jump-target")`            | No Need to Update                     |
    //   | `Label(label_name="jump-target")` | `Label(target="jump-target")`         |
    //   | `LabelPlaceholder()`              | `Label()`                             |
    //   | `LabelPlaceholder("L")`           | `Label("L", placeholder=True)`        |
    //   | `LabelPlaceholder(prefix="L")`    | `Label(target="L", placeholder=True)` |
    //   | `LabelPlaceholder(placeholder=p)` | `Label(target=p)`                     |
    //
    //   Note the `LabelPlaceholder("L", placeholder=p)` was technically legal,
    //   but the `prefix` was ignored, so it is equivalent to just giving a `placeholder`.
    /// Create a new `Label`.
    ///
    /// A `Label` represents a ``LABEL`` instruction, which in Quil reads as ``LABEL @some-name``.
    /// You can make a program execute from that point onward via a ``JUMP @some-name`` instruction
    /// or its siblings, ``JUMP-WHEN @some-name foo[0]`` and ``JUMP-UNLESS @some-name bar[0]``.
    /// The `@some-name` part of these instructions is the "target".
    ///
    /// You can construct a `Label` with a fixed `target` using ``Label("some-name")``,
    /// and then you can reference that point using, for example, ``Jump("some-name")``.
    ///
    /// When constructing a `Program` in code,
    /// you can reference the `Label` object itself when creating a `Jump` instruction.
    /// In fact, in this case, you don't need to give an explicit `Label` name::
    ///
    /// ```python
    /// def get_body() -> list[Instruction]:
    ///     return []
    ///
    /// top = Label()
    /// prog = Program(top, *get_body(), Jump(top))
    ///
    /// prog.resolve_placeholders()  # Use this to automatically assign targets for Labels/Jumps.
    /// print(prog.to_quil())
    /// ```
    ///
    /// You can create a new `Label` from a particular `Target`,
    /// or you can let the constructor create the `Target` instance for you.
    /// Use ``Label("name")`` for fixed target or ``Label()`` to create a placeholder target;
    /// if you want a placeholder with a specific `base_label`,
    /// you'll need to specify ``Label("name", placeholder=True)``.
    ///
    /// To summarize:
    ///
    /// | Simple                           | Equivalent                                                      |
    /// | `Label("A")`                     | `Label(Target::Fixed("A"))`                                     |
    /// | `Label()`                        | `Label(Target::Placeholder(TargetPlaceholder(base_label="L")))` |
    /// | `Label("A", placeholder=True)`   | `Label(Target::Placeholder(TargetPlaceholder(base_label="A")))` |
    #[new]
    #[pyo3(signature = (target=None, *, placeholder=None))]
    fn __new__(target: Option<LabelTargetParameter>, placeholder: Option<bool>) -> PyResult<Self> {
        let target = match (target, placeholder) {
            (Some(LabelTargetParameter::Target(target)), None) => target,
            (Some(LabelTargetParameter::Target(_)), Some(_)) => {
                return Err(PyValueError::new_err(
                    "`placeholder` is not permitted with an explicit `target`",
                ));
            }
            (Some(LabelTargetParameter::Fixed(label)), Some(false) | None) => Target::Fixed(label),
            (Some(LabelTargetParameter::Fixed(label)), Some(true)) => {
                Target::Placeholder(TargetPlaceholder::new(label))
            }
            (None, Some(true) | None) => {
                Target::Placeholder(TargetPlaceholder::new("L".to_string()))
            }
            (None, Some(false)) => {
                return Err(PyValueError::new_err("missing label target"));
            }
        };

        Ok(Self { target })
    }

    fn __getnewargs__(&self) -> (Target,) {
        (self.target.clone(),)
    }

    /// Get the `Label`'s name, assuming it is a `Fixed` target.
    ///
    /// This is deprecated because a `Label`'s `target` might not be `Fixed`.
    #[getter]
    fn name(&self, py: Python<'_>) -> PyResult<String> {
        py_deprecated!(
            py,
            c"`name` is deprecated; if `target` is `Fixed`, use `target._0`"
        )?;

        match self.target {
            Target::Fixed(ref name) => Ok(name.clone()),
            Target::Placeholder(_) => Err(PyValueError::new_err("label `target` is not `Fixed`")),
        }
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
        type_repr = "builtins.tuple[
            builtins.tuple[Qubit, typing.Optional[builtins.str]],
            builtins.dict[builtins.str, typing.Optional[builtins.str]]
        ]",
        imports = ("builtins", "typing")
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
        type_repr = "builtins.tuple[
            builtins.tuple[Qubit, typing.Optional[MemoryReference]],
            builtins.dict[builtins.str, typing.Optional[builtins.str]]
        ]",
        imports = ("builtins", "typing")
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

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl MemoryReference {
    // PyQuil v4 uses the term `offset` instead of `index`, so this handles both.
    /// Construct a new `MemoryReference`.
    ///
    /// Note that `offset` is an older (deprecated) term for `index`.
    /// New code should use `index`, but using `offset` as a keyword argument is still accepted;
    /// if it is not `None`, it'll be used instead of `index`, regardless of how `index` is passed.
    #[new]
    #[pyo3(signature = (name, index = 0, *, offset = None))]
    fn py_new(py: Python<'_>, name: String, index: u64, offset: Option<u64>) -> PyResult<Self> {
        let index = if let Some(offset) = offset {
            deprecated_param!(py, offset, index)?;
            offset
        } else {
            index
        };

        Ok(Self { name, index })
    }

    #[gen_stub(override_return_type(type_repr = "tuple[str, int, int | None]"))]
    fn __getnewargs__(&self) -> (String, u64) {
        (self.name.clone(), self.index)
    }

    /// Return a new `MemoryReference` with the given `index`.
    ///
    /// This requires that `self` has an `index` of 0.
    fn __getitem__(&self, index: u64) -> PyResult<Self> {
        if self.index != 0 {
            return Err(PyValueError::new_err(
                "indexing only allowed on base MemoryReferences (those with 0 `index`)",
            ));
        }

        Ok(Self {
            name: self.name.clone(),
            index,
        })
    }

    // -------------------------------------------------------------------------------------
    // The following are deprecated PyQuil v4 methods present for backwards compatibility.
    // -------------------------------------------------------------------------------------

    #[getter]
    fn offset(&self, py: Python<'_>) -> PyResult<u64> {
        py_deprecated!(py, c"`offset` is deprecated; use `index` instead")?;
        Ok(self.index)
    }

    #[staticmethod]
    fn _from_parameter_str(py: Python<'_>, memory_reference_str: &str) -> PyResult<Self> {
        py_deprecated!(
            py,
            c"`_from_parameter_str` is deprecated; use `parse` instead"
        )?;
        match <Expression as std::str::FromStr>::from_str(memory_reference_str)? {
            Expression::Address(addr) => Ok(addr),
            _ => Err(PyValueError::new_err("not a memory reference")),
        }
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

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
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
impl PauliTerm {
    #[new]
    fn __new__(
        arguments: Vec<(PauliGate, String)>,
        #[gen_stub(override_type(
            type_repr = "_quil.expression.Expression",
            imports = ("quil._quil.expression")
        ))]
        expression: Expression,
    ) -> PauliTerm {
        Self::new(arguments, expression)
    }

    #[gen_stub(override_return_type(
        type_repr = "builtins.tuple[
            builtins.list[builtins.tuple[PauliGate, builtins.str]],
            _quil.expression.Expression
        ]",
        imports = ("quil._quil.expression", "builtins")
    ))]
    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        (self.arguments.clone(), self.expression.clone()).into_pyobject(py)
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PragmaArgument {
    #[gen_stub(override_return_type(type_repr = "builtins.tuple[builtins.int | builtins.str]", imports = ("builtins")))]
    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        match self {
            Self::Identifier(value) => (value.clone(),).into_pyobject(py),
            Self::Integer(value) => (*value,).into_pyobject(py),
        }
    }
}

#[derive(FromPyObject)]
enum QubitDesignator {
    Fixed(u64),
    Placeholder(QubitPlaceholder),
    Variable(String),
    Qubit(Qubit),
}

impl From<QubitDesignator> for Qubit {
    fn from(value: QubitDesignator) -> Self {
        match value {
            QubitDesignator::Fixed(value) => Self::Fixed(value),
            QubitDesignator::Placeholder(value) => Self::Placeholder(value),
            QubitDesignator::Variable(value) => Self::Variable(value),
            QubitDesignator::Qubit(value) => value,
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Qubit {
    #[new]
    fn __new__(q: QubitDesignator) -> Self {
        q.into()
    }

    #[gen_stub(override_return_type(type_repr = "builtins.tuple[builtins.int | builtins.str | QubitPlaceholder]", imports = ("builtins")))]
    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        match self {
            Self::Fixed(value) => (value,).into_pyobject(py),
            Self::Variable(value) => (value,).into_pyobject(py),
            Self::Placeholder(value) => (value.clone(),).into_pyobject(py),
        }
    }
}

#[cfg(feature = "stubs")]
mod stubs {
    use pyo3_stub_gen::impl_stub_type;

    #[allow(clippy::wildcard_imports)]
    use super::*;

    // TODO(migration-guide): The QubitDesignator type alias is replaced by Qubit,
    // covering ints (the old "Qubit" type), strings (the old "FormalArgument"), and placeholders.
    impl_stub_type!(QubitDesignator = QubitPlaceholder | i64 | String);
    impl_stub_type!(LabelTargetParameter = String | Target);
    impl_stub_type!(GateModifierDesignator = GateModifier | String);
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl QubitPlaceholder {
    #[new]
    fn new() -> Self {
        Self::default()
    }

    /// Return a 'register' of ``n`` qubit placeholders.
    #[staticmethod]
    fn register(n: isize) -> Vec<Self> {
        (0..n).map(|_| Self::default()).collect()
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
    #[gen_stub(override_return_type(type_repr = "builtins.tuple[builtins.str | TargetPlaceholder]", imports = ("builtins")))]
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
impl TargetPlaceholder {
    // TODO(migration-guide): This type replaces `LabelPlaceholder`,
    //   but uses `base_label` in places of `prefix`.
    //   The old type also accepted a `placeholder` parameter.
    /// Create a new `TargetPlaceholder`.
    ///
    /// If you are only constructing a `TargetPlaceholder` to pass to the `Label` constructor,
    /// note that you can use ``Label()`` or ``Label("L", placeholder=True)``
    /// and access the ``target`` attribute instead.
    ///
    /// The keyword-only `prefix` and `placeholder` parameters are deprecated,
    /// but made available to ease the transition from PyQuil v4's `LabelPlaceholder`.
    /// New code should use ``Label("L", placeholder=True)`` instead.
    #[pyo3(signature = (base_label="L", *, prefix=None, placeholder=None))]
    #[new]
    fn __new__(
        py: Python<'_>,
        base_label: &str,
        prefix: Option<String>,
        placeholder: Option<Self>,
    ) -> PyResult<Self> {
        if let Some(label) = placeholder {
            py_deprecated!(py, c"passing a `placeholder` is deprecated")?;
            Ok(Self::new(label.as_inner().to_string()))
        } else if let Some(label) = prefix {
            deprecated_param!(py, prefix, base_label)?;
            Ok(Self::new(label))
        } else {
            Ok(Self::new(base_label.to_string()))
        }
    }

    fn __getnewargs__(&self) -> PyResult<(String,)> {
        Ok((self.as_inner().to_string(),))
    }

    /// Get the `Placeholder`'s `base_label`.
    ///
    /// This is deprecated; use `base_label` instead.
    #[getter]
    fn prefix(&self, py: Python<'_>) -> PyResult<&str> {
        py_deprecated!(py, c"`prefix` is deprecated; use `base_label`")?;
        Ok(self.as_inner())
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl UnresolvedCallArgument {
    #[gen_stub(override_return_type(type_repr = "builtins.tuple[builtins.str | MemoryReference | builtins.complex]", imports = ("builtins")))]
    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        match self {
            Self::Identifier(value) => (value.clone(),).into_pyobject(py),
            Self::MemoryReference(value) => (value.clone(),).into_pyobject(py),
            Self::Immediate(value) => (*value,).into_pyobject(py),
        }
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl WaveformInvocation {
    #[pyo3(signature = (name, parameters=None))]
    #[new]
    fn __new__(name: String, parameters: Option<IndexMap<String, ExpressionLike>>) -> Self {
        let parameters = parameters
            .map(|dict| dict.into_iter().map(|(k, v)| (k, v.into())).collect())
            .unwrap_or_default();
        Self::new(name, parameters)
    }

    fn __getnewargs__(&self) -> (String, WaveformParameters) {
        (self.name.clone(), self.parameters.clone())
    }
}
