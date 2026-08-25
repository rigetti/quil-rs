use indexmap::IndexMap;
use num_complex::Complex64;
use numpy::{PyArray2, ToPyArray};
use pastey::paste;
use pyo3::{
    exceptions::{PyDeprecationWarning, PyIndexError, PyTypeError, PyValueError},
    prelude::*,
    sync::PyOnceLock,
    types::{IntoPyDict as _, PyDict, PyInt, PyList, PyString, PyTuple},
    CastError, IntoPyObjectExt, PyClass, PyTraverseError, PyTypeCheck, PyVisit,
};
use rigetti_pyo3::{create_init_submodule, impl_repr};

#[cfg(feature = "stubs")]
use pyo3_stub_gen::{
    derive::{gen_methods_from_python, gen_stub_pyclass, gen_stub_pyfunction, gen_stub_pymethods},
    impl_stub_type,
    inventory::submit,
};

use super::*;
use crate::{
    expression::quilpy::{ExpressionArgs, ExpressionLike},
    instruction::gate::GateSignature,
    pickleable_new,
    quilpy::{
        deprecated_or_new, deprecated_param,
        errors::{self, PickleError},
        from_sequence, impl_newargs, impl_to_quil, py_deprecated, py_friendly_enum, IntoNewArgs,
        Like, NewArgs, NonZeroU64,
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
        WaveformInvocation,

        DeclarationAt,

        PyInstruction,
        HaltType,
        NopType,
        WaitType,
    ],

    complex_enums: [
        ArithmeticOperand,
        AttributeValue,
        BinaryOperand,
        ComparisonOperand,
        ExternParameterType,
        GateSpecification,
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

    funcs: [
        unpack_classical_reg,
    ],

    // post_init: post_init,
}

pub(crate) fn post_init(m: &Bound<'_, PyModule>) -> PyResult<()> {
    use crate::quilpy::union;

    let py = m.py();

    m.add_singleton::<HaltType>()?;
    m.add_singleton::<NopType>()?;
    m.add_singleton::<WaitType>()?;

    // Add TypeAliases for use in annotations.
    m.add("LabelTargetParameter", union!(py, PyString, Target, Label)?)?;
    m.add(
        "QubitDesignator",
        union!(py, Qubit, QubitPlaceholder, PyInt, PyString)?,
    )?;

    m.add(
        "MemoryReferenceDesignator",
        union!(py, MemoryReference, DeclarationAt, Declaration, PyTuple)?,
    )?;

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

/// Add an `out` implementation to a `#[pyclass]` to use the type's `Quil` implementation.
///
/// The method reports that it is deprecated and recommends using `to_quil` instead.
/// It is added to maintain backwards compatibility with `QuilAtom` types.
macro_rules! impl_out {
    ($($name: ty),* $(,)?) => {
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

/// Implement [IntoPyObject] for a `pyclass` that subclasses [PyInstruction].
///
/// Implement `From<$T> for PyClassInitializer<$T>`
/// by returning a [`PyClassInitializer`] that subclasses [`PyInstruction`].
macro_rules! py_instruction {
    ($T:ty) => {
        impl From<$T> for PyClassInitializer<$T> {
            fn from(value: $T) -> Self {
                PyClassInitializer::from(PyInstruction).add_subclass(value)
            }
        }
    };
}

/// Trait for types that can be converted into an `Instruction`.
trait ToInstruction {
    fn to_instruction(&self) -> Instruction;
}

/// Convert bound python objects into their corresponding `Instruction` variant.
impl<'py, T> From<Bound<'py, T>> for Instruction
where
    T: ToInstruction + PyClass,
{
    fn from(value: Bound<'py, T>) -> Self {
        value.borrow().to_instruction()
    }
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
///
/// Types that are also variants of the [`Instruction`] enum should additionally
/// include an `instruction` item in their sublist:
///
/// - `instruction` if the variant name matches the type name and holds an inner value
/// - `instruction(Name)` if the variant name (`Name`) differs from the type name
/// - `instruction(Name, Empty)` if the variant has no inner value (e.g., `Halt`, `Nop`, `Wait`)
///
/// This generates the `ToInstruction` implementation for the type, as well as the
/// combined `IntoPyObject` and `FromPyObject` implementations for `Instruction` itself.
macro_rules! impl_instruction {
    // Initial capture: this lets us grab all the names in one go,
    // which we can then use to generate parts of the module initializer,
    // as well as the combined `IntoPyObject`/`FromPyObject` implementations for
    // `Instruction` itself. After we generate that, the entire input is passed on
    // to the @list rule, which will chew through the tokens recursively.
    ([$( $name:ident $([$($args: tt)*])? ),* ,]) => {
        impl_instruction!(@list [$($name $([$($args)*])? ,)*]);
        impl_instruction!(@collect [$($name $([$($args)*])? ,)*] []);
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

    (@one $name: ident [+ out $($tail: tt)*]) => {
        impl_out!($name);
        impl_instruction!(@one $name [$($tail)*]);
    };

    // Implement the `Instruction` variant conversions for a type whose variant name
    // (given in parens) differs from the type name.
    (@one $name: ident [+ instruction($variant: ident, Empty) $($tail: tt)*]) => {
        impl_instruction!(@instr_one $name [variant=$variant, Empty]);
        impl_instruction!(@one $name [$($tail)*]);
    };

    (@one $name: ident [+ instruction($variant: ident) $($tail: tt)*]) => {
        impl_instruction!(@instr_one $name [variant=$variant]);
        impl_instruction!(@one $name [$($tail)*]);
    };

    // Implement the `Instruction` variant conversions for a type,
    // assuming the variant name matches the type name and holds an inner value.
    (@one $name: ident [+ instruction $($tail: tt)*]) => {
        impl_instruction!(@instr_one $name [variant=$name]);
        impl_instruction!(@one $name [$($tail)*]);
    };

    // The `@instr_one` rules implement `ToInstruction` as `Instruction::$name(value.clone())`.

    // Don't use `Clone` if there's no inner value.
    (@instr_one $kind: ty [variant=$name: ident, Empty]) => {
        py_instruction!($kind);

        impl ToInstruction for $kind {
            fn to_instruction(&self) -> Instruction {
                //TODO(migration-guide): make this an empty variant and remove the `()`.
                Instruction::$name()
            }
        }
    };

    // Otherwise, clone the inner value.
    (@instr_one $kind: ident [variant=$name: ident]) => {
        py_instruction!($kind);

        impl ToInstruction for $kind {
            fn to_instruction(&self) -> Instruction {
                Instruction::$name(self.clone())
            }
        }

        impl<'py> IntoPyObject<'py> for $kind {
            type Target = Self;
            type Output = Bound<'py, Self::Target>;
            type Error = PyErr;

            fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
                Ok(Py::new(py, PyClassInitializer::from(self))?.into_bound(py))
            }
        }
    };

    // The `@collect` rules filter the full list down to only the types that make up
    // an `Instruction` variant (those with an `instruction` item in their sublist),
    // recording their variant name and whether they hold an inner value. Once fully
    // filtered, the result is used to generate the combined `IntoPyObject` and
    // `FromPyObject` implementations for `Instruction` itself.

    (@collect [] [$($ready: tt)*]) => {
        impl_instruction!(@finalize [$($ready)*]);
    };

    // Bare names never carry an `instruction` item, so skip them.
    (@collect [$name: ident, $($tail: tt)*] [$($ready: tt)*]) => {
        impl_instruction!(@collect [$($tail)*] [$($ready)*]);
    };

    (@collect [$name: ident [$($args: tt)*], $($tail: tt)*] [$($ready: tt)*]) => {
        impl_instruction!(@scan $name [+ $($args)*] [$($tail)*] [$($ready)*]);
    };

    // The `@scan` rules walk a single type's sublist looking for the `instruction` item,
    // ignoring any other items (`repr`, `quil`, `parse`, `out`).

    (@scan $name: ident [] [$($tail: tt)*] [$($ready: tt)*]) => {
        impl_instruction!(@collect [$($tail)*] [$($ready)*]);
    };

    (@scan $name: ident [+ repr $($rest: tt)*] [$($tail: tt)*] [$($ready: tt)*]) => {
        impl_instruction!(@scan $name [$($rest)*] [$($tail)*] [$($ready)*]);
    };

    (@scan $name: ident [+ quil $($rest: tt)*] [$($tail: tt)*] [$($ready: tt)*]) => {
        impl_instruction!(@scan $name [$($rest)*] [$($tail)*] [$($ready)*]);
    };

    (@scan $name: ident [+ parse $($rest: tt)*] [$($tail: tt)*] [$($ready: tt)*]) => {
        impl_instruction!(@scan $name [$($rest)*] [$($tail)*] [$($ready)*]);
    };

    (@scan $name: ident [+ out $($rest: tt)*] [$($tail: tt)*] [$($ready: tt)*]) => {
        impl_instruction!(@scan $name [$($rest)*] [$($tail)*] [$($ready)*]);
    };

    (@scan $name: ident [+ instruction($variant: ident, Empty) $($rest: tt)*] [$($tail: tt)*] [$($ready: tt)*]) => {
        impl_instruction!(@collect [$($tail)*] [$($ready)* $name [variant=$variant, Empty] ,]);
    };

    (@scan $name: ident [+ instruction($variant: ident) $($rest: tt)*] [$($tail: tt)*] [$($ready: tt)*]) => {
        impl_instruction!(@collect [$($tail)*] [$($ready)* $name [variant=$variant] ,]);
    };

    (@scan $name: ident [+ instruction $($rest: tt)*] [$($tail: tt)*] [$($ready: tt)*]) => {
        impl_instruction!(@collect [$($tail)*] [$($ready)* $name [variant=$name] ,]);
    };

    // Once we have the filtered list of `Instruction` variants, generate the combined
    // `FromPyObject` implementation for `Instruction`, and hand off to `@into` to build
    // the combined `IntoPyObject` implementation.
    (@finalize [$( $kind: tt [$($args: tt)*] ,)*]) => {
        impl_instruction!(@into [$( $kind [$($args)*] ,)*] [] []);

        impl<'a, 'py> pyo3::FromPyObject<'a, 'py> for Instruction {
            type Error = pyo3::PyErr;

            fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
                if false {
                    unreachable!("this makes the macro easier to write");
                }$( else if let Ok(value) = obj.cast::<$kind>() {
                    Ok(<$kind as ToInstruction>::to_instruction(&value.borrow()))
                })* else {
                    Err(CastError::new(obj, PyInstruction::classinfo_object(obj.py())))?
                }
            }
        }
    };

    // Once the left list is empty (see below for how that happens),
    // we generate the `IntoPyObject` implementation from the names in the right list.
    (@into [] [$( $name:ident, )*] [$( [$empty:ident, $empty_variant:ident], )* ]) => {
        impl<'py> IntoPyObject<'py> for Instruction {
            type Target = PyAny;
            type Output = Bound<'py, Self::Target>;
            type Error = PyErr;

           fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
                match self {
                    $(Instruction::$name(value) => value.into_bound_py_any(py),)*
                    $(Instruction::$empty_variant() => $empty.into_bound_py_any(py),)*
                }
            }
        }
    };

    // Put Empty variants into the `empty` list.
    (@into
         [ $kind:tt [variant=$name:tt, Empty], $($tail:tt)* ]
         [ $( $ready:ident, )* ]
         [ $( [ $empty:ident, $empty_variant:ident ], )* ]
     ) => {
        impl_instruction!(@into
            [ $($tail)* ]
            [ $($ready,)* ]
            [ $([$empty, $empty_variant],)* [$kind, $name], ]
            );
    };

    // Append all other `name`s to first list.
    (@into
        [ $_kind:tt [variant=$name:tt], $($tail:tt)* ]
        [ $( $ready:ident, )* ]
        [ $( [ $empty:ident, $empty_variant:ident ], )* ]
     ) => {
        impl_instruction!(@into
            [ $($tail)* ]
            [ $($ready,)* $name, ]
            [ $( [$empty, $empty_variant], )* ]
            );
    };
}

impl_instruction!([
    Arithmetic[repr + quil + instruction],
    ArithmeticOperand,
    ArithmeticOperator,
    AttributeValue,
    BinaryLogic[repr + quil + instruction],
    BinaryOperand,
    BinaryOperator,
    CalibrationDefinition[repr + quil + instruction],
    CalibrationIdentifier,
    Call[repr + quil + instruction],
    Capture[repr + quil + instruction],
    CircuitDefinition[repr + quil + instruction],
    Comparison[repr + quil + instruction],
    ComparisonOperand,
    ComparisonOperator,
    Convert[repr + quil + instruction],
    Declaration[repr + quil + instruction],
    Delay[repr + quil + instruction],
    DefGateSequence[repr],
    Exchange[repr + quil + instruction],
    ExternParameter,
    ExternParameterType,
    ExternSignature,
    Fence[repr + quil + instruction],
    FrameDefinition[repr + quil + instruction],
    FrameIdentifier[repr + quil + out],
    Gate[repr + quil + out + instruction],
    GateDefinition[repr + quil + instruction],
    GateModifier,
    GateSpecification,
    GateType,
    Include[repr + quil + instruction],
    Jump[repr + quil + instruction],
    JumpUnless[repr + quil + instruction],
    JumpWhen[repr + quil + instruction],
    Label[repr + quil + out + instruction],
    Load[repr + quil + instruction],
    MeasureCalibrationDefinition[repr + quil + instruction],
    MeasureCalibrationIdentifier,
    Measurement[repr + quil + instruction],
    MemoryReference[repr + quil + parse + out],
    Move[repr + quil + instruction],
    Offset,
    OwnedGateSignature[repr],
    PauliGate[repr],
    PauliTerm[repr],
    PauliSum[repr],
    Pragma[repr + quil + instruction],
    PragmaArgument,
    Pulse[repr + quil + instruction],
    Qubit,
    QubitPlaceholder[repr],
    RawCapture[repr + quil + instruction],
    Reset[repr + quil + instruction],
    ScalarType,
    SetFrequency[repr + quil + instruction],
    SetPhase[repr + quil + instruction],
    SetScale[repr + quil + instruction],
    Sharing[repr],
    ShiftFrequency[repr + quil + instruction],
    ShiftPhase[repr + quil + instruction],
    Store[repr + quil + instruction],
    SwapPhases[repr + quil + instruction],
    Target,
    TargetPlaceholder[repr],
    UnaryLogic[repr + quil + instruction],
    UnaryOperator,
    UnresolvedCallArgument, // Python name: CallArgument
    Vector,
    Waveform[repr],
    WaveformDefinition[repr + quil + instruction],
    WaveformInvocation[repr + quil + out],
    HaltType[instruction(Halt, Empty)],
    NopType[instruction(Nop, Empty)],
    WaitType[instruction(Wait, Empty)],
]);

/// Superclass for all [Instruction] variants in Python.
///
/// Rather than expose the complex enum directly,
/// we annotate each variant `#[pyclass(parent = PyInstruction)]`
/// and add a constructor that attaches the parent class to new instances.
///
/// Via the macros below, each variant implements `From<Bound<'_, T>>
/// for Instruction`
#[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(
    name = "Instruction",
    module = "quil._quil.instructions",
    subclass,
    from_py_object,
    frozen,
    eq,
    hash
)]
pub struct PyInstruction;

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PyInstruction {
    /// Returns true if the instruction is a Quil-T instruction.
    #[pyo3(name = "is_quil_t")]
    fn py_is_quil_t(&self) -> bool {
        // Instruction::is_quil_t(self).unwrap_or(false)
        todo!()
    }

    /// Parse an [`Instruction`] from a string.
    #[staticmethod]
    fn parse(string: &str) -> PyResult<Instruction> {
        Ok(Instruction::from_str(string)?)
    }
}

#[cfg(feature = "stubs")]
pyo3_stub_gen::impl_stub_type!(Instruction = PyInstruction);

/// A trait for types that should be used as singletons in Python.
///
/// For example, in Python there's a single `None` value of type `NoneType`.
/// You could model that in Rust as an empty struct type like so:
///
/// ```rust,ignore
/// #[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq)]
/// #[pyclass(frozen)]
/// struct NoneType;
///
/// impl PySingleton for NoneType {
///     const NAME: &'static str = "None";
///     fn get(py: Python<'_>) -> PyResult<&Bound<'_, Self>> {
///         static CELL: PyOnceLock<Py<NoneType>> = PyOnceLock::new();
///         CELL.get_or_try_init(py, || Py::new(py, NoneType))
///     }
/// }
///
/// #[pymodule]
/// fn builtins(m: &Bound<'_, PyModule>) -> PyResult<()> {
///     m.add_class::<NoneType>()?;
///     m.add(NoneType::NAME, NoneType::get(m.py())?)?;
///     Ok(())
/// }
/// ```
pub trait PySingleton: PyClass {
    /// The Python name of the singleton value that inhabits this type.
    const NAME: &'static str;

    /// Get the singleton instance of the type.
    fn get(py: Python<'_>) -> PyResult<&Bound<'_, Self>>;
}

pub trait PyModuleSingletonExt: private::Sealed {
    /// Add the class type and singleton instance of an instruction to a module.
    fn add_singleton<T: PySingleton>(&self) -> PyResult<()>;
}

impl PyModuleSingletonExt for Bound<'_, PyModule> {
    /// Add the class type and singleton instance of an instruction to a module.
    fn add_singleton<T: PySingleton>(&self) -> PyResult<()> {
        self.add_class::<T>()?;
        self.add(
            <T as PySingleton>::NAME,
            <T as PySingleton>::get(self.py())?,
        )
    }
}

// Prevent external code from implementing `PyModuleSingletonExt`.
#[doc(hidden)]
mod private {
    pub trait Sealed {}

    impl Sealed for pyo3::Bound<'_, pyo3::types::PyModule> {}
}

/// Implement [`PySingleton`] for a `#[pyclass]`.
macro_rules! py_singleton {
    ($T:ty, $name:expr, |$py:ident| $($value:tt)+) => {
        impl private::Sealed for $T {}

        impl PySingleton for $T {
            const NAME: &'static str = $name;

            /// Get the singleton instance of the type, creating it if necessary.
            fn get($py: Python<'_>) -> PyResult<&Bound<'_, Self>> {
                static CELL: PyOnceLock<Py<$T>> = PyOnceLock::new();
                CELL.get_or_try_init($py, || {
                    let value = $($value)+;
                    Py::new($py, value)
                }).map(|inst| inst.bind($py))
            }
        }

        impl<'py> IntoPyObject<'py> for $T {
            type Target = Self;
            type Output = Bound<'py, Self::Target>;
            type Error = PyErr;

            fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
                Ok(<$T as PySingleton>::get(py)?.to_owned())
            }
        }

        #[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
        #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
        #[pymethods]
        impl $T {
            /// Get a reference to the singleton instance of this type.
            #[new]
            fn new(py: Python<'_>) -> PyResult<&Bound<'_, Self>> {
                Self::get(py)
            }

            /// Returns the name of the singleton instance relative its module.
            ///
            /// Enables [`pickling`][] of singleton instances.
            ///
            /// [`pickling`]: https://docs.python.org/3/library/pickle.html#object.__reduce__
            fn __reduce__<'py>(&self, py: Python<'py>) -> &Bound<'py, PyString> {
                ::pyo3::intern!(py, <$T as PySingleton>::NAME)
            }
        }
    };
}

/// Create a Python singleton instance for an instruction type that has no inner value.
///
/// This macro handles the setup for the Python equivalents of
/// [Instruction::Halt], [Instruction::Nop], and [Instruction::Wait].
macro_rules! py_instruction_singleton {
    ($T:ident, $name:literal) => {
        py_singleton!($T, $name, |py| $T);

        // Add the constant value to the stubs.
        #[cfg(feature = "stubs")]
        pyo3_stub_gen::module_variable!("quil._quil.instructions", $name, $T);
    };
}

/// The type of the `Halt` [`Instruction`].
#[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(module = "quil._quil.instructions",
    extends = PyInstruction, from_py_object, frozen, eq, hash
)]
pub(crate) struct HaltType;

/// The type of the `Nop` [`Instruction`].
#[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(module = "quil._quil.instructions",
    extends = PyInstruction, from_py_object, frozen, eq, hash
)]
pub(crate) struct NopType;

/// The type of the `Wait` [`Instruction`].
#[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(module = "quil._quil.instructions",
    extends = PyInstruction, from_py_object, frozen, eq, hash
)]
pub(crate) struct WaitType;

py_instruction_singleton!(HaltType, "Halt");
py_instruction_singleton!(NopType, "Nop");
py_instruction_singleton!(WaitType, "Wait");

/// A wrapper around an [`Instruction`] for use in Python-exposed functions and methods
/// where we want to accept any `Instruction` variant.
///
/// This type correctly reports its type to the stub generator.
#[derive(FromPyObject)]
pub(crate) struct AnyInstruction(pub Instruction);

#[cfg(feature = "stubs")]
impl pyo3_stub_gen::PyStubType for AnyInstruction {
    fn type_output() -> pyo3_stub_gen::TypeInfo {
        pyo3_stub_gen::TypeInfo::with_module("_quil.instructions.Instruction", "quil._quil".into())
    }
}

impl From<AnyInstruction> for Instruction {
    fn from(value: AnyInstruction) -> Self {
        value.0
    }
}

// The following types implement `__getnewargs__` manually because,
// as (complex-)enums, they get their `__new__` methods from PyO3 directly,
// so we can't wrap them in the `pickleable_new!` macro.
// In any case, this lets us correctly set the type stubs' return types,
// which would otherwise require either creating our own derive macro,
// or using `paste!` (as is done in the macro version for `Instruction`).

struct ArithmeticOperandLike(ArithmeticOperand);
#[cfg(feature = "stubs")]
impl_stub_type!(
    ArithmeticOperandLike =
        ArithmeticOperand | i64 | f64 | MemoryReference | DeclarationAt | Declaration
);

impl<'a, 'py> FromPyObject<'a, 'py> for ArithmeticOperandLike {
    type Error = PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        if let Ok(val) = obj.cast::<ArithmeticOperand>() {
            Ok(Self(val.get().clone()))
        } else if let Ok(val) = obj.cast::<pyo3::types::PyInt>() {
            Ok(Self(ArithmeticOperand::LiteralInteger(val.extract()?)))
        } else if let Ok(val) = obj.cast::<pyo3::types::PyFloat>() {
            Ok(Self(ArithmeticOperand::LiteralReal(val.extract()?)))
        } else if let Ok(val) = obj.cast::<DeclarationAt>() {
            Ok(Self(ArithmeticOperand::MemoryReference(
                val.borrow().memref(obj.py()),
            )))
        } else if let Ok(val) = obj.cast::<MemoryReference>() {
            Ok(Self(ArithmeticOperand::MemoryReference(
                val.borrow().clone(),
            )))
        } else if let Ok(val) = obj.cast::<Declaration>() {
            Ok(Self(ArithmeticOperand::MemoryReference(
                val.get().to_memory_reference(0),
            )))
        } else {
            Err(CastError::new(
                obj,
                ArithmeticOperand::classinfo_object(obj.py()),
            ))?
        }
    }
}

impl From<ArithmeticOperandLike> for ArithmeticOperand {
    fn from(value: ArithmeticOperandLike) -> Self {
        value.0
    }
}

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
        #[pyo3(signature = (name, parameters = Vec::new(), qubits = Vec::new(), modifiers = Vec::new()))]
        #[pyo3(text_signature = "(name, parameters = [], qubits = [], modifiers = [])")]
        fn __new__(
            name: String,
            #[pyo3(from_py_with = from_sequence::<ExpressionLike, _>)]
            parameters: Vec<Expression>,
            qubits: Vec<Qubit>,
            modifiers: Vec<GateModifier>,
        ) -> Result<CalibrationIdentifier, IdentifierValidationError> {
            // Note that  the parameter order is different for the Python version :(
            Self::new(name, modifiers, parameters, qubits)
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Declaration {
    // TODO(migration-guide): The `__new__` method here is adjusted to match PyQuil v4's `Declare` constructor.
    #[new]
    #[pyo3(signature = (name, memory_type, memory_size = 1, shared_region = None, offsets = None))]
    fn __new__(
        name: String,
        memory_type: ScalarTypeLike,
        memory_size: u64,
        shared_region: Option<String>,
        offsets: Option<Vec<(u64, ScalarTypeLike)>>,
    ) -> Self {
        Self {
            name,
            size: Vector::new(memory_type.0, memory_size),
            sharing: shared_region.map(|name| Sharing {
                name,
                offsets: offsets
                    .unwrap_or_default()
                    .into_iter()
                    .map(|(offset, data_type)| Offset::new(offset, data_type.0))
                    .collect(),
            }),
        }
    }

    #[allow(clippy::type_complexity)]
    fn __getnewargs__(
        &self,
    ) -> (
        String,
        ScalarType,
        u64,
        Option<String>,
        Option<Vec<(u64, ScalarType)>>,
    ) {
        let (shared_region, offsets) = match &self.sharing {
            None => (None, None),
            Some(s) => (
                Some(s.name.clone()),
                Some(
                    s.offsets
                        .iter()
                        .map(|o| (o.offset(), o.data_type()))
                        .collect::<Vec<_>>(),
                ),
            ),
        };

        (
            self.name.clone(),
            self.size.data_type,
            self.size.length,
            shared_region,
            offsets,
        )
    }

    /// Return a new `DeclarationAt` to this `Declaration` at the given `index`.
    ///
    /// The result can be used in places where a `MemoryReference` is expected,
    /// but has the advantage of sharing the underlying `Declaration` object;
    /// that allows it to use less memory and to validate certain memory operations
    /// while constructing a `quil.program.Program`.
    fn __getitem__(slf: Bound<'_, Self>, index: u64) -> PyResult<DeclarationAt> {
        let length = slf.get().size.length;
        if index >= length {
            return Err(PyIndexError::new_err(format!(
                "index {index} out of bounds for declaration of size {length}",
            )));
        }

        Ok(DeclarationAt {
            declaration: slf.unbind(),
            index,
        })
    }
}

impl Declaration {
    /// Create a `MemoryReference` from `self`, copying the underlying `Declaration`'s name.
    fn to_memory_reference(&self, index: u64) -> MemoryReference {
        MemoryReference::new(self.name.clone(), index)
    }
}

/// A wrapper around a [`Declaration`] for use in places we'd normally need a `MemoryReference`.
///
/// You can get an instance of `DeclarationAt` by indexing a `Declaration`,
/// and you can then use it in places where a `MemoryReference` is expected.
/// The underlying objects share the same `Declaration` memory
/// and can provide additional validation on bounds-checking.
///
/// # Example
///
/// With use of the warlus operator, you can often write a list of instructions
/// without needing to explicitly declare `MemoryReference` objects:
///
/// ```python
/// instructions = [
///     top := Label("top"),                                        # LABEL @top
///     counter := Declaration("counter", ScalarType.INTEGER),      # DECLARE counter INTEGER
///     counter[0].move(10),                                        # MOVE counter[0] 10
///     # additional instructions...
///     counter[0] - 1,                                             # SUB counter[0] 1
///     JumpWhen(top, counter[0]),                                  # JUMP-WHEN @top counter[0]
/// ]
/// ```
#[derive(Debug)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(module = "quil._quil.instructions", frozen, skip_from_py_object)]
pub(crate) struct DeclarationAt {
    declaration: Py<Declaration>,
    index: u64,
}

impl DeclarationAt {
    /// Return a `MemoryReference` to the underlying `Declaration` at the given index.
    ///
    /// This makes a clone the `Declaration`'s name.
    fn memref<'py>(&self, py: Python<'py>) -> MemoryReference {
        self.declaration
            .bind(py)
            .get()
            .to_memory_reference(self.index)
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl DeclarationAt {
    /// Return a new `Move` instruction representing `self = value`.
    ///
    /// # Example
    ///
    /// ```python
    /// from quil.instructions import Declaration, ScalarType, Move
    ///
    /// x = Declaration("x", ScalarType.INTEGER, 3)
    /// mv = x[2].set(5)
    /// assert isinstance(mv, Move)
    /// assert mv.to_quil() == "MOVE x[2] 5"
    /// ```
    fn set<'py>(&self, py: Python<'py>, value: ArithmeticOperandLike) -> Move {
        Move::new(self.memref(py), value.into())
    }

    // Note: These are not implemented as Python dunder arithmetic methods,
    // because the point is to return an `Arithmetic` instruction.
    // If we implement them as operators, the semantics are confusing:
    // what you really want to write is something like `x[0] += 1`,
    // but you'd have to write `x[0] + 1` instead, which is not that intuitive.

    /// Return a new `Arithmetic` instruction representing `ADD self other`.
    ///
    /// # Example
    ///
    /// ```python
    /// from quil.instructions import Declaration, ScalarType
    ///
    /// x = Declaration("x", ScalarType.INTEGER, 3)
    /// arith = x[2].add(5)
    /// assert isinstance(arith, Arithmetic)
    /// assert arith.to_quil() == "ADD x[2] 5"
    /// ```
    fn add<'py>(&self, py: Python<'py>, other: ArithmeticOperandLike) -> Arithmetic {
        Arithmetic::new(ArithmeticOperator::Add, self.memref(py), other.into())
    }

    fn sub<'py>(&self, py: Python<'py>, other: ArithmeticOperandLike) -> Arithmetic {
        Arithmetic {
            operator: ArithmeticOperator::Subtract,
            destination: self.memref(py),
            source: other.into(),
        }
    }

    fn div<'py>(&self, py: Python<'py>, other: ArithmeticOperandLike) -> Arithmetic {
        Arithmetic {
            operator: ArithmeticOperator::Divide,
            destination: self.memref(py),
            source: other.into(),
        }
    }

    fn mul<'py>(&self, py: Python<'py>, other: ArithmeticOperandLike) -> Arithmetic {
        Arithmetic {
            operator: ArithmeticOperator::Multiply,
            destination: self.memref(py),
            source: other.into(),
        }
    }

    /// Return a new `Move` instruction representing `self = source`.
    #[pyo3(name = "move")]
    fn py_move<'py>(&self, py: Python<'py>, source: ArithmeticOperand) -> Move {
        Move {
            destination: self.memref(py),
            source,
        }
    }

    // Garbage collection integration. For more information, see:
    // https://pyo3.rs/v0.29.0/class/protocols.html#garbage-collector-integration
    #[gen_stub(skip)]
    fn __traverse__(&self, visit: PyVisit<'_>) -> Result<(), PyTraverseError> {
        visit.call(&self.declaration)?;
        Ok(())
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
        import typing
        import typing_extensions

        from quil._quil import expression

        class Gate:
            @typing.overload
            def __new__(
                cls,
                name: builtins.str,
                parameters: pyo3_stub_gen.RustType["Vec<ExpressionLike>"],
                qubits: pyo3_stub_gen.RustType["Vec<QubitLike>"],
                modifiers: pyo3_stub_gen.RustType["Option<Vec<GateModifierDesignator>>"] = None,
            ) -> Gate: ...

            @typing.overload
            @typing_extensions.deprecated("The `params` parameter is deprecated; use `parameters` instead.")
            def __new__(
                cls,
                name: builtins.str,
                parameters: pyo3_stub_gen.RustType["Vec<ExpressionLike>"],
                qubits: pyo3_stub_gen.RustType["Vec<QubitLike>"],
                modifiers: pyo3_stub_gen.RustType["Option<Vec<GateModifierDesignator>>"] = None,
                *,
                params: pyo3_stub_gen.RustType["Option<Vec<ExpressionLike>>"],
            ) -> typing_extensions.NoReturn: ...
        "#
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Gate {
    // TODO(migration-guide): `params` was renamed `parameters`.
    #[new]
    #[gen_stub(skip)]
    #[pyo3(signature = (name, parameters, qubits, modifiers = None, *, params = None))]
    fn __new__(
        py: Python<'_>,
        name: String,
        parameters: Vec<ExpressionLike>,
        #[pyo3(from_py_with = from_sequence::<Qubit, _>)] qubits: Vec<Qubit>,
        modifiers: Option<Vec<GateModifierDesignator>>,
        // `params` is for backwards compatibility and will raise a deprecation warning if used.
        params: Option<Vec<ExpressionLike>>,
    ) -> PyResult<Gate> {
        let parameters = deprecated_or_new!(py, new = parameters, old = params)?
            .into_iter()
            .map(|p| p.into())
            .collect();

        let modifiers = modifiers
            .unwrap_or_default()
            .into_iter()
            .map(TryInto::<GateModifier>::try_into)
            .collect::<Result<Vec<GateModifier>, PyErr>>()?;

        Ok(Self::new(&name, parameters, qubits, modifiers)?)
    }

    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        (
            self.name.clone(),
            self.parameters.clone(),
            self.qubits.clone(),
            self.modifiers.clone(),
        )
            .into_pyobject_or_pyerr(py)
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
    fn py_controlled(&self, control_qubit: Like<Qubit>) -> Self {
        self.clone().controlled(control_qubit.into_inner())
    }

    /// Return a copy of the ``Gate`` with the ``FORKED`` modifier added to it.
    ///
    /// Raises a ``GateError`` if the number of provided alternate parameters
    /// don't equal the number of existing parameters.
    #[pyo3(name = "forked")]
    fn py_forked(
        &self,
        fork_qubit: Like<Qubit>,
        alt_params: Vec<Expression>,
    ) -> Result<Self, GateError> {
        self.clone().forked(fork_qubit.into_inner(), alt_params)
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

impl_newargs!(GateSpecificationArgs = Vec<Vec<ExpressionArgs>> | Vec<i64> | PauliSum | DefGateSequence);

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl GateSpecification {
    #[gen_stub(override_return_type(
        type_repr = "builtins.tuple[builtins.list[builtins.list[expression.Expression]] | builtins.list[builtins.int] | PauliSum | DefGateSequence]",
        imports = ("quil._quil.expression")
    ))]
    fn __getnewargs__<'py>(
        &self,
        py: Python<'py>,
    ) -> PyResult<NewArgs<'py, GateSpecificationArgs>> {
        match self {
            Self::Matrix(value) => value.clone().into_new_args(py),
            Self::Permutation(value) => value.into_new_args(py),
            Self::PauliSum(value) => value.clone().into_new_args(py),
            Self::Sequence(value) => value.clone().into_new_args(py),
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

// TODO(migration-guide): PyQuil v4 had a `quilatom.FormalArgument` class,
// which corresponds to `Qubit.Variable`, which here is just backed by a `String`.
// At best, we could create an "alias" class for it, but it probably isn't worth it,
// since users need to update the namespace for `quilatom` anyway.

// TODO(migration-guide): `pyquil`'s `unpack_classical_reg` would convert to a `MemoryReference`
// from a `MemoryReference`, `(str,int)`, or `[str,int]`.
// Now, all places that require a `MemoryReference` can extract it from those types,
// as well as from a `DeclarationAt` or a `Declaration` (or its `Instruction` wrapper).

/// Get the address for a classical register.
///
/// This can be used to convert a `(str, int)` or `[str, int]` into a `MemoryReference`,
/// or to convert a `DeclarationAt` or `Declaration` into a `MemoryReference`.
#[cfg_attr(
    feature = "stubs",
    gen_stub_pyfunction(module = "quil._quil.instructions")
)]
#[pyfunction]
#[pyo3(warn(message = "use `MemoryReference(...)` directly instead", category = PyDeprecationWarning))]
fn unpack_classical_reg<'py>(obj: &Bound<'py, PyAny>) -> PyResult<MemoryReference> {
    MemoryReference::extract(obj.as_borrowed())
}

/// Extract a `MemoryReference` from a Python instance of the same type,
/// or from a `DeclarationAt` created from a `Declaration` instance.
impl<'a, 'py> FromPyObject<'a, 'py> for MemoryReference {
    type Error = PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        if let Ok(mem_ref) = obj.cast::<MemoryReference>() {
            // This is the implementation PyO3 would use and clones the existing `MemoryReference`.
            Ok(mem_ref.borrow().clone())
        } else if let Ok(decl) = obj.cast::<DeclarationAt>() {
            // Create a new `MemoryReference` from an underlying `Declaration` and index.
            Ok(decl.get().memref(obj.py()))
        } else if let Ok(decl) = obj.cast::<Declaration>() {
            // Create a new `MemoryReference` from a `Declaration` assuming an index of 0.
            Ok(decl.get().to_memory_reference(0))
        } else if let Ok(s) = obj.cast::<PyTuple>() {
            // Create a new `MemoryReference` from a tuple of `(str, int)` pair.
            let (name, index) = s.extract()?;
            Ok(MemoryReference::new(name, index))
        } else if let Ok(s) = obj.cast::<PyList>() {
            // As above, but from a list of `[str, int]` pair.
            let len = obj.len()?;
            if len != 2 {
                return Err(PyValueError::new_err(
                    "expected list of length 2, but got list of length {len}",
                ))?;
            }
            let MemoryReferencePair { name, index } = s.extract()?;
            Ok(MemoryReference::new(name, index))
        }
        /*
        else if let Ok(s) = obj.cast::<PyString>() {
            // TODO: reconsider this case, as it makes it too easy to mistakenly extract
            // particularly when included in another enum that derives `FromPyObject`
            let name = s.extract()
                .map_err(|_| CastError::new(obj, MemoryReference::classinfo_object(obj.py())))?;
            Ok(MemoryReference::new(name, 0))
        }
        */
        else {
            Err(CastError::new(
                obj,
                MemoryReference::classinfo_object(obj.py()),
            ))?
        }
    }
}

#[derive(FromPyObject)]
struct MemoryReferencePair {
    #[pyo3(item(0))]
    name: String,
    #[pyo3(item(1))]
    index: u64,
}

/// Used to create `Label`s from existing `Target`s as well as Python `str` instances.
///
/// Since a `Label`'s `target` can be either a `Target::Fixed` or a `Target::Placeholder`,
/// and we implement `FromPyObject` for `Target` to accept either a `str` or a `Target`,
/// we need a way to distinguish between the two cases when constructing a `Label` from Python.
///
/// See the documentation on `Label.__new__` for more information.
enum LabelTargetLike<'a> {
    Str(String),
    Existing(&'a Target),
}

impl<'a, 'py> FromPyObject<'a, 'py> for LabelTargetLike<'a> {
    type Error = pyo3::PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        if let Ok(value) = obj.cast::<PyString>() {
            Ok(LabelTargetLike::Str(value.extract()?))
        } else if let Ok(value) = obj.cast::<Target>() {
            Ok(LabelTargetLike::Existing(value.get()))
        } else if let Ok(value) = obj.cast::<Label>() {
            Ok(LabelTargetLike::Existing(&value.get().target))
        } else {
            Err(PyTypeError::new_err("cannot convert to Target"))
        }
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Label {
    // TODO(migration-guide):
    //   In `quil-rs`, the `Label` struct represents a Quil `LABEL` instruction,
    //   but the real meat of its behavior comes from the `Target` enum,
    //   which can be a `Fixed(String)` or a `Placeholder(TargetPlaceholder)`.
    //   A Quil `LABEL` is `Label { target: Target::Fixed("jump-target") }`,
    //   and hence `Label` is essentially just a wrapper around a `Target`.
    //   The `Placeholder` is for the benefit of `Program` construction,
    //   particularly loops.
    //
    //   The old PyQuil v4 had `quilatom.Label` and `quilatom.LabelPlaceholder`,
    //   which each had a `target` parameter of type `quil.Target`.
    //   The former was always `Target::Fixed` while the latter was `Target::Placeholder`,
    //   meaning the class basically represented the `quil.Target` type anyway,
    //   and neither was the actual `quil.Label` class.
    //
    //   So, in v5, we get rid of those and tell users to just use the `Label` class directly,
    //   and let them construct it from an existing `Target`, or from a `str` or `None`,
    //   letting the constructor create the `Target::Fixed` or `Target::Placeholder` for them.
    //   On the offchance they want to specify the `TargetPlaceholder`'s `base_label` explicitly,
    //   they can pass a `str` and use the keyword-only `placeholder=True` argument.
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
    //   Since we repurposed the `placeholder` name to be a `bool` for the new `Label` constructor,
    //   that usage is no longer valid, and will raise a `TypeError`.

    /// Create a new `Label`.
    ///
    /// A `Label` represents a ``LABEL`` instruction, which in Quil reads as ``LABEL @target-name``.
    /// Labels are used by unconditional jump instructions, e.g. ``JUMP @target-name``,
    /// and its siblings, ``JUMP-WHEN @target-name foo[0]`` and ``JUMP-UNLESS @target-name bar[0]``.
    /// The `@target-name` part of these instructions is this class's ``target`` attribute.
    ///
    /// # Example Usage
    ///
    /// You can use a `Label` directly as the ``target`` of a ``Jump`` instruction.
    ///
    /// ```python
    /// prog = Program(
    ///     top := Label("top"),
    ///     counter_memory := Declaration("counter", ScalarType.INTEGER),
    ///     Declaration("ro", ScalarType.BIT, 2),
    ///     H(0),
    ///     Jump(top),
    /// )
    /// prog.resolve_placeholders()
    /// print(prog.to_quil())
    /// ```
    ///
    /// Use ``Program.resolve_placeholders()`` to fill in the value before outputting Quil::
    /// You can construct a `Label` with a fixed `target` using ``Label("some-name")``,
    /// and then you can reference that point using, for example, ``Jump("some-name")``.
    ///
    /// You can create a new `Label` from a particular `Target`,
    /// or you can let the constructor create the `Target` instance for you.
    /// Use ``Label("name")`` for fixed target or ``Label()`` to create a placeholder target;
    /// if you want a placeholder with a specific `base_label`,
    /// you'll need to specify ``Label("name", placeholder=True)``.
    ///
    /// To summarize:
    ///
    /// |   Simple                         |   Equivalent                                                   |
    /// | -------------------------------- | -------------------------------------------------------------- |
    /// | `Label("A")`                     | `Label(Target.Fixed("A"))`                                     |
    /// | `Label()`                        | `Label(Target.Placeholder(TargetPlaceholder(base_label="L")))` |
    /// | `Label("A", placeholder=True)`   | `Label(Target.Placeholder(TargetPlaceholder(base_label="A")))` |

    #[new]
    #[pyo3(signature = (target=None, *, placeholder=None))]
    fn __new__(target: Option<LabelTargetLike>, placeholder: Option<bool>) -> PyResult<Self> {
        let target = match (target, placeholder) {
            // Label(target=Target.Placeholder(TargetPlaceholder()), placeholder=False)
            (Some(LabelTargetLike::Existing(Target::Placeholder(_))), Some(false)) => {
                return Err(PyValueError::new_err(
                    "`target` is a `Placeholder`, so `placeholder=False` is invalid",
                ));
            }

            // Label(target=Target.Fixed(name), placeholder=True)
            (Some(LabelTargetLike::Existing(Target::Fixed(_))), Some(true)) => {
                return Err(PyValueError::new_err(
                    "`target` is `Fixed`, so `placeholder=True` is invalid",
                ));
            }

            // Label(placeholder=False)
            (None, Some(false)) => {
                return Err(PyValueError::new_err(
                    "`target` cannot be `None` if `placeholder=False`",
                ));
            }

            // Label(), Label(target=None), Label(placeholder=True), Label(placeholder=None),
            // Label(target=None, placeholder=True), Label(target=None, placeholder=None)
            (None, Some(true) | None) => {
                Target::Placeholder(TargetPlaceholder::new("L".to_string()))
            }

            // Label("prefix", placeholder=True), Label(target="prefix", placeholder=True)
            (Some(LabelTargetLike::Str(base)), Some(true)) => {
                Target::Placeholder(TargetPlaceholder::new(base))
            }

            // Label("name"), Label("name", placeholder=False), Label("name", placeholder=None)
            // Label(target="name"), Label(target="name", placeholder=False), Label(target="name", placeholder=None)
            (Some(LabelTargetLike::Str(label)), Some(false) | None) => Target::Fixed(label),

            // (The validity of the Target relative the `placeholder` parameter is checked above.)
            // Label(t), Label(t, placeholder=None), Label(target=t, placeholder=None)
            // Label(t), Label(t, placeholder=False), Label(target=t), Label(target=t, placeholder=False)
            // Label(t, placeholder=True), Label(target=t, placeholder=True)
            (Some(LabelTargetLike::Existing(target)), _) => target.clone(),
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
    /// The `declared_size` parameter is deprecated and no longer used.
    /// Previously, it was only used to pretty-print `MemoryReference`s
    /// by hiding the square brackets (``[]``) when they weren't technically necessary.
    /// If the parameter is passed or the attribute accessed,
    /// it'll issue a ``DeprecationWarning``.
    ///
    /// Note that `offset` is an older (deprecated) term for `index`.
    /// New code should use `index`, but using `offset` as a keyword argument is still accepted;
    /// if it is not `None`, it'll be used instead of `index`, regardless of how `index` is passed.
    #[new]
    #[pyo3(signature = (name, index = 0, declared_size = None, *, offset = None))]
    fn __new__(
        py: Python<'_>,
        name: String,
        index: u64,
        declared_size: Option<NonZeroU64>,
        offset: Option<u64>,
    ) -> PyResult<Self> {
        let index = deprecated_or_new!(py, new = index, old = offset)?;
        if declared_size.is_some() {
            py_deprecated!(py, c"`declared_size` is deprecated and no longer used")?;
        }

        Ok(Self { name, index })
    }

    // This is implemented manually (rather than with `pickleable_new!`)
    // because the Rust struct doesn't include `declared_size`.
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

    /// Return a new `Arithmetic` instruction
    /// representing the addition of this `MemoryReference` to the given operand.
    ///
    /// # Example
    ///
    /// ```python
    /// from quil.instructions import MemoryReference, Arithmetic
    ///
    /// mem_ref = MemoryReference("counter", 0)
    /// arith = mem_ref + 5
    /// assert isinstance(arith, Arithmetic)
    /// assert arith.to_quil() == "ADD counter[0] 5"
    /// ```
    fn __add__(&self, other: ArithmeticOperand) -> Arithmetic {
        Arithmetic {
            operator: ArithmeticOperator::Add,
            destination: self.clone(),
            source: other,
        }
    }

    fn __sub__(&self, other: ArithmeticOperand) -> Arithmetic {
        Arithmetic {
            operator: ArithmeticOperator::Subtract,
            destination: self.clone(),
            source: other,
        }
    }

    fn __truediv__(&self, other: ArithmeticOperand) -> Arithmetic {
        Arithmetic {
            operator: ArithmeticOperator::Divide,
            destination: self.clone(),
            source: other,
        }
    }

    fn __mul__(&self, other: ArithmeticOperand) -> Arithmetic {
        Arithmetic {
            operator: ArithmeticOperator::Multiply,
            destination: self.clone(),
            source: other,
        }
    }

    // -------------------------------------------------------------------------------------
    // The following are deprecated PyQuil v4 methods present for backwards compatibility.
    // -------------------------------------------------------------------------------------

    // TODO(migration-guide): `offset` was renamed `index`.
    #[getter]
    #[pyo3(warn(message = "use `index` instead", category = PyDeprecationWarning))]
    fn offset(&self) -> u64 {
        self.index
    }

    // TODO(migration-guide): `declared_size` was only used for pretty-printing,
    // and it can't be inferred from parsing, so we're dropping that implementation.
    #[gen_stub(override_return_type(type_repr = "None"))]
    #[getter]
    #[pyo3(warn(message = "`declared_size` is deprecated", category=PyDeprecationWarning))]
    fn declared_size(&self, py: Python<'_>) -> Py<PyAny> {
        py.None()
    }

    #[staticmethod]
    #[pyo3(warn(message = "use `parse` instead", category=PyDeprecationWarning))]
    fn _from_parameter_str(memory_reference_str: &str) -> PyResult<Self> {
        match <Expression as std::str::FromStr>::from_str(memory_reference_str)? {
            Expression::Address(addr) => Ok(addr),
            _ => Err(PyValueError::new_err(
                "not a valid memory reference expression",
            )),
        }
    }
}

#[derive(FromPyObject)]
struct ScalarTypeLike(ScalarType);

#[cfg(feature = "stubs")]
impl pyo3_stub_gen::PyStubType for ScalarTypeLike {
    fn type_output() -> pyo3_stub_gen::TypeInfo {
        ScalarType::type_output()
            | pyo3_stub_gen::TypeInfo::with_module(
                r#"typing.Literal["BIT", "INTEGER", "REAL", "OCTET"]"#,
                "typing".into(),
            )
    }
}

impl<'a, 'py> FromPyObject<'a, 'py> for ScalarType {
    type Error = PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        if let Ok(scalar_type) = obj.cast::<ScalarType>() {
            Ok(*scalar_type.get())
        } else if let Ok(mut type_str) = obj.extract::<String>() {
            type_str.make_ascii_uppercase();
            let ret = match type_str.as_str() {
                "BIT" => ScalarType::Bit,
                "INTEGER" => ScalarType::Integer,
                "REAL" => ScalarType::Real,
                "OCTET" => ScalarType::Octet,
                _ => {
                    return Err(PyValueError::new_err(format!(
                        "{type_str} is not a valid ScalarType"
                    )))
                }
            };

            // Compile-time check that we cover all variants.
            #[cfg(debug_assertions)]
            {
                match ret {
                    ScalarType::Bit => (),
                    ScalarType::Integer => (),
                    ScalarType::Real => (),
                    ScalarType::Octet => (),
                };
            }

            Ok(ret)
        } else {
            match obj.str() {
                Ok(s) => Err(PyTypeError::new_err(format!(
                    "{s} is not a valid ScalarType"
                ))),
                Err(_) => Err(PyTypeError::new_err("object is not a valid ScalarType")),
            }
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
    // This implements `__new__` and `__getnewargs__` manually
    // to avoid a conflict in the generated stubs due to the type's `expression` `@property`.
    // See: https://github.com/python/mypy/issues/4146
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
    fn __getnewargs__(&self) -> (Vec<(PauliGate, String)>, Expression) {
        (self.arguments.clone(), self.expression.clone())
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

py_friendly_enum!(
    for Qubit = QubitPlaceholder | u64 | String
);

impl<'a, 'py> FromPyObject<'a, 'py> for Qubit {
    type Error = PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        if let Ok(obj) = obj.cast::<Qubit>() {
            Ok(obj.get().clone())
        } else if let Ok(obj) = obj.cast::<PyInt>() {
            Ok(Qubit::Fixed(obj.extract::<u64>()?))
        } else if let Ok(obj) = obj.cast::<PyString>() {
            Ok(Qubit::Variable(obj.extract::<String>()?))
        } else if let Ok(obj) = obj.cast::<QubitPlaceholder>() {
            Ok(Qubit::Placeholder(obj.get().clone()))
        } else {
            Err(PyTypeError::new_err(
                "expected a Qubit or one of its variants",
            ))
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Qubit {
    #[gen_stub(override_return_type(type_repr = "builtins.tuple[builtins.int | builtins.str | QubitPlaceholder]", imports = ("builtins")))]
    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<NewArgs<'py, Qubit>> {
        match self {
            Self::Fixed(value) => NewArgs::new(py, *value),
            Self::Variable(value) => NewArgs::new(py, value),
            Self::Placeholder(value) => NewArgs::new(py, value.clone()),
        }
    }
}

#[cfg(feature = "stubs")]
mod stubs {
    use pyo3_stub_gen::{impl_stub_type, type_alias};

    // pyo3_stub_gen::export_verbatim!("quil.instructions", "Halt");

    #[allow(clippy::wildcard_imports)]
    use super::*;

    // TODO(migration-guide):
    // There was a `QubitDesignator` type alias = `QubitPlaceholder | int | str` in PyQuil v4,
    // but now we can explicitly type parameters to accept those (or a `Qubit` itself) instead.
    // impl_stub_type!(Like<'_, '_, Qubit> = Qubit | i64 | String | QubitPlaceholder);

    impl_stub_type!(LabelTargetLike<'_> = String | Label | Target);

    impl_stub_type!(GateModifierDesignator = GateModifier | String);

    impl_stub_type!(
        MemoryReferenceLike = MemoryReference | DeclarationAt | Declaration | (String, u64)
    );

    type_alias!(
        "quil._quil.instructions",
        LabelTargetParameter = String | Target | Label
    );
    type_alias!(
        "quil._quil.instructions",
        MemoryReferenceDesignator = MemoryReferenceLike
    );
    type_alias!(
        "quil._quil.instructions",
        QubitDesignator = Qubit | QubitPlaceholder | String | u64
    );
}

pub(crate) type QubitLike<'a, 'py> = Like<'a, 'py, Qubit>;

#[derive(FromPyObject)]
struct MemoryReferenceLike(MemoryReference);
impl From<MemoryReferenceLike> for MemoryReference {
    fn from(value: MemoryReferenceLike) -> Self {
        value.0
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

impl<'a, 'py> FromPyObject<'a, 'py> for Target {
    type Error = pyo3::PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        if let Ok(value) = obj.cast::<Target>() {
            Ok(value.get().clone())
        } else if let Ok(value) = obj.cast::<Label>() {
            Ok(value.get().target.clone())
        } else if let Ok(value) = obj.cast::<TargetPlaceholder>() {
            Ok(Target::Placeholder(value.get().clone()))
        } else if let Ok(value) = obj.cast::<PyString>() {
            Ok(Target::Fixed(value.extract()?))
        } else {
            Err(PyTypeError::new_err("cannot convert to Target"))
        }
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
            deprecated_param!(py, new = base_label, old = prefix)?;
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
    #[pyo3(warn(message = "use `base_label` instead", category = PyDeprecationWarning))]
    fn prefix(&self) -> &str {
        self.as_inner()
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
