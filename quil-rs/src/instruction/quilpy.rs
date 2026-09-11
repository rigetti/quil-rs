use std::{f64::consts::PI, mem, ops::{Mul, MulAssign}, sync::atomic::{AtomicUsize, Ordering}};

use indexmap::IndexMap;
use num_complex::Complex64;
use numpy::{PyArray2, ToPyArray};
use pyo3::{
    CastError, IntoPyObjectExt, PyTraverseError, PyTypeCheck, PyVisit, exceptions::{PyDeprecationWarning, PyIndexError, PyKeyError, PyNotImplementedError, PyTypeError, PyValueError}, prelude::*, sync::PyOnceLock, types::{IntoPyDict as _, PyDict, PyFrozenSet, PyInt, PyList, PyNotImplemented, PyString, PyTuple},
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
        from_sequence, impl_newargs, impl_to_quil, py_deprecated, py_friendly_enum, singleton,
        IntoNewArgs, Like, Migrate, NewArgs, NonZeroU64,
    },
    validation::identifier::IdentifierValidationError,
};

use singleton::{py_singleton, PyModuleSingletonExt};

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

        PauliTermIter,
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
    m.add("PauliTargetDesignator", union!(py, Qubit, PyInt, PyString)?)?;

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

/// Add an `is_quil_t` method to the `#[pyclass]`.
macro_rules! impl_is_quil_t {
    ($name:ident, $bool:literal) => {
        #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
        #[pyo3::pymethods]
        impl $name {
            /// Returns true if the instruction is a Quil-T instruction.
            #[staticmethod]
            #[pyo3(name = "is_quil_t")]
            fn is_quil_t() -> bool {
                $bool
            }
        }
    };
}

/// Implement [IntoPyObject] and `From<$T> for PyClassInitializer<$T>`
/// for a `#[pyclass(extends = PyInstruction)]`.
macro_rules! py_instruction {
    ($T:ty) => {
        impl From<$T> for PyClassInitializer<$T> {
            fn from(value: $T) -> Self {
                PyClassInitializer::from(PyInstruction).add_subclass(value)
            }
        }

        impl<'py> IntoPyObject<'py> for $T {
            type Target = Self;
            type Output = Bound<'py, Self::Target>;
            type Error = PyErr;

            fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
                Ok(Py::new(py, PyClassInitializer::from(self))?.into_bound(py))
            }
        }
    };
}

/// Create a Python singleton instance for an instruction type that has no inner value.
///
/// This macro handles the setup for the Python equivalents of
/// [`Instruction::Halt`], [`Instruction::Nop`], and [`Instruction::Wait`].
macro_rules! py_instruction_singleton {
    ($T:ident, $name:ident) => {
        py_singleton!($T, stringify!($name), |py| {
            PyClassInitializer::from(PyInstruction).add_subclass($T)
        });

        // Add the constant value to the stubs.
        #[cfg(feature = "stubs")]
        pyo3_stub_gen::module_variable!("quil._quil.instructions", stringify!($name), $T);

        impl Quil for $T {
            fn write(
                &self,
                writer: &mut impl std::fmt::Write,
                fall_back_to_debug: bool,
            ) -> Result<(), crate::quil::ToQuilError> {
                Instruction::$name.write(writer, fall_back_to_debug)
            }
        }
    };
}

/// Implement expected methods on each of the instruction-related types, given as a list.
///
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
///     *C [quil + parse],
/// ]);
/// ```
///
/// Those prefixed with `*` should be `PyInstruction` subclasses,
/// and can optionally specify their variant name and/or if they lack an inner value
/// using `*RustTypeName(variant=InstructionVariantName, empty=true)`.
///
/// Any type can override the default implementations produced by specifying a sublist
/// of some combination of `repr`, `quil`, `parse`, and `out` separated by `+`s.
macro_rules! impl_instruction {
    // This macro has a lot of matching rules, but is conceptually very simple.
    // Given a non-empty list of types, it outputs two new macro invocations:
    // the first processes the head of the list, and the second processes the rest.
    // Of course, that second invocation is a recursive call to this macro,
    // so once the list is empty, it knows it has processed the full list.
    //
    // The rules to operate on a single item _could_ be pushed to another macro,
    // but instead they are included here to just keep everything together.
    // They are prefixed with the `@one` token and take a type name and list of args,
    // and they operate essentially the same way as the broader macro:
    // strip off and process the first arg to produce some output,
    // then output another invocation with the remaining arguments.
    //
    // The internal process to handle the list is formed of rules prefixed with `@list`.
    // In addition to kicking off the `@one` handling for each type,
    // they also need to accumulate those types that are `Instruction` variants
    // so that once the full list is processed, it can output additional methods
    // implemented on `Instruction` that need to know about all of its variants.
    // Because some of those variants have inner values and some do not,
    // the macro needs to split them into two different accumulator lists,
    // and hence the actual `@list` matching is a series of rules.
    // Nevertheless, their basic output is as described above.
    //
    // Because the `IntoPyObject` and `FromPyObject` implementations for `Instruction`
    // should only happen _once_, after all the variants are accumulated,
    // we can't easily include arguments to modify it within the `@one` rules
    // without adopting an approach that trades expansions between `@one` and `@list`;
    // doing that would work fine, but would end up with a deeply-nested expansion,
    // and we'd have to raise the recursion limit just to get it to compile.
    //
    // Instead, we use a small set of `@list` rules to match a `*`-prefix,
    // and accept additional modifiers within a separate `()`-style sublist.
    // Although it'd be nifty to token-munch those arguments too,
    // it'd be a lot more work for not much syntactic sugar,
    // so we should aim to keep those options simple and explicit.
    //
    // In other words, the args in square brackets are for the "per-type" expansions,
    // while the those in parentheses are for the "at-the-end" expansion.
    // Processing the former is done independently of the list recursion,
    // so each type expands at a depth only one (or a few) levels deeper than the last.

    // The initial capture kicks off the recursive processing with empty accumulators.
    ($tokens:tt) => {
        impl_instruction!(@list $tokens [] []);
    };

    // The terminal rule (empty input list) processes the accumulated lists,
    // though the actual implementation is below to make this easier to grok.
    (@list [] $variants:tt $empties:tt) => {
        impl_instruction!(@finalize $variants $empties);
    };

    // Types without `*` are not `Instruction` variants and aren't accumulated.
    (@list [$name:ident $([$($args:tt)*])?, $($tail:tt)*] $variants:tt $empties:tt) => {
        impl_instruction!(@one $name $([$($args)*])?);
        impl_instruction!(@list [$($tail)*] $variants $empties);
    };

    // Types prefixed with `*` are `Instruction` variants to accumulate.
    // They can use specific arguments in `()` to specify an alternative variant name
    // and whether or not the variant has an inner value.

    // This has an explicit variant name and no inner value.
    (@list [*$name:ident(variant=$variant:ident, empty=true)  $([$($args:tt)*])?, $($tail:tt)*] $variants:tt [$($empties:tt)*]) => {
        py_instruction_singleton!($name, $variant);
        impl_instruction!(@one * $name $([$($args)*])?);
        impl_instruction!(@list [$($tail)*] $variants [$($empties)* [$name, $variant],]);
    };

    // This has an explicit variant name and has an inner value.
    (@list [*$name:ident(variant=$variant:ident $(, empty=false)?) $([$($args:tt)*])?, $($tail:tt)*] [$($variants:tt)*] $empties:tt) => {
        py_instruction!($name);
        impl_instruction!(@one * $name $([$($args)*])?);
        impl_instruction!(@list [$($tail)*] [$($variants)* [$name, $variant],] $empties);
    };

    // This assumes the variant name matches the type name and expands to one of the above.
    (@list [*$name:ident$((empty=$is_empty:tt))? $([$($args:tt)*])?, $($tail:tt)*] $variants:tt $empties:tt) => {
        impl_instruction!(@list [*$name(variant=$name $(,empty=$is_empty)?) $([$($args)*])?, $($tail)*] $variants $empties);
    };

    // The `@one` rules walk a single type's sublist of arguments one at a time.

    // Default rules when no args are given;
    // `PyInstructions` get a different default than other types.
    // These rules additionally expand `+ ..` to `repr + quil`.
    (@one * $name:ident) => {
        impl_instruction!(@one $name [+ repr + quil + is_quil_t(false)]);
    };
    (@one $name:ident $([+ ..])?) => {
        impl_instruction!(@one $name [+ repr + quil]);
    };

    // Strip the instruction indicator prefix if explicit args were given.
    (@one * $name:ident $args:tt) => {
        impl_instruction!(@one $name $args);
    };

    // Terminal rule when done processing arguments.
    (@one $name:ident []) => {};

    (@one $name:ident [+repr $($rest:tt)*]) => {
        impl_repr!($name);
        impl_instruction!(@one $name [$($rest)*]);
    };

    (@one $name:ident [+quil $($rest:tt)*]) => {
        impl_to_quil!($name);
        impl_instruction!(@one $name [$($rest)*]);
    };

    (@one $name:ident [+parse $($rest:tt)*]) => {
        impl_parse!($name);
        impl_instruction!(@one $name [$($rest)*]);
    };

    (@one $name:ident [+is_quil_t($bool:literal) $($rest:tt)*]) => {
        impl_is_quil_t!($name, $bool);
        impl_instruction!(@one $name [$($rest)*]);
    };

    (@one $name:ident [+out $($rest:tt)*]) => {
        impl_out!($name);
        impl_instruction!(@one $name [$($rest)*]);
    };

    (@one $name:ident [+ $unknown:tt $($rest:tt)*]) => {
        compile_error!(concat!(
            "Unknown impl argument `",
            stringify!($unknown),
            "` when processing type `",
            stringify!($name),
            "`"
        ));
    };

    // Add `+` to the front of the list if it was omitted.
    // This has to be the last `@one` rule to avoid infinite recursion.
    (@one $name:ident [$($rest:tt)*]) => {
        impl_instruction!(@one $name [+ $($rest)*]);
    };

    // Expand the collected accumulators into `Instruction` methods.
    (@finalize
        [$([$instr:ident, $instr_variant:ident],)*]
        [$([$empty:ident, $empty_variant:ident],)*]
    ) => {
        impl<'py> IntoPyObject<'py> for Instruction {
            type Target = PyAny;
            type Output = Bound<'py, Self::Target>;
            type Error = PyErr;

            fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
                match self {
                    $(Instruction::$instr_variant(value) => value.into_bound_py_any(py),)*
                    $(Instruction::$empty_variant => $empty.into_bound_py_any(py),)*
                }
            }
        }

        impl<'a, 'py> pyo3::FromPyObject<'a, 'py> for Instruction {
            type Error = pyo3::PyErr;

            fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
                if false {
                    unreachable!("this makes the macro easier to write");
                }$( else if let Ok(value) = obj.cast::<$instr>() {
                    Ok(Instruction::$instr_variant(value.extract()?))
                })* $( else if let Ok(_) = obj.cast::<$empty>() {
                    Ok(Instruction::$empty_variant)
                })* else {
                    Err(CastError::new(obj, PyInstruction::classinfo_object(obj.py())))?
                }
            }
        }
    };
}

impl_instruction!([
    *Arithmetic,
    ArithmeticOperand,
    ArithmeticOperator,
    AttributeValue,
    *BinaryLogic,
    BinaryOperand,
    BinaryOperator,
    *CalibrationDefinition[is_quil_t(true) + ..],
    CalibrationIdentifier,
    *Call,
    *Capture[is_quil_t(true) + ..],
    *CircuitDefinition,
    *Comparison,
    ComparisonOperand,
    ComparisonOperator,
    *Convert,
    *Declaration,
    *Delay[is_quil_t(true) + ..],
    DefGateSequence[repr],
    *Exchange,
    ExternParameter,
    ExternParameterType,
    ExternSignature,
    *Fence[is_quil_t(true) + ..],
    *FrameDefinition[is_quil_t(true) + ..],
    FrameIdentifier[out + ..],
    *Gate[is_quil_t(false) + out + ..],
    *GateDefinition,
    GateModifier,
    GateSpecification,
    GateType,
    *Include,
    *Jump,
    *JumpUnless,
    *JumpWhen,
    *Label[is_quil_t(false) + out + ..],
    *Load,
    *MeasureCalibrationDefinition[is_quil_t(true) + ..],
    MeasureCalibrationIdentifier,
    *Measurement,
    MemoryReference[parse + out + ..],
    *Move,
    Offset,
    OwnedGateSignature[repr],
    PauliGate[repr],
    PauliTerm[repr],
    PauliSum[repr],
    *Pragma,
    PragmaArgument,
    *Pulse[is_quil_t(true) + ..],
    Qubit,
    QubitPlaceholder[repr],
    *RawCapture[is_quil_t(true) + ..],
    *Reset,
    ScalarType,
    *SetFrequency[is_quil_t(true) + ..],
    *SetPhase[is_quil_t(true) + ..],
    *SetScale[is_quil_t(true) + ..],
    Sharing[repr],
    *ShiftFrequency[is_quil_t(true) + ..],
    *ShiftPhase,
    *Store,
    *SwapPhases[is_quil_t(true) + ..],
    Target,
    TargetPlaceholder[repr],
    *UnaryLogic,
    UnaryOperator,
    UnresolvedCallArgument, // Python name: CallArgument
    Vector,
    Waveform[repr],
    *WaveformDefinition[is_quil_t(true) + ..],
    WaveformInvocation[out + ..],
    *HaltType(variant = Halt, empty = true),
    *NopType(variant = Nop, empty = true),
    *WaitType(variant = Wait, empty = true),
]);

/// Superclass for all [`Instruction`] variants in Python.
///
/// The subclasses of this class are the various Quil instructions types.
///
/// ```python
/// >>> from quil.instructions import Instruction, Gate, Qubit
/// >>> g = Gate("X", (), (Qubit.Fixed(0),), ())
/// >>> isinstance(g, Gate)
/// True
/// >>> isinstance(g, Instruction)
/// True
/// ```
///
/// You can check for different instruction variants and destructure them using `match`:
///
/// ```python
/// match x:
///     case Gate():
///         print("A gate instruction!")
///     case Wait | Nop | Halt:
///         print("A singleton instruction!")
///     case Instruction():
///         print("Some other instruction!")
///     case _:
///         print("Not an instruction!")
/// ```
///
// Rust Developer Notes:
//
// Rather than expose the `Instruction` complex enum directly,
// we annotate each variant's inner type as `#[pyclass(parent = PyInstruction)]`
// and add a constructor that attaches the parent class to new instances.
// For the unit variants, we create a new type and expose them as singleton instances.
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
    /// Parse an [`Instruction`] from a string.
    #[staticmethod]
    fn parse(string: &str) -> PyResult<Instruction> {
        Ok(Instruction::from_str(string)?)
    }
}

#[cfg(feature = "stubs")]
pyo3_stub_gen::impl_stub_type!(Instruction = PyInstruction);

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
    #[new]
    fn __new__(input: &str) -> Result<Self, ParseInstructionError> {
        Self::parse(input)
    }

    /// Parse a ``PauliGate`` from a string.
    ///
    /// Raises a ``ParseExpressionError`` error if the string isn't a valid Quil expression.
    #[staticmethod]
    fn parse(input: &str) -> Result<Self, ParseInstructionError> {
        <Self as std::str::FromStr>::from_str(input)
            .map_err(|err| ParseInstructionError::Parse(err.to_string()))
    }
}

impl PauliGate {
    /// Return the product and a complex phase result of multiplying two gates.
    fn product(self, other: PauliGate) -> (PauliGate, Complex64) {
        match (self, other) {
            (PauliGate::I, g) | (g, PauliGate::I) => (g, Complex64::new(1.0, 0.0)),
            (PauliGate::X, PauliGate::X) | (PauliGate::Y, PauliGate::Y) | (PauliGate::Z, PauliGate::Z) => (PauliGate::I, Complex64::new(1.0, 0.0)),
            (PauliGate::X, PauliGate::Y) => (PauliGate::Z, Complex64::new(0.0, 1.0)),
            (PauliGate::X, PauliGate::Z) => (PauliGate::Y, Complex64::new(0.0, -1.0)),
            (PauliGate::Y, PauliGate::X) => (PauliGate::Z, Complex64::new(0.0, -1.0)),
            (PauliGate::Y, PauliGate::Z) => (PauliGate::X, Complex64::new(0.0, 1.0)),
            (PauliGate::Z, PauliGate::X) => (PauliGate::Y, Complex64::new(0.0, 1.0)),
            (PauliGate::Z, PauliGate::Y) => (PauliGate::X, Complex64::new(0.0, -1.0)),
        }
    }
}

impl<'a, 'py> FromPyObject<'a, 'py> for PauliGate {
    type Error = PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        if let Ok(gate) = obj.cast::<PauliGate>() {
            Ok(*gate.get())
        } else if let Ok(s) = obj.extract::<String>() {
            Ok(PauliGate::parse(&s)?)
        } else {
            Err(CastError::new(obj, PauliGate::classinfo_object(obj.py())))?
        }
    }
}

/// Argument type when constructing a `PauliTerm` from Python.
///
/// Technically, we only accept `Qubit::Variable` and `Qubit::Fixed` (or `str` and `int`),
/// but this wraps `Qubit` and raises a type error if the input is `Qubit::Placeholder`.
#[derive(FromPyObject)]
struct PauliArg(Qubit);

impl TryFrom<PauliArg> for String {
    type Error = PyErr;

    fn try_from(value: PauliArg) -> Result<Self, Self::Error> {
        match value.0 {
            Qubit::Variable(v) => Ok(v),
            Qubit::Fixed(v) => Ok(format!("q{v}")),
            Qubit::Placeholder(_) => Err(PyTypeError::new_err(
                "cannot use Qubit::Placeholder as a PauliTerm argument",
            )),
        }
    }
}

fn convert_pauli_targets(values: Vec<PauliArg>) -> PyResult<Vec<String>> {
    let mut result = Vec::with_capacity(values.len());
    for value in values {
        result.push(String::try_from(value)?);
    }
    Ok(result)
}

// Overloads for `PauliTerm.__new__`.
#[cfg(feature = "stubs")]
pyo3_stub_gen::inventory::submit! {
    gen_methods_from_python! {
        r#"
        import typing
        import typing_extensions

        from quil._quil.expression import ExpressionDesignator

        class PauliTerm:
            @typing.overload
            def __new__(
                cls,
                op: typing.Literal[PauliGate.I] | typing.Literal["I"],
                index: PauliTargetDesignator | None,
                coefficient: ExpressionDesignator = 1.0,
            ) -> PauliTerm:
                """Construct a `PauliTerm` for a single Identity operator."""

            @typing.overload
            def __new__(
                cls,
                op: PauliGate | str,
                index: PauliTargetDesignator | None,
                coefficient: ExpressionDesignator = 1.0,
            ) -> PauliTerm:
                """Construct a `PauliTerm` for a single operator and argument."""

            @typing.overload
            @typing_extensions.deprecated("this constructor is deprecated; use `PauliTerm.from_list` instead")
            def __new__(
                cls,
                arguments: collections.abc.Sequence[tuple[PauliGate | str, PauliTargetDesignator]],
                expression: ExpressionDesignator = 1.0,
            ) -> PauliTerm:
                """Construct a `PauliTerm` from a sequence of arguments."""

            @typing.overload
            def __mul__(self, other: PauliTerm | ExpressionDesignator) -> PauliTerm: ...
            @typing.overload
            def __mul__(self, other: PauliSum) -> PauliSum: ...
        "#
    }
}

const ONE: Expression = Expression::Number(Complex64::new(1.0, 0.0));

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PauliTerm {
    // TODO(migration-guide):
    // - Rust users making use of the `python` feature need to update usage of `__new__`.
    // - Python users of `quil` should be aware of the (temporary) combined API.
    // - In constrast to the behavior of earlier versions of `quil-rs`,
    //   the current version may rearrange and/or simplify arguments here and in `PauliSum`s.
    //   The result is always logically equivalent, but may lead to different Quil output,
    //   and consequently, different compilation and hence observable differences.
    //   Given that a PauliSum is already a pretty high-level abstraction of a gate,
    //   users should expect that compiler optimizations and physical realizations
    //   will impact the observable behavior of a program, in any case.
    // - The PyQuil v4 methods accepted `Qubit::Placeholder` instances
    //   and just converted them into a `str`, leading to invalid Quil identifiers.
    //   Since that was a bug, `Placeholder`s now explicitly raise an error if used.
    // - Relatedly, the PyQuil v4 methods accepted integers and `Qubit::Fixed` instances
    //   and converted them into `str`s directly (e.g., `Qubit::Fixed(0)` became `"0"`),
    //   which again are not a valid Quil identifiers.
    //   This now prefixes them with a `q` to make them valid identifiers,
    //   but that could cause conflicts if a user gave a mix of strings and integers.
    //   Since that's a pretty unlikely thing to see in the wild,
    //   we just document that it's their responsibility to avoid such conflicts.
    //   And as always, nothing prevents them from giving an invalid identifier
    //   directly as a string/`Qubit::Variable`.
    /// Construct a new `PauliTerm` from a single operator and qubit index.
    ///
    /// To construct a `PauliTerm`, provide a `PauliGate` operator and an argument string.
    /// As a special case, if `op` is the identity operator, the argument may be `None`.
    /// Additionally, the argument parameter can be derived automatically
    /// from a non-placeholder `Qubit` instance or from a non-negative integer;
    /// in the latter case, the argument will be formatted as ``"q{index}"``
    /// to generate a valid Quil argument string.
    /// Optionally, you can provide a `coefficient`,
    /// either directly as an `Expression` or as a numeric literal.
    ///
    /// ```python
    /// from quil.instructions import PauliTerm, PauliGate
    /// term = PauliTerm("X", 0, 1.5) * PauliTerm(PauliGate.Y, "q") * PauliTerm("I", None)
    /// ```
    ///
    /// At present and for compatibility purposes,
    /// you can construct a `PauliTerm` using a sequence of `(operator, qubit)` pairs,
    /// but doing so is deprecated in favor of the `PauliTerm.from_list` static method.
    ///
    /// ```python
    /// # This will raise a deprecation warning in PyQuil v5.
    /// term = PauliTerm([(PauliGate.X, 0), (PauliGate.Y, "q"), ("I", None)], 1.5)
    /// # Prefer this form:
    /// term = PauliTerm.from_list([(PauliGate.X, 0), (PauliGate.Y, "q"), ("I", None)], 1.5)
    /// ```
    ///
    /// Note that to be valid `quil`, the  `coefficient` must be real-valued
    /// and only reference real numeric literals or parameters.
    // Developer note:
    // The stubs for the documented constructors are added manually above.
    // The reason for two constructors here is backwards compatibility:
    // PyQuil v4 used the first form, while `quil` had used the second.
    // The second form is deprecated in favor of `PauliTerm.from_list`,
    // and at some point, we should remove it from `quil` and simplify this constructor.
    #[new]
    #[pyo3(signature = (
            op=None,
            index=None,
            coefficient=None,
            arguments=None,
            expression=None,
    ))]
    fn __new__(
        py: Python<'_>,
        op: Option<Migrate<PauliGate, Vec<(PauliGate, PauliArg)>>>,
        index: Option<Migrate<PauliArg, ExpressionLike>>,
        coefficient: Option<ExpressionLike>,
        arguments: Option<Vec<(PauliGate, PauliArg)>>,
        expression: Option<ExpressionLike>,
    ) -> PyResult<Self> {
        match (op, index, arguments, expression) {
            // Allow `index` to be `None` if `op` is `I`.
            (Some(Migrate::New(PauliGate::I)), None, None, None) => {
                let expression = coefficient.map(Into::into).unwrap_or(ONE);
                Ok(Self::new(Vec::new(), expression))
            }

            // Otherwise, given an `op`, we require an `index`.
            (Some(Migrate::New(op)), Some(Migrate::New(index)), None, None) => {
                let expression = coefficient.map(Into::into).unwrap_or(ONE);
                Ok(Self::new(vec![(op, index.try_into()?)], expression))
            }

            // Second constructor, account for positional vs keyword arguments.
            (
                // all positional
                Some(Migrate::Old(arguments)),
                Some(Migrate::Old(expression)),
                None,
                None,
            )
            | (
                // positional arguments, keyword expression
                Some(Migrate::Old(arguments)),
                None,
                None,
                Some(expression),
            )
            | (
                // all keywords
                None,
                None,
                Some(arguments),
                Some(expression),
            ) => {
                py_deprecated!(
                    py,
                    c"PauliTerm constructor with a sequence of arguments is deprecated; use `PauliTerm.from_list` instead"
                )?;
                Self::from_list(arguments, expression)
            }

            // Second constructor, with default expression of 1.0.
            (Some(Migrate::Old(arguments)), None, None, None)
            | (None, None, Some(arguments), None) => {
                py_deprecated!(
                    py,
                    c"PauliTerm constructor with a sequence of arguments is deprecated; use `PauliTerm.from_list` instead"
                )?;
                Self::from_list(arguments, ExpressionLike::Expression(ONE))
            }

            (Some(Migrate::New(_)), None, None, None) => Err(PyValueError::new_err(
                "PauliTerm with non-identity operator must have an `index` qubit",
            )),

            _ => Err(PyValueError::new_err(
                "invalid combination of arguments for PauliTerm constructor",
            )),
        }
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

    /// Construct a new `PauliTerm` from a list of operators and an optional coefficient.
    #[pyo3(signature = (terms_list, coefficient=ExpressionLike::Expression(ONE)))]
    #[staticmethod]
    fn from_list(terms_list: Vec<(PauliGate, PauliArg)>, coefficient: ExpressionLike) -> PyResult<Self> {
        let arguments = terms_list
            .into_iter()
            .filter_map(|(gate, qubit)| {
                // Drop identity operators.
                match gate {
                    PauliGate::I => None,
                    _ => Some(qubit.try_into().map(|qubit_str| (gate, qubit_str))),
                }
            })
            .collect::<Result<_, _>>()?;
        Ok(Self::new(arguments, coefficient.into()))
    }

    /// Length of the PauliTerm is the number of Pauli operators in the term.
    ///
    /// A term that consists of only a scalar has a length of zero.
    fn __len__(&self) -> usize {
        self.arguments.len()
    }

    /// Create a new copy of this [`PauliTerm`].
    #[pyo3(warn(
        message = "`copy` is deprecated; use `copy.copy(term)` instead.",
        category = PyDeprecationWarning
    ))]
    fn copy(&self) -> Self {
        self.clone()
    }

    /// Create [`Program`] from the [`PauliTerm`].
    #[getter]
    fn program(&self) -> PyResult<Program> {
        let mut program = Program::new();
        for (op, qubit) in &self.arguments {
            let g = Gate::new(
                op.to_string(),
                vec![],
                vec![Qubit::Variable(qubit.clone())],
                vec![]
            )?;
            program.add_instruction(Instruction::Gate(g));
        }

        Ok(program)
    }

    /// Get the arguments of the [`PauliTerm`] as [`Qubit`]s.
    fn get_qubits(&self) -> Vec<Qubit> {
        self.arguments.iter().map(|(_, q)| Qubit::Variable(q.clone())).collect()
    }

    /// Get the [`PauliGate`] matching the argument in the [`PauliTerm`],
    /// or [`PauliGate::I`] if the argument is not present in the term.
    fn __getitem__(&self, argument: &str) -> PauliGate {
        self.arguments.iter().find_map(|(gate, qubit)| {
            if qubit == argument {
                Some(*gate)
            } else {
                None
            }
        }).unwrap_or(PauliGate::I)
    }

    /// Iterate over the arguments in this [`PauliTerm`].
    fn __iter__(slf: Bound<'_, Self>) -> PauliTermIter {
        PauliTermIter::new(slf.unbind())
    }

    /// Return the product of this [`PauliTerm`] with another `PauliTerm`,
    /// [`PauliSum`], or number according to the Pauli algebra rules.
    fn __mul__<'py>(&self, py: Python<'py>, other: Bound<'py, PyAny>) -> PyResult<Bound<'py, PyAny>> {
        if let Ok(other) = other.cast::<PauliTerm>() {
            let result = self.clone().multiply_term(other.get());
            result.into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<PauliSum>() {
            todo!()
        } else if let Ok(other) = other.cast::<Expression>() {
            let expression = expr_prod_simple(
                &self.expression, other.get(), Complex64::ONE
            );
            PauliTerm {
                arguments: self.arguments.clone(),
                expression,
            }.into_bound_py_any(py)
        } else if let Ok(other) = other.extract::<Complex64>() {
            let expression = expr_prod_simple(
                &self.expression, &Expression::Number(other), Complex64::ONE
            );

            PauliTerm {
                arguments: self.arguments.clone(),
                expression,
            }.into_bound_py_any(py)
        } else {
            py.NotImplemented().into_bound_py_any(py)
        }
    }

    fn __rmul__<'py>(&self, py: Python<'py>, other: Bound<'py, PyAny>) -> PyResult<Bound<'py, PyAny>> {
        self.__mul__(py, other)
    }

    fn __pow__(&self, exponent: u32, modulo: Option<Bound<'_, PyAny>>) -> PyResult<Self> {
        if modulo.is_some() {
            return Err(PyNotImplementedError::new_err(
                "`modulo` is not supported for `PauliTerm.__pow__`",
            ));
        }

        if self.arguments.is_empty() {
            return Ok(Self::new(Vec::new(), ONE.clone()));
        }

        let args = if exponent.is_multiple_of(2) {
            Vec::new()
        } else {
            self.arguments.clone()
        };

        let expr =  if self.expression == ONE || exponent == 0 {
            ONE.clone()
        } else {
            self.expression.clone() ^ Expression::Number((exponent as f64).into())
        };

        Ok(Self::new(args, expr))
    }

    // TODO: This produces ambiguous strings if any argument contains X, Y, or Z,
    // so we should either deprecated this, validate the identifiers and warn/error,
    // or change the output format to assign arbitrary numeric identifiers to arguments
    // (which most closely matches the usage of the original PyQuil v4 implementation).
    // Likely, getting rid of it is the best option,
    // particularly since naming a function `id` is pretty confusing in Python.
    /// Return an identifier string for the PauliTerm (ignoring the coefficient).
    ///
    /// For example, ``PauliTerm.from_list([("X", 0), ("Y", "q")]).id() == "Xq0Yq"``.
    ///
    /// Don't use this to compare terms (use ``pt0 == pt1`` or ``hash(pt0)`` for that).
    /// By default, this function sorts the qubits in the term,
    /// but you can pass ``sort_ops=False`` to disable sorting by qubit.
    /// This is currently ``True`` by default, but will change in a future version.
    ///
    /// Note that if the term has no operators,
    /// this function will return ``"I"`` if ``sort_ops=False`` and ``""`` otherwise
    /// to maintain backwards compatibility with versions prior to adding ``sort_ops``;
    /// this is expected to change in a future version and should not be relied upon.
    /// If you need to check for identity, use ``term.is_identity()`` instead.
    fn id(&self, sort_ops: bool) -> String {
        if sort_ops {
            self.arguments
                .iter()
                .sorted_by(|(_, a), (_, b)| a.cmp(b))
                .map(|(g, q)| format!("{g}{q}"))
                .join("")
        } else if !self.arguments.is_empty() {
            self.arguments
                .iter()
                .map(|(g, q)| format!("{g}{q}"))
                .join("")
        } else {
            "I".to_string()
        }
    }

    /// Return a frozenset of operations in this term.
    ///
    /// Use this in place of `id` if the order of operations in the term does not matter.
    #[gen_stub(override_return_type(type_repr = "builtins.frozenset[builtins.tuple[PauliGate, builtins.str]]", imports = ("builtins")))]
    fn operations_as_set<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyFrozenSet>> {
        PyFrozenSet::new(py, self.arguments.clone())
    }

    /// Return `True` if and only if all operators are identity,
    /// including the case in which the list of operators is empty.
    fn is_identity(&self) -> bool {
        self.arguments.iter().all(|(gate, _)| *gate == PauliGate::I)
    }
}

/// Get the product of two coefficients and a phase, simplifying if possible.
fn expr_prod_simple(a: &Expression, b: &Expression, phase: Complex64) -> Expression {
    match (a, b) {
        (Expression::Number(a), Expression::Number(b)) =>
            Expression::Number(a * b * phase),

        (Expression::PiConstant(), Expression::Number(b))
            | (Expression::Number(b), Expression::PiConstant()) =>
            Expression::Number(PI * b * phase),

        (Expression::PiConstant(), Expression::PiConstant()) =>
            Expression::Number(PI * PI * phase),

        (Expression::Number(a), b)
            | (b, Expression::Number(a)) =>
                Expression::Number(a * phase) * b.clone(),

        (a, b) => {
            if phase == Complex64::ONE {
                a.clone() * b.clone()
            } else {
                Expression::Number(phase) * a.clone() * b.clone()
            }
        }
    }
}

impl PauliTerm {
    /// Return the product of this `PauliTerm` and `other`.
    ///
    /// This consumes `self` to reduce the number of clones of the coefficient and arguments.
    /// The order of the resulting arguments is not guaranteed.
    fn multiply_term(self, other: &PauliTerm) -> PauliTerm {
        // For each the terms in the other, if this term has the same argument,
        // combine their operators and multiply the coefficients.
        //
        // This method searches for matching arguments by iterating the terms,
        // which is asymptotically less efficient than a HashMap-based approach,
        // but the typical number of arguments in a term is small.


        let mut phase = Complex64::new(1.0, 0.0);
        let mut arguments = self.arguments;

        for (op, qubit) in other.arguments.iter() {
            match arguments.iter().position(|(_, q)| q == qubit) {
                Some(idx) => {
                    let (new_op, new_phase) = arguments[idx].0.product(*op);
                    if new_op == PauliGate::I {
                        arguments.swap_remove(idx);
                    } else {
                        phase *= new_phase;
                        arguments[idx] = (new_op, qubit.clone());
                    }
                }
                None => {
                    arguments.push((*op, qubit.clone()));
                }
            }
        }

        PauliTerm {
            arguments,
            expression: expr_prod_simple(&self.expression, &other.expression, phase),
        }
    }
}

/// An iterator over the qubit indices and Pauli operators in a [`PauliTerm`].
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(module = "quil._quil.instructions", frozen)]
struct PauliTermIter {
    // Using `Py<_>` avoids cloning the entire `PauliTerm`,
    // and since that class is frozen, we can skip all the Python reference counting
    // by using an atomic index.
    term: Py<PauliTerm>,
    index: AtomicUsize,
}

impl PauliTermIter {
    fn new(term: Py<PauliTerm>) -> Self {
        Self {
            term,
            index: AtomicUsize::new(0),
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PauliTermIter {
    fn __iter__(slf: PyRef<'_, Self>) -> PyRef<'_, Self> {
        slf
    }

    fn __next__(slf: PyRef<'_, Self>) -> Option<(PauliGate, String)> {
        slf.term.get().arguments.get(slf.index.fetch_add(1, Ordering::Relaxed)).cloned()
    }

    #[gen_stub(skip)]
    fn __traverse__(&self, visit: pyo3::PyVisit) -> Result<(), pyo3::PyTraverseError> {
        visit.call(&self.term)
    }
}

// PauliSum constructor stub overloads:
// - the first is the new (PyQuil) preferred order `(terms, arguments=None)`
// - the second is the old (`quil`) deprecated order `(arguments, terms)`
// The former is preferred because `arguments` can (and should?) be inferred from `terms`.
#[cfg(feature = "stubs")]
pyo3_stub_gen::inventory::submit! {
    gen_methods_from_python! {
        r#"
        import collections.abc
        import typing
        import typing_extensions

        class PauliSum:
            @typing.overload
            def __new__(
                cls,
                terms: collections.abc.Sequence[PauliTerm],
                arguments: collections.abc.Sequence[PauliTargetDesignator] | None = None,
            ) -> PauliSum:
                """Construct a new `PauliSum` from a list of `PauliTerm`s
                and an optional list of arguments.
                """

            @typing.overload
            @typing_extensions.deprecated("This parameter order is deprecated; use `(terms, arguments)` instead.")
            def __new__(
                cls,
                arguments: collections.abc.Sequence[PauliTargetDesignator],
                terms: collections.abc.Sequence[PauliTerm],
            ) -> PauliSum:
                """Construct a new `PauliSum` from arguments and `PauliTerm`s.

                This constructor is deprecated; use `(terms, arguments)` instead.
                """
        "#
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PauliSum {
    // TODO(migration-guide):
    //
    // - Rust users of `quil-rs` with the `python` feature
    //   should be aware that `PauliSum::new` is no longer shared directly with Python.
    // - Python users of `quil` will receive deprecation warnings
    //   for existing usage of the `PauliSum.__new__` constructor
    //   and should update usage from `PauliSum(arguments, terms)` to `PauliSum(terms)`.
    /// Construct a new `PauliSum` from a list of `PauliTerm`s and an optional list of arguments.
    ///
    /// For backwards compatibility, this constructor supports `(arguments, terms)`,
    /// but if `arguments` are given, the preferred order is `(terms, arguments)`,
    /// and the other order will issue a deprecation warning.
    /// If not given, `arguments` are inferred from the `PauliTerm`s.
    // Developer Note: The stubs for the documented constructors are added manually above.
    // The two signatures are united in a backwards-compatible way
    // by inserting a positional-only parameter that can work as `terms` or `arguments`;
    // the second argument is still `terms` to support both positional and keyword usage,
    // but likewise accepts either `terms` or `arguments`.
    // Finally, the `arguments` parameter has been moved to the end
    // to maintain compatibility with keyword-only usage.
    //
    // At some point, we'll act on the deprecation notice
    // and stop accepting the `(arguments, terms)` order,
    // and this whole constructor can be greatly simplified.
    #[new]
    #[pyo3(signature = (terms_or_args=None, /, terms=None, arguments=None))]
    fn __new__(
        py: Python<'_>,
        terms_or_args: Option<Migrate<Vec<PauliTerm>, Vec<PauliArg>>>,
        terms: Option<Migrate<Vec<PauliTerm>, Vec<PauliArg>>>,
        arguments: Option<Vec<PauliArg>>,
    ) -> PyResult<PauliSum> {
        match (terms_or_args, terms, arguments) {
            // Single-parameter `terms` as positional or keyword parameters.
            (Some(Migrate::New(terms)), None, None)
                | (None, Some(Migrate::New(terms)), None) => {
                let arguments = PauliSum::into_args(&terms);
                Ok(PauliSum { arguments, terms })
            }

            // New-style two-parameter new-style constructor `(terms, arguments)`,
            // as positional, mixed, and keyword-only versions.
            (Some(Migrate::New(terms)), Some(Migrate::Old(arguments)), None)
                | (Some(Migrate::New(terms)), None, Some(arguments))
                | (None, Some(Migrate::New(terms)), Some(arguments)) => {
                let arguments = convert_pauli_targets(arguments)?;
                // Let the existing constructor check for valid parameters.
                Ok(PauliSum::new(arguments, terms)?)
            }

            // Old-style two-parameter constructor `(arguments, terms)`;
            // the first covers both positional and mixed, the second keyword-only versions.
            (Some(Migrate::Old(arguments)), Some(Migrate::New(terms)), None) => {
                py_deprecated!(
                    py,
                    c"`PauliSum(arguments, terms)` is deprecated; use `PauliSum(terms, arguments)` instead."
                )?;

                let arguments = convert_pauli_targets(arguments)?;
                // Let the existing constructor check for valid parameters.
                Ok(PauliSum::new(arguments, terms)?)
            }

            // Only given `arguments`, by position or keyword,
            (Some(Migrate::Old(_)), None, None) | (None, None, _) => {
                Err(PyValueError::new_err("missing argument `terms`"))
            }

            // Given `terms=<list of strings>` or two lists of strings positionally.
            (Some(Migrate::Old(_)), None, Some(_)) | (_, Some(Migrate::Old(_)), _) => {
                Err(PyTypeError::new_err("`terms` must be a list of `PauliTerm`s"))
            }

            // Given two lists of `PauliTerm`s, but one should be `arguments`.
            (Some(Migrate::New(_)), Some(Migrate::New(_)), None) => {
                Err(PyTypeError::new_err("`arguments` must be a list of `str`s"))
            },

            (Some(_), Some(_), Some(_)) => Err(PyValueError::new_err(
                "too many arguments; use `PauliSum(terms, arguments)`",
            )),
        }
    }

    fn __getnewargs__(&self) -> (Vec<PauliTerm>, Vec<String>) {
        (self.terms.clone(), self.arguments.clone())
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

    impl_stub_type!(PauliArg = String | u64 | Qubit);

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
    type_alias!("quil._quil.instructions", PauliTargetDesignator = PauliArg);
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
