use std::{
    collections::HashMap,
    mem,
    sync::atomic::{AtomicUsize, Ordering},
};

use indexmap::IndexMap;
use num_complex::Complex64;
use numpy::{PyArray2, ToPyArray};
use pyo3::{
    exceptions::{
        PyDeprecationWarning, PyIndexError, PyNotImplementedError, PyTypeError, PyUserWarning,
        PyValueError,
    },
    prelude::*,
    sync::PyOnceLock,
    types::{IntoPyDict as _, PyDict, PyFrozenSet, PyInt, PyList, PyString, PyTuple},
    CastError, IntoPyObjectExt, PyTraverseError, PyTypeCheck, PyVisit,
};
use rigetti_pyo3::{create_init_submodule, impl_repr};

#[cfg(feature = "stubs")]
use pyo3_stub_gen::{
    derive::{gen_methods_from_python, gen_stub_pyclass, gen_stub_pyfunction, gen_stub_pymethods},
    inventory::submit,
};

use super::*;
#[cfg(feature = "stubs")]
use crate::expression::quilpy::stubs::QualifiedExpression;
use crate::{
    expression::{
        consts::{IMAGINARY_UNIT, ONE, ZERO},
        quilpy::{quil_exp, ExpressionArgs, ExpressionLike},
    },
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

        PauliArgIter,
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

pub(crate) struct ArithmeticOperandLike(ArithmeticOperand);
pub(crate) struct BinaryOperandLike(BinaryOperand);
pub(crate) struct MemoryReferenceLike(MemoryReference);

impl From<ArithmeticOperandLike> for ArithmeticOperand {
    fn from(value: ArithmeticOperandLike) -> Self {
        value.0
    }
}

impl From<BinaryOperandLike> for BinaryOperand {
    fn from(value: BinaryOperandLike) -> Self {
        value.0
    }
}

impl From<MemoryReferenceLike> for MemoryReference {
    fn from(value: MemoryReferenceLike) -> Self {
        value.0
    }
}

impl<'a, 'py> FromPyObject<'a, 'py> for ArithmeticOperandLike {
    type Error = PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        let operand = if let Ok(val) = obj.cast::<ArithmeticOperand>() {
            val.get().clone()
        } else if let Ok(val) = obj.cast::<pyo3::types::PyInt>() {
            ArithmeticOperand::LiteralInteger(val.extract()?)
        } else if let Ok(val) = obj.cast::<pyo3::types::PyFloat>() {
            ArithmeticOperand::LiteralReal(val.extract()?)
        } else if let Ok(val) = obj.cast::<DeclarationAt>() {
            ArithmeticOperand::MemoryReference(val.get().memref(obj.py()))
        } else if let Ok(val) = obj.cast::<MemoryReference>() {
            ArithmeticOperand::MemoryReference(val.get().clone())
        } else if let Ok(val) = obj.cast::<Declaration>() {
            ArithmeticOperand::MemoryReference(val.get().to_memory_reference(0))
        } else {
            return Err(CastError::new(
                obj,
                ArithmeticOperand::classinfo_object(obj.py()),
            ))?;
        };

        Ok(Self(operand))
    }
}

impl<'py> IntoPyObject<'py> for ArithmeticOperandLike {
    type Target = ArithmeticOperand;
    type Output = Bound<'py, Self::Target>;
    type Error = PyErr;

    fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
        self.0.into_pyobject(py)
    }
}

impl<'a, 'py> FromPyObject<'a, 'py> for BinaryOperandLike {
    type Error = PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        let operand = if let Ok(operand) = obj.cast::<BinaryOperand>() {
            operand.get().clone()
        } else if let Ok(int) = obj.cast::<PyInt>() {
            BinaryOperand::LiteralInteger(int.extract()?)
        } else if let Ok(decl) = obj.cast::<DeclarationAt>() {
            BinaryOperand::MemoryReference(decl.get().memref(obj.py()))
        } else if let Ok(mem_ref) = obj.cast::<MemoryReference>() {
            BinaryOperand::MemoryReference(mem_ref.borrow().clone())
        } else if let Ok(decl) = obj.cast::<Declaration>() {
            BinaryOperand::MemoryReference(decl.get().to_memory_reference(0))
        } else {
            return Err(CastError::new(
                obj,
                BinaryOperand::classinfo_object(obj.py()),
            ))?;
        };

        Ok(Self(operand))
    }
}

#[derive(FromPyObject)]
pub(crate) enum ComparisonOperandLike<'py> {
    Int(i64),
    Float(f64),
    Wrapped(ComparisonOperand),
    DeclarationAt(Bound<'py, DeclarationAt>),
    Declaration(Bound<'py, Declaration>),
    MemoryReference(MemoryReference),
}

impl From<ComparisonOperandLike<'_>> for ComparisonOperand {
    fn from(value: ComparisonOperandLike<'_>) -> Self {
        match value {
            ComparisonOperandLike::Int(i) => ComparisonOperand::LiteralInteger(i),
            ComparisonOperandLike::Float(f) => ComparisonOperand::LiteralReal(f),
            ComparisonOperandLike::Wrapped(op) => op,
            ComparisonOperandLike::DeclarationAt(decl_at) => {
                ComparisonOperand::MemoryReference(decl_at.get().memref(decl_at.py()))
            }
            ComparisonOperandLike::Declaration(decl) => {
                ComparisonOperand::MemoryReference(decl.get().to_memory_reference(0))
            }
            ComparisonOperandLike::MemoryReference(mem_ref) => {
                ComparisonOperand::MemoryReference(mem_ref)
            }
        }
    }
}

/// An object in the Python heap that can be converted into a [`MemoryReference`].
///
/// If a Python user derives values from [`Declaration`]s to use as [`MemoryReference`]s,
/// we can provide additional validation when they use them to create instructions.
/// See [`DeclarationAt`] for examples how these fit together for program building.
#[derive(FromPyObject)]
enum PyMemRef<'py> {
    DeclarationAt(Bound<'py, DeclarationAt>),
    Declaration(Bound<'py, Declaration>),
    MemoryReference(Bound<'py, MemoryReference>),
}

impl From<PyMemRef<'_>> for MemoryReference {
    fn from(value: PyMemRef<'_>) -> Self {
        match value {
            PyMemRef::DeclarationAt(decl_at) => decl_at.get().memref(decl_at.py()),
            PyMemRef::Declaration(decl) => decl.get().to_memory_reference(0),
            PyMemRef::MemoryReference(mem_ref) => mem_ref.borrow().clone(),
        }
    }
}

/// A trait for types that might be able to provide a [`Borrowed`] to Python data.
trait TryAsBorrowed<'py, T> {
    fn try_borrow<'a>(&'a self) -> Option<Borrowed<'a, 'py, T>>;
}

impl<'py> TryAsBorrowed<'py, Declaration> for ComparisonOperandLike<'py> {
    fn try_borrow<'a>(&'a self) -> Option<Borrowed<'a, 'py, Declaration>> {
        match self {
            ComparisonOperandLike::Declaration(decl) => Some(decl.as_borrowed()),
            ComparisonOperandLike::DeclarationAt(decl_at) => {
                Some(decl_at.get().declaration.bind_borrowed(decl_at.py()))
            }
            _ => None,
        }
    }
}

impl<'py> TryAsBorrowed<'py, Declaration> for PyMemRef<'py> {
    fn try_borrow<'a>(&'a self) -> Option<Borrowed<'a, 'py, Declaration>> {
        match self {
            PyMemRef::Declaration(decl) => Some(decl.as_borrowed()),
            PyMemRef::DeclarationAt(decl_at) => {
                Some(decl_at.get().declaration.bind_borrowed(decl_at.py()))
            }
            _ => None,
        }
    }
}

enum MemoryTypeMatch {
    Valid,
    Invalid,
    Unknown,
}

/// Check if two values derived from [`Declaration`]s have the same data types.
///
/// If the underlying [`Declaration`]s are known, this returns `Some(b)`,
/// where `b` is true if the data types match and false otherwise.
/// If we can't get the underlying [`Declaration`]s, this returns `None`.
fn same_type<'py, T, U>(lhs: &T, rhs: &U) -> Option<bool>
where
    T: TryAsBorrowed<'py, Declaration>,
    U: TryAsBorrowed<'py, Declaration>,
{
    lhs.try_borrow().and_then(|l| {
        rhs.try_borrow()
            .map(|r| l.get().size.data_type == r.get().size.data_type)
    })
}

impl<'a, 'py> FromPyObject<'a, 'py> for MemoryReferenceLike {
    type Error = PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        let mem_ref = if let Ok(mem_ref) = obj.cast::<MemoryReference>() {
            mem_ref.borrow().clone()
        } else if let Ok(decl) = obj.cast::<DeclarationAt>() {
            // Create a new `MemoryReference` from an underlying `Declaration` and index.
            decl.get().memref(obj.py())
        } else if let Ok(decl) = obj.cast::<Declaration>() {
            // Create a new `MemoryReference` from a `Declaration` assuming an index of 0.
            decl.get().to_memory_reference(0)
        } else if let Ok(s) = obj.cast::<PyTuple>() {
            // Create a new `MemoryReference` from a tuple of `(str, int)` pair.
            let (name, index) = s.extract()?;
            MemoryReference::new(name, index)
        } else if let Ok(s) = obj.cast::<PyList>() {
            // As above, but from a list of `[str, int]` pair.
            let len = obj.len()?;
            if len != 2 {
                return Err(PyValueError::new_err(
                    "expected list of length 2, but got list of length {len}",
                ));
            }
            let MemoryReferencePair { name, index } = s.extract()?;
            MemoryReference::new(name, index)
        } else {
            return Err(CastError::new(
                obj,
                MemoryReference::classinfo_object(obj.py()),
            ))?;
        };

        Ok(Self(mem_ref))
    }
}

// TODO(migration-guide):
// - PyQuil v4 called `Arithmetic` `ArithmeticBinaryOp`
// - The signature was `Arithmetic(left, right)`, with `operator` (called `op`)
//   provided as a class attribute in subclasses.
pickleable_new! {
    impl Arithmetic {
        fn __new__(
            operator: ArithmeticOperator,
            destination: MemoryReference,
            source: ArithmeticOperand as ArithmeticOperandLike,
        ) -> Arithmetic {
            Self::new(operator, destination, source.into())
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Arithmetic {
    #[pyo3(warn(message = "use `operator` instead", category = PyDeprecationWarning))]
    #[getter]
    fn op(&self) -> ArithmeticOperator {
        self.operator
    }

    /// Get the destination [`MemoryReference`] for this arithmetic operation.
    #[pyo3(warn(message = "use `destination` instead", category = PyDeprecationWarning))]
    #[getter]
    fn left(&self) -> MemoryReference {
        self.destination.clone()
    }

    /// Get the source [`ArithmeticOperand`] for this arithmetic operation.
    #[pyo3(warn(message = "use `source` instead", category = PyDeprecationWarning))]
    #[getter]
    fn right(&self) -> ArithmeticOperand {
        self.source.clone()
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
    #[new]
    fn __new__(operand: BinaryOperandLike) -> Self {
        operand.into()
    }

    #[gen_stub(override_return_type(type_repr = "builtins.tuple[builtins.int | MemoryReference]", imports = ("builtins")))]
    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        match self {
            Self::LiteralInteger(value) => (value,).into_pyobject(py),
            Self::MemoryReference(value) => (value.clone(),).into_pyobject(py),
        }
    }
}

// TODO(migration-guide):
// - PyQuil v4 called `BinaryLogic` `LogicalBinaryOp` and had subclassed variants.
// - The constructor took two parameters: `left` and `right`, which it exposed as properties.
//   The `left` corresponds to the `destination`, and the `right` corresponds to the `source`.
// - The operator was stored as a class attribute called `op`, defined within subclasses.
// - As with many other classes, those properties were mutable;
//   that's no longer the case because we intended for the class to be hashable.
pickleable_new! {
    impl BinaryLogic {
        fn __new__(
            operator: BinaryOperator,
            destination: MemoryReference as MemoryReferenceLike,
            source: BinaryOperand as BinaryOperandLike
        ) -> BinaryLogic {
            Self::new(operator, destination.into(), source.into())
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl BinaryLogic {
    #[pyo3(warn(message = "use `destination` instead", category = PyDeprecationWarning))]
    #[getter]
    fn left(&self) -> MemoryReference {
        self.destination.clone()
    }

    #[pyo3(warn(message = "use `source` instead", category = PyDeprecationWarning))]
    #[getter]
    fn right(&self) -> BinaryOperand {
        self.source.clone()
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

// TODO(migration-guide):
// - PyQuil v4 called `Comparison` `ClassicalComparison`, with subclasses
//   (`ClassicalEqual`, `ClassicalLessThan`, etc.) providing `operator` as a class attribute.
// - The signature was `(target, left, right)`.
pickleable_new! {
    impl Comparison {
        fn __new__(
            operator: ComparisonOperator,
            destination: MemoryReference as MemoryReferenceLike,
            lhs: MemoryReference as MemoryReferenceLike,
            rhs: ComparisonOperand as ComparisonOperandLike<'_>,
        ) -> Comparison {
            Self::new(operator, destination.into(), lhs.into(), rhs.into())
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Comparison {
    #[pyo3(warn(message = "use `operator` instead", category = PyDeprecationWarning))]
    #[getter]
    fn op(&self) -> ComparisonOperator {
        self.operator
    }

    #[pyo3(warn(message = "use `destination` instead", category = PyDeprecationWarning))]
    #[getter]
    fn target(&self) -> MemoryReference {
        self.destination.clone()
    }

    #[pyo3(warn(message = "use `lhs` instead", category = PyDeprecationWarning))]
    #[getter]
    fn left(&self) -> MemoryReference {
        self.lhs.clone()
    }

    #[pyo3(warn(message = "use `rhs` instead", category = PyDeprecationWarning))]
    #[getter]
    fn right(&self) -> ComparisonOperand {
        self.rhs.clone()
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

// This is essentially what a "memory reference" really is:
// a reference at a specific index to a declared region of memory.
/// A wrapper around a [`Declaration`] for use in places we'd normally need a `MemoryReference`.
///
/// You can get an instance of `DeclarationAt` by indexing a `Declaration`,
/// and you can then use it in places where a `MemoryReference` is expected.
///
/// # Example
///
/// With use of the warlus operator, you can often write a list of instructions
/// without needing to explicitly declare `MemoryReference` objects:
///
/// ```python
/// instructions = [
///     counter := Declaration("counter", ScalarType.INTEGER),      # DECLARE counter INTEGER
///     counter[0].move(10),                                        # MOVE counter[0] 10
///     top := Label("top"),                                        # LABEL @top
///     # additional instructions...
///     counter[0].sub(1),                                          # SUB counter[0] 1
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

impl<'a, 'py> FromPyObject<'a, 'py> for DeclarationAt {
    type Error = PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        // This "extraction" actually just increases the refcount.
        let decl_at = if let Ok(decl) = obj.cast::<DeclarationAt>() {
            DeclarationAt {
                declaration: decl.get().declaration.clone_ref(obj.py()),
                index: decl.get().index,
            }
        } else if let Ok(decl) = obj.cast::<Declaration>() {
            DeclarationAt {
                declaration: decl.to_owned().unbind(), // This just increases the refcount.
                index: 0,
            }
        } else {
            return Err(CastError::new(obj, Declaration::classinfo_object(obj.py())))?;
        };

        Ok(decl_at)
    }
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

    /// Return an error if a comparison instruction for `self := a <op> b`
    /// is known to produce invalid Quil.
    fn check_comparison<'py>(
        &self,
        a: &PyMemRef<'py>,
        b: &ComparisonOperandLike<'py>,
    ) -> PyResult<()> {
        if self.declaration.get().size.data_type != ScalarType::Bit {
            // TODO: make a custom error type for this
            return Err(PyValueError::new_err(
                "the destination of a comparison must be BIT-typed",
            ));
        }

        if matches!(same_type(a, b), Some(false)) {
            return Err(PyValueError::new_err(
                "the left-hand side and right-hand side of a comparison must have the same type",
            ));
        }

        Ok(())
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
    /// mv = x[2].move(5)
    /// assert isinstance(mv, Move)
    /// assert mv.to_quil() == "MOVE x[2] 5"  # x[2] := 5
    /// ```
    #[pyo3(name = "move", signature = (value, /))]
    fn py_move<'py>(&self, py: Python<'py>, value: ArithmeticOperandLike) -> Move {
        Move::new(self.memref(py), value.into())
    }

    /// Return a new `Exchange` instruction representing the exchange of `self` and `other`.
    ///
    /// # Example
    ///
    /// ```Python
    /// from quil.instructions import Declaration, ScalarType, Exchange
    ///
    /// x = Declaration("x", ScalarType.INTEGER, 3)
    /// y = Declaration("y", ScalarType.INTEGER, 3)
    /// ex = x[2].exchange(y[1])
    /// assert isinstance(ex, Exchange)
    /// assert ex.to_quil() == "EXCHANGE x[2] y[1]"  # x[2] <-> y[1]
    /// ```
    #[pyo3(signature = (other, /))]
    fn exchange<'py>(&self, py: Python<'py>, other: MemoryReferenceLike) -> Exchange {
        Exchange::new(self.memref(py), other.into())
    }

    /// Return a new `Load` instruction representing the indirect load `self = source[offset]`.
    ///
    /// # Example
    ///
    /// ```python
    /// from quil.instructions import Declaration, ScalarType, Load
    ///
    /// x = Declaration("x", ScalarType.INTEGER, 3)
    /// y = Declaration("y", ScalarType.INTEGER, 3)
    /// z = Declaration("z", ScalarType.INTEGER, 3)
    /// ld = x[2].load_from(y, z[1])
    /// assert isinstance(ld, Load)
    /// assert ld.to_quil() == "LOAD x[2] y z[1]"  # x[2] := y[z[1]]
    /// ```
    #[pyo3(signature = (source, offset, /))]
    fn load_from<'py>(
        &self,
        py: Python<'py>,
        source: &Declaration,
        offset: MemoryReferenceLike,
    ) -> Load {
        Load::new(self.memref(py), source.name.clone(), offset.into())
    }

    /// Return a new `Store` instruction representing the indirect store `destination[offset] = self`.
    ///
    /// # Example
    ///
    /// ```python
    /// from quil.instructions import Declaration, ScalarType, Store
    ///
    /// x = Declaration("x", ScalarType.INTEGER, 3)
    /// y = Declaration("y", ScalarType.INTEGER, 3)
    /// z = Declaration("z", ScalarType.INTEGER, 3)
    /// st = x[2].store_to(y, z[1])
    /// assert isinstance(st, Store)
    /// assert st.to_quil() == "STORE y z[1] x[2]"  # y[z[1]] := x[2]
    /// ```
    #[pyo3(signature = (destination, offset, /))]
    fn store_to<'py>(
        &self,
        py: Python<'py>,
        destination: &Declaration,
        offset: MemoryReferenceLike,
    ) -> Store {
        Store::new(
            destination.name.clone(),
            offset.into(),
            ArithmeticOperand::MemoryReference(self.memref(py)),
        )
    }

    /// Return a new `Convert` instruction representing `self = (T)source`.
    ///
    /// This is like a `Move` instruction with a cast from the source type to the destination type.
    /// Note that in Quil, the source of a `CONVERT` instruction MUST be a `MemoryReference`,
    /// so this method only accepts `MemoryReference`s and things that can be converted into them,
    /// not arbitrary numeric literals.
    ///
    /// # Example
    ///
    /// ```python
    /// from quil.instructions import Declaration, ScalarType, Convert
    ///
    /// x = Declaration("x", ScalarType.INTEGER, 3)
    /// y = Declaration("y", ScalarType.REAL, 3)
    /// cvt = x[2].convert(y[1])
    /// assert isinstance(cvt, Convert)
    /// assert cvt.to_quil() == "CONVERT x[2] y[1]"  # x[2] := (INTEGER) y[1]
    /// ```
    #[pyo3(signature = (source, /))]
    fn convert<'py>(&self, py: Python<'py>, source: MemoryReferenceLike) -> Convert {
        Convert::new(self.memref(py), source.into())
    }

    // Note: These are not implemented as Python dunder arithmetic methods,
    // because the point is to return an `Arithmetic` instruction.
    //
    // What might be nice in the future is to implement something that supports chained operations.
    // For example, a user could write `x[2] + 5 - 3`, and we'd know it represents
    // a sequence of instructions (`ADD x[2] 5` followed by `SUB x[2] 3`);
    // but that would require a fair bit of implementation work we're not interested in right now,
    // and we don't want to block ourselves from doing it in the future.

    /// Return a new instruction representing a logical `NOT` of this memory reference.
    ///
    /// # Example
    ///
    /// ```python
    /// from quil.instructions import Declaration, ScalarType, UnaryLogic
    ///
    /// x = Declaration("x", ScalarType.INTEGER, 3)
    /// n = x[2].not()
    /// assert isinstance(n, UnaryLogic)
    /// assert n.to_quil() == "NOT x[2]"  # x[2] := ~x[2]
    /// ```
    fn not<'py>(&self, py: Python<'py>) -> UnaryLogic {
        UnaryLogic::new(UnaryOperator::Not, self.memref(py))
    }

    fn neg<'py>(&self, py: Python<'py>) -> UnaryLogic {
        UnaryLogic::new(UnaryOperator::Neg, self.memref(py))
    }

    #[pyo3(signature = (other, /))]
    fn and<'py>(&self, py: Python<'py>, other: BinaryOperandLike) -> BinaryLogic {
        BinaryLogic::new(BinaryOperator::And, self.memref(py), other.into())
    }

    #[pyo3(signature = (other, /))]
    fn ior<'py>(&self, py: Python<'py>, other: BinaryOperandLike) -> BinaryLogic {
        BinaryLogic::new(BinaryOperator::And, self.memref(py), other.into())
    }

    #[pyo3(signature = (other, /))]
    fn xor<'py>(&self, py: Python<'py>, other: BinaryOperandLike) -> BinaryLogic {
        BinaryLogic::new(BinaryOperator::And, self.memref(py), other.into())
    }

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
    #[pyo3(signature = (other, /))]
    fn add<'py>(&self, py: Python<'py>, other: ArithmeticOperandLike) -> Arithmetic {
        Arithmetic::new(ArithmeticOperator::Add, self.memref(py), other.into())
    }

    #[pyo3(signature = (other, /))]
    fn sub<'py>(&self, py: Python<'py>, other: ArithmeticOperandLike) -> Arithmetic {
        Arithmetic::new(ArithmeticOperator::Subtract, self.memref(py), other.into())
    }

    #[pyo3(signature = (other, /))]
    fn div<'py>(&self, py: Python<'py>, other: ArithmeticOperandLike) -> Arithmetic {
        Arithmetic::new(ArithmeticOperator::Divide, self.memref(py), other.into())
    }

    #[pyo3(signature = (other, /))]
    fn mul<'py>(&self, py: Python<'py>, other: ArithmeticOperandLike) -> Arithmetic {
        Arithmetic::new(ArithmeticOperator::Multiply, self.memref(py), other.into())
    }

    /// Return a new `Comparison` instruction representing `self = (a == b)`.
    ///
    /// Note that `self` must represent a `MemoryReference` of type `BIT`,
    /// the left-hand side of the comparison must be a `MemoryReference`-like object,
    /// and the right-hand side must have a type compatible with the left-hand side.
    /// See the Quil specification for more details on valid comparisons.
    #[pyo3(signature = (a, b, /))]
    fn store_eq<'py>(
        &self,
        py: Python<'py>,
        a: PyMemRef<'py>,
        b: ComparisonOperandLike<'py>,
    ) -> PyResult<Comparison> {
        self.check_comparison(&a, &b)?;
        Ok(Comparison::new(
            ComparisonOperator::Equal,
            self.memref(py),
            a.into(),
            b.into(),
        ))
    }

    #[pyo3(signature = (a, b, /))]
    fn store_gt<'py>(
        &self,
        py: Python<'py>,
        a: PyMemRef<'py>,
        b: ComparisonOperandLike<'py>,
    ) -> PyResult<Comparison> {
        self.check_comparison(&a, &b)?;
        Ok(Comparison::new(
            ComparisonOperator::GreaterThan,
            self.memref(py),
            a.into(),
            b.into(),
        ))
    }

    #[pyo3(signature = (a, b, /))]
    fn store_ge<'py>(
        &self,
        py: Python<'py>,
        a: PyMemRef<'py>,
        b: ComparisonOperandLike<'py>,
    ) -> PyResult<Comparison> {
        self.check_comparison(&a, &b)?;
        Ok(Comparison::new(
            ComparisonOperator::GreaterThanOrEqual,
            self.memref(py),
            a.into(),
            b.into(),
        ))
    }

    #[pyo3(signature = (a, b, /))]
    fn store_lt<'py>(
        &self,
        py: Python<'py>,
        a: PyMemRef<'py>,
        b: ComparisonOperandLike<'py>,
    ) -> PyResult<Comparison> {
        self.check_comparison(&a, &b)?;
        Ok(Comparison::new(
            ComparisonOperator::LessThan,
            self.memref(py),
            a.into(),
            b.into(),
        ))
    }

    #[pyo3(signature = (a, b, /))]
    fn store_le<'py>(
        &self,
        py: Python<'py>,
        a: PyMemRef<'py>,
        b: ComparisonOperandLike<'py>,
    ) -> PyResult<Comparison> {
        self.check_comparison(&a, &b)?;
        Ok(Comparison::new(
            ComparisonOperator::LessThanOrEqual,
            self.memref(py),
            a.into(),
            b.into(),
        ))
    }

    // Garbage collection integration. For more information, see:
    // https://pyo3.rs/v0.29.0/class/protocols.html#garbage-collector-integration
    #[gen_stub(skip)]
    fn __traverse__(&self, visit: PyVisit<'_>) -> Result<(), PyTraverseError> {
        visit.call(&self.declaration)?;
        Ok(())
    }
}

// TODO(migration-guide):
// - PyQuil v4 called `Convert` `ClassicalConvert`.
// - The signature was `(left, right)`.
pickleable_new! {
    impl Convert {
        fn __new__(
            destination: MemoryReference as MemoryReferenceLike,
            source: MemoryReference as MemoryReferenceLike,
        ) -> Convert {
            Self::new(destination.into(), source.into())
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Convert {
    #[pyo3(warn(message = "use `destination` instead", category = PyDeprecationWarning))]
    #[getter]
    fn left(&self) -> MemoryReference {
        self.destination.clone()
    }

    #[pyo3(warn(message = "use `source` instead", category = PyDeprecationWarning))]
    #[getter]
    fn right(&self) -> MemoryReference {
        self.source.clone()
    }
}

pickleable_new! {
    impl Exchange {
        fn __new__(
            left: MemoryReference as MemoryReferenceLike,
            right: MemoryReference as MemoryReferenceLike,
        ) -> Exchange {
            Self::new(left.into(), right.into())
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

    fn __getnewargs__(&self) -> (String, Vec<Expression>, Vec<Qubit>, Vec<GateModifier>) {
        (
            self.name.clone(),
            self.parameters.clone(),
            self.qubits.clone(),
            self.modifiers.clone(),
        )
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

// TODO(migration-guide):
// - PyQuil v4 called `Load` `ClassicalLoad`.
// - The signature was `(target, left, right)`.
pickleable_new! {
    impl Load {
        fn __new__(
            destination: MemoryReference as MemoryReferenceLike,
            source: String,
            offset: MemoryReference as MemoryReferenceLike,
        ) -> Load {
            Self::new(destination.into(), source, offset.into())
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Load {
    #[pyo3(warn(message = "use `destination` instead", category = PyDeprecationWarning))]
    #[getter]
    fn target(&self) -> MemoryReference {
        self.destination.clone()
    }

    #[pyo3(warn(message = "use `source` instead", category = PyDeprecationWarning))]
    #[getter]
    fn left(&self) -> String {
        self.source.clone()
    }

    #[pyo3(warn(message = "use `offset` instead", category = PyDeprecationWarning))]
    #[getter]
    fn right(&self) -> MemoryReference {
        self.offset.clone()
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
    Ok(obj.extract()?)
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

pickleable_new! {
    impl Reset {
        /// Initialize a new reset instruction, optionally targeting a specific qubit.
        #[pyo3(signature = (qubit = None))]
        fn __new__(qubit: Option<Qubit>);
    }
}

// TODO(migration-guide): PyQuil v4 had a `Reset` class _and_ a `ResetQubit` class,
// which was functionally equivalent to `Reset`, except required the `Qubit` argument.
// In v5, we just make `ResetQubit` an alias for `Reset`.
#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Reset {
    /// Return `None` if the `Reset` has no `Qubit`,
    /// or return a set containing the index of the `Qubit` if it is a `Fixed` qubit;
    /// otherwise, raise a `ValueError`.
    fn get_qubit_indices(&self) -> PyResult<Option<HashSet<u64>>> {
        match self.qubit {
            Some(Qubit::Fixed(index)) => Ok(Some(HashSet::from([index]))),
            Some(_) => Err(errors::ValueError::new_err(
                "this Reset operates on a non-fixed qubit",
            )),
            None => Ok(None),
        }
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
    #[gen_stub(override_return_type(type_repr = "tuple[str, int]"))]
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

// TODO(migration-guide):
// - PyQuil v4 called `Move` `ClassicalMove`.
// - The signature was `Arithmetic(left, right)`.
pickleable_new! {
    impl Move {
        fn __new__(
            destination: MemoryReference,
            source: ArithmeticOperand as ArithmeticOperandLike,
        ) -> Move {
            Self::new(destination, source.into())
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Move {
    /// Get the destination [`MemoryReference`] for this operation.
    #[pyo3(warn(message = "use `destination` instead", category = PyDeprecationWarning))]
    #[getter]
    fn left(&self) -> MemoryReference {
        self.destination.clone()
    }

    /// Get the source [`ArithmeticOperand`] for this operation.
    #[pyo3(warn(message = "use `source` instead", category = PyDeprecationWarning))]
    #[getter]
    fn right(&self) -> ArithmeticOperand {
        self.source.clone()
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

/// Return the set of fixed qubit indices among `qubits`,
/// or return a `ValueError` if any aren't `Fixed`.
///
/// PyQuil v4 used this basic logic in `get_qubit_indices` methods.
fn frame_qubit_indices(qubits: &[Qubit]) -> PyResult<HashSet<u64>> {
    qubits
        .iter()
        .map(|qubit| match qubit {
            Qubit::Fixed(index) => Ok(*index),
            _ => Err(errors::ValueError::new_err(format!(
                "this instruction operates on a non-fixed qubit: {qubit:?}"
            ))),
        })
        .collect()
}

// TODO(migration-guide): PyQuil v4's `Pulse` took `nonblocking=False` instead of `blocking`.
// This version has `blocking=True`, so for callers that didn't specify either, nothing changes.
// For users that specified `nonblocking` as a keyword argument,
// they'll now receive a deprecation warning, but the behavior will be consistent.
// If a caller set `nonblocking` as a positional argument, they'll now receive an error,
// as this change makes it keyword-only to avoid accidentally changing behavior when upgrading.
//
// This is implemented manually (rather than with `pickleable_new!`)
// because `nonblocking` isn't a field on the underlying struct.
#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Pulse {
    #[new]
    #[pyo3(signature = (frame, waveform, *, blocking = true, nonblocking = None))]
    fn __new__(
        py: Python<'_>,
        frame: FrameIdentifier,
        waveform: WaveformInvocation,
        blocking: bool,
        nonblocking: Option<bool>,
    ) -> PyResult<Self> {
        let blocking = deprecated_or_new!(py, new = blocking, old = nonblocking, |nb| Ok(!nb))?;
        Ok(Self::new(blocking, frame, waveform))
    }

    #[gen_stub(override_return_type(
        type_repr = "builtins.tuple[
            builtins.tuple[FrameIdentifier, WaveformInvocation],
            builtins.dict[builtins.str, builtins.bool]
        ]",
        imports = ("builtins",)
    ))]
    fn __getnewargs_ex__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        let positional: Bound<'py, PyTuple> =
            (self.frame.clone(), self.waveform.clone()).into_pyobject(py)?;
        let keyword: Bound<'py, PyDict> = [("blocking", self.blocking)].into_py_dict(py)?;
        (positional, keyword).into_pyobject(py)
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Pulse {
    #[pyo3(warn(message = "use `not blocking` instead", category = PyDeprecationWarning))]
    #[getter]
    fn nonblocking(&self) -> bool {
        !self.blocking
    }

    /// Return the fixed qubit indices used by this instruction's `frame`.
    ///
    /// Raises a ``ValueError`` if the frame operates on any non-fixed qubits.
    fn get_qubit_indices(&self) -> PyResult<HashSet<u64>> {
        frame_qubit_indices(&self.frame.qubits)
    }
}

// TODO(migration-guide): PyQuil v4's `Capture` took `kernel` and `memory_region` (in that
// order) and `nonblocking` (defaulting to `False`) instead of `waveform`, `memory_reference`,
// and `blocking`.
//
// This is implemented manually (rather than with `pickleable_new!`)
// because `nonblocking` isn't a field on the underlying struct.
#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Capture {
    #[new]
    #[pyo3(signature = (frame, kernel, memory_region, *, blocking = true, nonblocking = None))]
    fn __new__(
        py: Python<'_>,
        frame: FrameIdentifier,
        kernel: WaveformInvocation,
        memory_region: MemoryReferenceLike,
        blocking: bool,
        nonblocking: Option<bool>,
    ) -> PyResult<Self> {
        let blocking = deprecated_or_new!(py, new = blocking, old = nonblocking, |nb| Ok(!nb))?;
        Ok(Self::new(blocking, frame, memory_region.into(), kernel))
    }

    #[gen_stub(override_return_type(
        type_repr = "builtins.tuple[
            builtins.tuple[FrameIdentifier, WaveformInvocation, MemoryReference],
            builtins.dict[builtins.str, builtins.bool]
        ]",
        imports = ("builtins",)
    ))]
    fn __getnewargs_ex__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        let positional: Bound<'py, PyTuple> = (
            self.frame.clone(),
            self.waveform.clone(),
            self.memory_reference.clone(),
        )
            .into_pyobject(py)?;
        let keyword: Bound<'py, PyDict> = [("blocking", self.blocking)].into_py_dict(py)?;
        (positional, keyword).into_pyobject(py)
    }

    #[pyo3(warn(message = "use `not capture.blocking` instead", category = PyDeprecationWarning))]
    #[getter]
    fn nonblocking(&self) -> bool {
        !self.blocking
    }

    /// Return the fixed qubit indices used by this instruction's `frame`.
    ///
    /// Raises a ``ValueError`` if the frame operates on any non-fixed qubits.
    fn get_qubit_indices(&self) -> PyResult<HashSet<u64>> {
        frame_qubit_indices(&self.frame.qubits)
    }
}

// TODO(migration-guide): PyQuil v4's `RawCapture` took `memory_region` instead of
// `memory_reference`, and `nonblocking` (defaulting to `False`) instead of `blocking`.
//
// This is implemented manually (rather than with `pickleable_new!`)
// because `nonblocking` isn't a field on the underlying struct.
#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl RawCapture {
    #[new]
    #[pyo3(signature = (frame, duration, memory_region, *, blocking = true, nonblocking = None))]
    fn __new__(
        py: Python<'_>,
        frame: FrameIdentifier,
        duration: ExpressionLike,
        memory_region: MemoryReferenceLike,
        blocking: bool,
        nonblocking: Option<bool>,
    ) -> PyResult<Self> {
        let blocking = deprecated_or_new!(py, new = blocking, old = nonblocking, |nb| Ok(!nb))?;

        Ok(Self::new(
            blocking,
            frame,
            duration.into(),
            memory_region.into(),
        ))
    }

    #[gen_stub(override_return_type(
        type_repr = "builtins.tuple[
            builtins.tuple[FrameIdentifier, expression.Expression, MemoryReference],
            builtins.dict[builtins.str, builtins.bool]
        ]",
        imports = ("builtins", "quil._quil.expression")
    ))]
    fn __getnewargs_ex__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        let positional: Bound<'py, PyTuple> = (
            self.frame.clone(),
            self.duration.clone(),
            self.memory_reference.clone(),
        )
            .into_pyobject(py)?;
        let keyword: Bound<'py, PyDict> = [("blocking", self.blocking)].into_py_dict(py)?;
        (positional, keyword).into_pyobject(py)
    }

    #[pyo3(warn(message = "use `not blocking` instead", category = PyDeprecationWarning))]
    #[getter]
    fn nonblocking(&self) -> bool {
        !self.blocking
    }

    #[pyo3(warn(message = "use `memory_reference` instead", category = PyDeprecationWarning))]
    #[getter]
    fn memory_region(&self) -> MemoryReference {
        self.memory_reference.clone()
    }

    /// Return the fixed qubit indices used by this instruction's `frame`.
    ///
    /// Raises a ``ValueError`` if the frame operates on any non-fixed qubits.
    fn get_qubit_indices(&self) -> PyResult<HashSet<u64>> {
        frame_qubit_indices(&self.frame.qubits)
    }
}

// TODO(migration-guide): PyQuil v4's `SetFrequency` took `freq` instead of `frequency`.
//
// This is implemented manually (rather than with `pickleable_new!`)
// because `freq` isn't a field on the underlying struct.
#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl SetFrequency {
    #[new]
    #[pyo3(signature = (frame, frequency, *, freq = None))]
    fn __new__(
        py: Python<'_>,
        frame: FrameIdentifier,
        frequency: ExpressionLike,
        freq: Option<ExpressionLike>,
    ) -> PyResult<Self> {
        let frequency = deprecated_or_new!(py, new = frequency, old = freq)?;
        Ok(Self::new(frame, frequency.into()))
    }

    fn __getnewargs__(&self) -> (FrameIdentifier, Expression) {
        (self.frame.clone(), self.frequency.clone())
    }

    #[pyo3(warn(message = "use `frequency` instead", category = PyDeprecationWarning))]
    #[getter]
    fn freq(&self) -> Expression {
        self.frequency.clone()
    }

    /// Return the fixed qubit indices used by this instruction's `frame`.
    ///
    /// Raises a ``ValueError`` if the frame operates on any non-fixed qubits.
    fn get_qubit_indices(&self) -> PyResult<HashSet<u64>> {
        frame_qubit_indices(&self.frame.qubits)
    }
}

// TODO(migration-guide): PyQuil v4's `ShiftFrequency` took `freq` instead of `frequency`.
//
// This is implemented manually (rather than with `pickleable_new!`)
// because `freq` isn't a field on the underlying struct.
#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl ShiftFrequency {
    #[new]
    #[pyo3(signature = (frame, frequency, *, freq = None))]
    fn __new__(
        py: Python<'_>,
        frame: FrameIdentifier,
        frequency: ExpressionLike,
        freq: Option<ExpressionLike>,
    ) -> PyResult<Self> {
        let frequency = deprecated_or_new!(py, new = frequency, old = freq)?;
        Ok(Self::new(frame, frequency.into()))
    }

    fn __getnewargs__(&self) -> (FrameIdentifier, Expression) {
        (self.frame.clone(), self.frequency.clone())
    }

    #[pyo3(warn(message = "use `frequency` instead", category = PyDeprecationWarning))]
    #[getter]
    fn freq(&self) -> Expression {
        self.frequency.clone()
    }

    /// Return the fixed qubit indices used by this instruction's `frame`.
    ///
    /// Raises a ``ValueError`` if the frame operates on any non-fixed qubits.
    fn get_qubit_indices(&self) -> PyResult<HashSet<u64>> {
        frame_qubit_indices(&self.frame.qubits)
    }
}

pickleable_new! {
    impl SetPhase {
        fn __new__(
            frame: FrameIdentifier,
            phase: Expression as ExpressionLike,
        ) -> SetPhase {
            Self::new(frame, phase.into())
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl SetPhase {
    /// Return the fixed qubit indices used by this instruction's `frame`.
    ///
    /// Raises a ``ValueError`` if the frame operates on any non-fixed qubits.
    fn get_qubit_indices(&self) -> PyResult<HashSet<u64>> {
        frame_qubit_indices(&self.frame.qubits)
    }
}

pickleable_new! {
    impl ShiftPhase {
        fn __new__(
            frame: FrameIdentifier,
            phase: Expression as ExpressionLike,
        ) -> ShiftPhase {
            Self::new(frame, phase.into())
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl ShiftPhase {
    /// Return the fixed qubit indices used by this instruction's `frame`.
    ///
    /// Raises a ``ValueError`` if the frame operates on any non-fixed qubits.
    fn get_qubit_indices(&self) -> PyResult<HashSet<u64>> {
        frame_qubit_indices(&self.frame.qubits)
    }
}

pickleable_new! {
    impl SetScale {
        fn __new__(
            frame: FrameIdentifier,
            scale: Expression as ExpressionLike,
        ) -> SetScale {
            Self::new(frame, scale.into())
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl SetScale {
    /// Return the fixed qubit indices used by this instruction's `frame`.
    ///
    /// Raises a ``ValueError`` if the frame operates on any non-fixed qubits.
    fn get_qubit_indices(&self) -> PyResult<HashSet<u64>> {
        frame_qubit_indices(&self.frame.qubits)
    }
}

/// A frame name, or a `FrameIdentifier` (in which case just its name is used).
///
/// Accepting `FrameIdentifier`s directly is deprecated: it issues a ``DeprecationWarning``.
#[derive(Debug, Clone)]
enum FrameNameLike {
    Name(String),
    Frame(FrameIdentifier),
}

impl<'a, 'py> FromPyObject<'a, 'py> for FrameNameLike {
    type Error = PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        if let Ok(value) = obj.cast::<PyString>() {
            Ok(Self::Name(value.extract()?))
        } else if let Ok(value) = obj.cast::<FrameIdentifier>() {
            let py = obj.py();
            py_deprecated!(
                py,
                c"giving a `Frame` here is deprecated; give its name (a `str`) instead"
            )?;
            Ok(Self::Frame(value.get().clone()))
        } else {
            Err(PyTypeError::new_err("expected a str or Frame"))
        }
    }
}

impl From<FrameNameLike> for String {
    fn from(value: FrameNameLike) -> Self {
        match value {
            FrameNameLike::Name(name) => name,
            FrameNameLike::Frame(frame) => frame.name,
        }
    }
}

// TODO(migration-guide):
// - PyQuil v4's `Delay` (and its subclasses `DelayFrames`/`DelayQubits`) took a list of
//   `Frame`s, merging their qubits with the given qubits; here, we expect `frame_names`.
// - PyQuil v4 required that `duration` was a `float`, but here we accept arbitrary `Expression`s
//   and things that can be converted into them (including `float`s);
//   however, v4 _also_ coerced the `duration` back into a `float` in the property getter,
//   which is no longer done in this version: instead, you must use `frame.duration.to_real()`.
// - PyQuil v4's `DelayFrames` and `DelayQubits` were separate classes,
//   but here we just have a single `Delay` class, but are more flexible in our constructor.
//   The other two classes are aliases to this one within PyQuil v5.
//
// This is implemented manually (rather than with `pickleable_new!`)
// because `frames` isn't a field on the underlying struct,
// and `frame_names`/`frames` accept `str`s or `FrameIdentifier`s
// rather than the plain `Vec<String>` the struct holds.
#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Delay {
    #[new]
    #[pyo3(signature = (frame_names, qubits, duration, *, frames = None))]
    fn __new__(
        py: Python<'_>,
        frame_names: Vec<FrameNameLike>,
        #[pyo3(from_py_with = from_sequence::<Qubit, _>)] qubits: Vec<Qubit>,
        duration: ExpressionLike,
        frames: Option<Vec<FrameNameLike>>,
    ) -> PyResult<Self> {
        let frame_names = deprecated_or_new!(py, new = frame_names, old = frames)?
            .into_iter()
            .map(String::from)
            .collect();

        Ok(Self::new(duration.into(), frame_names, qubits))
    }

    fn __getnewargs__(&self) -> (Vec<String>, Vec<Qubit>, Expression) {
        (
            self.frame_names.clone(),
            self.qubits.clone(),
            self.duration.clone(),
        )
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Delay {
    #[pyo3(warn(message = "use `frame_names` instead", category = PyDeprecationWarning))]
    #[getter]
    fn frames(&self) -> Vec<FrameIdentifier> {
        self.frame_names
            .iter()
            .map(|name| FrameIdentifier {
                name: name.clone(),
                qubits: self.qubits.clone(),
            })
            .collect()
    }
}

pickleable_new! {
    impl Fence {
        fn __new__(
            #[pyo3(from_py_with = from_sequence::<Qubit, _>)]
            qubits: Vec<Qubit>,
        ) -> Fence {
            Self::new(qubits)
        }
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
            (PauliGate::X, PauliGate::X)
            | (PauliGate::Y, PauliGate::Y)
            | (PauliGate::Z, PauliGate::Z) => (PauliGate::I, Complex64::new(1.0, 0.0)),
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

#[cfg(feature = "stubs")]
pyo3_stub_gen::inventory::submit! {
    gen_methods_from_python! {
        r#"
        import builtins
        import typing
        import typing_extensions

        from quil._quil.expression import ExpressionDesignator

        class PauliTerm:
            @typing.overload
            def __new__(
                cls,
                op: typing.Literal[PauliGate.I] | typing.Literal["I"],
                index: PauliTargetDesignator | None,
                coefficient: _quil.expression.ExpressionDesignator = 1.0,
            ) -> PauliTerm:
                """Construct a `PauliTerm` for a single Identity operator."""

            @typing.overload
            def __new__(
                cls,
                op: PauliGate | str,
                index: PauliTargetDesignator | None,
                coefficient: _quil.expression.ExpressionDesignator = 1.0,
            ) -> PauliTerm:
                """Construct a `PauliTerm` for a single operator and argument."""

            @typing.overload
            @typing_extensions.deprecated("this constructor is deprecated; use `PauliTerm.from_list` instead")
            def __new__(
                cls,
                arguments: collections.abc.Sequence[tuple[PauliGate | str, PauliTargetDesignator]],
                expression: _quil.expression.ExpressionDesignator = 1.0,
            ) -> PauliTerm:
                """Construct a `PauliTerm` from a sequence of arguments."""

            @typing.overload
            def __mul__(self, other: PauliTerm | _quil.expression.ExpressionDesignator) -> PauliTerm: ...
            @typing.overload
            def __mul__(self, other: PauliSum) -> PauliSum: ...

            @typing.overload
            def __add__(self, other: _quil.expression.ExpressionDesignator) -> PauliTerm: ...
            @typing.overload
            def __add__(self, other: PauliTerm | PauliSum) -> PauliSum: ...

            @typing.overload
            def __radd__(self, other: _quil.expression.ExpressionDesignator) -> PauliTerm: ...
            @typing.overload
            def __radd__(self, other: PauliTerm | PauliSum) -> PauliSum: ...

            # Note: Do **NOT** specify `float` or `complex` in these annotations,
            # because Pyright special-cases them as if `int | float` and `int | float | complex`,
            # both of which are unsound, causing overlapping overloads/errors.
            # See: https://discuss.python.org/t/clarifying-the-float-int-complex-special-case/54018/71
            @typing.overload
            def __pow__(self, exponent: builtins.int, modulo: None=None) -> PauliTerm: ...
            @typing.overload
            def __pow__(self, exponent: _quil.expression.Expression | builtins.str, modulo: None=None) -> PauliSum: ...
            @typing.overload
            def __pow__(self, exponent: typing.Any, modulo: typing.Any) -> typing.NoReturn: ...
        "#
    }
}

/// Used to extract the `exponent` in [`PauliTerm::__pow__`].
///
/// Integer arguments are more likely, and they have a more efficient implementation,
/// so we handle them separately from other things we can convert into an `Expression`.
/// See the documentation on [`PauliTerm::__pow__`] for more information.
#[derive(FromPyObject)]
enum PauliExponent {
    // Expression must come before numbers, since Expression implements __complex__.
    Expression(Expression),
    Int(i64),
    Complex(Complex64),
    Variable(String),
}

// Do not specify `float` or `complex` in this annotation.
// See notes in `PauliTerm::__pow__` for an explanation.
#[cfg(feature = "stubs")]
pyo3_stub_gen::impl_stub_type!(PauliExponent = PyInt | PyString | QualifiedExpression);

/// Apply a binary operation to two [`Expression`]s,
/// with special handling to leave the result as a `Number` when possible.
///
/// Specifically, if both operands are `Number`s,
/// the result is a `Number` with the result of the operation applied.
/// Otherwise, if the operation would mathematically be a no-op (e.g., adding 0 or dividing by 1),
/// the result is just a clone of the other operand.
/// Finally, if neither condition is true, the result is the result of `<a> <op> <b>`.
///
/// # Example Usage
///
/// ```rust,ignore
/// let (a, b) = (Complex64::new(1.0, 2.0), Complex64::new(3.0, 4.0));
/// assert_eq!(simple!(Expression::Number(a) * Expression::Number(b)), Expression::Number(a * b));
/// assert_matches!(simple!(Expression::PiConstant() * Expression::Number(b)), Expression::InfixExpression(_));
/// ```
macro_rules! simple {
    // Division is a bit special compared to other operators, so we handle it separately.
    (($a:expr) / ($b:expr)) => {
        match ($a, $b) {
            (_, Expression::Number(Complex64::ZERO)) => $a / $b,
            (Expression::Number(a), Expression::Number(b)) => Expression::Number(*a / *b),
            (a, Expression::Number(Complex64::ONE)) a,
            (a, b) => a / b,
        }
    };

    (($a:expr) ^ ($b:expr)) => {
        match ($a, $b) {
            (_, Expression::Number(Complex64::ZERO)) => Expression::Number(Complex64::ONE),
            (Expression::Number(Complex64::ONE), _) => Expression::Number(Complex64::ONE),
            (a, Expression::Number(Complex64::ONE)) => a,
            (Expression::Number(a), Expression::Number(b)) => {
                // Keep it as an expression if the result is not finite.
                let n = a.powc(b);
                if n.is_finite() {
                    Expression::Number(n)
                } else {
                    Expression::Number(a) ^ Expression::Number(b)
                }
            }
            (a, b) => a ^ b,
        }
    };

    // Handle addition, subtraction, and multiplication by comparing against the group identity.
    (($a:expr) $op:tt ($b:expr), $unit:pat) => {
        match ($a, $b) {
            (Expression::Number(a), Expression::Number(b)) => Expression::Number(a $op b),
            (a, $unit) => a,
            ($unit, b) => b,
            (a, b) => a $op b,
        }
    };

    // Look for an operator token at the head of the left list.
    (@search [* $b:expr] [$a:expr]) => { simple!(($a) * ($b), Expression::Number(Complex64::ONE)) };
    (@search [+ $b:expr] [$a:expr]) => { simple!(($a) + ($b), Expression::Number(Complex64::ZERO)) };
    (@search [- $b:expr] [$a:expr]) => { simple!(($a) - ($b), Expression::Number(Complex64::ZERO)) };
    (@search [/ $b:expr] [$a:expr]) => { simple!(($a) / ($b)) };
    (@search [^ $b:expr] [$a:expr]) => { simple!(($a) ^ ($b)) };

    // Put all the tokens in a left list, then shuffle them to the right until we find an operator.
    (@search [$head:tt $($rest:tt)*] [$($a:tt)+]) => { simple!(@search [$($rest)*] [$($a)+ $head]) };
    (@search [] [$($a:tt)*]) => { compile_error!(
        concat!("expected `<expr> <op> <expr>`; got: ", stringify!($($a),*)))
    };
    ($head:tt $($rest:tt)+) => { simple!(@search [$($rest)+] [$head]) };
}

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
    ///
    /// If the given arguments are disjoint (as required by a well-formed Quil `PauliTerm`),
    /// the resulting `PauliTerm` will preserve that order and use the given coefficient as-is.
    ///
    /// Otherwise, operations on the same argument are combined via Pauli algebra rules,
    /// and the coefficient is multiplied by any resulting complex phase,
    /// and the resulting `PauliTerm` may have a different order of arguments than the input list.
    #[pyo3(signature = (terms_list, coefficient=ExpressionLike::Expression(ONE)))]
    #[staticmethod]
    fn from_list(
        terms_list: Vec<(PauliGate, PauliArg)>,
        coefficient: ExpressionLike,
    ) -> PyResult<Self> {
        let mut result = PauliTerm::new(Vec::new(), ONE);
        for term in terms_list {
            let term = (term.0, term.1.try_into()?);
            result *= term;
        }
        result.expression = simple!(coefficient.into() * result.expression);
        Ok(result)
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
                vec![],
            )?;
            program.add_instruction(Instruction::Gate(g));
        }

        Ok(program)
    }

    /// Get the arguments of the [`PauliTerm`] as [`Qubit`]s.
    fn get_qubits(&self) -> Vec<Qubit> {
        self.arguments
            .iter()
            .map(|(_, q)| Qubit::Variable(q.clone()))
            .collect()
    }

    /// Get the [`PauliGate`] matching the argument in the [`PauliTerm`],
    /// or [`PauliGate::I`] if the argument is not present in the term.
    fn __getitem__(&self, argument: &str) -> PauliGate {
        self.arguments
            .iter()
            .find_map(
                |(gate, qubit)| {
                    if qubit == argument {
                        Some(*gate)
                    } else {
                        None
                    }
                },
            )
            .unwrap_or(PauliGate::I)
    }

    /// Iterate over the arguments in this [`PauliTerm`].
    fn __iter__(slf: Bound<'_, Self>) -> PauliArgIter {
        PauliArgIter::new(slf.unbind())
    }

    /// Return the product of this [`PauliTerm`] with another `PauliTerm`,
    /// [`PauliSum`], or number according to the Pauli algebra rules.
    fn __mul__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        if let Ok(other) = other.cast::<PauliTerm>() {
            (self.clone() * other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<PauliSum>() {
            (self.clone() * other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<Expression>() {
            (self.clone() * other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.extract::<ExpressionLike>() {
            (self.clone() * Expression::from(other)).into_bound_py_any(py)
        } else {
            other.py().NotImplemented().into_bound_py_any(py)
        }
    }

    fn __rmul__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        self.__mul__(py, other)
    }

    fn __add__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        if let Ok(other) = other.cast::<PauliTerm>() {
            (self.clone() + other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<PauliSum>() {
            (self.clone() + other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<Expression>() {
            (self.clone() + other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.extract::<ExpressionLike>() {
            (self.clone() + Expression::from(other)).into_bound_py_any(py)
        } else {
            other.py().NotImplemented().into_bound_py_any(py)
        }
    }

    fn __radd__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        self.__add__(py, other)
    }

    fn __sub__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        if let Ok(other) = other.cast::<PauliTerm>() {
            (self.clone() - other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<PauliSum>() {
            (self.clone() - other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<Expression>() {
            (self.clone() - other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.extract::<ExpressionLike>() {
            (self.clone() - Expression::from(other)).into_bound_py_any(py)
        } else {
            other.py().NotImplemented().into_bound_py_any(py)
        }
    }

    fn __rsub__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        if let Ok(other) = other.cast::<PauliTerm>() {
            (other.get().clone() - self.clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<PauliSum>() {
            (other.get().clone() - self.clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<Expression>() {
            (other.get().clone() - self.clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.extract::<ExpressionLike>() {
            (Expression::from(other) - self.clone()).into_bound_py_any(py)
        } else {
            other.py().NotImplemented().into_bound_py_any(py)
        }
    }

    /// Compute the power of this [`PauliTerm`].
    ///
    /// This returns a new [`PauliTerm`] or [`PauliSum`]
    /// representing the result of raising this term to the given exponent.
    ///
    /// As with other operations on this type,
    /// the arguments and coefficient may be rearranged and/or simplified,
    /// so logically equivalent results may not be identical one another.
    /// The simplifications and rearrangements are not guaranteed to be stable across versions,
    /// and may change the observable behavior of a program when executed on a QPU.
    ///
    /// In the common case that the exponent is a small integer, this returns a [`PauliTerm`].
    /// To handle more general exponents, this returns a two-term [`PauliSum`].
    ///
    /// # General Mathematical Explanation
    ///
    /// To be specific, for a scaled Pauli operator `T = cP` and complex `k`, `T^k = c^k * P^k`.
    /// Note that for Pauli operators, `P^2 = I` with eigenvalues `+1` and `-1`, so we can write
    /// `P^k = (1/2)(I+P)(1^k) + (1/2)(I-P)((-1)^k) = (1/2)(1+(-1)^k)*I + (1/2)(1-(-1)^k)*P`.
    /// Define `a = (1/2)(1+(-1)^k)` and `b = (1/2)(1-(-1)^k)`, so that `P^k = aI + bP`,
    /// and we can write `T^k = c^k * P^k = c^k * (aI + bP) = (c^k * a)I + (c^k * b)P`.
    /// Thus, the result is a two-term `PauliSum` with coefficients `c^k * a` and `c^k * b`.
    ///
    /// # Special Cases for Integer Exponents
    ///
    /// When `k` is an even integer, `(-1)^k = 1`, so `a = 1`, `b = 0`, and `T^k = c^k * I`.
    /// When `k` is an odd integer,  `(-1)^k = 0`, so `a = 0`, `b = 1`, and `T^k = c^k * P`.
    /// Thus, we can simplify the result to a single `PauliTerm` when given an integer exponent.
    /// If the existing term's coefficient is numeric, we simplify the result to its numeric form.
    ///
    /// Note that this only applies to exponents given as integers that fit in an `i32`;
    /// integers with larger magnitudes are converted to [`Expression::Number`]s,
    /// and [`Expression`]-based exponents are treated as described below.
    ///
    /// # Large Integer, Complex Numbers, and General Expressions
    ///
    /// Large integers, floats, and complex numbers are converted to [`Expression::Number`]s,
    /// and `str`s are converted to [`Expression::Variable`]s.
    /// and general [`Expression`]s are used directly.
    ///
    /// General [`Expression`]s are supported via the above substitutions, processed symbolically.
    /// The exact [`Expression`]s are not formally part of the API and may change between versions.
    /// At present, we evaluate `(-1)^k` with the principal branch `exp(i * pi * k)`,
    /// resulting in `a = (1 + exp(i * pi * k)) / 2` and `b = (1 - exp(i * pi * k)) / 2`;
    /// as with the numeric case, the exact [`Expression`]s produced may be simplified
    /// when the existing term's coefficient is numeric.
    ///
    /// For non-integer exponents, we
    ///
    /// evaluated via the principal branch
    /// which in general results in complex coefficients,
    /// and hence the two-term `PauliSum` result.
    ///
    /// Note that a valid Quil ``DEFGATE ... AS PAULI-SUM`` requires coefficient expressions
    /// to be real-valued and reference only real numeric literals or gate-defined parameters.
    ///
    /// If called with Python's 3-argument `pow` function, the `modulo` argument is not supported,
    /// and will raise a `NotImplementedError` if provided.
    ///
    /// Note: As explained above, this method returns `PauliTerm` for (most) integer exponents,
    /// and a `PauliSum` for (most) non-integer exponents, and accepts `float` and `complex`.
    /// Despite this, due to the way Python and type-checkers special-case them,
    /// it is not possible to express this accurately as a type annotation overload.
    /// Consequently, this is typed with an expectation that integers are the most common numbers,
    /// and also are likely to be in the range of a 32-bit signed integer,
    /// in which case the result is a `PauliTerm`.
    /// Although it cannot be annotated as such, it is valid to pass a `float` or `complex`,
    /// and the result will be a `PauliSum` (as will be true for `Expression`s and large ints).
    /// If you find yourself running into type-checking errors when passing a `float` or `complex`,
    /// you can work around it by wrapping the argument in an `Expression`.
    ///
    /// This arises because Python specifies that `int` is assignable to `float` and `complex`
    /// and that `float` is assignable to `complex`, and from the definition of `assignable`
    /// (see: https://typing.python.org/en/latest/spec/glossary.html#term-assignable),
    /// the consequence is that `int` is treated as a subtype of `float` and `complex`,
    /// leading to all kinds of unsoundness in type-checking vs actual runtime behavior.
    /// For more information, see this discussion:
    /// https://discuss.python.org/t/clarifying-the-float-int-complex-special-case/54018/71
    #[gen_stub(override_return_type(type_repr = "PauliTerm | PauliSum"))]
    fn __pow__<'py>(
        &self,
        py: Python<'py>,
        exponent: PauliExponent,
        modulo: Option<Bound<'py, PyAny>>,
    ) -> PyResult<Bound<'py, PyAny>> {
        if modulo.is_some() {
            return Err(PyNotImplementedError::new_err(
                "`modulo` is not supported for `PauliTerm.__pow__`",
            ));
        }

        fn powi(term: &PauliTerm, exponent: i64) -> PauliTerm {
            // If `k=2x`, so `P^(2x) == (P^2)^x == I^x == I`;
            // else, `k=2x+1`, so `P^(2x+1) == P^(2x) * P == I * P == P`.
            let args = if exponent % 2 == 0 {
                Vec::new()
            } else {
                term.arguments.clone()
            };

            let scalar =
                simple!(term.expression.clone() ^ Expression::Number((exponent as f64).into()));
            PauliTerm::new(args, scalar)
        }

        let expr = match exponent {
            PauliExponent::Int(exponent) => {
                return powi(self, exponent).into_bound_py_any(py);
            }

            // If the complex number is small integer, use the shortcut.
            PauliExponent::Complex(n)
                if n.im == 0.0
                    && n.re.fract() == 0.0
                    && (i64::MIN as f64) <= n.re
                    && n.re <= (i64::MAX as f64) =>
            {
                return powi(self, n.re as i64).into_bound_py_any(py);
            }

            PauliExponent::Complex(n) => Expression::Number(n),
            PauliExponent::Variable(v) => Expression::Variable(v),
            PauliExponent::Expression(expr) => expr,
        };

        // (-1)^k = exp(i * pi * k)
        let exp_i_pi_k = Expression::FunctionCall(quil_exp(
            (IMAGINARY_UNIT * Expression::PiConstant() * expr.clone()).into(),
        ));

        let a = (ONE + exp_i_pi_k.clone()) / Expression::Number(Complex64::from(2.0));
        let b = (ONE - exp_i_pi_k) / Expression::Number(Complex64::from(2.0));

        let c_pow_k = self.expression.clone() ^ expr;

        let term_ident = PauliTerm {
            arguments: Vec::new(),
            expression: c_pow_k.clone() * a,
        };

        let term_pauli = PauliTerm {
            arguments: self.arguments.clone(),
            expression: c_pow_k * b,
        };

        PauliSum {
            arguments: self.arguments().cloned().collect(),
            terms: vec![term_ident, term_pauli],
        }
        .into_bound_py_any(py)
    }

    // TODO(migration-guide):
    // - At one time, this method returned an empty string if arguments was empty,
    // but later the expected logic changed to return `"I"` in that case.
    // That happened at the same time a `sort_ops=True` argument was added,
    // which came with an explanation that both would change in the future.
    // This makes those changes: the default is now `False`, and both empty cases return `"I"`.
    //
    // - This adds a new keyword-only `delimiter` argument which is used to join the
    // operator-argument pair. By default (and for backwards compatibility), it's the empty string.
    // The `__str__` method below uses `*` to match the original output of the `__repr__` method.
    // The `__repr__` method itself is replaced with the Rust-derived default,
    // matching that of all the other classes in this crate.
    //
    // - When the afforementioned `delimiter` argument is empty (matching the original behavior),
    // if an argument contains 'X', 'Y', 'Z', or 'I', then the output would be produced ambiguous.
    // This works around it by wrapping such arguments in parentheses,
    // so the output no longer matches the original behavior.
    //
    // - The original class primarily focused on integer qubit indicies,
    // but those are not valid Quil identifiers; the constructor for this class
    // generally forces those into valid identifier by prefixing them with `q`,
    // which is to say the original output would have only had output like `X0Y1` etc,
    // but this will produce `Xq0Yq1` instead.
    //
    // - The companion method below to parses these strings is updated with these changes in mind,
    // so it can still handle unambiguous output from the original class,
    // but will also handle the more general from this updated method produces.
    //
    /// Return an identifier string for the PauliTerm (ignoring the coefficient).
    ///
    /// For example, ``PauliTerm.from_list([("X", 0), ("Y", "q")]).id() == "Xq0Yq"``.
    ///
    /// If an argument contains a character that would otherwise be interpreted as a Pauli operator
    /// (i.e., `X`, `Y`, or `Z`), its wrapped in parentheses to avoid ambiguity. For example,
    /// ``PauliTerm.from_list([("X", "X0")]).id() == "X(X0)"``.
    ///
    /// Don't use this to compare terms (use ``pt0 == pt1`` or ``hash(pt0)`` for that).
    /// You can pass ``sort_ops=True`` to sort arguments by qubit.
    ///
    /// Note that if the term has no operators, this function will return ``"I"``.
    /// If you need to check for identity, use ``term.is_identity()`` instead.
    #[pyo3(signature = (sort_ops=false, /, delimiter=""))]
    fn id(&self, sort_ops: bool, delimiter: &str) -> String {
        if self.arguments.is_empty() {
            return "I".to_string();
        }

        fn format_arg(op: &PauliGate, q: &str, delimiter: &str) -> String {
            if delimiter.is_empty() && q.contains(['X', 'Y', 'Z', 'I']) {
                format!("{op}({q})")
            } else {
                format!("{op}{q}")
            }
        }

        if sort_ops {
            self.arguments
                .iter()
                .sorted_by(|(_, a), (_, b)| a.cmp(b))
                .map(|(op, q)| format_arg(op, q, delimiter))
                .join(delimiter)
        } else {
            self.arguments
                .iter()
                .map(|(op, q)| format_arg(op, q, delimiter))
                .join(delimiter)
        }
    }

    /// Return a compact string representation of the PauliTerm.
    fn compact_str(&self) -> PyResult<String> {
        Ok(format!(
            "({})*{}",
            self.expression.to_quil()?,
            self.id(false, "")
        ))
    }

    /// Return a string representation of the PauliTerm.
    fn __str__(&self) -> PyResult<String> {
        Ok(format!(
            "{}*{}",
            self.expression.to_quil()?,
            self.id(false, "*")
        ))
    }

    #[staticmethod]
    fn from_compact_str(str_pauli_term: &str) -> PyResult<Self> {
        let mut arguments = Vec::new();
        // A valid string consists of an expression followed by a sequence terms,
        // which are each a Pauli operator and a (possibly parenthesized) qubit identifier.
        // The operators act as delimiters, as "<expr>*<op><qubit><op><qubit>...".
        let mut parts = str_pauli_term.match_indices(&['X', 'Y', 'Z', 'I']);

        fn err(msg: &str) -> PyErr {
            PyValueError::new_err(format!("Invalid compact string representation: {msg}"))
        }

        // The first part is the expression, which should have the form `(<coefficient>)*`.
        let (mut last_idx, expr_str) = parts.next().ok_or_else(|| err("no operators found"))?;
        let expression = expr_str
            .strip_prefix("(")
            .and_then(|s| s.strip_suffix(")*"))
            .ok_or_else(|| err("expected (<coefficient>)*<terms>"))?
            .trim()
            .parse::<Expression>()?;

        for (idx, qubit) in parts {
            let op_str = &str_pauli_term[last_idx..idx];
            let op = PauliGate::parse(op_str).map_err(|_| {
                PyValueError::new_err(format!("unknown operator at {idx}: {op_str}"))
            })?;
            last_idx = idx + op_str.len() + qubit.len();

            let qubit = qubit
                .trim()
                .strip_prefix("(")
                .and_then(|s| s.strip_suffix(")"))
                .unwrap_or(qubit)
                .trim();
            if qubit.is_empty() {
                return Err(err("missing qubit identifier after final operator"));
            }

            // Special-case for backwards compatbility: accept a numeric qubit identifier,
            // even though it isn't a valid Quil identifier. Prefix it with `q` to make it valid.
            let qubit = if qubit.matches(char::is_alphabetic).next().is_some() {
                qubit.to_string()
            } else {
                format!("q{qubit}")
            };

            arguments.push((op, qubit));
        }

        if !str_pauli_term[last_idx..].trim().is_empty() {
            return Err(err("trailing characters after last Pauli operator"));
        }

        Ok(PauliTerm::new(arguments, expression))
    }

    /// Return a string representation of the PauliTerm without its coefficient
    /// and with implicit qubit indices.
    ///
    /// If an iterable of qubits is provided, each character in the resulting string
    /// represents the Pauli operator acting on the corresponding qubit.
    ///
    /// ```python
    /// >>> p = PauliTerm("X", 0) * PauliTerm("Y", 1, 1.0j)
    /// >>> p.pauli_string()
    /// 'XY'
    /// >>> p.pauli_string(qubits=[0])
    /// 'X'
    /// >>> p.pauli_string(qubits=[0, 2])
    /// 'XI'
    /// ```
    #[pyo3(signature = (qubits=None))]
    fn pauli_string(&self, qubits: Option<Vec<u64>>) -> String {
        match qubits {
            None => self
                .arguments
                .iter()
                .map(|(gate, _)| gate.to_string())
                .collect::<Vec<_>>()
                .join(""),

            Some(qubits) => {
                // TODO: We should really likely have a Python-specific version of PauliTerm
                // using a HashMap<Qubit, PauliGate> rather than the Quil-specific representation.
                let mut qubit_map: HashMap<u64, PauliGate> = HashMap::new();
                for (gate, qubit) in &self.arguments {
                    if let Ok(index) = qubit.parse::<u64>() {
                        qubit_map.insert(index, *gate);
                    }
                }
                qubits
                    .iter()
                    .map(|q| {
                        qubit_map
                            .get(q)
                            .copied()
                            .unwrap_or(PauliGate::I)
                            .to_string()
                    })
                    .collect::<Vec<_>>()
                    .join("")
            }
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

    // This is the name that PyQuil v4 used for this property;
    // it's probably a better name anyway, so there's no immediate plan to deprecate it.
    /// Return a copy of the coefficient [`Expression`] of the [`PauliTerm`],
    /// aka its `expression` property.
    #[getter]
    fn coefficient(&self) -> Expression {
        self.expression.clone()
    }

    /// Supported for backwards compatibility, though only for use within PyQuil itself.
    /// This is not part of the public API and may be removed or changed at any time.
    #[pyo3(warn(message = "`_ops` is not part of the public API", category = PyUserWarning))]
    #[getter]
    fn _ops<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyDict>> {
        self.arguments.iter().cloned().into_py_dict(py)
    }
}

impl std::ops::Add<Expression> for PauliTerm {
    type Output = PauliTerm;

    fn add(self, rhs: Expression) -> Self::Output {
        PauliTerm {
            arguments: self.arguments,
            expression: simple!(self.expression + rhs),
        }
    }
}

impl std::ops::Sub<Expression> for PauliTerm {
    type Output = PauliTerm;

    fn sub(self, rhs: Expression) -> Self::Output {
        PauliTerm {
            arguments: self.arguments,
            expression: simple!(self.expression - rhs),
        }
    }
}

impl std::ops::Sub<PauliTerm> for Expression {
    type Output = PauliTerm;

    fn sub(self, rhs: PauliTerm) -> Self::Output {
        PauliTerm {
            arguments: rhs.arguments,
            expression: simple!(self - rhs.expression),
        }
    }
}

impl std::ops::Add<PauliTerm> for PauliTerm {
    type Output = PauliSum;

    fn add(self, rhs: PauliTerm) -> Self::Output {
        let terms = vec![self, rhs];
        let arguments = PauliSum::into_args(&terms);
        PauliSum { arguments, terms }
    }
}

impl std::ops::Sub<PauliTerm> for PauliTerm {
    type Output = PauliSum;

    fn sub(self, rhs: PauliTerm) -> Self::Output {
        self + (Expression::Number(-Complex64::ONE) * rhs)
    }
}

impl std::ops::Add<PauliSum> for PauliTerm {
    type Output = PauliSum;

    fn add(self, rhs: PauliSum) -> Self::Output {
        // Technically, this puts `self` at the end of the `PauliTerm` list,
        rhs + self
    }
}

impl std::ops::Sub<PauliSum> for PauliTerm {
    type Output = PauliSum;

    fn sub(self, rhs: PauliSum) -> Self::Output {
        self + (Expression::Number(-Complex64::ONE) * rhs)
    }
}

impl std::ops::Mul<Expression> for PauliTerm {
    type Output = PauliTerm;

    fn mul(self, rhs: Expression) -> Self::Output {
        PauliTerm {
            arguments: self.arguments,
            expression: simple!(self.expression * rhs),
        }
    }
}

impl std::ops::Mul<PauliTerm> for Expression {
    type Output = PauliTerm;

    fn mul(self, rhs: PauliTerm) -> Self::Output {
        PauliTerm {
            arguments: rhs.arguments,
            expression: simple!(self * rhs.expression),
        }
    }
}

impl std::ops::Mul<PauliTerm> for PauliTerm {
    type Output = PauliTerm;

    fn mul(self, rhs: PauliTerm) -> Self::Output {
        let mut result = self;
        for pair in rhs.arguments.into_iter() {
            result *= pair;
        }
        result * rhs.expression
    }
}

impl std::ops::Mul<PauliSum> for PauliTerm {
    type Output = PauliSum;

    fn mul(self, rhs: PauliSum) -> Self::Output {
        PauliSum::mul(rhs, self)
    }
}

impl std::ops::Add<Expression> for PauliSum {
    type Output = PauliSum;

    fn add(self, rhs: Expression) -> Self::Output {
        let PauliSum {
            arguments,
            mut terms,
        } = self;
        terms.push(PauliTerm {
            arguments: Vec::new(),
            expression: rhs,
        });
        PauliSum { arguments, terms }
    }
}

impl std::ops::Add<PauliTerm> for PauliSum {
    type Output = PauliSum;

    fn add(self, rhs: PauliTerm) -> Self::Output {
        let PauliSum {
            mut arguments,
            mut terms,
        } = self;
        for (_, qubit) in rhs.arguments.iter() {
            if !arguments.contains(qubit) {
                arguments.push(qubit.clone());
            }
        }
        terms.push(rhs);
        PauliSum { arguments, terms }
    }
}

impl std::ops::Add<PauliSum> for PauliSum {
    type Output = PauliSum;

    fn add(self, rhs: PauliSum) -> Self::Output {
        let PauliSum {
            mut arguments,
            mut terms,
        } = self;

        for arg in rhs.arguments {
            if !arguments.contains(&arg) {
                arguments.push(arg);
            }
        }

        terms.extend(rhs.terms);
        PauliSum { arguments, terms }
    }
}

impl std::ops::Sub<Expression> for PauliSum {
    type Output = PauliSum;

    fn sub(self, rhs: Expression) -> Self::Output {
        let PauliSum {
            arguments,
            mut terms,
        } = self;
        terms.push(PauliTerm {
            arguments: Vec::new(),
            expression: simple!(Expression::Number(-Complex64::ONE) * rhs),
        });
        PauliSum { arguments, terms }
    }
}

impl std::ops::Sub<PauliSum> for Expression {
    type Output = PauliSum;

    fn sub(self, rhs: PauliSum) -> Self::Output {
        (Expression::Number(-Complex64::ONE) * rhs) + self
    }
}

impl std::ops::Sub<PauliTerm> for PauliSum {
    type Output = PauliSum;

    fn sub(self, rhs: PauliTerm) -> Self::Output {
        self + (Expression::Number(-Complex64::ONE) * rhs)
    }
}

impl std::ops::Sub<PauliSum> for PauliSum {
    type Output = PauliSum;

    fn sub(self, rhs: PauliSum) -> Self::Output {
        self + (Expression::Number(-Complex64::ONE) * rhs)
    }
}

impl std::ops::Mul<Expression> for PauliSum {
    type Output = PauliSum;

    fn mul(self, rhs: Expression) -> Self::Output {
        if rhs == ONE {
            self
        } else if rhs == ZERO {
            PauliSum {
                arguments: self.arguments,
                terms: Vec::new(),
            }
        } else {
            let PauliSum { arguments, terms } = self;
            let terms = terms.into_iter().map(|term| term * rhs.clone()).collect();
            PauliSum { arguments, terms }
        }
    }
}

impl std::ops::Mul<PauliSum> for Expression {
    type Output = PauliSum;

    fn mul(self, rhs: PauliSum) -> Self::Output {
        PauliSum::mul(rhs, self)
    }
}

impl std::ops::Mul<PauliTerm> for PauliSum {
    type Output = PauliSum;

    fn mul(self, rhs: PauliTerm) -> Self::Output {
        let mut terms = self.terms;
        let mut unique_args = HashSet::new();

        for term in terms.iter_mut() {
            let mut new_term = rhs.clone();
            for pair in term.arguments.iter() {
                new_term *= pair.clone();
            }
            *term = new_term;

            for (_, qubit) in term.arguments.iter() {
                unique_args.insert(qubit);
            }
        }

        let arguments = unique_args.into_iter().cloned().collect();
        PauliSum { arguments, terms }
    }
}

impl std::ops::Mul<PauliSum> for PauliSum {
    type Output = PauliSum;

    fn mul(self, rhs: PauliSum) -> Self::Output {
        let mut terms = Vec::new();
        let mut unique_args = HashSet::new();

        for term1 in self.terms.iter() {
            for term2 in rhs.terms.iter() {
                let new_term = term1.clone() * term2.clone();
                for (_, qubit) in new_term.arguments.iter() {
                    if !unique_args.contains(qubit) {
                        unique_args.insert(qubit.clone());
                    }
                }
                terms.push(new_term);
            }
        }

        let arguments = unique_args.into_iter().collect();
        PauliSum { arguments, terms }
    }
}

impl std::ops::MulAssign<(PauliGate, String)> for PauliTerm {
    /// Return the product of this [`PauliTerm`] with a single `(PauliGate, String)` pair;
    /// no guarantees are made about the order of the resulting arguments.
    fn mul_assign(&mut self, rhs: (PauliGate, String)) {
        let (op, qubit) = rhs;

        // Check if this qubit is already present in this term.
        // Assume we're not likely to have many arguments; if benchmarking proves this to be slow,
        // we can introduce a Python-specific version of `PauliTerm` that uses a `HashMap`,
        // or we can just enforce that `arguments` is always sorted, then use binary search.
        match self.arguments.iter().position(|(_, q)| q == &qubit) {
            Some(idx) => {
                let (new_op, phase) = self.arguments[idx].0.product(op);
                if new_op == PauliGate::I {
                    self.arguments.swap_remove(idx);
                } else {
                    self.arguments[idx] = (new_op, qubit);
                    // Take the expression out of self so we expand simple! without cloning.
                    let expr = mem::replace(&mut self.expression, Expression::PiConstant());
                    self.expression = simple!(expr * Expression::Number(phase))
                }
            }
            None => {
                self.arguments.push((op, qubit));
            }
        };
    }
}

/// An iterator over the qubit indices and Pauli operators in a [`PauliTerm`].
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(module = "quil._quil.instructions", frozen)]
struct PauliArgIter {
    // Using `Py<_>` avoids cloning the entire `PauliTerm`,
    // and since that class is frozen, we can skip all the Python reference counting
    // by using an atomic index.
    // Note that individual `(PauliGate, String)` pairs still must be cloned when iterated.
    inner: Py<PauliTerm>,
    index: AtomicUsize,
}

impl PauliArgIter {
    fn new(inner: Py<PauliTerm>) -> Self {
        Self {
            inner,
            index: AtomicUsize::new(0),
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PauliArgIter {
    fn __iter__(slf: PyRef<'_, Self>) -> PyRef<'_, Self> {
        slf
    }

    #[gen_stub(override_return_type(type_repr = "builtins.tuple[PauliGate, builtins.str]", imports = ("builtins")))]
    fn __next__(slf: PyRef<'_, Self>) -> Option<(PauliGate, String)> {
        slf.inner
            .get()
            .arguments
            .get(slf.index.fetch_add(1, Ordering::Relaxed))
            .cloned()
    }

    #[gen_stub(skip)]
    fn __traverse__(&self, visit: pyo3::PyVisit) -> Result<(), pyo3::PyTraverseError> {
        visit.call(&self.inner)
    }
}

/// An iterator over the [`PauliTerm`]s of a [`PauliSum`].
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(module = "quil._quil.instructions", frozen)]
struct PauliTermIter {
    inner: Py<PauliSum>,
    index: AtomicUsize,
}

impl PauliTermIter {
    fn new(inner: Py<PauliSum>) -> Self {
        Self {
            inner,
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

    #[gen_stub(override_return_type(type_repr = "PauliTerm"))]
    fn __next__(slf: PyRef<'_, Self>) -> Option<PauliTerm> {
        slf.inner
            .get()
            .terms
            .get(slf.index.fetch_add(1, Ordering::Relaxed))
            .cloned()
    }

    #[gen_stub(skip)]
    fn __traverse__(&self, visit: pyo3::PyVisit) -> Result<(), pyo3::PyTraverseError> {
        visit.call(&self.inner)
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
        from quil import _quil

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
            (Some(Migrate::New(terms)), None, None) | (None, Some(Migrate::New(terms)), None) => {
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
            (Some(Migrate::Old(_)), None, Some(_)) | (_, Some(Migrate::Old(_)), _) => Err(
                PyTypeError::new_err("`terms` must be a list of `PauliTerm`s"),
            ),

            // Given two lists of `PauliTerm`s, but one should be `arguments`.
            (Some(Migrate::New(_)), Some(Migrate::New(_)), None) => {
                Err(PyTypeError::new_err("`arguments` must be a list of `str`s"))
            }

            (Some(_), Some(_), Some(_)) => Err(PyValueError::new_err(
                "too many arguments; use `PauliSum(terms, arguments)`",
            )),
        }
    }

    fn __getnewargs__(&self) -> (Vec<PauliTerm>, Vec<String>) {
        (self.terms.clone(), self.arguments.clone())
    }

    // TODO(migration-guide): The `__repr__` method is replaced with the Rust-derived default,
    // which is more consistent with the other classes in this crate.
    // This __str__ method replaces the original `__repr__`.
    /// Return a string representation of the PauliSum.
    fn __str__(&self) -> PyResult<String> {
        let terms_str = self
            .terms
            .iter()
            .map(|term| term.compact_str())
            .collect::<Result<Vec<_>, _>>()?
            .join(" + ");
        Ok(format!("PauliSum({})", terms_str))
    }

    /// Return the number of terms in this [`PauliSum`].
    fn __len__(&self) -> usize {
        self.terms.len()
    }

    fn __getitem__<'py>(&self, py: Python<'py>, index: isize) -> PyResult<Bound<'py, PyAny>> {
        let len = self.terms.len() as isize;
        let index = if index < 0 { len + index } else { index };
        if index < 0 || index >= len {
            Err(PyIndexError::new_err("index out of range"))
        } else {
            self.terms[index as usize].clone().into_bound_py_any(py)
        }
    }

    // TODO: is this just going to clone the PauliSum anyway?
    /// Iterate over the [`PauliTerm`]s in this [`PauliSum`].
    fn __iter__(slf: Bound<'_, Self>) -> PauliTermIter {
        PauliTermIter::new(slf.unbind())
    }

    fn __add__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        if let Ok(other) = other.cast::<PauliTerm>() {
            (self.clone() + other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<PauliSum>() {
            (self.clone() + other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<Expression>() {
            (self.clone() + other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.extract::<ExpressionLike>() {
            (self.clone() + Expression::from(other)).into_bound_py_any(py)
        } else {
            other.py().NotImplemented().into_bound_py_any(py)
        }
    }

    fn __radd__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        self.__add__(py, other)
    }

    fn __mul__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        if let Ok(other) = other.cast::<PauliTerm>() {
            (self.clone() * other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<PauliSum>() {
            (self.clone() * other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<Expression>() {
            (self.clone() * other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.extract::<ExpressionLike>() {
            (self.clone() * Expression::from(other)).into_bound_py_any(py)
        } else {
            other.py().NotImplemented().into_bound_py_any(py)
        }
    }

    fn __rmul__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        self.__mul__(py, other)
    }

    fn __pow__(&self, exponent: u64, modulo: Option<Bound<'_, PyAny>>) -> PyResult<Self> {
        if modulo.is_some() {
            return Err(PyNotImplementedError::new_err(
                "`modulo` is not supported for `PauliSum.__pow__`",
            ));
        }

        if exponent == 0 {
            return Ok(PauliSum {
                arguments: Vec::new(),
                terms: Vec::new(),
            });
        }

        let mut result = self.clone();
        for _ in 1..exponent {
            result = result * self.clone();
        }
        Ok(result)
    }

    fn __sub__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        if let Ok(other) = other.cast::<PauliTerm>() {
            (self.clone() - other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<PauliSum>() {
            (self.clone() - other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<Expression>() {
            (self.clone() - other.get().clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.extract::<ExpressionLike>() {
            (self.clone() - Expression::from(other)).into_bound_py_any(py)
        } else {
            other.py().NotImplemented().into_bound_py_any(py)
        }
    }

    fn __rsub__<'py>(
        &self,
        py: Python<'py>,
        other: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        if let Ok(other) = other.cast::<PauliTerm>() {
            (other.get().clone() - self.clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<PauliSum>() {
            (other.get().clone() - self.clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.cast::<Expression>() {
            (other.get().clone() - self.clone()).into_bound_py_any(py)
        } else if let Ok(other) = other.extract::<ExpressionLike>() {
            (Expression::from(other) - self.clone()).into_bound_py_any(py)
        } else {
            other.py().NotImplemented().into_bound_py_any(py)
        }
    }

    // TODO(migration-guide): This used to return `Qubit`s.
    /// Get a list of all the qubits in the sum of the terms.
    fn get_qubits(&self) -> Vec<String> {
        self.arguments.clone()
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

/// Accept an existing `PragmaArgument`, or a bare `int`/`str`,
/// converting the latter into the appropriate variant.
impl<'a, 'py> FromPyObject<'a, 'py> for PragmaArgument {
    type Error = PyErr;

    fn extract(obj: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        if let Ok(value) = obj.cast::<PragmaArgument>() {
            Ok(value.get().clone())
        } else if let Ok(value) = obj.cast::<PyInt>() {
            Ok(PragmaArgument::Integer(value.extract::<u64>()?))
        } else if let Ok(value) = obj.cast::<PyString>() {
            Ok(PragmaArgument::Identifier(value.extract::<String>()?))
        } else {
            Err(PyTypeError::new_err(
                "expected a PragmaArgument, int, or str",
            ))
        }
    }
}

// TODO(migration-guide):
// - PyQuil v4 accepted `Qubit | FormalArgument | int | str` items for `args`;
//   only integers and identifiers (`str`) can be given directly as `PragmaArgument`s here.
// - PyQuil v4's `Pragma` took `command` and `freeform_string` instead of `name` and `data`.
//
// This is implemented manually (rather than with `pickleable_new!`) because `command`
// and `freeform_string` aren't fields on the underlying struct.
#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Pragma {
    /// Initialize a new PRAGMA instruction.
    #[new]
    #[pyo3(signature = (name, args = Vec::new(), data = None, *, command = None, freeform_string = None))]
    fn __new__(
        py: Python<'_>,
        name: String,
        args: Vec<PragmaArgument>,
        data: Option<String>,
        command: Option<String>,
        freeform_string: Option<String>,
    ) -> PyResult<Self> {
        let name = deprecated_or_new!(py, new = name, old = command)?;

        let data =
            match deprecated_or_new!(py, new = data, old = freeform_string, |old| Ok(Some(old)))? {
                None => None,
                Some(s) if s.is_empty() => {
                    py_deprecated!(
                        py,
                        c"providing `data` as an empty string is deprecated; use `None` instead"
                    )?;
                    None
                }
                some => some,
            };

        Ok(Self::new(name, args, data))
    }

    fn __getnewargs__(&self) -> (String, Vec<PragmaArgument>, Option<String>) {
        (self.name.clone(), self.arguments.clone(), self.data.clone())
    }

    #[pyo3(warn(message = "use `name` instead", category = PyDeprecationWarning))]
    #[getter]
    fn command(&self) -> String {
        self.name.clone()
    }

    #[pyo3(warn(message = "use `data` instead", category = PyDeprecationWarning))]
    #[getter]
    fn freeform_string(&self) -> Option<String> {
        self.data.clone()
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

    impl_stub_type!(
        ArithmeticOperandLike =
            ArithmeticOperand | i64 | f64 | MemoryReference | DeclarationAt | Declaration
    );

    impl_stub_type!(
        BinaryOperandLike = BinaryOperand | i64 | MemoryReference | DeclarationAt | Declaration
    );

    impl_stub_type!(
        ComparisonOperandLike<'_> =
            ComparisonOperand | i64 | f64 | MemoryReference | DeclarationAt | Declaration
    );

    impl_stub_type!(
        MemoryReferenceLike = MemoryReference | DeclarationAt | Declaration | (String, u64)
    );
    impl_stub_type!(PyMemRef<'_> = MemoryReferenceLike);

    impl_stub_type!(LabelTargetLike<'_> = String | Label | Target);

    impl_stub_type!(GateModifierDesignator = GateModifier | String);

    impl_stub_type!(PauliArg = String | u64 | Qubit);

    impl_stub_type!(FrameNameLike = String | FrameIdentifier);

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

// TODO(migration-guide):
// - PyQuil v4 called `Store` `ClassicalStore`.
// - The signature was `(target, left, right)`.
pickleable_new! {
    impl Store {
        fn __new__(
            destination: String,
            offset: MemoryReference as MemoryReferenceLike,
            source: ArithmeticOperand as ArithmeticOperandLike,
        ) -> Store {
            Self::new(destination, offset.into(), source.into())
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Store {
    #[pyo3(warn(message = "use `destination` instead", category = PyDeprecationWarning))]
    #[getter]
    fn target(&self) -> String {
        self.destination.clone()
    }

    #[pyo3(warn(message = "use `offset` instead", category = PyDeprecationWarning))]
    #[getter]
    fn left(&self) -> MemoryReference {
        self.offset.clone()
    }

    #[pyo3(warn(message = "use `source` instead", category = PyDeprecationWarning))]
    #[getter]
    fn right(&self) -> ArithmeticOperand {
        self.source.clone()
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

// TODO(migration-guide):
// - PyQuil v4 called `UnaryLogic` `UnaryClassicalInstruction`
// - The constructor signature just took `target`; `op` was a classvar.
pickleable_new! {
    impl UnaryLogic {
        fn __new__(
            operator: UnaryOperator,
            operand: MemoryReference as MemoryReferenceLike,
        ) -> UnaryLogic {
            Self::new(operator, operand.into())
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl UnaryLogic {
    #[pyo3(warn(message = "use `operator` instead", category = PyDeprecationWarning))]
    #[getter]
    fn op(&self) -> UnaryOperator {
        self.operator
    }

    #[pyo3(warn(message = "use `operand` instead", category = PyDeprecationWarning))]
    #[getter]
    fn target(&self) -> MemoryReference {
        self.operand.clone()
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

// TODO(asaites): add v5 alias from DefWaveform to WaveformDefinition.
// TODO(migration-guide): PyQuil v4's `DefWaveform` took `parameters` (a list of `Parameter`s,
// here just their names) and `entries` directly, building the `quil_rs.Waveform` internally.
//
// This is implemented manually (rather than with `pickleable_new!`)
// because `parameters` and `entries` aren't fields on the underlying struct;
// use `WaveformDefinition.quil` to construct one directly from a `name` and a `definition`.
#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl WaveformDefinition {
    #[new]
    fn __new__(
        name: String,
        parameters: Vec<String>,
        #[pyo3(from_py_with = from_sequence::<ExpressionLike, _>)] entries: Vec<Expression>,
    ) -> Self {
        Self::new(name, Waveform::new(entries, parameters))
    }

    fn __getnewargs__(&self) -> (String, Vec<String>, Vec<Expression>) {
        (
            self.name.clone(),
            self.definition.parameters.clone(),
            self.definition.matrix.clone(),
        )
    }

    /// Construct a `WaveformDefinition` directly from a `name` and existing waveform `definition`.
    #[staticmethod]
    fn from_waveform(name: String, definition: Waveform) -> Self {
        Self::new(name, definition)
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
