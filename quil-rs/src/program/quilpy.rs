use std::collections::{HashMap, HashSet};
use std::str::FromStr;

use numpy::{Complex64, PyArray2, ToPyArray};
use pyo3::types::PyTuple;
use pyo3::{
    prelude::*,
    types::{PyBytes, PyFunction, PyRange},
};

#[cfg(feature = "stubs")]
use pyo3_stub_gen::{
    derive::{gen_stub_pyclass, gen_stub_pymethods},
    PyStubType,
};

use crate::{impl_repr, impl_to_quil};
use crate::{
    instruction::{
        Calibration, Declaration, FrameAttributes, FrameIdentifier, Gate, Instruction,
        MeasureCalibrationDefinition, Measurement, QubitPlaceholder, TargetPlaceholder,
    },
    quil::Quil,
};

use super::{
    analysis::{
        BasicBlock, BasicBlockOwned, BasicBlockScheduleError, BasicBlockTerminator,
        ControlFlowGraph, ControlFlowGraphOwned, QubitGraph, QubitGraphError,
    },
    scheduling::{ComputedScheduleItem, Schedule, Seconds, TimeSpan},
    CalibrationExpansion, CalibrationSource, Calibrations, FrameSet, InstructionIndex,
    MaybeCalibrationExpansion, MemoryRegion, Program, ProgramCalibrationExpansion, Result,
    SourceMap, SourceMapEntry,
};

#[pymodule]
#[pyo3(name = "program", module = "quil", submodule)]
pub(crate) fn init_submodule(m: &Bound<'_, PyModule>) -> PyResult<()> {
    use crate::quilpy::errors;

    let py = m.py();
    m.add("ProgramError", py.get_type::<errors::ProgramError>())?;
    m.add(
        "ComputedScheduleError",
        py.get_type::<errors::ComputedScheduleError>(),
    )?;
    m.add(
        "BasicBlockScheduleError",
        py.get_type::<errors::BasicBlockScheduleError>(),
    )?;
    m.add("QubitGraphError", py.get_type::<errors::QubitGraphError>())?;

    m.add_class::<BasicBlockOwned>()?; // Python name: BasicBlock
    m.add_class::<CalibrationExpansion>()?;
    m.add_class::<PyCalibrationExpansionSourceMap>()?; // Python: CalibrationExpansionSourceMap
    m.add_class::<PyCalibrationExpansionSourceMapEntry>()?; // Python: CalibrationExpansionSourceMapEntry
    m.add_class::<Calibrations>()?; // Python: CalibrationSet
    m.add_class::<CalibrationSource>()?;
    m.add_class::<ControlFlowGraphOwned>()?; // Python: ControlFlowGraph
    m.add_class::<FrameSet>()?;
    m.add_class::<MaybeCalibrationExpansion>()?;
    m.add_class::<MemoryRegion>()?;
    m.add_class::<Program>()?;
    m.add_class::<ProgramCalibrationExpansion>()?;
    m.add_class::<PyProgramCalibrationExpansionSourceMap>()?;
    m.add_class::<PyProgramCalibrationExpansionSourceMapEntry>()?;
    m.add_class::<PyScheduleSeconds>()?;
    m.add_class::<ScheduleSecondsItem>()?;
    m.add_class::<TimeSpanSeconds>()?;

    Ok(())
}

impl_repr!(BasicBlockOwned);
impl_repr!(CalibrationExpansion);
impl_repr!(PyCalibrationExpansionSourceMap);
impl_repr!(PyCalibrationExpansionSourceMapEntry);
impl_repr!(Calibrations);
impl_repr!(CalibrationSource);
impl_repr!(ControlFlowGraphOwned);
impl_repr!(FrameSet);
impl_repr!(MaybeCalibrationExpansion);
impl_repr!(MemoryRegion);
impl_repr!(Program);
impl_repr!(ProgramCalibrationExpansion);
impl_repr!(PyProgramCalibrationExpansionSourceMap);
impl_repr!(PyProgramCalibrationExpansionSourceMapEntry);
impl_repr!(PyScheduleSeconds);
impl_repr!(ScheduleSecondsItem);
impl_repr!(TimeSpanSeconds);

impl_to_quil!(Program);

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Program {
    /// Parse a ``Program`` from a string.
    ///
    /// Raises a `ProgramError`` if the string isn't a valid Quil expression.
    #[staticmethod]
    fn parse(input: &str) -> Result<Self> {
        <Self as std::str::FromStr>::from_str(input)
    }

    #[getter(body_instructions)]
    fn py_body_instructions(&self) -> Vec<Instruction> {
        self.instructions.to_vec()
    }

    /// Return a deep copy of the `Program`.
    fn copy(&self) -> Self {
        self.clone()
    }

    fn control_flow_graph(&self) -> ControlFlowGraphOwned {
        ControlFlowGraphOwned::from(ControlFlowGraph::from(self))
    }

    // TODO: (9b9690d4 Marquess Valdez 2023-06-01 14:10:25 -0700) Should this filtering move to Program? Should we assume memory_regions will always make up all
    // declarations and simplify this?
    #[getter]
    fn declarations(&self) -> HashMap<String, Declaration> {
        self.to_instructions()
            .into_iter()
            .filter_map(|inst| match inst {
                Instruction::Declaration(declaration) => {
                    Some((declaration.name.clone(), declaration))
                }
                _ => None,
            })
            .collect()
    }

    #[pyo3(name = "expand_calibrations_with_source_map")]
    fn py_expand_calibrations_with_source_map(&self) -> Result<ProgramCalibrationExpansion> {
        self.expand_calibrations_with_source_map()
    }

    #[pyo3(name = "add_instructions")]
    fn py_add_instructions(&mut self, instructions: Vec<Instruction>) {
        self.add_instructions(instructions)
    }

    /// Return a new ``Program`` containing only the instructions
    /// for which `predicate` returns true.
    #[pyo3(name = "filter_instructions")]
    fn py_filter_instructions<'py>(
        &self,
        py: Python<'py>,
        #[gen_stub(
            override_type(
                type_repr="collections.abc.Callable[[Instruction], bool]",
                imports=("collections.abc")
            )
        )]
        predicate: &Bound<'py, PyFunction>,
    ) -> Self {
        self.filter_instructions(|inst| {
            predicate
                .call1((inst.clone().into_pyobject(py).unwrap(),))
                .unwrap_or_else(|err| panic!("predicate function returned an error: {err}"))
                .extract()
                .unwrap_or_else(|err| panic!("predicate function must return a bool: {err}"))
        })
    }

    // Because we can't bubble up an error from inside the closures, they panic when the given
    // Python functions return an error or an unexpected type. This is unusual, but in a Python
    // program, this function will only raise because [`pyo3`] wraps Rust panics in a
    // `PanicException`.
    #[pyo3(
        name = "resolve_placeholders_with_custom_resolvers",
        signature = (*, target_resolver = None, qubit_resolver = None)
    )]
    fn py_resolve_placeholders_with_custom_resolvers(
        &mut self,
        target_resolver: Option<PyTargetResolver>,
        qubit_resolver: Option<PyQubitResolver>,
    ) {
        #[allow(clippy::type_complexity)]
        let rs_qubit_resolver: Box<dyn Fn(&QubitPlaceholder) -> Option<u64>> =
            if let Some(resolver) = qubit_resolver {
                Box::new(move |placeholder: &QubitPlaceholder| -> Option<u64> {
                    Python::with_gil(|py| {
                        let placeholder = placeholder
                            .clone()
                            .into_pyobject(py)
                            .expect("QubitPlaceholder.into_python() should be infallible");
                        let resolved_qubit =
                            resolver.0.call1(py, (placeholder,)).unwrap_or_else(|err| {
                                panic!("qubit_resolver returned an error: {err}")
                            });
                        resolved_qubit.extract(py).unwrap_or_else(|err| {
                            panic!("qubit_resolver must return None or int: {err}")
                        })
                    })
                })
            } else {
                self.default_qubit_resolver()
            };

        #[allow(clippy::type_complexity)]
        let rs_target_resolver: Box<dyn Fn(&TargetPlaceholder) -> Option<String>> =
            if let Some(resolver) = target_resolver {
                Box::new(move |placeholder: &TargetPlaceholder| -> Option<String> {
                    Python::with_gil(|py| {
                        let placeholder = placeholder
                            .clone()
                            .into_pyobject(py)
                            .expect("TargetPlaceholder.into_python() should be infallible");
                        let resolved_target =
                            resolver.0.call1(py, (placeholder,)).unwrap_or_else(|err| {
                                panic!("target_resolver returned an error: {err}")
                            });
                        resolved_target.extract(py).unwrap_or_else(|err| {
                            panic!("target_resolver must return None or str: {err}")
                        })
                    })
                })
            } else {
                self.default_target_resolver()
            };

        self.resolve_placeholders_with_custom_resolvers(rs_target_resolver, rs_qubit_resolver);
    }

    /// Return the unitary of a program.
    ///
    /// # Errors
    ///
    /// Returns an error if the program contains instructions other than `Gate`s.
    #[pyo3(name = "to_unitary")]
    fn py_to_unitary<'py>(
        &self,
        py: Python<'py>,
        n_qubits: u64,
    ) -> PyResult<Bound<'py, PyArray2<Complex64>>> {
        Ok(self.to_unitary(n_qubits)?.to_pyarray(py))
    }

    fn __add__(&self, rhs: Self) -> Self {
        // TODO: Since we already own `rhs` (and thus must have cloned it),
        // it might be cheaper if we can implement this prepending `self`'s content instead.
        // If users are regularly adding a small program to a larger one,
        // (say, if they're appending a common program to an initial set of calibrations),
        // the performance impact might be non-trivial.
        self.clone() + rhs
    }

    fn __iadd__(&mut self, rhs: Self) {
        *self += rhs
    }

    /// This will raise an error if the program contains any unresolved
    /// placeholders. This is because they can't be converted to valid quil,
    /// nor can they be serialized and deserialized in a consistent way.
    fn __getstate__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyBytes>> {
        Ok(PyBytes::new(py, self.to_quil()?.as_bytes()))
    }

    pub fn __setstate__(&mut self, state: &Bound<'_, PyBytes>) -> PyResult<()> {
        *self = Program::from_str(std::str::from_utf8(state.as_bytes())?)?;
        Ok(())
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Calibrations {
    #[new]
    fn new(
        calibrations: Vec<Calibration>,
        measure_calibrations: Vec<MeasureCalibrationDefinition>,
    ) -> Self {
        Self {
            calibrations: calibrations.into(),
            measure_calibrations: measure_calibrations.into(),
        }
    }

    /// Return a list of all [`Calibration`]s in the set.
    #[getter(calibrations)]
    fn py_calibrations(&self) -> Vec<Calibration> {
        self.iter_calibrations().cloned().collect()
    }

    /// Return a list of all [`MeasureCalibrationDefinition`]s in the set.
    #[getter(measure_calibrations)]
    fn py_measure_calibrations(&self) -> Vec<MeasureCalibrationDefinition> {
        self.iter_measure_calibrations().cloned().collect()
    }

    /// Given an instruction, return the instructions to which it is expanded if there is a match.
    /// Recursively calibrate instructions, returning an error if a calibration directly or indirectly
    /// expands into itself.
    ///
    /// Return only the expanded instructions; for more information about the expansion process,
    /// see [`Self::expand_with_detail`].
    #[pyo3(name = "expand")]
    fn py_expand(
        &self,
        instruction: &Instruction,
        previous_calibrations: Vec<Instruction>,
    ) -> Result<Option<Vec<Instruction>>> {
        self.expand(instruction, &previous_calibrations)
    }

    /// Returns the last-specified ``MeasureCalibrationDefinition`` that matches the target
    /// qubit (if any), or otherwise the last-specified one that specified no qubit.
    ///
    /// If multiple calibrations match the measurement, the precedence is as follows:
    ///
    ///   1. Match fixed qubit.
    ///   2. Match variable qubit.
    ///   3. Match no qubit.
    ///
    /// In the case of multiple calibrations with equal precedence, the last one wins.
    #[pyo3(name = "get_match_for_measurement")]
    fn py_get_match_for_measurement(
        &self,
        measurement: &Measurement,
    ) -> Option<MeasureCalibrationDefinition> {
        self.get_match_for_measurement(measurement).cloned()
    }

    /// Return the final calibration which matches the gate per the QuilT specification:
    ///
    /// A calibration matches a gate if:
    /// 1. It has the same name
    /// 2. It has the same modifiers
    /// 3. It has the same qubit count (any mix of fixed & variable)
    /// 4. It has the same parameter count (both specified and unspecified)
    /// 5. All fixed qubits in the calibration definition match those in the gate
    /// 6. All specified parameters in the calibration definition match those in the gate
    #[pyo3(name = "get_match_for_gate")]
    fn py_get_match_for_gate(&self, gate: &Gate) -> Option<Calibration> {
        self.get_match_for_gate(gate).cloned()
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[pymethods]
impl CalibrationExpansion {
    #[gen_stub(override_return_type(type_repr="range", imports=()))]
    #[getter(range)]
    pub fn py_range<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyRange>> {
        PyRange::new(
            py,
            self.range.start.0.try_into()?,
            self.range.end.0.try_into()?,
        )
    }

    #[getter(expansions)]
    fn py_expansions(&self) -> PyCalibrationExpansionSourceMap {
        PyCalibrationExpansionSourceMap(self.expansions.clone())
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl ProgramCalibrationExpansion {
    #[getter(source_map)]
    fn py_source_map(&self) -> PyProgramCalibrationExpansionSourceMap {
        PyProgramCalibrationExpansionSourceMap(self.source_map.clone())
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl FrameSet {
    #[pyo3(name = "get")]
    fn py_get(&self, identifier: &FrameIdentifier) -> Option<FrameAttributes> {
        self.get(identifier).cloned()
    }

    #[pyo3(name = "get_keys")]
    fn py_get_keys(&self) -> Vec<FrameIdentifier> {
        self.get_keys().into_iter().cloned().collect()
    }

    fn get_all_frames(&self) -> HashMap<FrameIdentifier, FrameAttributes> {
        self.frames.clone()
    }

    /// Return a new [`FrameSet`] which describes only the given [`FrameIdentifier`]s.
    #[pyo3(name = "intersection")]
    pub fn py_intersection(&self, identifiers: HashSet<FrameIdentifier>) -> Self {
        self.intersection(&identifiers)
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl CalibrationSource {
    #[gen_stub(override_return_type(
        type_repr = "tuple[CalibrationIdentifier | MeasureCalibrationIdentifier]"
    ))]
    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        match self {
            Self::Calibration(value) => (value.clone(),).into_pyobject(py),
            Self::MeasureCalibration(value) => (value.clone(),).into_pyobject(py),
        }
    }
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl MaybeCalibrationExpansion {
    #[gen_stub(override_return_type(type_repr = "tuple[CalibrationExpansion, InstructionIndex]"))]
    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        match self {
            Self::Expanded(value) => (value.clone(),).into_pyobject(py),
            Self::Unexpanded(value) => (value,).into_pyobject(py),
        }
    }
}

/// Creates Python wrappers for `SourceMap<$srcT, $tgtT>` and `SourceMapEntry<$T, $U>`,
/// then implements `pymethods` for them to forward to the inner type.
/// This is necessary because a `#[pyclass]` can't have generic parameters,
/// and concrete implementations must be generated explicitly.
///
/// The syntax of the macro is meant to look like a regular newtype declaration
/// for a wrappers around the above types, except you can't specify the `SourceMapEntry` types
/// (they're forced to match the types for the `SourceMap` itself).
/// The structs automatically get `#[pyclass]` and `#[derive(Clone, Debug, PartialEq)]`,
/// but you can add additional annotations as desired, e.g. to change the Python name and module:
///
/// ```
/// py_source_map! {
///     #[pyo3(name = "CalibrationExpansionSourceMap", module = "quil.program", frozen)]
///     pub struct PyCalibrationExpansionSourceMap(SourceMap<InstructionIndex, MaybeCalibrationExpansion>);
///
///     #[pyo3(name = "CalibrationExpansionSourceMapEntry", module = "quil.program", frozen)]
///     pub struct PyCalibrationExpansionSourceMapEntry(SourceMapEntry<...>);
/// }
/// ```
macro_rules! py_source_map {
    (
        $(#[$meta_map: meta])*
        $mapvis:vis struct $mapT: ident(SourceMap<$srcT: ty, $tgtT: ty>);

        $(#[$meta_entry: meta])*
        $entryvis:vis struct $entryT: ident(SourceMapEntry<...>);
    ) => {
        #[derive(Clone, Debug, PartialEq)]
        #[cfg_attr(feature = "stubs", gen_stub_pyclass)]
        #[pyclass]
        $(#[$meta_map])*
        $mapvis struct $mapT(pub SourceMap<$srcT, $tgtT>);

        #[derive(Clone, Debug, PartialEq)]
        #[cfg_attr(feature = "stubs", gen_stub_pyclass)]
        #[pyclass]
        $(#[$meta_entry])*
        $entryvis struct $entryT(pub SourceMapEntry<$srcT, $tgtT>);

        #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
        #[pymethods]
        impl $entryT {
            pub fn source_location(&self) -> $srcT {
                *self.0.source_location()
            }

            pub fn target_location(&self) -> $tgtT {
                self.0.target_location().clone()
            }
        }

        #[cfg_attr(feature = "stubs", gen_stub_pymethods)]
        #[pymethods]
        impl $mapT {
            fn entries(&self) -> Vec<$entryT> {
                self.0
                    .entries()
                    .iter()
                    .map(|entry| $entryT(entry.clone()))
                    .collect()
            }

            /// Given an instruction index within the resulting expansion, return the locations in the source
            /// which were expanded to generate that instruction.
            ///
            /// This is `O(n)` where `n` is the number of first-level calibration expansions performed.
            fn list_sources_for_target_index(&self, target_index: $srcT) -> Vec<&$srcT> {
                self.0.list_sources(&target_index)
            }

            /// Given a particular calibration (`DEFCAL` or `DEFCAL MEASURE`), return the locations in the source
            /// program which were expanded using that calibration.
            ///
            /// This is `O(n)` where `n` is the number of first-level calibration expansions performed.
            fn list_sources_for_calibration_used(
                &self,
                calibration_used: CalibrationSource,
            ) -> Vec<&$srcT> {
                self.0.list_sources(&calibration_used)
            }

            /// Given a source index, return information about its expansion.
            ///
            /// This is `O(n)` where `n` is the number of first-level calibration expansions performed.
            fn list_targets_for_source_index(&self, source_index: $srcT) -> Vec<$tgtT> {
                self.0
                    .list_targets(&source_index)
                    .into_iter()
                    .map(|ext| ext.clone())
                    .collect()
            }
        }
    };
}

py_source_map! {
    #[pyo3(name = "CalibrationExpansionSourceMap", module = "quil.program", frozen)]
    pub(crate) struct PyCalibrationExpansionSourceMap(SourceMap<InstructionIndex, CalibrationExpansion>);

    #[pyo3(name = "CalibrationExpansionSourceMapEntry", module = "quil.program", frozen)]
    pub(crate) struct PyCalibrationExpansionSourceMapEntry(SourceMapEntry<...>);
}

py_source_map! {
    #[pyo3(name = "ProgramCalibrationExpansionSourceMap", module = "quil.program", frozen)]
    pub(crate) struct PyProgramCalibrationExpansionSourceMap(SourceMap<InstructionIndex, MaybeCalibrationExpansion>);

    #[pyo3(name = "ProgramCalibrationExpansionSourceMapEntry", module = "quil.program", frozen)]
    pub(crate) struct PyProgramCalibrationExpansionSourceMapEntry(SourceMapEntry<...>);
}

#[derive(FromPyObject, IntoPyObject, IntoPyObjectRef)]
struct PyQubitResolver(Py<PyFunction>);
#[derive(FromPyObject, IntoPyObject, IntoPyObjectRef)]
struct PyTargetResolver(Py<PyFunction>);

#[cfg(feature = "stubs")]
mod stubs {
    use super::*;

    impl PyStubType for PyQubitResolver {
        fn type_output() -> pyo3_stub_gen::TypeInfo {
            pyo3_stub_gen::TypeInfo::with_module(
                "collections.abc.Callable[[QubitPlaceholder], int | None]",
                "collections.abc".into(),
            )
        }
    }

    impl PyStubType for PyTargetResolver {
        fn type_output() -> pyo3_stub_gen::TypeInfo {
            pyo3_stub_gen::TypeInfo::with_module(
                "collections.abc.Callable[[TargetPlaceholder], str | None]",
                "collections.abc".into(),
            )
        }
    }

    impl PyStubType for InstructionIndex {
        fn type_output() -> pyo3_stub_gen::TypeInfo {
            pyo3_stub_gen::TypeInfo::builtin("int")
        }
    }

    impl PyStubType for Seconds {
        fn type_output() -> pyo3_stub_gen::TypeInfo {
            pyo3_stub_gen::TypeInfo::builtin("float")
        }
    }
}

/// A Schedule is a ``DependencyGraph`` flattened into a linear sequence of instructions,
/// each of which is assigned a start time and duration.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(
    name = "ScheduleSeconds",
    module = "quil.program",
    subclass,
    eq,
    frozen
)]
pub(crate) struct PyScheduleSeconds(pub Schedule<Seconds>);

/// A single item within a schedule, representing a single instruction within a basic block.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(
    name = "ScheduleSecondsItem",
    module = "quil.program",
    subclass,
    eq,
    frozen
)]
pub(crate) struct ScheduleSecondsItem(pub ComputedScheduleItem<Seconds>);

/// A time span, in seconds.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(
    name = "TimeSpanSeconds",
    module = "quil.program",
    subclass,
    eq,
    frozen
)]
pub(crate) struct TimeSpanSeconds(pub TimeSpan<Seconds>);

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl PyScheduleSeconds {
    /// Scheduled items, in an unspecified order.
    #[getter]
    fn items(&self) -> Vec<ScheduleSecondsItem> {
        self.0
            .items
            .iter()
            .map(|item| ScheduleSecondsItem(item.clone()))
            .collect()
    }

    /// The schedule duration, in seconds.
    ///
    /// This is the maximum end time among all scheduled items.
    #[getter]
    fn duration(&self) -> Seconds {
        self.0.duration().clone()
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl ScheduleSecondsItem {
    /// The index of the instruction within the basic block.
    #[getter]
    fn time_span(&self) -> TimeSpanSeconds {
        TimeSpanSeconds(self.0.time_span.clone())
    }

    /// The time span during which the instruction is scheduled.
    #[getter]
    fn instruction_index(&self) -> usize {
        self.0.instruction_index
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl TimeSpanSeconds {
    /// The inclusive start time of the time span,
    /// in seconds relative to the start of the scheduling context (such as the basic block).
    #[getter]
    fn start(&self) -> Seconds {
        self.0.start_time().clone()
    }

    /// The duration of the time span, in seconds.
    #[getter]
    fn duration(&self) -> Seconds {
        self.0.duration().clone()
    }

    /// The end time of the time span, in seconds.
    ///
    /// This is the sum of the start time and duration.
    #[getter]
    fn end(&self) -> Seconds {
        self.0.end()
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl ControlFlowGraphOwned {
    #[new]
    fn new(instance: Self) -> Self {
        instance
    }

    /// Return ``True`` if the program has dynamic control flow, i.e. contains a conditional branch instruction.
    ///
    /// ``False`` does not imply that there is only one basic block in the program.
    /// Multiple basic blocks may have non-conditional control flow among them,
    /// in which the execution order is deterministic and does not depend on program state.
    /// This may be a sequence of basic blocks with fixed `JUMP`s or without explicit terminators.
    fn has_dynamic_control_flow(&self) -> bool {
        ControlFlowGraph::from(self).has_dynamic_control_flow()
    }

    /// Return a list of all the basic blocks in the control flow graph, in order of definition.
    fn basic_blocks(&self) -> Vec<BasicBlockOwned> {
        self.blocks.clone()
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl BasicBlockOwned {
    #[new]
    fn new(instance: Self) -> Self {
        instance
    }

    fn as_schedule_seconds(
        &self,
        program: &Program,
    ) -> std::result::Result<PyScheduleSeconds, BasicBlockScheduleError> {
        BasicBlock::from(self)
            .as_schedule_seconds(program)
            .map(PyScheduleSeconds)
    }

    fn gate_depth(
        &self,
        gate_minimum_qubit_count: usize,
    ) -> std::result::Result<usize, QubitGraphError> {
        // TODO: this copies everything twice: once to make the block, and again for the graph.
        // Then it throws them both away. There's got to be a better way.
        let block = BasicBlock::from(self);
        QubitGraph::try_from(&block).map(|graph| graph.gate_depth(gate_minimum_qubit_count))
    }

    #[getter]
    fn terminator(&self) -> Option<Instruction> {
        BasicBlockTerminator::from(&self.terminator).into_instruction()
    }
}
