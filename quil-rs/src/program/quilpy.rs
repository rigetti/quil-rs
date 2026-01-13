use std::collections::{HashMap, HashSet};
use std::ops::Range;
use std::str::FromStr;

use numpy::{Complex64, PyArray2, ToPyArray};
use pyo3::types::PyTuple;
use pyo3::{
    prelude::*,
    types::{PyBytes, PyFunction, PyRange},
};

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pyclass_complex_enum, gen_stub_pymethods};

use crate::{
    instruction::{
        quilpy::OwnedGateSignature, CalibrationDefinition, Declaration, DefaultHandler,
        FrameAttributes, FrameIdentifier, Gate, Instruction, MeasureCalibrationDefinition,
        Measurement, QubitPlaceholder, TargetPlaceholder,
    },
    program::{DefGateSequenceExpansion, ExpansionResult},
    quil::Quil,
    quilpy::{fix_complex_enums, impl_repr, impl_to_quil},
};

use super::{
    analysis::{
        BasicBlock, BasicBlockOwned, BasicBlockScheduleError, BasicBlockTerminator,
        ControlFlowGraph, ControlFlowGraphOwned, QubitGraph, QubitGraphError,
    },
    scheduling::{ComputedScheduleItem, Schedule, Seconds, TimeSpan},
    CalibrationExpansion, CalibrationSource, Calibrations, FrameSet, InstructionIndex,
    MemoryRegion, Program, Result, SourceMap, SourceMapEntry, SourceMapIndexable,
};

/// The `quil.program` module contains classes for constructing and representing a Quil program.
///
/// # Examples
///
/// ## Source Mapping for Calibration Expansion
///
/// ```python
/// import inspect
/// from quil.program import Program
///
/// program_text = inspect.cleandoc(
///     """
///     DEFCAL X 0:
///         Y 0
///
///     DEFCAL Y 0:
///         Z 0
///
///     X 0 # This instruction is index 0
///     Y 0 # This instruction is index 1
///     """
/// )
///
/// # First, we parse the program and expand its calibrations
/// program = Program.parse(program_text)
/// expansion = program.expand_calibrations_with_source_map()
/// source_map = expansion.source_map()
///
/// # This is what we expect the expanded program to be. X and Y have each been replaced by Z.
/// expected_program_text = inspect.cleandoc(
///     """
///     DEFCAL X 0:
///         Y 0
///
///     DEFCAL Y 0:
///         Z 0
///
///     Z 0 # This instruction is index 0
///     Z 0 # This instruction is index 1
///     """
/// )
/// assert expansion.program().to_quil() == Program.parse(expected_program_text).to_quil()
///
/// # In order to discover _which_ calibration led to the first Z in the resulting program, we
/// # can interrogate the expansion source mapping.
/// #
/// # For instance, the X at index 0 should have been replaced with a Z at index 0.
/// # Here's how we can confirm that:
///
/// # First, we list the calibration expansion targets for that first instruction...
/// targets = source_map.list_targets_for_source_index(0)
///
/// # ...then we extract the expanded instruction.
/// # If the instruction had _not_ been expanded (i.e. there was no matching calibration), then `as_expanded()` would return `None`.
/// expanded = targets[0].as_expanded()
///
/// # This line shows how that `X 0` was expanded into instruction index 0 (only) within the expanded program.
/// # The end of the range is exclusive.
/// assert expanded.range() == range(0, 1)
///
/// # We can also map instructions in reverse: given an instruction index in the expanded program, we can find the source index.
/// # This is useful for understanding the provenance of instructions in the expanded program.
/// sources = source_map.list_sources_for_target_index(1)
///
/// # In this case, the instruction was expanded from the source program at index 1.
/// assert sources == [1]
/// ```
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
    m.add_class::<CalibrationSource>()?;
    m.add_class::<Calibrations>()?; // Python: CalibrationSet
    m.add_class::<ControlFlowGraphOwned>()?; // Python: ControlFlowGraph
    m.add_class::<FlatExpansionResult>()?; // Python: InstructionTarget
    m.add_class::<FrameSet>()?;
    m.add_class::<InstructionSourceMap>()?;
    m.add_class::<InstructionSourceMapEntry>()?;
    m.add_class::<MemoryRegion>()?;
    m.add_class::<OwnedDefGateSequenceExpansion>()?;
    m.add_class::<Program>()?;
    m.add_class::<PyScheduleSeconds>()?;
    m.add_class::<ScheduleSecondsItem>()?;
    m.add_class::<TimeSpanSeconds>()?;

    fix_complex_enums!(py, CalibrationSource, FlatExpansionResult);

    Ok(())
}

impl_repr!(BasicBlockOwned);
impl_repr!(CalibrationExpansion);
impl_repr!(Calibrations);
impl_repr!(CalibrationSource);
impl_repr!(ControlFlowGraphOwned);
impl_repr!(FrameSet);
impl_repr!(FlatExpansionResult);
impl_repr!(MemoryRegion);
impl_repr!(Program);
impl_repr!(PyScheduleSeconds);
impl_repr!(ScheduleSecondsItem);
impl_repr!(TimeSpanSeconds);

impl_to_quil!(Program);

type ExpandedProgram = (Program, InstructionSourceMap);

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Program {
    /// Parse a `Program` from a string.
    ///
    /// # Errors
    ///
    /// Raises a `ProgramError` if the string isn't a valid ``Quil`` expression.
    #[staticmethod]
    fn parse(input: &str) -> Result<Self> {
        <Self as std::str::FromStr>::from_str(input)
    }

    #[getter(body_instructions)]
    fn py_body_instructions(&self) -> Vec<Instruction> {
        self.instructions.clone()
    }

    /// Return a deep copy of the `Program`.
    fn copy(&self) -> Self {
        self.clone()
    }

    /// Return the [control flow graph][] of the program.
    ///
    /// [control flow graph]: https://en.wikipedia.org/wiki/Control-flow_graph
    fn control_flow_graph(&self) -> ControlFlowGraphOwned {
        ControlFlowGraphOwned::from(ControlFlowGraph::from(self))
    }

    // TODO (#470): Should this filtering move to Program?
    // Should we assume memory_regions will always make up all
    // declarations and simplify this?
    // Note: the original comment predates the `quil-py`/`quil-rs` merge
    // and can be found at 9db8cb4cfe75e67a4fa572e96e86ae8e59d0bdfc
    // in the file /quil-py/src/program/mod.rs on line 180.
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

    /// Expand any instructions in the program which have a matching calibration,
    /// leaving the others unchanged.
    /// Return the expanded copy of the program
    /// and a source mapping describing the expansions made.
    #[pyo3(name = "expand_calibrations_with_source_map")]
    fn py_expand_calibrations_with_source_map(&self) -> Result<ExpandedProgram> {
        let (expanded, source_map) = self.expand_calibrations_with_source_map()?;
        Ok((expanded, source_map.into()))
    }

    /// Expand any instructions in the program which have a matching sequence gate definition, leaving
    /// the others unchanged. Note, the new program will drop any gate definitions which are no longer
    /// referenced in the program.
    ///
    /// Recurses though each instruction while ensuring there is no cycle in the expansion graph (i.e. no sequence
    /// gate definitions expand directly or indirectly into itself).
    ///
    /// :param predicate: If provided, only sequence gate definitions which match the predicate will be expanded.
    /// Defaults to expanding all sequence gate definitions.
    ///
    /// Return the expanded copy of the program and a source mapping describing the expansions made.
    ///
    /// # Example
    ///
    /// See `expand_defgate_sequences`.
    #[pyo3(
        signature = (predicate = None),
        name = "expand_defgate_sequences_with_source_map",
    )]
    fn py_expand_defgate_sequences_with_source_map<'py>(
        &self,
        py: Python<'py>,
        #[gen_stub(
            override_type(
                type_repr="collections.abc.Callable[[str], bool] | None",
                imports=("collections.abc")
            )
        )]
        predicate: Option<&Bound<'py, PyFunction>>,
    ) -> Result<ExpandedProgram> {
        let (expanded_program, source_map) =
            self.expand_defgate_sequences_with_source_map(|key: &str| -> bool {
                predicate.map_or(true, |f| match call_user_func(py, f, key) {
                    Ok(val) => val,
                    Err(err) => panic!("error calling predicate: {err}"),
                })
            })?;

        Ok((expanded_program, source_map.into()))
    }

    /// Expand any instructions in the program which have a matching sequence gate definition, leaving
    /// the others unchanged.
    ///
    /// Recurses though each instruction while ensuring there is no cycle in the expansion graph (i.e. no sequence
    /// gate definitions expand directly or indirectly into itself).
    ///
    /// :param predicate: If provided, only sequence gate definitions which match the predicate will be expanded.
    ///     Defaults to expanding all sequence gate definitions.
    ///
    /// # Example
    ///
    /// Below, we show the results of gate sequence expansion on a program that has two gate
    /// sequence definitions. The first, `seq1`, has a matching calibration and we do not
    /// want to expand it. The second, `seq2`, does not have a matching calibration and
    /// we do want to expand it.
    ///
    /// >>> quil = '''
    /// ... DEFCAL seq1 0 1:
    /// ...     FENCE 0 1
    /// ...     NONBLOCKING PULSE 0 "rf" drag_gaussian(duration: 6.000000000000001e-08, fwhm: 1.5000000000000002e-08, t0: 3.0000000000000004e-08, anh: -190000000.0, alpha: -1.6453719598238201, scale: 0.168265925924524, phase: 0.0, detuning: 0)
    /// ...     NONBLOCKING PULSE 1 "rf" drag_gaussian(duration: 6.000000000000001e-08, fwhm: 1.5000000000000002e-08, t0: 3.0000000000000004e-08, anh: -190000000.0, alpha: -1.6453719598238201, scale: 0.168265925924524, phase: 0.0, detuning: 0)
    /// ...     FENCE 0 1
    /// ...
    /// ... DEFGATE seq1() a b AS SEQUENCE:
    /// ...     RX(pi/2) a
    /// ...     RX(pi/2) b
    /// ...
    /// ... DEFGATE seq2(%theta, %psi, %phi) a AS SEQUENCE:
    /// ...     RZ(%theta) a
    /// ...     RX(pi/2) a
    /// ...     RZ(%phi) a
    /// ...
    /// ... seq1 0 1
    /// ... seq2(1.5707963267948966, 3.141592653589793, 0) 0
    /// ... seq2(3.141592653589793, 0, 1.5707963267948966) 1
    /// ... '''
    /// >>> program = Program.parse(quil);
    /// >>> calibrated_gate_names = {calibration.identifier.name for calibration in program.calibrations.calibrations}
    /// >>> expanded_program = program.expand_defgate_sequences(lambda name: name not in calibrated_gate_names)
    /// >>>
    /// >>> expected_quil = '''
    /// ... DEFCAL seq1 0 1:
    /// ...     FENCE 0 1
    /// ...     NONBLOCKING PULSE 0 "rf" drag_gaussian(duration: 6.000000000000001e-08, fwhm: 1.5000000000000002e-08, t0: 3.0000000000000004e-08, anh: -190000000.0, alpha: -1.6453719598238201, scale: 0.168265925924524, phase: 0.0, detuning: 0)
    /// ...     NONBLOCKING PULSE 1 "rf" drag_gaussian(duration: 6.000000000000001e-08, fwhm: 1.5000000000000002e-08, t0: 3.0000000000000004e-08, anh: -190000000.0, alpha: -1.6453719598238201, scale: 0.168265925924524, phase: 0.0, detuning: 0)
    /// ...     FENCE 0 1
    /// ...
    /// ... DEFGATE seq1 a b AS SEQUENCE:
    /// ...     RX(pi/2) a
    /// ...     RX(pi/2) b
    /// ...
    /// ... seq1 0 1
    /// ...
    /// ... RZ(1.5707963267948966) 0
    /// ... RX(pi/2) 0
    /// ... RZ(3.141592653589793) 0
    /// ... RX(pi/2) 0
    /// ... RZ(0) 0
    /// ...
    /// ... RZ(3.141592653589793) 1
    /// ... RX(pi/2) 1
    /// ... RZ(0) 1
    /// ... RX(pi/2) 1
    /// ... RZ(1.5707963267948966) 1
    /// ... '''
    /// >>>
    /// >>> expected_program = Program.parse(expected_quil)
    /// >>>
    /// >>> assert expanded_program == expected_program
    ///
    // NOTE: A similar example is documented in the Rust documentation for `Program::expand_defgate_sequences`.
    // These examples should be kept in sync.
    #[pyo3(signature = (predicate = None), name = "expand_defgate_sequences")]
    fn py_expand_defgate_sequences<'py>(
        &self,
        py: Python<'py>,
        #[gen_stub(
            override_type(
                type_repr="collections.abc.Callable[[str], bool] | None",
                imports=("collections.abc")
            )
        )]
        predicate: Option<&Bound<'py, PyFunction>>,
    ) -> Result<Self> {
        self.clone().expand_defgate_sequences(|key: &str| -> bool {
            predicate.map_or(true, |f| match call_user_func(py, f, key) {
                Ok(val) => val,
                Err(err) => panic!("error calling predicate: {err}"),
            })
        })
    }

    /// Add a list of instructions to the end of the program.
    #[pyo3(name = "add_instructions")]
    fn py_add_instructions(&mut self, instructions: Vec<Instruction>) {
        self.add_instructions(instructions);
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
        self.filter_instructions(|inst| match call_user_func(py, predicate, inst.clone()) {
            Ok(val) => val,
            Err(err) => panic!("error calling predicate: {err}"),
        })
    }

    /// Resolve ``TargetPlaceholder``s and ``QubitPlaceholder``s within the program.
    ///
    /// The resolved values will remain unique to that placeholder
    /// within the scope of the program.
    /// If you provide ``target_resolver`` and/or ``qubit_resolver``,
    /// those will be used to resolve those values respectively.
    /// If your resolver returns `None` for a particular placeholder,
    /// it will not be replaced but will be left as a placeholder.
    /// If you do not provide a resolver for a placeholder,
    /// a default resolver will be used which will generate
    /// a unique value for that placeholder within the scope of the program
    /// using an auto-incrementing value (for qubit) or suffix (for target)
    /// while ensuring that unique value is not already in use within the program.
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
                    Python::attach(|py| {
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
                    Python::attach(|py| {
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

    // These docs are copied from [`Program::simplify`].
    /// Simplify this program into a new [`Program`] which contains only instructions
    /// and definitions which are executed; effectively, perform dead code removal.
    ///
    /// Removes:
    /// - All calibrations, following calibration expansion
    /// - Frame definitions which are not used by any instruction such as `PULSE` or `CAPTURE`
    /// - Waveform definitions which are not used by any instruction
    /// - `PRAGMA EXTERN` instructions which are not used by any `CALL` instruction (see
    ///   [`Program::extern_pragma_map`]).
    ///
    /// When a valid program is simplified, it remains valid.
    #[pyo3(name = "into_simplified")]
    fn py_into_simplified(&self) -> Result<Self> {
        self.simplify(&DefaultHandler)
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
        // TODO (#471): Can we reuse `rhs` to avoid an extra `clone`?
        self.clone() + rhs
    }

    fn __iadd__(&mut self, rhs: Self) {
        *self += rhs;
    }

    /// This will raise an error if the program contains any unresolved
    /// placeholders. This is because they can't be converted to valid quil,
    /// nor can they be serialized and deserialized in a consistent way.
    fn __getstate__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyBytes>> {
        Ok(PyBytes::new(py, self.to_quil()?.as_bytes()))
    }

    pub fn __setstate__(&mut self, state: &Bound<'_, PyBytes>) -> PyResult<()> {
        *self = Self::from_str(std::str::from_utf8(state.as_bytes())?)?;
        Ok(())
    }
}

/// Errors returned when calling a user-supplied `Callable`.
#[derive(Debug, thiserror::Error)]
enum UserFunctionError {
    #[error("calling the function resulted in an error: {0}")]
    CallError(#[source] PyErr),
    #[error("function returned the wrong type")]
    TypeError(#[source] PyErr),
}

/// Given `user_func: fn(T) -> U` and `param: T`, return `user_func(param)`.
///
/// # Errors
///
/// This returns a [`UserFunctionError::CallError`] if calling the user's function fails,
/// or a [`UserFunctionError::TypeError`] if the user's function returns the wrong type.
#[inline]
fn call_user_func<'py, T, U>(
    py: Python<'py>,
    user_func: &Bound<'py, PyFunction>,
    param: T,
) -> std::result::Result<U, UserFunctionError>
where
    T: IntoPyObject<'py>,
    U: FromPyObject<'py>,
{
    user_func
        .call1(
            (param,)
                .into_pyobject(py)
                .expect("Tuple::into_pyobject() should be infallible"),
        )
        .map_err(UserFunctionError::CallError)?
        .extract()
        .map_err(UserFunctionError::TypeError)
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl Calibrations {
    #[new]
    fn new(
        calibrations: Vec<CalibrationDefinition>,
        measure_calibrations: Vec<MeasureCalibrationDefinition>,
    ) -> Self {
        Self {
            calibrations: calibrations.into(),
            measure_calibrations: measure_calibrations.into(),
        }
    }

    /// Return a list of all [`CalibrationDefinition`]s in the set.
    #[getter(calibrations)]
    fn py_calibrations(&self) -> Vec<CalibrationDefinition> {
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

    /// Return the final calibration which matches the gate per the `QuilT` specification:
    ///
    /// A calibration matches a gate if:
    /// 1. It has the same name
    /// 2. It has the same modifiers
    /// 3. It has the same qubit count (any mix of fixed & variable)
    /// 4. It has the same parameter count (both specified and unspecified)
    /// 5. All fixed qubits in the calibration definition match those in the gate
    /// 6. All specified parameters in the calibration definition match those in the gate
    #[pyo3(name = "get_match_for_gate")]
    fn py_get_match_for_gate(&self, gate: &Gate) -> Option<CalibrationDefinition> {
        self.get_match_for_gate(gate).cloned()
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[pymethods]
impl CalibrationExpansion {
    /// The range of instructions in the expanded list
    /// which were generated by this expansion.
    #[gen_stub(override_return_type(type_repr = "range"))]
    #[getter(range)]
    pub fn py_range<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyRange>> {
        PyRange::new(
            py,
            self.range.start.0.try_into()?,
            self.range.end.0.try_into()?,
        )
    }

    /// The source map describing the nested expansions made.
    #[getter(expansions)]
    fn py_expansions(&self) -> InstructionSourceMap {
        (&self.expansions).into()
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl FrameSet {
    /// Retrieve the `FrameAttributes` of a `Frame` by its `FrameIdentifier`.
    #[pyo3(name = "get")]
    fn py_get(&self, identifier: &FrameIdentifier) -> Option<FrameAttributes> {
        self.get(identifier).cloned()
    }

    /// Return a list of all `FrameIdentifier`s described by this `FrameSet`.
    #[pyo3(name = "get_keys")]
    fn py_get_keys(&self) -> Vec<FrameIdentifier> {
        self.get_keys().into_iter().cloned().collect()
    }

    fn get_all_frames(&self) -> HashMap<FrameIdentifier, FrameAttributes> {
        self.frames.clone()
    }

    /// Return a new `FrameSet` which describes only the given `FrameIdentifier`s.
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

// --------------------------------

/// A source map describing how instructions in a source program were
/// expanded into a target program. Each entry describes an instruction
/// index in the source program which were expanded accordingto either
/// a calibration or a sequence gate definition.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(name = "InstructionSourceMap", module = "quil.program", frozen)]
pub(crate) struct InstructionSourceMap(SourceMap<InstructionIndex, FlatExpansionResult>);

/// A source map entry, mapping a range of source instructions by index to an
/// `InstructionTarget`.
///
/// Note that both `source_location` and `target_location` are relative to the scope of expansion.
/// In the case of a nested expansion, both describe the location relative only to that
/// level of expansion and *not* the original program.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyclass(name = "InstructionSourceMapEntry", module = "quil.program", frozen)]
pub(crate) struct InstructionSourceMapEntry(SourceMapEntry<InstructionIndex, FlatExpansionResult>);

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl InstructionSourceMapEntry {
    /// The instruction index within the source program's body instructions.
    pub fn source_location(&self) -> InstructionIndex {
        *self.0.source_location()
    }

    /// The location of the expanded instruction within the target program's body instructions.
    pub fn target_location(&self) -> FlatExpansionResult {
        self.0.target_location().clone()
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl InstructionSourceMap {
    fn entries(&self) -> Vec<InstructionSourceMapEntry> {
        self.0
            .entries
            .iter()
            .cloned()
            .map(InstructionSourceMapEntry)
            .collect()
    }

    /// Return all source ranges in the source map
    /// which were used to generate the target index.
    ///
    /// This is `O(n)` where `n` is the number of first-level expansions performed,
    /// which is at worst `O(i)` where `i` is the number of source instructions.
    fn list_sources_for_target_index(
        &self,
        target_index: InstructionIndex,
    ) -> Vec<&InstructionIndex> {
        self.0.list_sources(&target_index)
    }

    /// Given a particular calibration (`DEFCAL` or `DEFCAL MEASURE`), =
    /// return the locations in the source which were expanded using that calibration.
    ///
    /// This is `O(n)` where `n` is the number of first-level calibration expansions performed,
    /// which is at worst `O(i)` where `i` is the number of source instructions.
    fn list_sources_for_calibration_used(
        &self,
        calibration_used: CalibrationSource,
    ) -> Vec<&InstructionIndex> {
        self.0.list_sources(&calibration_used)
    }

    /// Given a gate signature, return the locations in the source program which were
    /// expanded using that gate signature.
    ///
    /// This is `O(n)` where `n` is the number of first-level sequence gate expansions performed,
    /// which is at worst `O(i)` where `i` is the number of source instructions.
    pub fn list_sources_for_gate_expansion(
        &self,
        gate_signature: OwnedGateSignature,
    ) -> Vec<&InstructionIndex> {
        self.0.list_sources(&gate_signature)
    }

    /// Return all target ranges which were used to generate the source range.
    ///
    /// This is `O(n)` where `n` is the number of first-level expansions performed,
    /// which is at worst `O(i)` where `i` is the number of source instructions.
    fn list_targets_for_source_index(
        &self,
        source_index: InstructionIndex,
    ) -> Vec<FlatExpansionResult> {
        self.0
            .list_targets(&source_index)
            .into_iter()
            .cloned()
            .collect()
    }
}

impl<R: Into<FlatExpansionResult> + Clone> From<SourceMap<InstructionIndex, ExpansionResult<R>>>
    for InstructionSourceMap
{
    fn from(value: SourceMap<InstructionIndex, ExpansionResult<R>>) -> Self {
        let entries = value
            .entries
            .into_iter()
            .map(|entry| SourceMapEntry {
                source_location: entry.source_location,
                target_location: FlatExpansionResult::from(entry.target_location),
            })
            .collect();
        InstructionSourceMap(SourceMap::new(entries))
    }
}

impl<R: Into<FlatExpansionResult> + Clone> From<&SourceMap<InstructionIndex, ExpansionResult<R>>>
    for InstructionSourceMap
{
    fn from(value: &SourceMap<InstructionIndex, ExpansionResult<R>>) -> Self {
        let entries = value
            .entries
            .iter()
            .map(|entry| SourceMapEntry {
                source_location: *entry.source_location(),
                target_location: FlatExpansionResult::from(entry.target_location()),
            })
            .collect();
        InstructionSourceMap(SourceMap::new(entries))
    }
}

/// [`DefGateSequenceExpansion`] references data from [`quil_rs::instruction::GateDefinition`]s used to expand instructions.
/// As such, it is incompatible with Python's memory management,
/// so we define an owned type here.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[pyo3::pyclass(module = "quil.program", eq)]
pub struct OwnedDefGateSequenceExpansion {
    /// The signature of the sequence gate definition which was used to expand the instruction.
    #[pyo3(get)]
    source_signature: OwnedGateSignature,
    range: Range<InstructionIndex>,
    nested_expansions: SourceMap<InstructionIndex, ExpansionResult<OwnedDefGateSequenceExpansion>>,
}

impl<'a> From<&'a DefGateSequenceExpansion<'a>> for OwnedDefGateSequenceExpansion {
    fn from(value: &'a DefGateSequenceExpansion<'a>) -> Self {
        Self {
            source_signature: value.source_signature().clone().into(),
            range: value.range().clone(),
            nested_expansions: SourceMap::new(
                value
                    .nested_expansions()
                    .entries()
                    .iter()
                    .map(|entry| {
                        let target_location = entry.target_location();
                        SourceMapEntry {
                            source_location: *entry.source_location(),
                            target_location: match target_location {
                                ExpansionResult::Unmodified(index) => {
                                    ExpansionResult::Unmodified(*index)
                                }
                                ExpansionResult::Rewritten(rewrite) => ExpansionResult::Rewritten(
                                    OwnedDefGateSequenceExpansion::from(rewrite),
                                ),
                            },
                        }
                    })
                    .collect(),
            ),
        }
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[pymethods]
impl OwnedDefGateSequenceExpansion {
    /// The range of instructions in the expanded list which were generated by this expansion.
    #[gen_stub(override_return_type(type_repr = "range"))]
    #[getter(range)]
    pub fn py_range<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyRange>> {
        PyRange::new(
            py,
            self.range.start.0.try_into()?,
            self.range.end.0.try_into()?,
        )
    }

    /// The source map describing the nested expansions made.
    #[pyo3(name = "expansions")]
    fn py_expansions(&self) -> InstructionSourceMap {
        (&self.nested_expansions).into()
    }
}

/// The result of having expanded a certain instruction within a program.
///
/// - `calibration`: The instruction has a matching Quil-T calibration and was expanded by it into
///   other instructions, as described by a `CalibrationExpansion`.
/// - `defgate_sequence`: The instruction has a matching `DEFGATE ... AS SEQUENCE` and was expanded
///   by it into other instructions, as described by a `DefGateSequenceExpansion`.
/// - `unmodified`: The instruction was not expanded and is described by an integer, the index of the instruction
///   within the resulting program's body instructions.
//
// Note, should we want to support rewrites from Python, we can expose an additional
// pyo3 type that wraps classes implementing the required Python interface. See the pyo3
// [trait bound](https://pyo3.rs/main/trait-bounds.html) documentation.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass_complex_enum)]
#[pyo3::pyclass(name = "InstructionTarget", module = "quil.program", eq, frozen)]
pub enum FlatExpansionResult {
    Unmodified(InstructionIndex),
    Calibration(CalibrationExpansion),
    DefGateSequence(OwnedDefGateSequenceExpansion),
}

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl FlatExpansionResult {
    #[gen_stub(override_return_type(type_repr = "tuple[CalibrationExpansion, builtins.int]"))]
    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        match self {
            Self::Unmodified(value) => (value,).into_pyobject(py),
            Self::Calibration(value) => (value.clone(),).into_pyobject(py),
            Self::DefGateSequence(value) => (value.clone(),).into_pyobject(py),
        }
    }
}

impl From<CalibrationExpansion> for FlatExpansionResult {
    fn from(value: CalibrationExpansion) -> Self {
        Self::Calibration(value)
    }
}

impl<'a> From<DefGateSequenceExpansion<'a>> for FlatExpansionResult {
    fn from(value: DefGateSequenceExpansion<'a>) -> Self {
        Self::DefGateSequence(OwnedDefGateSequenceExpansion::from(&value))
    }
}

impl From<OwnedDefGateSequenceExpansion> for FlatExpansionResult {
    fn from(value: OwnedDefGateSequenceExpansion) -> Self {
        Self::DefGateSequence(value)
    }
}

impl<R: Into<FlatExpansionResult> + Clone> From<&ExpansionResult<R>> for FlatExpansionResult {
    fn from(value: &ExpansionResult<R>) -> Self {
        match value {
            ExpansionResult::Unmodified(index) => Self::Unmodified(*index),
            ExpansionResult::Rewritten(rewrite) => rewrite.clone().into(),
        }
    }
}

impl<R: Into<FlatExpansionResult>> From<ExpansionResult<R>> for FlatExpansionResult {
    fn from(value: ExpansionResult<R>) -> Self {
        match value {
            ExpansionResult::Unmodified(index) => Self::Unmodified(index),
            ExpansionResult::Rewritten(rewrite) => rewrite.into(),
        }
    }
}

impl SourceMapIndexable<InstructionIndex> for FlatExpansionResult {
    fn contains(&self, other: &InstructionIndex) -> bool {
        match self {
            Self::Unmodified(index) => index == other,
            Self::Calibration(expansion) => expansion.contains(other),
            Self::DefGateSequence(expansion) => expansion.contains(other),
        }
    }
}

impl SourceMapIndexable<CalibrationSource> for FlatExpansionResult {
    fn contains(&self, other: &CalibrationSource) -> bool {
        if let Self::Calibration(expansion) = self {
            expansion.contains(other)
        } else {
            false
        }
    }
}

impl SourceMapIndexable<InstructionIndex> for OwnedDefGateSequenceExpansion {
    fn contains(&self, other: &InstructionIndex) -> bool {
        self.range.contains(other)
    }
}

impl SourceMapIndexable<OwnedGateSignature> for OwnedDefGateSequenceExpansion {
    fn contains(&self, other: &OwnedGateSignature) -> bool {
        &self.source_signature == other
    }
}

impl<R> SourceMapIndexable<OwnedGateSignature> for ExpansionResult<R>
where
    R: SourceMapIndexable<OwnedGateSignature>,
{
    fn contains(&self, other: &OwnedGateSignature) -> bool {
        if let Self::Rewritten(rewrite) = self {
            rewrite.contains(other)
        } else {
            false
        }
    }
}

impl SourceMapIndexable<OwnedGateSignature> for FlatExpansionResult {
    fn contains(&self, other: &OwnedGateSignature) -> bool {
        if let Self::DefGateSequence(expansion) = self {
            expansion.contains(other)
        } else {
            false
        }
    }
}

#[derive(FromPyObject, IntoPyObject, IntoPyObjectRef)]
struct PyQubitResolver(Py<PyFunction>);
#[derive(FromPyObject, IntoPyObject, IntoPyObjectRef)]
struct PyTargetResolver(Py<PyFunction>);

#[cfg(feature = "stubs")]
mod stubs {
    use pyo3_stub_gen::{PyStubType, TypeInfo};

    #[allow(clippy::wildcard_imports)]
    use super::*;

    /// Create a `Callable[[A], R]` stub.
    fn callable_of<A: PyStubType, R: PyStubType>() -> TypeInfo {
        let TypeInfo {
            name: name_a,
            mut import,
        } = A::type_output();
        let TypeInfo {
            name: name_r,
            import: import_r,
        } = R::type_output();
        import.extend(import_r);
        import.insert("collections.abc".into());
        TypeInfo {
            name: format!("collections.abc.Callable[[{name_a}], {name_r}]"),
            import,
        }
    }

    impl PyStubType for PyQubitResolver {
        fn type_output() -> TypeInfo {
            callable_of::<QubitPlaceholder, Option<u64>>()
        }
    }

    impl PyStubType for PyTargetResolver {
        fn type_output() -> TypeInfo {
            callable_of::<TargetPlaceholder, Option<String>>()
        }
    }

    impl PyStubType for InstructionIndex {
        fn type_output() -> TypeInfo {
            TypeInfo::builtin("int")
        }
    }

    impl PyStubType for Seconds {
        fn type_output() -> TypeInfo {
            TypeInfo::builtin("float")
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
            .as_schedule_seconds(program, &DefaultHandler)
            .map(PyScheduleSeconds)
    }

    #[expect(clippy::doc_markdown)]
    /// Return the length of the longest path
    /// from an initial instruction (one with no prerequisite instructions)
    /// to a final instruction (one with no dependent instructions),
    /// where the length of a path is the number of gate instructions in the path.
    ///
    /// :param gate_minimum_qubit_count:
    ///     The minimum number of qubits in a gate
    ///     for it to be counted in the depth.
    fn gate_depth(
        &self,
        gate_minimum_qubit_count: usize,
    ) -> std::result::Result<usize, QubitGraphError> {
        // TODO (#472): This copies everything twice: once to make the block,
        // and again for the graph. Then it throws them both away. There's got to be a better way.
        let block = BasicBlock::from(self);
        QubitGraph::try_from_basic_block(&block, &DefaultHandler)
            .map(|graph| graph.gate_depth(gate_minimum_qubit_count))
    }

    /// The control flow terminator instruction of the block, if any.
    ///
    /// If this is ``None``, the implicit behavior is to "continue" to the subsequent block.
    #[getter]
    fn terminator(&self) -> Option<Instruction> {
        BasicBlockTerminator::from(&self.terminator).into_instruction()
    }
}
