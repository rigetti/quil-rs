"""Tests for deprecation warnings (and intentionally non-deprecated aliases)
on the constructors and properties of `quil.instructions` classes.

Most classes below use the internal `deprecated_or_new!` macro
to accept an old parameter name as a backwards-compatible alias for a new one;
using the old name should raise a `DeprecationWarning`,
but a few classes instead use the `aliased!` macro,
which intentionally accepts an old name without a warning.
"""

import warnings

import pytest
import quil
from quil.instructions import (
    CalibrationDefinition,
    CalibrationIdentifier,
    Capture,
    CircuitDefinition,
    Comparison,
    ComparisonOperator,
    Delay,
    FrameIdentifier,
    Gate,
    MeasureCalibrationDefinition,
    Measurement,
    MemoryReference,
    Pragma,
    Pulse,
    Qubit,
    RawCapture,
    SetFrequency,
    ShiftFrequency,
    SwapPhases,
    WaveformInvocation,
)


def _frame(name: str = "frm") -> FrameIdentifier:
    return FrameIdentifier([Qubit(0)], name)


def _waveform(name: str = "wf") -> WaveformInvocation:
    return WaveformInvocation(name)


class TestConstructorAliasDeprecationWarnings:
    """Constructors whose old parameter name goes through `deprecated_or_new!`:
    using the old name should raise a `DeprecationWarning` and behave
    identically to using the new name.
    """

    def test_comparison_target_left_right(self):
        with pytest.warns(DeprecationWarning):
            old = Comparison(
                ComparisonOperator.EQUAL,
                target=MemoryReference("dst", 0),
                left=MemoryReference("a", 0),
                right=MemoryReference("b", 0),
            )
        new = Comparison(
            ComparisonOperator.EQUAL,
            MemoryReference("dst", 0),
            MemoryReference("a", 0),
            MemoryReference("b", 0),
        )
        assert old == new

    def test_comparison_op_getter_warns(self):
        comparison = Comparison(
            ComparisonOperator.EQUAL,
            MemoryReference("dst", 0),
            MemoryReference("a", 0),
            MemoryReference("b", 0),
        )
        with pytest.warns(DeprecationWarning):
            assert comparison.op == ComparisonOperator.EQUAL
        with pytest.warns(DeprecationWarning):
            assert comparison.target == MemoryReference("dst", 0)
        with pytest.warns(DeprecationWarning):
            assert comparison.left == MemoryReference("a", 0)
        with pytest.warns(DeprecationWarning):
            # `right` is a `ComparisonOperand`, not a bare `MemoryReference`.
            assert comparison.right is not None

    def test_circuit_definition_qubits(self):
        with pytest.warns(DeprecationWarning):
            old = CircuitDefinition("CIRC", [], instructions=[], qubits=["a", "b"])
        new = CircuitDefinition("CIRC", [], ["a", "b"], [])
        assert old == new

    def test_memory_reference_offset(self):
        with pytest.warns(DeprecationWarning):
            old = MemoryReference("ro", offset=3)
        new = MemoryReference("ro", 3)
        assert old == new

    def test_memory_reference_declared_size_is_deprecated(self):
        # `declared_size` is deprecated outright (not an alias for another parameter).
        with pytest.warns(DeprecationWarning):
            MemoryReference("ro", declared_size=4)

    def test_pulse_nonblocking(self):
        frame, waveform = _frame(), _waveform()
        with pytest.warns(DeprecationWarning):
            old = Pulse(frame, waveform, nonblocking=True)
        new = Pulse(frame, waveform, blocking=False)
        assert old == new

    def test_pulse_nonblocking_getter_warns(self):
        pulse = Pulse(_frame(), _waveform(), blocking=False)
        with pytest.warns(DeprecationWarning):
            assert pulse.nonblocking is True

    def test_capture_nonblocking(self):
        frame, waveform = _frame(), _waveform()
        memory_region = MemoryReference("ro", 0)
        with pytest.warns(DeprecationWarning):
            old = Capture(frame, waveform, memory_region, nonblocking=True)
        new = Capture(frame, waveform, memory_region, blocking=False)
        assert old == new

    def test_raw_capture_nonblocking(self):
        frame = _frame()
        memory_region = MemoryReference("ro", 0)
        with pytest.warns(DeprecationWarning):
            old = RawCapture(frame, 1e-6, memory_region, nonblocking=True)
        new = RawCapture(frame, 1e-6, memory_region, blocking=False)
        assert old == new

    def test_set_frequency_freq(self):
        frame = _frame()
        with pytest.warns(DeprecationWarning):
            old = SetFrequency(frame, freq=100.0)
        new = SetFrequency(frame, 100.0)
        assert old == new

    def test_shift_frequency_freq(self):
        frame = _frame()
        with pytest.warns(DeprecationWarning):
            old = ShiftFrequency(frame, freq=100.0)
        new = ShiftFrequency(frame, 100.0)
        assert old == new

    def test_swap_phases_frame_a_frame_b(self):
        f1, f2 = _frame("f1"), _frame("f2")
        with pytest.warns(DeprecationWarning):
            old = SwapPhases(frameA=f1, frameB=f2)
        new = SwapPhases(f1, f2)
        assert old == new

    def test_swap_phases_rejects_mixing_old_and_new(self):
        f1, f2 = _frame("f1"), _frame("f2")
        with pytest.raises(quil.ValueError):
            SwapPhases(f1, frameB=f2)

    def test_pragma_command_and_freeform_string(self):
        with pytest.warns(DeprecationWarning):
            old = Pragma(command="INITIAL_REWIRING", freeform_string="data")
        new = Pragma("INITIAL_REWIRING", data="data")
        assert old == new

    def test_pragma_empty_string_data_is_deprecated(self):
        with pytest.warns(DeprecationWarning):
            Pragma("FOO", data="")


class TestMutuallyExclusiveAliasErrors:
    """Passing both the old and new parameter names should raise an error."""

    def test_comparison_rejects_both_destination_and_target(self):
        with pytest.raises(quil.ValueError):
            Comparison(
                ComparisonOperator.EQUAL,
                MemoryReference("dst", 0),
                MemoryReference("a", 0),
                MemoryReference("b", 0),
                target=MemoryReference("dst", 0),
            )

    def test_measurement_rejects_both_target_and_classical_reg(self):
        qubit = Qubit(0)
        target = MemoryReference("ro", 0)
        with pytest.raises(quil.ValueError):
            Measurement(qubit, target, classical_reg=target)

    def test_measure_calibration_definition_rejects_both_instructions_and_instrs(self):
        with pytest.raises(quil.ValueError):
            MeasureCalibrationDefinition(Qubit(0), None, instructions=[], instrs=[])

    def test_gate_rejects_both_parameters_and_params(self):
        with pytest.raises(quil.ValueError):
            Gate("RX", parameters=[1.0], params=[1.0], qubits=[Qubit(0)])


class TestIntentionalNonDeprecatedAliases:
    """These aliases are permanent, backwards-compatible alternate names that
    are *not* scheduled for removal, so using them should **not** raise a
    `DeprecationWarning`.
    """

    def test_gate_params_alias_is_silent(self):
        with warnings.catch_warnings():
            warnings.simplefilter("error")
            via_params = Gate("RX", params=[1.0], qubits=[Qubit(0)])
        via_parameters = Gate("RX", parameters=[1.0], qubits=[Qubit(0)])
        assert via_params == via_parameters

    def test_measurement_classical_reg_alias_is_silent(self):
        qubit = Qubit(0)
        target = MemoryReference("ro", 0)
        with warnings.catch_warnings():
            warnings.simplefilter("error")
            via_classical_reg = Measurement(qubit, classical_reg=target)
        via_target = Measurement(qubit, target)
        assert via_classical_reg == via_target

        with warnings.catch_warnings():
            warnings.simplefilter("error")
            # The getter is likewise just a plain, non-deprecated alias.
            assert via_classical_reg.classical_reg == target

    def test_measure_calibration_definition_instrs_constructor_kwarg_is_silent(self):
        qubit = Qubit(0)
        with warnings.catch_warnings():
            warnings.simplefilter("error")
            via_instrs = MeasureCalibrationDefinition(qubit, None, instrs=[])
        via_instructions = MeasureCalibrationDefinition(qubit, None, instructions=[])
        assert via_instrs == via_instructions


class TestInstrsGetterDeprecations:
    """Unlike the `instrs`/`instructions` constructor keyword (a silent,
    non-deprecated alias -- see above), the `instrs` *getter* on both
    `CalibrationDefinition` and `MeasureCalibrationDefinition` is explicitly
    marked deprecated in favor of the `instructions` getter.
    """

    def test_calibration_definition_instrs_getter_warns(self):
        placeholder = Qubit()
        calibration = CalibrationDefinition(
            CalibrationIdentifier("MYCAL", [], [placeholder], []),
            [Delay(0.5, [placeholder])],
        )
        with pytest.warns(DeprecationWarning):
            assert calibration.instrs == calibration.instructions

    def test_measure_calibration_definition_instrs_getter_warns(self):
        calibration = MeasureCalibrationDefinition(Qubit(0), None, [])
        with pytest.warns(DeprecationWarning):
            assert calibration.instrs == calibration.instructions


class TestCalibrationDefinitionOldStyleConstructor:
    """`CalibrationDefinition`'s fully-positional
    `(name, parameters, qubits, instrs, modifiers)` constructor is marked
    `@typing_extensions.deprecated` in the generated type stubs, but nothing
    in the Rust `__new__` implementation actually raises a runtime
    `DeprecationWarning` for it. This test documents that gap and is
    expected to fail (`xfail`) until the implementation is fixed to match
    the stubs.
    """

    @pytest.mark.xfail(
        reason=(
            "CalibrationDefinition's old-style positional constructor is "
            "marked `@typing_extensions.deprecated` in the stubs, but no "
            "runtime `DeprecationWarning` is actually raised."
        ),
        strict=True,
    )
    def test_old_style_constructor_warns(self):
        placeholder = Qubit()
        with pytest.warns(DeprecationWarning):
            CalibrationDefinition(
                "MYCAL",
                [],
                [placeholder],
                [Delay(0.5, [placeholder])],
            )
