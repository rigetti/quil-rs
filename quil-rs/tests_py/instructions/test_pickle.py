import pickle

import pytest
from quil import PickleError
from quil.instructions import (
    CalibrationDefinition,
    CalibrationIdentifier,
    Capture,
    CircuitDefinition,
    Comparison,
    ComparisonOperator,
    Delay,
    FrameDefinition,
    FrameIdentifier,
    Gate,
    Halt,
    MeasureCalibrationDefinition,
    MeasureCalibrationIdentifier,
    Measurement,
    MemoryReference,
    Nop,
    Pragma,
    Pulse,
    Qubit,
    QubitPlaceholder,
    RawCapture,
    SetFrequency,
    ShiftFrequency,
    SwapPhases,
    Target,
    Wait,
    WaveformInvocation,
)


def _roundtrip(value):
    return pickle.loads(pickle.dumps(value))


def _frame(name: str = "frm") -> FrameIdentifier:
    return FrameIdentifier([Qubit(0)], name)


class TestPickle:
    def test_gate(self):
        g = Gate("H", (), (Qubit(0),))
        p = pickle.dumps(g)
        g2 = pickle.loads(p)
        assert g == g2

    def test_gate_with_parameters_alias(self):
        # Constructed via the `params` alias, to also exercise that the
        # resulting instance still pickles/unpickles correctly.
        g = Gate("RX", params=[1.0], qubits=[Qubit(0)])
        assert _roundtrip(g) == g

    def test_halt(self):
        h = Halt
        p = pickle.dumps(h)
        q = pickle.loads(p)
        assert q == h

    def test_nop(self):
        h = Nop
        p = pickle.dumps(h)
        q = pickle.loads(p)
        assert q == h

    def test_wait(self):
        h = Wait
        p = pickle.dumps(h)
        q = pickle.loads(p)
        assert q == h

    def test_memory_reference(self):
        m = MemoryReference("ro", 3)
        assert _roundtrip(m) == m

    def test_comparison(self):
        c = Comparison(
            ComparisonOperator.EQUAL,
            MemoryReference("dst", 0),
            MemoryReference("a", 0),
            MemoryReference("b", 0),
        )
        assert _roundtrip(c) == c

    def test_circuit_definition(self):
        c = CircuitDefinition("CIRC", [], ["a", "b"], [])
        assert _roundtrip(c) == c

    def test_measurement(self):
        m = Measurement(Qubit(0), MemoryReference("ro", 0))
        assert _roundtrip(m) == m

    def test_pulse(self):
        p = Pulse(_frame(), WaveformInvocation("wf"), blocking=False)
        assert _roundtrip(p) == p

    def test_capture(self):
        c = Capture(_frame(), WaveformInvocation("wf"), MemoryReference("ro", 0))
        assert _roundtrip(c) == c

    def test_raw_capture(self):
        c = RawCapture(_frame(), 1e-6, MemoryReference("ro", 0))
        assert _roundtrip(c) == c

    def test_set_frequency(self):
        s = SetFrequency(_frame(), 100.0)
        assert _roundtrip(s) == s

    def test_shift_frequency(self):
        s = ShiftFrequency(_frame(), 100.0)
        assert _roundtrip(s) == s

    def test_swap_phases(self):
        s = SwapPhases(_frame("f1"), _frame("f2"))
        assert _roundtrip(s) == s

    def test_pragma(self):
        p = Pragma("INITIAL_REWIRING", data="data")
        assert _roundtrip(p) == p

    def test_frame_definition(self):
        f = FrameDefinition(_frame(), {"SAMPLE-RATE": 1e9})
        assert _roundtrip(f) == f

    def test_waveform_invocation(self):
        w = WaveformInvocation("wf", {"amp": 0.5})
        assert _roundtrip(w) == w

    def test_target(self):
        t = Target("my-label")
        assert _roundtrip(t) == t

    def test_calibration_identifier(self):
        c = CalibrationIdentifier("MYCAL", [], [Qubit(0)], [])
        assert _roundtrip(c) == c

    def test_calibration_definition(self):
        c = CalibrationDefinition(
            CalibrationIdentifier("MYCAL", [], [Qubit(0)], []),
            [Delay(0.5, [Qubit(0)])],
        )
        assert _roundtrip(c) == c

    def test_measure_calibration_identifier(self):
        c = MeasureCalibrationIdentifier(Qubit(0), None)
        assert _roundtrip(c) == c

    def test_measure_calibration_definition(self):
        c = MeasureCalibrationDefinition(Qubit(0), None, [])
        assert _roundtrip(c) == c

    def test_qubit_placeholder_cannot_be_pickled(self):
        # Documents the intentional restriction: `QubitPlaceholder`s are
        # identity-based and don't support `pickle`/`deepcopy`.
        with pytest.raises(PickleError):
            pickle.dumps(QubitPlaceholder())


class TestParse:
    def test_memory_ref(self):
        input_str = "ro[0]"
        ref = MemoryReference.parse(input_str)
        assert isinstance(ref, MemoryReference)
        assert ref.index == 0
        assert ref.name == "ro"
        assert ref.to_quil() == input_str
