import numpy as np

from quil.expression import Expression
from quil.instructions import (
    DefGateSequence,
    Gate,
    GateDefinition,
    GateSpecification,
    MeasureCalibrationDefinition,
    MeasureCalibrationIdentifier,
    PauliGate,
    PauliTerm,
    Qubit,
    Calibration,
    CalibrationIdentifier,
    QubitPlaceholder,
    Instruction,
    Delay,
)
from quil.program import Program


class TestPauliTerm:
    def test_new(self):
        pt = PauliTerm([(PauliGate.X, "a")], Expression.new_pi())
        assert pt.arguments == [(PauliGate.X, "a")]
        pt.arguments = [(PauliGate.Y, "b")]
        assert pt.arguments == [(PauliGate.Y, "b")]


def test_calibration_getters():
    placeholder = Qubit.from_placeholder(QubitPlaceholder())

    calibration = Calibration(
        CalibrationIdentifier("MYCAL", [], [placeholder], []),
        [Instruction.from_delay(Delay(Expression.from_number(complex(0.5)), [], [placeholder]))],
    )

    assert calibration.name == "MYCAL"
    assert calibration.parameters == []
    assert calibration.qubits == [placeholder]
    assert calibration.modifiers == []


def test_measure_calibration_getters():
    placeholder = Qubit.from_placeholder(QubitPlaceholder())

    calibration = MeasureCalibrationDefinition(
        MeasureCalibrationIdentifier(placeholder, "param"),
        [],
    )

    assert calibration.qubit == placeholder
    assert calibration.instructions == []


def test_gate_sequence_definitions():
    """Test that a program with a gate sequence definition can be programmatically built and roundtripped through Quil."""
    gates = [
        Gate("X", [], [Qubit.from_variable("a")], []),
        Gate("Y", [], [Qubit.from_variable("b")], []),
        Gate("RZ", [Expression.from_variable("theta")], [Qubit.from_variable("c")], []),
    ]
    sequence = DefGateSequence(["a", "b", "c"], gates)
    specification = GateSpecification.from_sequence(sequence)
    gate_definition = GateDefinition("MY_GATE", ["theta"], specification)
    program1 = Program()
    program1.add_instruction(Instruction.from_gate_definition(gate_definition))
    program1.add_instruction(Instruction.from_gate(Gate("MY_GATE", [Expression.from_number(complex(np.pi / 4))], [Qubit.from_variable("a")], [])))

    quil = program1.to_quil()
    program2 = Program.parse(quil)

    assert program1 == program2
