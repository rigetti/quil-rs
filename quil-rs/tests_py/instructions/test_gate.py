from quil.expression import Expression
from quil.instructions import (
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

class TestPauliTerm:
    def test_new(self):
        pt = PauliTerm([(PauliGate.X, "a")], Expression.Pi())
        assert pt.arguments == [(PauliGate.X, "a")]
        pt = PauliTerm([(PauliGate.Y, "b")], Expression.Pi())
        assert pt.arguments == [(PauliGate.Y, "b")]


def test_calibration_getters():
    placeholder = Qubit.Placeholder(QubitPlaceholder())

    calibration = Calibration(
        CalibrationIdentifier("MYCAL", [], [placeholder], []),
        [Instruction.Delay(Delay(Expression.Number(complex(0.5)), [], [placeholder]))],
    )

    assert calibration.name == "MYCAL"
    assert calibration.parameters == []
    assert calibration.qubits == [placeholder]
    assert calibration.modifiers == []


def test_measure_calibration_getters():
    placeholder = Qubit.Placeholder(QubitPlaceholder())

    calibration = MeasureCalibrationDefinition(
        MeasureCalibrationIdentifier(placeholder, "param"),
        [],
    )

    assert calibration.qubit == placeholder
    assert calibration.instructions == []
