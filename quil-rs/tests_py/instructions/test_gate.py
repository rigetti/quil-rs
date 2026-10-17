import pytest

from quil.instructions import (
    MeasureCalibrationDefinition,
    MeasureCalibrationIdentifier,
    Measurement,
    MemoryReference,
    Qubit,
    CalibrationDefinition,
    CalibrationIdentifier,
    Delay,
    Gate,
)


def test_calibration_getters():
    placeholder = Qubit()

    calibration = CalibrationDefinition(
        CalibrationIdentifier("MYCAL", [], [placeholder], []),
        [Delay(0.5, [placeholder])],
    )

    assert calibration.name == "MYCAL"
    assert calibration.parameters == []
    assert calibration.qubits == [placeholder]
    assert calibration.modifiers == []


def test_measure_calibration_getters():
    placeholder = Qubit()

    calibration = MeasureCalibrationDefinition.from_calibration(
        MeasureCalibrationIdentifier(placeholder, "addr"),
        [],
    )

    assert calibration.name is None
    assert calibration.qubit == placeholder
    assert calibration.target == "addr"
    assert calibration.instructions == []

def test_named_measure_calibration_getters():
    placeholder = Qubit()

    calibration = MeasureCalibrationDefinition.from_calibration(
        MeasureCalibrationIdentifier(placeholder, "addr", name = "midcircuit"),
        [],
    )

    assert calibration.name == "midcircuit"
    assert calibration.qubit == placeholder
    assert calibration.target == "addr"
    assert calibration.instructions == []


def test_measurement_getters():
    placeholder = Qubit()
    addr = MemoryReference("addr", 0)

    measurement = Measurement(placeholder, addr)

    assert measurement.name is None
    assert measurement.qubit == placeholder
    assert measurement.target == addr


def test_named_measurement_getters():
    placeholder = Qubit()
    addr = MemoryReference("addr", 0)

    measurement = Measurement(placeholder, addr, name = "midcircuit")

    assert measurement.name == "midcircuit"
    assert measurement.qubit == placeholder
    assert measurement.target == addr


@pytest.mark.xfail(reason="This underflows, but should raise a proper exception.")
def test_to_unitary():
    g = Gate("X", (), (Qubit.Fixed(0),), ())
    g.to_unitary(0)

if __name__ == "__main__":
    test_to_unitary()

