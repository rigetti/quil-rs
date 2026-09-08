from dataclasses import dataclass

import pytest
from quil.expression import Expression, ExpressionDesignator
from quil.instructions import (
    MeasureCalibrationDefinition,
    MeasureCalibrationIdentifier,
    Measurement,
    MemoryReference,
    PauliGate,
    PauliSum,
    PauliTerm,
    Qubit,
    CalibrationDefinition,
    CalibrationIdentifier,
    QubitDesignator,
    QubitPlaceholder,
    Delay,
    Gate,
)

@dataclass
class TestTerm:
    """Constructor arguments/expected values for a `PauliTerm`."""

    gate: PauliGate
    qubit: QubitDesignator
    qubit_str: str  # what we expect str(qubit) to be
    coeff: ExpressionDesignator
    expr: Expression # what we expect Expression(coeff) to be

    def __iter__(self) -> tuple[PauliGate, QubitDesignator, str, ExpressionDesignator, Expression]:
        """Facilitate automatic destructuring."""
        return self.gate, self.qubit, self.qubit_str, self.coeff, self.expr


@pytest.fixture
def terms() -> list[TestTerm]:
    """Get a list of non-identity `PauliTerm` constructor arguments."""
    return [
        TestTerm(PauliGate.X, 0, "0", Expression.Pi(), Expression.Pi()),
        TestTerm(PauliGate.Y, "b", "b", -1.0j, Expression.Number(-1.0j)),
    ]

@pytest.fixture
def terms_list(terms: list[TestTerm]) -> list[tuple[PauliGate, QubitDesignator]]:
    return [(term.gate, term.qubit) for term in terms]

class TestPauliTerm:
    def test_from_list_matches_constructor(self, terms_list):
        """Confirm the ``from_list`` method is equivalent to using the constructor.

        The original ``quil`` constructor was the ``(list, coeff)`` version,
        but that was expanded with a ``(gate, qubit, coeff)`` for PyQuil compatibility;
        likewise, PyQuil had a ``from_list`` alternate constructor
        which acted more similarly to the original ``quil`` constructor.
        """
        assert PauliTerm(terms_list) == PauliTerm.from_list(terms_list)

    def test_list_constructor(self, terms):
        """Test the ``(list, coeff)`` version of the constructor."""
        for gate, qubit, qubit_str, coeff, expr in terms:
            pt = PauliTerm([(gate, qubit)], coeff)
            assert pt.arguments == [(gate, qubit_str)]
            assert pt.expression == expr

    def test_single_constructor(self, terms):
        """Test the ``(gate, qubit, coeff)`` version of the constructor."""
        for gate, qubit, qubit_str, coeff, expr in terms:
            pt = PauliTerm(gate, qubit, coeff)
            assert pt.arguments == [(gate, qubit_str)]
            assert pt.expression == expr

    def test_list_default_coeff(self, terms):
        """Test the ``(list, coeff)`` version of the constructor."""
        for gate, qubit, qubit_str, coeff, expr in terms:
            pt = PauliTerm([(gate, qubit)], coeff)
            assert pt.arguments == [(gate, qubit_str)]
            assert pt.expression == expr


    def test_single_default_coeff(self, terms):
        """Test the ``(gate, qubit)`` version of the constructor."""
        for gate, qubit, qubit_str, _, _ in terms:
            pt = PauliTerm(gate, qubit)
            assert pt.arguments == [(gate, qubit_str)]
            assert pt.expression == Expression.Number(1.0)


class TestPauliSumConstructor:
    """Confirm all variants of the `PauliSum` constructor work as expected."""

    def test_terms_only(self, terms):
        """The constructor should accept a single argument, interpretted as `terms`"""
        assert PauliSum(terms) == PauliSum(terms=terms)

    def test_new_two_param(self, terms, arguments):
        """Confirm that the new construcotr works when given two parameters.

        An update to for PyQuil backwards compatibility swapped the parameter order,
        so we want to make sure it works regardless of positional vs keyword arguments.
        """
        positional = PauliSum(terms, arguments)
        mixed = PauliSum(terms, arguments=arguments)
        keyword = PauliSum(terms=terms, arguments=arguments)
        assert positional == mixed == keyword

    def test_old_two_param(self, terms, arguments):
        """Confirm the new constructor is backwards compatible with the old constructor."""
        assert PauliSum(arguments, terms) == PauliSum(arguments, terms=terms)

    def test_deprecations(self, terms, arguments):
        """Confirm we get deprecation warnings when using the old constructor."""

    def test_invalid(self, terms, arguments):
        """Confirm we get a TypeError if we try to mix arguments."""

def test_calibration_getters():
    placeholder = Qubit(QubitPlaceholder())

    calibration = CalibrationDefinition(
        CalibrationIdentifier("MYCAL", [], [placeholder], []),
        [Delay(Expression.Number(0.5), [], [placeholder])],
    )

    assert calibration.name == "MYCAL"
    assert calibration.parameters == []
    assert calibration.qubits == [placeholder]
    assert calibration.modifiers == []


def test_measure_calibration_getters():
    placeholder = Qubit.Placeholder(QubitPlaceholder())

    calibration = MeasureCalibrationDefinition(
        MeasureCalibrationIdentifier(placeholder, "addr"),
        [],
    )

    assert calibration.name is None
    assert calibration.qubit == placeholder
    assert calibration.target == "addr"
    assert calibration.instructions == []

def test_named_measure_calibration_getters():
    placeholder = Qubit.Placeholder(QubitPlaceholder())

    calibration = MeasureCalibrationDefinition(
        MeasureCalibrationIdentifier(placeholder, "addr", name = "midcircuit"),
        [],
    )

    assert calibration.name == "midcircuit"
    assert calibration.qubit == placeholder
    assert calibration.target == "addr"
    assert calibration.instructions == []


def test_measurement_getters():
    placeholder = Qubit.Placeholder(QubitPlaceholder())
    addr = MemoryReference("addr", 0)

    measurement = Measurement(placeholder, addr)

    assert measurement.name is None
    assert measurement.qubit == placeholder
    assert measurement.target == addr


def test_named_measurement_getters():
    placeholder = Qubit.Placeholder(QubitPlaceholder())
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

