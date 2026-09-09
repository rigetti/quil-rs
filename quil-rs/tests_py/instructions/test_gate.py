import itertools
from collections.abc import Iterator
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
    PauliTargetDesignator,
    Qubit,
    CalibrationDefinition,
    CalibrationIdentifier,
    QubitPlaceholder,
    Delay,
    Gate,
)

@dataclass
class TestTerm:
    """Constructor arguments/expected values for a `PauliTerm`."""

    op: PauliGate
    index: PauliTargetDesignator
    qubit_str: str  # what we expect the actual argument to be
    coeff: ExpressionDesignator
    expr: Expression # what we expect Expression(coeff) to be

    def __iter__(self) -> Iterator:
        """Facilitate automatic destructuring."""
        return iter((self.op, self.index, self.qubit_str, self.coeff, self.expr))


@pytest.fixture
def test_terms() -> list[TestTerm]:
    """Get a list of non-identity `PauliTerm` constructor arguments."""
    return [
        TestTerm(PauliGate.X, 0, "q0", Expression.Pi(), Expression.Pi()),
        TestTerm(PauliGate.Y, "b", "b", -1.0, Expression.Number(-1.0)),
    ]

@pytest.fixture
def terms(test_terms: list[TestTerm]) -> list[PauliTerm]:
    return [PauliTerm(term.op, term.index, term.coeff) for term in test_terms]

@pytest.fixture
def arguments(test_terms: list[TestTerm]) -> list[PauliTargetDesignator]:
    return [term.index for term in test_terms]


class TestPauliTerm:
    def test_from_list_matches_constructor(self, test_terms: list[TestTerm]):
        """Confirm the ``from_list`` method is equivalent to using the constructor.

        The original ``quil`` constructor was the ``(list, coeff)`` version,
        but that was expanded with a ``(gate, qubit, coeff)`` for PyQuil compatibility;
        likewise, PyQuil had a ``from_list`` alternate constructor
        which acted more similarly to the original ``quil`` constructor.
        """
        terms_list = [(term.op, term.index) for term in test_terms]
        with pytest.warns(DeprecationWarning):
            from_constructor = PauliTerm(terms_list)
        assert from_constructor == PauliTerm.from_list(terms_list)

    def test_single_constructor(self, test_terms: list[TestTerm]):
        for op, index, qubit_str, coeff, expr in test_terms:
            pt = PauliTerm(op, index, coeff)
            assert pt.arguments == [(op, qubit_str)]
            assert pt.expression == expr

    def test_single_default_coeff(self, test_terms: list[TestTerm]):
        for op, index, qubit_str, _, _ in test_terms:
            pt = PauliTerm(op, index)
            assert pt.arguments == [(op, qubit_str)]
            assert pt.expression == Expression.Number(1.0)

    def test_single_ident_no_index(self):
        pt = PauliTerm(PauliGate.I, None, 2.0)
        assert pt.arguments == []
        assert pt.expression == Expression.Number(2.0)

    def test_single_ident_no_index_default_coeff(self):
        pt = PauliTerm(PauliGate.I, None)
        assert pt.arguments == []
        assert pt.expression == Expression.Number(1.0)

    def test_list_constructor(self, test_terms: list[TestTerm]):
        for op, index, qubit_str, coeff, expr in test_terms:
            with pytest.warns(DeprecationWarning):
                pt = PauliTerm([(op, index)], coeff)
            assert pt.arguments == [(op, qubit_str)]
            assert pt.expression == expr

    def test_list_default_coeff(self, test_terms: list[TestTerm]):
        for op, index, qubit_str, _, _ in test_terms:
            with pytest.warns(DeprecationWarning):
                pt = PauliTerm([(op, index)])
            assert pt.arguments == [(op, qubit_str)]
            assert pt.expression == Expression.Number(1.0)

    def test_placeholder_raises(self):
        """Confirm that we cannot construct a PauliTerm with a placeholder qubit."""
        with pytest.raises(TypeError):
            PauliTerm(PauliGate.X, QubitPlaceholder(), 1.0)  # pyright: ignore


class TestPauliSumConstructor:
    """Confirm all variants of the `PauliSum` constructor work as expected."""

    def test_terms_only(self, terms: list[PauliTerm]):
        """The constructor should accept a single argument, interpretted as `terms`"""
        positional = PauliSum(terms)
        for sum_term, term in zip(positional.terms, terms):
            assert sum_term == term

        keyword = PauliSum(terms=terms)
        assert positional == keyword


    def test_new_two_param(self, terms: list[PauliTerm], arguments: list[PauliTargetDesignator]):
        """Confirm that the new construcotr works when given two parameters.

        An update to for PyQuil backwards compatibility swapped the parameter order,
        so we want to make sure it works regardless of positional vs keyword arguments.
        """
        positional = PauliSum(terms, arguments)
        mixed = PauliSum(terms, arguments=arguments)
        keyword = PauliSum(terms=terms, arguments=arguments)
        assert positional == mixed == keyword

    def test_old_two_param(self, terms: list[PauliTerm], arguments: list[PauliTargetDesignator]):
        """Confirm the new constructor is backwards compatible with the old constructor."""
        with pytest.warns(DeprecationWarning):
            old = PauliSum(arguments, terms)
        assert old == PauliSum(terms, arguments)

    def test_deprecations(self, terms: list[PauliTerm], arguments: list[PauliTargetDesignator]):
        """Confirm we get deprecation warnings when using the old constructor."""
        with pytest.warns(DeprecationWarning):
            positional = PauliSum(arguments, terms)
        with pytest.warns(DeprecationWarning):
            mixed = PauliSum(arguments, terms=terms)
        assert positional == mixed

    def test_invalid(self,terms: list[PauliTerm], arguments: list[PauliTargetDesignator]):
        """Confirm we get errors for invalid constructions."""
        for args in itertools.combinations_with_replacement([terms, arguments], 3):
            with pytest.raises((TypeError, ValueError)):
                PauliSum(*args)  # pyright: ignore

        with pytest.raises(TypeError):
            PauliSum(terms, terms)
        with pytest.raises(TypeError):
            PauliSum(terms=arguments)
        with pytest.raises(TypeError):
            PauliSum(arguments, arguments)
        with pytest.raises(TypeError):
            PauliSum(arguments=terms) # pyright: ignore

        with pytest.raises(ValueError):
            PauliSum(arguments)

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

