import itertools
from collections.abc import Iterator
from dataclasses import dataclass

import pytest
import numpy as np
from quil.expression import Expression, ExpressionDesignator, quil_exp
from quil.instructions import (
    PauliGate,
    PauliSum,
    PauliTerm,
    PauliTargetDesignator,
    QubitPlaceholder,
)


@dataclass
class ConstrArg:
    """Constructor arguments/expected values for a `PauliTerm`."""

    op: PauliGate
    index: PauliTargetDesignator
    qubit_str: str  # what we expect the actual argument to be
    coeff: ExpressionDesignator
    expr: Expression  # what we expect Expression(coeff) to be

    def __iter__(self) -> Iterator:
        """Facilitate automatic destructuring."""
        return iter((self.op, self.index, self.qubit_str, self.coeff, self.expr))


@pytest.fixture
def constr_args() -> list[ConstrArg]:
    """Get a list of non-identity `PauliTerm` constructor arguments."""
    return [
        ConstrArg(PauliGate.X, 0, "q0", Expression.Pi(), Expression.Pi()),
        ConstrArg(PauliGate.Y, "b", "b", -1.0, Expression.Number(-1.0)),
    ]


@pytest.fixture
def terms(constr_args: list[ConstrArg]) -> list[PauliTerm]:
    return [PauliTerm(term.op, term.index, term.coeff) for term in constr_args]


@pytest.fixture
def arguments(constr_args: list[ConstrArg]) -> list[PauliTargetDesignator]:
    return [term.index for term in constr_args]


class TestTermConstructor:
    def test_single(self, constr_args: list[ConstrArg]):
        for op, index, qubit_str, coeff, expr in constr_args:
            pt = PauliTerm(op, index, coeff)
            assert pt.arguments == [(op, qubit_str)]
            assert pt.expression == expr

    def test_single_default_coeff(self, constr_args: list[ConstrArg]):
        for op, index, qubit_str, _, _ in constr_args:
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

    def test_list_constructor(self, constr_args: list[ConstrArg]):
        for op, index, qubit_str, coeff, expr in constr_args:
            with pytest.warns(DeprecationWarning):
                pt = PauliTerm([(op, index)], coeff)
            assert pt.arguments == [(op, qubit_str)]
            assert pt.expression == expr

    def test_list_default_coeff(self, constr_args: list[ConstrArg]):
        for op, index, qubit_str, _, _ in constr_args:
            with pytest.warns(DeprecationWarning):
                pt = PauliTerm([(op, index)])
            assert pt.arguments == [(op, qubit_str)]
            assert pt.expression == Expression.Number(1.0)

    def test_from_list(self, constr_args: list[ConstrArg]):
        """Confirm the ``from_list`` method is equivalent to using the constructor."""
        terms_list = [(term.op, term.index) for term in constr_args]
        with pytest.warns(DeprecationWarning):
            from_constructor = PauliTerm(terms_list)
        assert from_constructor == PauliTerm.from_list(terms_list)

    def test_placeholder_raises(self):
        """Confirm that we cannot construct a PauliTerm with a placeholder qubit."""
        with pytest.raises(TypeError):
            PauliTerm(PauliGate.X, QubitPlaceholder(), 1.0)  # pyright: ignore
        with pytest.raises(TypeError):
            PauliTerm([PauliGate.X, QubitPlaceholder()], 1.0)  # pyright: ignore


class TestPauliTerm:
    def test_iteration(self, constr_args: list[ConstrArg]):
        """Confirm we can iterate over the arguments of a PauliTerm."""

        terms_list = [(term.op, term.index) for term in constr_args]
        args_list = [(term.op, term.qubit_str) for term in constr_args]
        term = PauliTerm.from_list(terms_list)

        assert list(iter(term)) == args_list == list(term)


@pytest.fixture
def t_2_X0() -> PauliTerm:
    return PauliTerm(PauliGate.X, 0, 2.0)


class TestTermMultiplication:
    def test_term(self, t_2_X0: PauliTerm):
        product = t_2_X0 * PauliTerm(PauliGate.Y, 1, 3.0)
        assert product == PauliTerm.from_list([(PauliGate.X, "q0"), (PauliGate.Y, "q1")], 6.0)

    @pytest.mark.parametrize(
        "three",
        (
            3,
            3.0,
            3.0 + 0.0j,
            Expression.Number(3.0),
            np.int8(3),
            np.int16(3),
            np.int32(3),
            np.int64(3),
            np.float32(3.0),
            np.float64(3.0),
            np.complex64(3.0),
            np.complex128(3.0),
        ),
    )
    def test_numbers(self, t_2_X0: PauliTerm, three: int | float | complex | Expression):
        expected = PauliTerm("X", 0, 6.0)
        assert t_2_X0 * three == expected
        assert three * t_2_X0 == expected


class TestTermPower:
    """Test exponentiation of PauliTerms."""

    def test_zero(self, t_2_X0: PauliTerm):
        assert t_2_X0**0 == PauliTerm("I", None)

    def test_one(self, t_2_X0: PauliTerm):
        assert t_2_X0**1 == PauliTerm.from_list(t_2_X0.arguments, coefficient=t_2_X0.expression)

    def test_even(self, t_2_X0: PauliTerm):
        coefficient = (t_2_X0.expression**2).into_simplified()
        assert t_2_X0**2 == PauliTerm("I", None, coefficient)

    def test_odd(self, t_2_X0: PauliTerm):
        coefficient = (t_2_X0.expression**3).into_simplified()
        assert t_2_X0**3 == PauliTerm.from_list(t_2_X0.arguments, coefficient=coefficient)

    @pytest.mark.parametrize("k", [0, 1, 2, 3, 4, 5, 2**10, 2**32, 2**33, 2**33+1, 2**63-1])
    def test_ints(self, t_2_X0: PauliTerm, k: int):
        actual = t_2_X0**k
        assert isinstance(actual, PauliTerm), f"integer exponents ({k=}) should reduce to a PauliTerm"

        if k % 2 == 0:
            assert all(op == PauliGate.I for op, _ in actual), f"even exponents ({k=})) should reduce to identity"
        else:
            assert set(actual.arguments) == set(t_2_X0.arguments), f"odd exponents ({k=}) should preserve the original arguments"

        assert (
            actual.expression.into_simplified() == (t_2_X0.expression**k).into_simplified()
        ), f"{actual.expression.to_quil()} != {t_2_X0.expression.to_quil()}"


    def test_expr(self, t_2_X0: PauliTerm):
        k = Expression.parse("2*pi + 1")
        # TODO: this may justify making Expression work like Instruction, as a class hierarchy.
        exp_i_pi_k = Expression.FunctionCall(quil_exp(1.0j * Expression.Pi() * k))
        a = (1 + exp_i_pi_k) / 2
        b = (1 - exp_i_pi_k) / 2
        c_pow_k = t_2_X0.expression**k
        i_term = PauliTerm("I", None, c_pow_k * a)
        p_term = PauliTerm.from_list(t_2_X0.arguments, coefficient=c_pow_k * b)

        actual = t_2_X0**k
        expected = i_term + p_term
        assert set(actual.arguments) == set(expected.arguments)
        for t0, t1 in zip(actual.terms, expected.terms):
            assert set(t0.arguments) == set(t1.arguments)
            assert (
                t0.expression.into_simplified() == t1.expression.into_simplified()
            ), f"{t0.expression.to_quil()} != {t1.expression.to_quil()}"

    def test_int_vs_float(self, t_2_X0: PauliTerm):
        assert t_2_X0**3 == t_2_X0**3.0


class TestSumConstructor:
    """Confirm all variants of the `PauliSum` constructor work as expected."""

    def test_terms_only(self, terms: list[PauliTerm]):
        """The constructor should accept a single argument, interpreted as `terms`"""
        positional = PauliSum(terms)
        assert positional.terms == terms
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

    def test_invalid(self, terms: list[PauliTerm], arguments: list[PauliTargetDesignator]):
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
            PauliSum(arguments=terms)  # pyright: ignore

        with pytest.raises(ValueError):
            PauliSum(arguments)
