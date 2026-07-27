from typing import TypeAlias, cast

import pytest
import numpy as np

from quil.instructions import MemoryReference
from quil.expression import Expression

VarMap: TypeAlias = dict[str | MemoryReference, complex | list[complex]]
ZERO = Expression(0)
ONE = Expression(1)

class TestOperations:
    """Test performing mathematical operations on `Expression`s.

    The general form of these tests that is
        "Expression mixed with [int, float, complex, Expression]"
    and
        "[int, float, complex, Expression] mixed with Expression"
    evaluate to the same (complex) result.
    """

    def test_conversion(self) -> None:
        assert ZERO.evaluate({}, {}) == complex(ZERO) == complex(float(ZERO)) == complex(0)
        assert ONE.evaluate({}, {}) == complex(ONE) == complex(float(ONE)) == complex(1)

    def test_add(self) -> None:
        assert complex(ZERO + 0) == complex(ZERO + 0.0) == complex(ZERO + 0.0j) == complex(ZERO + ZERO) == complex(0+0)
        assert complex(ZERO + ONE) == complex(ZERO) + complex(ONE)
        assert complex(ONE + ZERO) == complex(ONE) + complex(ZERO)

    def test_radd(self) -> None:
        assert complex(0 + ZERO) == complex(0.0 + ZERO) == complex(0.0j + ZERO) == complex(ZERO + ZERO) == complex(0+0)

    def test_sub(self) -> None:
        assert complex(ZERO - 0) == complex(ZERO - 0.0) == complex(ZERO - 0.0j) == complex(ZERO - ZERO) == complex(0-0)

    def test_rsub(self) -> None:
        assert complex(0 - ZERO) == complex(0.0 - ZERO) == complex(0.0j - ZERO) == complex(ZERO - ZERO) == complex(0-0)

    def test_mul(self) -> None:
        assert complex(ZERO * 0) == complex(ZERO * 0.0) == complex(ZERO * 0.0j) == complex(ZERO * ZERO) == complex(0*0)

    def test_rmul(self) -> None:
        assert complex(0 * ZERO) == complex(0.0 * ZERO) == complex(0.0j * ZERO) == complex(ZERO * ZERO) == complex(0*0)

    def test_div(self) -> None:
        assert complex(ZERO / 1) == complex(ZERO / 1.0) == complex(ZERO / (1+0j)) == complex(ZERO / ONE) == complex(0/1)

    def test_rdiv(self) -> None:
        assert complex(0 / ONE) == complex(0.0 / ONE) == complex(0.0j / ONE) == complex(ZERO / ONE) == complex(0/1)

    def test_pow(self) -> None:
        assert complex(ONE ** ONE) == complex(ONE) ** complex(ONE)
        assert complex(ZERO ** 1) == complex(ZERO ** 1.0) == complex(ZERO ** (1+0j)) == complex(ZERO ** ONE) == complex(0**1)

    def test_rpow(self) -> None:
        assert complex(0 ** ONE) == complex(0.0 ** ONE) == complex(0.0j ** ONE) == complex(ZERO ** ONE) == complex(0**1)

    def test_pos(self) -> None:
        assert complex(+ONE) == +complex(ONE)

    def test_neg(self) -> None:
        assert complex(-ONE) == -complex(ONE)


class TestExtraction:
    """Test extractions to `Expression` from various types."""

    def test_str(self):
        assert Expression("x") == Expression.Variable("x")

    @pytest.mark.parametrize("value", [1, 1.0, 1.0 + 0.0j])
    def test_number(self, value: int | float | complex):
        assert Expression(value) == Expression.Number(1.0+0.0j)

    @pytest.mark.parametrize("value", [1, 1.0, 1.0 + 0.0j])
    def test_number(self, value: int | float | complex):
        assert Expression(value) == Expression.Number(1.0+0.0j)


def test_substitute() -> None:
    """Check that substitution works with dictionaries."""

    e1 = Expression.parse(r"cis(pi / 2) + %x + y[1]")
    variable_values = {"x": 1.0j}
    other: VarMap = {"y": [1.0, 2]}
    full_mapping = other | variable_values

    e2 = e1.substitute_variables(variable_values)
    e3 = e2.substitute(full_mapping)
    e4 = e1.substitute(full_mapping)

    assert complex(e3) == e4
    x = ((np.array(e2) ** 0)[0]).into_simplified()
    assert float(x) == 1.0


def test_substitute_memref() -> None:
    """We can perform substitution with memory references."""

    expr = Expression.parse(r"%x + y[1]")
    assert expr.substitute(cast(VarMap, dict(x=1, y=(2, 3)))) == 4+0j
    assert expr.substitute(cast(VarMap, dict(x=1))) == Expression.parse("1 + y[1]")
    assert expr.substitute(cast(VarMap, dict(x=1, y=(2,)))) == Expression.parse("1 + y[1]")
    assert expr.substitute(cast(VarMap, dict(y=(2,3)))) == Expression.parse(r"%x + 3")


def test_gate_definition() -> None:
    """Check that we can correctly use `Expression`s in gate definitions."""

    # We need a couple helpers to don't really belong in `quil` necessarily.
