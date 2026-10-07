from typing import TypeAlias, cast

import pytest
import numpy as np

import quil
from quil.instructions import MemoryReference, GateDefinition, GateSpecification
from quil.expression import (
    Address,
    Expression,
    ExpressionDesignator,
    FunctionCallExpression,
    ExpressionFunction,
    InfixExpression,
    InfixOperator,
    Number,
    Parameter,
    Pi,
    PrefixExpression,
    PrefixOperator,
    quil_cos,
    quil_sin,
)

VarMap: TypeAlias = dict[str | MemoryReference, complex | list[complex]]
ZERO = Number(0)
ONE = Number(1)

class TestConstruction:
    """Test constructing `Expression`s from various `ExpressionLike` values."""

    def test_pi(self) -> None:
        assert Expression(Pi) == Pi == Expression.parse("pi")
        assert isinstance(Pi, Expression)

    def test_number(self) -> None:
        assert isinstance(Number(1), Expression)
        assert isinstance(Number(1.0), Expression)
        assert isinstance(Number(1.0 + 0.0j), Expression)

    def test_parameter(self) -> None:
        assert isinstance(Parameter("x"), Expression)
        assert isinstance(Parameter("y"), Expression)

    def test_address(self) -> None:
        assert isinstance(Address(MemoryReference("x")), Expression)
        assert isinstance(Address(MemoryReference("y")), Expression)

    def test_infix_expression(self) -> None:
        expr = InfixExpression(Number(1), InfixOperator.PLUS, Number(2))
        assert isinstance(expr, Expression)
        assert expr.left == Number(1)
        assert expr.right == Number(2)
        assert expr.operator == InfixOperator.PLUS

    def test_prefix_expression(self) -> None:
        expr = PrefixExpression(PrefixOperator.MINUS, Number(3))
        assert isinstance(expr, Expression)
        assert expr.expression == Number(3)
        assert expr.operator == PrefixOperator.MINUS

    def test_function_call_expression(self) -> None:
        expr = FunctionCallExpression(ExpressionFunction.SINE, Parameter("x"))
        assert isinstance(expr, Expression)
        assert expr.function == ExpressionFunction.SINE
        assert expr.expression == Parameter("x")

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


class TestConstructor:
    """Test flexible `ExpressionLike` conversions accepted by `Expression`-taking constructors."""

    def test_str(self):
        """A bare `str` is interpreted as a `Parameter` (i.e. `%<name>`)."""
        expr = PrefixExpression(PrefixOperator.MINUS, "x")
        assert expr.expression == Parameter("x")

    def test_memref(self):
        """A bare `MemoryReference` is interpreted as an `Address`."""
        memref = MemoryReference("x")
        expr = PrefixExpression(PrefixOperator.MINUS, memref)
        assert expr.expression == Address(memref)

    @pytest.mark.parametrize("value", [1, 1.0, 1.0 + 0.0j])
    def test_number(self, value: int | float | complex):
        """Bare `int`/`float`/`complex` values are interpreted as a `Number`."""
        expr = PrefixExpression(PrefixOperator.MINUS, value)
        assert expr.expression == Number(1.0 + 0.0j)


class TestExpressionLikeConstructors:
    """Check that constructors accepting `ExpressionLike` avoid requiring manual wrapping."""

    def test_infix_expression(self):
        expr = InfixExpression(1, InfixOperator.PLUS, 2)
        assert expr.left == Number(1)
        assert expr.right == Number(2)
        assert complex(expr.evaluate({}, {})) == complex(3)

    def test_prefix_expression(self):
        expr = PrefixExpression(PrefixOperator.MINUS, 3.0)
        assert expr.expression == Number(3.0)
        assert complex(expr.evaluate({}, {})) == complex(-3.0)

    def test_function_call_expression(self):
        expr = FunctionCallExpression(ExpressionFunction.SINE, "x")
        assert expr.expression == Parameter("x")


def test_substitute() -> None:
    """Check that substitution works with dictionaries."""

    e1 = Expression.parse(r"cis(pi / 2) + %x + y[1]")
    variables = {"x": 1.0j}
    memory_references = {"y": [1.0, 2]}
    full_mapping: VarMap = cast(VarMap, memory_references | variables)

    e2 = e1.substitute_variables(variables)
    with pytest.deprecated_call():
        e3 = e2.substitute(full_mapping)
        e4 = e1.substitute(full_mapping)

    assert complex(e3) == e4
    x = ((np.array(e2) ** 0)[0]).into_simplified()
    assert float(x) == 1.0

    # Validate that `evaluate(..., partial=True)` is equivalent to `substitute`.
    assert e3 == e2.evaluate(variables, memory_references, partial=True)
    assert e4 == e1.evaluate(variables, memory_references, partial=True)


def test_substitute_memref() -> None:
    """We can perform substitution with memory references."""

    expr = Expression.parse(r"%x + y[1]")
    with pytest.deprecated_call():
        assert expr.substitute(cast(VarMap, dict(x=1, y=(2, 3)))) == 4+0j
        assert expr.substitute(cast(VarMap, dict(x=1))) == Expression.parse("1 + y[1]")
        assert expr.substitute(cast(VarMap, dict(x=1, y=(2,)))) == Expression.parse("1 + y[1]")
        assert expr.substitute(cast(VarMap, dict(y=(2,3)))) == Expression.parse(r"%x + 3")


def test_gate_definition() -> None:
    """Check that `GateDefinition` accepts bare (unwrapped) matrices made of `ExpressionLike` values."""

    theta = Parameter("theta")

    crx: list[list[ExpressionDesignator]] = [
        [1, 0, 0, 0],
        [0, 1, 0, 0],
        [0, 0, quil_cos(theta / 2), quil_sin(theta / 2) * -1.0j],
        [0, 0, quil_sin(theta / 2) * -1.0j, quil_cos(theta / 2)],
    ]
    arr = np.array(crx, dtype=object)

    from_list = GateDefinition("CRX", ["theta"], crx)
    from_arr = GateDefinition("CRX", ["theta"], arr)
    assert from_list == from_arr
    assert isinstance(from_list.specification, GateSpecification)


def test_gate_definition_deprecated_constructor() -> None:
    """Check that the deprecated PyQuil v4 `(name, matrix, parameters=None)` constructor still works."""

    crx: list[list[ExpressionDesignator]] = [
        [1, 0, 0, 0],
        [0, 1, 0, 0],
    ]
    arr = np.array(crx, dtype=object)
    preferred = GateDefinition("CRX", ["theta"], crx)

    # Old positional order: `(name, matrix, parameters)`, with a bare list or ndarray matrix.
    with pytest.deprecated_call():
        assert GateDefinition("CRX", crx, ["theta"]) == preferred
    with pytest.deprecated_call():
        assert GateDefinition("CRX", arr, ["theta"]) == preferred

    # `parameters` is optional in the deprecated form.
    with pytest.deprecated_call():
        assert GateDefinition("CRX", crx).parameters == []

    # `matrix` is also accepted as a deprecated keyword-only alias for `specification`.
    with pytest.deprecated_call():
        assert GateDefinition("CRX", parameters=["theta"], matrix=crx) == preferred

    # `specification`/`matrix`, and `parameters` given both positionally (new-style)
    # and as part of the deprecated positional form, are mutually exclusive.
    with pytest.raises(ValueError):
        GateDefinition("CRX", crx, specification=GateSpecification.Matrix(crx))
    with pytest.raises(quil.ValueError):
        GateDefinition("CRX", crx, ["theta"], matrix=GateSpecification.Matrix(crx))
    with pytest.raises(quil.ValueError):
        GateDefinition("CRX")

