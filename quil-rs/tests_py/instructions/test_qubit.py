from typing import TypeAlias, cast

import pytest
import numpy as np

from quil.instructions import GateSpecification, Instruction, MemoryReference, TargetPlaceholder, Label, Qubit, Gate, Move
from quil.program import Program
from quil.expression import Expression
from quil.instructions import Declaration
from quil.instructions import QubitPlaceholder

class TestConstructor:
    """Test that we construct `Qubit`s in several different ways.

    We allow users to construct `Qubit`s directly by passing "obvious" values,
    such as an integer to create a `Qubit.Fixed` instance,
    and we accept those same things in other places that `Qubit`s are expected,
    automatically creating the `Qubit` instance when necessary.
    """

    def test_fixed(self):
        g = Gate("X", (), (1, Qubit(2), Qubit.Fixed(3)))
        for q in g.qubits:
            assert isinstance(q, Qubit)
            assert isinstance(q, Qubit.Fixed)
            assert q == Qubit.Fixed(q._0)

    def test_variable(self):
        g = Gate("X", (), ("x", Qubit("y"), Qubit.Variable("z")))
        for q in g.qubits:
            assert isinstance(q, Qubit)
            assert isinstance(q, Qubit.Variable)
            assert q == Qubit.Variable(q._0)

    def test_placeholder(self):
        g = Gate("X", (), (QubitPlaceholder(), Qubit.Placeholder(QubitPlaceholder())))
        for q in g.qubits:
            assert isinstance(q, Qubit)
            assert isinstance(q, Qubit.Placeholder)
