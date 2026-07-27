from copy import deepcopy

import numpy as np

from quil.expression import Expression
from quil.instructions import Gate, Qubit
from quil.program import Program


def test_instruction_eq():
    pi_expr = Expression.Number(complex(np.pi, 0))

    rx = Gate("RX", [pi_expr], [Qubit.Fixed(0)], [])
    rx_copy = deepcopy(rx)
    ry = Gate("RY", [pi_expr], [Qubit.Fixed(0)], [])
    assert rx == rx_copy
    assert not (rx != rx_copy)
    assert rx != ry
    assert not (rx == ry)


def test_program_eq():
    p1 = Program.parse("DECLARE ro BIT\nRX(pi) 0")
    p1_copy = deepcopy(p1)
    p2 = Program.parse("DECLARE theta BIT\nRX(pi) 0")

    assert p1 == p1_copy
    assert not (p1 != p1_copy)
    assert p1 != p2
    assert not (p1 == p2)
