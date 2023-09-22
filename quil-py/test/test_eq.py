from copy import deepcopy

import numpy as np

from quil.instructions import Instruction, Gate, Qubit
from quil.expression import Expression
from quil.program import Program

def test_instruction_eq():
    pi_expr = Expression.from_number(complex(np.pi, 0))

    rx = Gate("RX", [pi_expr], [Qubit.from_fixed(0)], [])
    rx_copy = deepcopy(rx)
    ry = Gate("RY", [pi_expr], [Qubit.from_fixed(0)], [])
    assert rx == rx_copy
    assert not (rx != rx_copy)
    assert rx != ry
    assert not (rx == ry)

    rx_inst = Instruction.from_gate(rx)
    rx_inst_copy = deepcopy(rx_inst)
    ry_inst = Instruction.from_gate(ry)
    assert rx_inst == rx_inst_copy
    assert not (rx_inst != rx_inst_copy)
    assert rx_inst != ry_inst
    assert not (rx_inst == ry_inst)

def test_program_eq():
    p1 = Program.parse("DECLARE ro BIT\nRX(pi) 0")
    p1_copy = deepcopy(p1)
    p2 = Program.parse("DECLARE theta BIT\nRX(pi) 0")

    assert p1 == p1_copy
    assert not (p1 != p1_copy)
    assert p1 != p2
    assert not (p1 == p2)
