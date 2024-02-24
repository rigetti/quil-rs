import pickle
import pytest
from typing import Optional

from quil.program import Program
from quil.instructions import Instruction, QubitPlaceholder, TargetPlaceholder, Gate, Qubit, Jump, Target


def test_pickle():
    program = Program.parse(
        """
DECLARE ro BIT[2]
H 0
CNOT 0 1
MEASURE 0 ro[0]
MEASURE 1 ro[1]
"""
    )
    pickled = pickle.dumps(program)
    unpickled = pickle.loads(pickled)
    assert program == unpickled


def test_custom_resolver():
    qubit_placeholder = QubitPlaceholder()

    def qubit_resolver(qubit: QubitPlaceholder) -> Optional[int]:
        return {qubit_placeholder: 9}.get(qubit)

    target_placeholder = TargetPlaceholder("base-target")

    def target_resolver(target: TargetPlaceholder) -> Optional[str]:
        print("resolving target", target)
        return {target_placeholder: "test"}.get(target)

    program = Program()
    program.add_instructions(
        [
            Instruction.from_gate(Gate("H", [], [Qubit.from_placeholder(qubit_placeholder)], [])),
            Instruction.from_jump(Jump(Target.from_placeholder(target_placeholder))),
        ]
    )

    with pytest.raises(ValueError):
        program.to_quil()

    program.resolve_placeholders_with_custom_resolvers(target_resolver=target_resolver, qubit_resolver=qubit_resolver)

    print(program.to_quil_or_debug())

    assert program.to_quil() == "H 9\nJUMP @test\n"

def test_single_block_control_flow_analysis():
    program = Program.parse(
        """
DECLARE ro BIT[2]
H 0
CNOT 0 1
MEASURE 0 ro[0]
MEASURE 1 ro[1]
""")
    assert program.get_used_qubits() == {Qubit.from_fixed(0), Qubit.from_fixed(1)}
    cfg = program.control_flow_graph()
    blocks = cfg.basic_blocks()
    assert len(blocks) == 1
    assert blocks[0].terminator() == None
    block_program = Program()
    block_program.add_instructions(blocks[0].instructions())
    assert block_program.to_quil() == """H 0
CNOT 0 1
MEASURE 0 ro[0]
MEASURE 1 ro[1]
"""

def test_multi_block_control_flow_analysis():
    program = Program.parse(
        """
DECLARE ro BIT[1]
LABEL @start
MEASURE 0 ro[0]
JUMP-UNLESS @end ro[0]
X 0
JUMP @start
LABEL @end
HALT
""")
    assert program.get_used_qubits() == {Qubit.from_fixed(0)}
    cfg = program.control_flow_graph()
    blocks = cfg.basic_blocks()
    assert len(blocks) == 3