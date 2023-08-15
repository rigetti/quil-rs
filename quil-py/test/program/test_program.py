import pickle
import pytest
from typing import Optional

from quil.program import Program
from quil.instructions import Instruction, QubitPlaceholder, LabelPlaceholder, Gate, Qubit, Jump, Label


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

    label_placeholder = LabelPlaceholder("base-label")

    def label_resolver(label: LabelPlaceholder) -> Optional[str]:
        print("resolving label", label)
        return {label_placeholder: "test"}.get(label)

    program = Program()
    program.add_instructions(
        [
            Instruction.from_gate(Gate("H", [], [Qubit.from_placeholder(qubit_placeholder)], [])),
            Instruction.from_jump(Jump(Label.from_placeholder(label_placeholder))),
        ]
    )

    with pytest.raises(ValueError):
        program.to_quil()

    program.resolve_placeholders_with_custom_resolvers(label_resolver=label_resolver, qubit_resolver=qubit_resolver)

    print(program.to_quil_or_debug())

    assert program.to_quil() == "H 9\nJUMP  @test\n"
