from collections.abc import Sequence

import pytest

from quil.instructions import Gate, Jump, Label, Qubit, Target, TargetPlaceholder
from quil.program import Program


def quil(instructions):
    return [instruction.to_quil_or_debug() for instruction in instructions]


@pytest.fixture
def program() -> Program:
    return Program.parse("H 0\nX 1\nH 0\nY 2")


def test_is_a_sequence(program: Program):
    assert isinstance(program.body_instructions, Sequence)


def test_indexing(program: Program):
    view = program.body_instructions
    assert len(view) == 4
    assert view[0].to_quil() == "H 0"
    assert view[-1].to_quil() == "Y 2"
    assert quil(view[1::2]) == ["X 1", "Y 2"]
    assert quil(view[::-1]) == ["Y 2", "H 0", "X 1", "H 0"]
    with pytest.raises(IndexError):
        view[4]
    with pytest.raises(IndexError):
        view[-5]


def test_iteration_and_search(program: Program):
    view = program.body_instructions
    h0 = Gate("H", [], [Qubit(0)], [])
    assert quil(view) == ["H 0", "X 1", "H 0", "Y 2"]
    assert quil(reversed(view)) == ["Y 2", "H 0", "X 1", "H 0"]
    assert h0 in view
    assert Gate("Z", [], [Qubit(0)], []) not in view
    assert view.index(h0) == 0
    assert view.count(h0) == 2
    with pytest.raises(ValueError):
        view.index(Gate("Z", [], [Qubit(0)], []))


def test_equality(program: Program):
    view = program.body_instructions
    assert view == list(view)
    assert view != list(view)[:-1]


def test_view_is_live(program: Program):
    view = program.body_instructions
    program.add_instruction(Gate("Z", [], [Qubit(3)], []))
    assert len(view) == 5
    assert view[-1].to_quil() == "Z 3"

    placeholder = Target.Placeholder(TargetPlaceholder("loop"))
    program.add_instruction(Label(placeholder))
    program.add_instruction(Jump(placeholder))
    program.resolve_placeholders()
    assert quil(view[-2:]) == ["LABEL @loop_0", "JUMP @loop_0"]


def test_reads_return_the_cached_object(program: Program):
    view = program.body_instructions
    first = view[0]
    assert view[0] is first
    assert next(iter(view)) is first


def test_append_keeps_the_cache(program: Program):
    view = program.body_instructions
    first = view[0]
    program.add_instruction(Gate("Z", [], [Qubit(3)], []))
    assert view[0] is first
    assert view[-1].to_quil() == "Z 3"


def test_other_mutation_drops_the_cache(program: Program):
    view = program.body_instructions
    first = view[0]
    program.resolve_placeholders()
    assert view[0] is not first
    assert view[0] == first


@pytest.mark.parametrize(
    "quil_text, field",
    [
        ('PULSE 0 "rf" flat(duration: 1e-6, iq: 1)', "blocking"),
        ('CAPTURE 0 "ro_rx" flat(duration: 1e-6, iq: 1) ro[0]', "blocking"),
        ("JUMP @start", "target"),
        ("JUMP-WHEN @start ro[0]", "target"),
        ("JUMP-UNLESS @start ro[0]", "target"),
    ],
)
def test_cached_instructions_are_frozen(quil_text: str, field: str):
    program = Program.parse(f"DECLARE ro BIT[1]\nLABEL @start\n{quil_text}")
    instruction = program.body_instructions[-1]
    with pytest.raises(AttributeError):
        setattr(instruction, field, getattr(instruction, field))
