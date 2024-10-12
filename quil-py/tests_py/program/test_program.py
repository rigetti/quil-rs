import pickle
from typing import Optional

import pytest
from syrupy.assertion import SnapshotAssertion

from quil.instructions import Gate, Instruction, Jump, Qubit, QubitPlaceholder, Target, TargetPlaceholder
from quil.program import Program


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
"""
    )
    assert program.get_used_qubits() == {Qubit.from_fixed(0), Qubit.from_fixed(1)}
    cfg = program.control_flow_graph()
    blocks = cfg.basic_blocks()
    assert len(blocks) == 1
    assert blocks[0].terminator() is None
    block_program = Program()
    block_program.add_instructions(blocks[0].instructions())
    assert (
        block_program.to_quil()
        == """H 0
CNOT 0 1
MEASURE 0 ro[0]
MEASURE 1 ro[1]
"""
    )


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
"""
    )
    assert program.get_used_qubits() == {Qubit.from_fixed(0)}
    cfg = program.control_flow_graph()

    assert cfg.has_dynamic_control_flow()

    blocks = cfg.basic_blocks()
    assert len(blocks) == 3


def test_basic_block_fixed_schedule():
    """Test that a simple, realistic program can be scheduled as expected."""
    program = Program.parse(
        """DEFFRAME 0 "flux_tx_cz":
    TEST: 1

DEFFRAME 1 "flux_tx_iswap":
    TEST: 1

DEFFRAME 1 "flux_tx_cz":
    TEST: 1

DEFFRAME 1 "flux_tx_iswap":
    TEST: 1

DEFFRAME 2 "flux_tx_cz":
    TEST: 1

DEFFRAME 2 "flux_tx_iswap":
    TEST: 1

DEFFRAME 3 "flux_tx_cz":
    TEST: 1

DEFFRAME 3 "flux_tx_iswap":
    TEST: 1

# Simplified version
DEFCAL CZ q0 q1:
    FENCE q0 q1
    SET-PHASE q0 "flux_tx_cz" 0.0
    SET-PHASE q1 "flux_tx_iswap" 0.0
    NONBLOCKING PULSE q0 "flux_tx_cz" erf_square(duration: 6.000000000000001e-08, pad_left: 0.1e-08, pad_right: 0.1e-08)
    NONBLOCKING PULSE q1 "flux_tx_iswap" erf_square(duration: 6.000000000000001e-08, pad_left: 0.1e-08, pad_right: 0.1e-08)
    SHIFT-PHASE q0 "flux_tx_cz" 1.0
    SHIFT-PHASE q1 "flux_tx_iswap" 1.0
    FENCE q0 q1

CZ 0 1
CZ 2 3
CZ 0 2
"""
    )
    cfg = program.control_flow_graph()
    blocks = cfg.basic_blocks()
    assert len(blocks) == 1
    block = blocks[0]

    schedule = block.as_schedule_seconds(program)
    items = schedule.items()

    # One for each CZ
    assert len(items) == 3

    items = {item.instruction_index: item for item in items}

    # The first CZ should start at 0
    assert items[0].time_span.start == 0.0

    # The first CZ should start and end at the same times
    assert items[0].time_span.start == items[1].time_span.start
    assert items[0].time_span.duration == items[1].time_span.duration

    # The third CZ should start when the first and second ones end and be of the same duration
    assert items[0].time_span.start + items[0].time_span.duration == items[2].time_span.start
    assert items[0].time_span.duration == items[2].time_span.duration


def test_filter_instructions(snapshot: SnapshotAssertion):
    input = """DECLARE foo REAL[1]
DEFFRAME 1 "rx":
\tHARDWARE-OBJECT: "hardware"
DEFCAL I 1:
\tDELAY 0 1
DEFGATE BAR AS MATRIX:
\t0, 1
\t1, 0

H 1
CNOT 2 3
"""
    program = Program.parse(input)
    program_without_quil_t = program.filter_instructions(lambda instruction: not instruction.is_quil_t())
    assert program_without_quil_t.to_quil() == snapshot


def test_calibration_expansion():
    """
    Assert that program calibration expansion happens as expected and that the source map is correct.
    """
    import inspect

    program_text = inspect.cleandoc(
        """
        DEFCAL X 0:
            Y 0

        DEFCAL Y 0:
            Z 0

        X 0
        Y 0
        """
    )
    program = Program.parse(program_text)
    expansion = program.expand_calibrations_with_source_map()
    source_map = expansion.source_map()

    expected_program_text = inspect.cleandoc(
        """
        DEFCAL X 0:
            Y 0

        DEFCAL Y 0:
            Z 0

        Z 0
        Z 0
        """
    )

    assert expansion.program().to_quil() == Program.parse(expected_program_text).to_quil()

    # The X at index 0 should have been replaced with a Z at index 0
    targets = source_map.list_targets_for_source_index(0)
    assert len(targets) == 1
    expanded = targets[0].as_expanded()
    assert expanded.range() == range(0, 1)
    assert source_map.list_sources_for_target_index(0) == [0]

    # The Y at index 1 should have been replaced with a Z at index 1
    targets = source_map.list_targets_for_source_index(1)
    assert len(targets) == 1
    expanded = targets[0].as_expanded()
    assert expanded.range() == range(1, 2)
    assert source_map.list_sources_for_target_index(1) == [1]

    # There is no source index 2 and so there should be no mapping
    assert source_map.list_targets_for_source_index(2) == []
