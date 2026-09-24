# Copyright 2026 Rigetti Computing
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import altair as alt
import pytest
from conftest import load
from quil.instructions import Instruction, Qubit
from quil.program import Program

from quil.plotting import PlottableProgramCircuit
from quil.plotting.circuit import _delay_label, _gate_label


def one_block(name: str):
    blocks = [
        block for block in PlottableProgramCircuit(load(name))._blocks if block.visible_events
    ]
    assert len(blocks) == 1
    return blocks[0]


def test_every_program_builds_and_draws_a_valid_spec(program):
    circuit = PlottableProgramCircuit(program)
    drawable = [block for block in circuit._blocks if block.visible_events]
    assert drawable

    for block in drawable:
        assert block.column_count >= 1

        # The packing invariant: two events in one column must not touch the same track, or they
        # would be drawn on top of each other.
        occupied: set[tuple[int, object]] = set()
        for event in block.visible_events:
            for track in event.tracks:
                assert (event.column, track) not in occupied
                occupied.add((event.column, track))

        chart = block.draw()
        assert isinstance(chart, alt.LayerChart)
        chart.to_dict()


def test_independent_operations_share_a_column():
    block = one_block("sequenced_hadamard_delays")
    assert block.column_count < len(block.visible_events)


def test_a_fence_serializes_the_qubits_it_spans():
    def columns_per_qubit(block) -> dict[str, set[int]]:
        columns: dict[str, set[int]] = {}
        for event in block.visible_events:
            for track in event.tracks:
                columns.setdefault(track.name, set()).add(event.column)
        return columns

    fenced = columns_per_qubit(one_block("sequenced_hadamard_barrier"))
    unfenced = columns_per_qubit(one_block("sequenced_hadamard_delays"))

    assert not fenced["10"] & fenced["11"], "the fence should have serialized the two qubits"
    assert unfenced["10"] & unfenced["11"], "without a fence they pack in parallel"


def test_a_measurement_drops_onto_its_register_element():
    block = one_block("multiple_gates_with_measures")
    measures = [event for event in block.visible_events if event.label == "MEASURE"]
    assert len(measures) > 1

    assert {event.target_reference for event in measures} == {"ro[0]", "ro[1]", "ro[2]"}
    assert len({event.column for event in measures}) == 1
    # Each measurement's target is a distinct packing track collapsing onto one drawn row.
    assert len({event.target_track for event in measures}) == len(measures)
    assert len({event.target_track.row for event in measures}) == 1

    assert [track.label for track in block.register_tracks] == ["ro"]
    assert block.register_tracks[0] is block.tracks[-1], "registers sink below the qubit wires"


def test_rows_are_qubits_in_natural_order_then_registers():
    block = one_block("multiple_gates_with_measures")
    assert [track.label for track in block.tracks] == [
        "Qubit: 10",
        "Qubit: 11",
        "Qubit: 12",
        "ro",
    ]


def test_gate_label_keeps_parameters_and_drops_qubits():
    (gate,) = [instruction for instruction in Program.parse("RZ(-pi/2) 10").body_instructions]
    assert _gate_label(gate, 1) == "RZ(-pi/2)"

    (controlled,) = [
        instruction for instruction in Program.parse("CONTROLLED RX(pi) 0 1").body_instructions
    ]
    assert _gate_label(controlled, 2) == "CONTROLLED RX(pi)"


def test_delay_label_picks_a_readable_unit():
    (short,) = Program.parse("DELAY 0 40e-9").body_instructions
    (long,) = Program.parse("DELAY 0 100e-6").body_instructions
    assert _delay_label(short._0) == "DELAY 40 ns"
    assert _delay_label(long._0) == "DELAY 100 µs"

    # A duration that does not land on a whole unit is the case that pins the formatting: an
    # integer result looks the same however many figures you asked for.
    (odd,) = Program.parse("DELAY 0 123.456789e-9").body_instructions
    (half,) = Program.parse("DELAY 0 1.5e-3").body_instructions
    assert _delay_label(odd._0) == "DELAY 123.46 ns"
    assert _delay_label(half._0) == "DELAY 1.5 ms"


def test_controls_are_drawn_as_dots_not_boxes():
    circuit = PlottableProgramCircuit(Program.parse("CONTROLLED RX(pi) 0 1\n"))
    (event,) = circuit._blocks[0].visible_events
    assert [track.name for track in event.controls] == ["0"]
    assert len(event.tracks) == 2


def test_colors_only_classify_actual_quantum_operations():
    circuit = PlottableProgramCircuit(load("test_blocks"))
    block = next(b for b in circuit._blocks if b.label == "rounds_start")
    color_map = block.resolve_color_map()

    assert color_map["RZ"] != color_map["CZ_CYCLE_1"], "a 1Q and a 2Q gate differ"

    # `SUB` is the case that matters: the heuristic matches on a prefix, so classifying it would
    # come back with a one-qubit gate's color (it starts with "S").
    classical = {event.gate for event in block.visible_events if not event.is_quantum}
    assert "SUB" in classical
    assert all(color_map[gate] == block.unrecognized_gate_color for gate in classical)

    # A hand-set color does not override the classification; naming a `color_key` is what
    # opts out of it, and there the caller's choice wins.
    assert circuit.with_color_of("RZ", "#123456")._blocks[1].resolve_color_map() == color_map

    by_qubit = PlottableProgramCircuit(load("test_blocks")).with_color_key("Qubit")
    key = next(iter(by_qubit._blocks[1].resolve_color_map()))
    assert by_qubit.with_color_of(key, "#123456")._blocks[1].resolve_color_map()[key] == "#123456"


def test_hide_and_show_toggle_exactly_the_matching_events():
    circuit = PlottableProgramCircuit(load("multiple_gates_with_measures"))
    events = [event for block in circuit._blocks for event in block.events]

    circuit.hide("MEASURE")
    assert {event.gate for event in events if event.hidden} == {"MEASURE"}

    circuit.show("MEASURE")
    assert not any(event.hidden for event in events)

    circuit.hide(lambda event: "12" in (track.name for track in event.tracks))
    assert all(event.hidden for event in events if any(t.name == "12" for t in event.tracks))


def test_a_program_with_nothing_to_draw_refuses():
    with pytest.raises(ValueError, match="nothing to draw"):
        PlottableProgramCircuit(Program.parse("DECLARE ro BIT[1]\n")).draw()


def test_non_program_input_is_rejected():
    with pytest.raises(TypeError, match="Expected quil.Program"):
        PlottableProgramCircuit("RX(pi) 0")


# -- DEFCIRCUIT inlining --------------------------------------------------------

INLINE = PlottableProgramCircuit._inline_defcircuits


def gates_of(program: Program) -> set[str]:
    events = [event for block in PlottableProgramCircuit(program)._blocks for event in block.events]
    return {event.gate for event in events}


def test_inlining_replaces_macro_calls_with_the_instructions_they_stand_for():
    program = load("rotated-surface-code")
    assert "SX_ANCILLA_ECHO" in gates_of(program)

    inlined = PlottableProgramCircuit(program, inline_defcircuits=True)
    events = [event for block in inlined._blocks for event in block.events]

    assert {event.gate for event in events} == {"CZ", "DELAY", "I", "MEASURE", "RX", "RZ"}
    assert len(events) > len(
        [e for b in PlottableProgramCircuit(program)._blocks for e in b.events]
    )


def test_inlining_leaves_no_qubit_variable_behind():
    # MEASURE is the case that catches a substitution gap: quil-rs's own calibration
    # expander does not descend into it, so a body's `MEASURE q23` can come out with
    # its qubit variable intact and no error raised.
    inlined = INLINE(load("rotated-surface-code"))
    live = [
        instruction.to_quil()
        for instruction in inlined.body_instructions
        if isinstance(instruction, Instruction.Measurement)
        and isinstance(instruction._0.qubit, Qubit.Variable)
    ]
    assert live == []
    assert "MEASURE 23" in {instruction.to_quil() for instruction in inlined.body_instructions}


def test_inlining_unfolds_nested_macros_and_substitutes_parameter_expressions():
    program = Program.parse(
        "DEFCIRCUIT INNER(%t) a b:\n    RZ(%t/2) a\n    CZ a b\n"
        "DEFCIRCUIT OUTER(%t) q1 q13:\n    INNER(%t) q1 q13\n"
        '    PULSE q1 "q1_charge_tx" flat(duration: 1e-8, iq: 1.0)\n'
        "OUTER(pi/3+1) 1 13\n"
    )
    assert [i.to_quil() for i in INLINE(program).body_instructions] == [
        # parenthesized, so the call's expression keeps its precedence
        "RZ(((pi/3)+1)/2) 1",
        "CZ 1 13",
        # the frame's qubit is substituted; the frame's *name* is left alone
        'PULSE 1 "q1_charge_tx" flat(duration: 1e-8, iq: 1)',
    ]


def test_inlining_substitutes_pulse_level_bodies_and_leaves_fixed_qubits_alone():
    program = Program.parse(
        "DECLARE ro BIT[1]\n"
        "DEFCIRCUIT P(%d) a:\n"
        "    FENCE a\n"
        '    PULSE a "rf" flat(duration: %d, iq: 1.0)\n'
        '    SHIFT-PHASE a "rf" %d*2\n'
        '    CAPTURE a "rf" flat(duration: 1e-8, iq: 1.0) ro[0]\n'
        "    RESET a\n"
        "    RX(pi) 3\n"
        "P(1e-8) 5\n"
    )
    assert [i.to_quil() for i in INLINE(program).body_instructions] == [
        "FENCE 5",
        'PULSE 5 "rf" flat(duration: 1e-8, iq: 1)',
        'SHIFT-PHASE 5 "rf" 1e-8*2',
        'CAPTURE 5 "rf" flat(duration: 1e-8, iq: 1) ro[0]',
        "RESET 5",
        "RX(pi) 3",
    ]


def test_inlining_keeps_whatever_classical_target_a_measurement_was_written_with():
    # The macros in `rotated-surface-code` hold bare `MEASURE q23` - that program routes
    # readout through PRAGMAs in the DEFCAL layer, not through a MEASURE target - so the
    # inlined diagram showing no classical output is the program, not a dropped field.
    program = Program.parse(
        "DECLARE ro BIT[2]\nDEFCIRCUIT M a b:\n    MEASURE a ro[0]\n    MEASURE b\nM 7 9\n"
    )
    assert [i.to_quil() for i in INLINE(program).body_instructions] == [
        "MEASURE 7 ro[0]",
        "MEASURE 9",
    ]


def test_inlining_a_program_without_defcircuits_changes_nothing():
    program = Program.parse("H 0\nCNOT 0 1\n")
    assert INLINE(program).to_quil() == program.to_quil()


def test_inlining_is_idempotent():
    once = INLINE(load("rotated-surface-code"))
    assert [i.to_quil() for i in INLINE(once).body_instructions] == [
        i.to_quil() for i in once.body_instructions
    ]


@pytest.mark.parametrize(
    ("quil", "match"),
    [
        ("DEFCIRCUIT F a:\n    RX(pi/2) a\nDAGGER F 0\n", "cannot carry a gate modifier"),
        ("DEFCIRCUIT F a b:\n    CZ a b\nF 0\n", "takes 2 qubits but the call passes 1"),
        ("DEFCIRCUIT F(%t) a:\n    RZ(%t) a\nF 0\n", "takes 1 parameters but the call passes 0"),
        ("DEFCIRCUIT F a:\n    F a\nF 0\n", "nesting exceeded 50 levels"),
    ],
)
def test_a_call_that_cannot_be_inlined_faithfully_refuses(quil: str, match: str):
    with pytest.raises(ValueError, match=match):
        INLINE(Program.parse(quil))
    with pytest.raises(ValueError, match="experimental and fragile"):
        PlottableProgramCircuit(Program.parse(quil), inline_defcircuits=True)


def test_inlining_still_rejects_a_non_program():
    with pytest.raises(TypeError, match="Expected quil.Program"):
        PlottableProgramCircuit("RX(pi) 0", inline_defcircuits=True)
