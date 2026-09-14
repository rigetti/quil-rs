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

import json

import pytest
from conftest import load

from quil.plotting import PlottableProgramCircuit, PlottableProgramPulseSchedule
from quil.program import Program


@pytest.fixture
def schedule() -> PlottableProgramPulseSchedule:
    return PlottableProgramPulseSchedule(load("test_blocks"))


def test_every_block_schedules(schedule):
    assert [block.label for block in schedule._blocks] == [
        None,
        "rounds_start",
        None,
        "rounds_end",
    ]
    assert [bool(block.events) for block in schedule._blocks] == [False, True, False, True]


def test_instruction_names_are_not_misattributed(schedule):
    by_label = {block.label: block for block in schedule._blocks}

    assert {pulse.logical_instruction_name for pulse in by_label["rounds_start"].pulses} == {
        "X90_CYCLE_1",
        "CZ_CYCLE_1",
        "CZ_CYCLE_2",
        "MIXED_X90_X180_CYCLE",
        "ANCILLA_QUBIT_MEASURE_CYCLE",
    }
    assert {pulse.logical_instruction_name for pulse in by_label["rounds_end"].pulses} == {
        "DATA_QUBIT_MEASURE_CYCLE"
    }


def test_captures_carry_their_memory_reference(schedule):
    by_label = {block.label: block for block in schedule._blocks}
    captures = {
        label: [pulse for pulse in by_label[label].pulses if pulse.memory_reference != ""]
        for label in ("rounds_start", "rounds_end")
    }

    assert len(captures["rounds_start"]) == 4
    assert len(captures["rounds_end"]) == 5
    assert all(pulse.memory_reference for group in captures.values() for pulse in group)


def test_control_flow_graph_recovers_the_loop(schedule):
    assert {(edge.source, edge.target, edge.kind, edge.back) for edge in schedule._cfg.edges} == {
        (0, 1, "fallthrough", False),
        (1, 3, "taken", False),
        (1, 2, "not-taken", False),
        (2, 1, "jump", True),
    }

    nodes = schedule._cfg.nodes
    assert [node.drawable for node in nodes] == [False, True, False, True]
    assert [node.title for node in nodes] == [
        "block 0",
        "rounds_start",
        "block 2",
        "rounds_end",
    ]
    # A block with no pulses still describes itself, so the shape of the graph stays readable.
    assert "idle" in nodes[0].subtitle
    assert "events" in nodes[1].subtitle

    graph = schedule._cfg.draw([None] * len(nodes))
    graph.to_dict()


def test_the_circuit_view_sees_the_same_graph():
    circuit = PlottableProgramCircuit(load("test_blocks"))
    schedule = PlottableProgramPulseSchedule(load("test_blocks"))
    assert {(e.source, e.target, e.kind, e.back) for e in circuit._cfg.edges} == {
        (e.source, e.target, e.kind, e.back) for e in schedule._cfg.edges
    }


def test_a_multi_block_program_draws_its_graph(schedule):
    spec = json.dumps(schedule.draw().to_dict())
    assert "Control-flow graph" in spec
    assert "block-01.rounds_start" not in spec


@pytest.mark.parametrize(
    ("view", "expected"),
    [
        # A pure-delay block plays no pulse, so the pulse view has nothing to draw for block 0 -
        # but the circuit view draws its DELAYs. Block 2, the empty back edge, is skipped by both.
        (
            PlottableProgramPulseSchedule,
            ["block-01.rounds_start.html", "block-03.rounds_end.html"],
        ),
        (
            PlottableProgramCircuit,
            [
                "block-00.html",
                "block-01.rounds_start.html",
                "block-03.rounds_end.html",
            ],
        ),
    ],
    ids=["schedule", "circuit"],
)
def test_only_drawable_blocks_get_a_file(view, expected, tmp_path):
    index = tmp_path / "index.html"
    view(load("test_blocks")).draw(index)
    sidecar = tmp_path / "index.html.blocks"

    assert index.is_file()
    assert sorted(path.name for path in sidecar.iterdir()) == expected
    # Every block page links back up to the graph it was reached from.
    for name in expected:
        assert 'href="../index.html"' in (sidecar / name).read_text()


def test_svg_links_stay_relative(schedule, tmp_path):
    schedule.draw(tmp_path / "graph.svg")
    index = (tmp_path / "graph.svg").read_text()
    assert "vega-datasets" not in index
    assert "graph.svg.blocks/block-01.rounds_start.svg" in index


def test_writing_a_multi_block_program_leaves_no_directory_named_for_the_file(tmp_path):
    chart = PlottableProgramCircuit(load("test_blocks")).draw(tmp_path / "circuit.html")

    assert (tmp_path / "circuit.html").is_file()
    assert (tmp_path / "circuit.html.blocks").is_dir()
    assert "Control-flow graph" in json.dumps(chart.to_dict())


def test_a_single_block_program_writes_one_file_and_no_sidecar(tmp_path):
    PlottableProgramCircuit(Program.parse("H 0\nCZ 0 1\n")).draw(tmp_path / "circuit.html")

    assert [path.name for path in tmp_path.iterdir()] == ["circuit.html"]


def test_shared_y_axis_gives_every_block_the_program_wide_lanes(schedule):
    per_block = [block.resolve_y_axis()[0] for block in schedule._blocks if block.events]
    assert per_block[0] != per_block[1]

    shared = schedule.with_shared_y_axis()._resolve_rows()
    assert set(shared) == set(per_block[0]) | set(per_block[1])
    assert schedule.with_shared_y_axis(False).shared_y_axis is False


def test_block_files_sort_in_execution_order(tmp_path):
    # Twelve blocks, so single- and double-digit indices sort against each other.
    source = "H 0\n" + "".join(f"LABEL @b-{index}\nH 0\n" for index in range(11))
    PlottableProgramCircuit(Program.parse(source)).draw(tmp_path / "index.html")

    names = [path.name for path in (tmp_path / "index.html.blocks").iterdir()]
    # The expected list is in execution order, so this asserts the two agree -
    # `block-09` sorts before `block-10`, where an unpadded name would not.
    assert sorted(names) == ["block-00.html"] + [
        f"block-{index + 1:02d}.b-{index}.html" for index in range(11)
    ]
