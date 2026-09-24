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
from conftest import load, sig_digits
from quil.program import Program

from quil.plotting import PlottableProgramPulseSchedule
from quil.plotting.waveform import PlottableWaveformCache, WaveformKey


def test_every_program_builds_and_draws_a_valid_spec(program):
    schedule = PlottableProgramPulseSchedule(program, allow_reset=True)
    drawable = [block for block in schedule._blocks if block.events]
    assert drawable, "every program in the corpus plays at least one pulse"

    for block in drawable:
        assert block.duration >= 0
        # Every event lands inside the block's own window; a start time outside it means the
        # scheduler's times and the block's duration came from different places.
        assert all(0 <= event.start_time <= block.duration + 1e-12 for event in block.events)

        # Waveform dedup leaves no orphans in either direction: every pulse points at a cached
        # shape, and every cached shape is played by at least one pulse.
        referenced = {pulse.waveform_id for pulse in block.pulses}
        assert referenced == set(range(len(block.waveforms.table)))

        chart = block.draw()
        assert isinstance(chart, alt.LayerChart)
        chart.to_dict()


def test_reset_raises_unless_allowed():
    with pytest.raises(ValueError, match="allow_reset=True"):
        PlottableProgramPulseSchedule(load("reset"))


def test_allowed_reset_is_marked_on_its_qubits_frames_before_the_pulse():
    block = PlottableProgramPulseSchedule(load("reset"), allow_reset=True)._blocks[0]
    resets = [event for event in block.frame_updates if event.instruction.name == "RESET"]
    assert resets
    assert {event.qubit for event in resets} == {"Qubit: 10"}
    assert len({event.frame for event in resets}) == len(resets), "one marker per frame"
    start = resets[0].start_time
    assert all(event.start_time == start for event in resets)
    assert all(pulse.start_time >= start for pulse in block.pulses)
    assert resets[0].build_record(lane=0)["label"] == "RESET"
    block.draw().to_dict()


def test_with_qubit_labels_relabels_only_the_given_frames():
    schedule = PlottableProgramPulseSchedule(load("single_gate_rx_pi"))
    block = schedule._blocks[0]
    relabeled = block.pulses[0].frame
    before = {event.frame: event.qubit for event in block.events}

    schedule.with_qubit_labels({relabeled: "Drive"})
    for event in block.events:
        assert event.qubit == ("Drive" if event.frame == relabeled else before[event.frame])
    block.draw().to_dict()


def test_identical_invocations_share_one_cached_shape():
    block = PlottableProgramPulseSchedule(load("measure_ancilla_cycle"))._blocks[0]
    assert len(block.waveforms.table) < len(block.pulses)


def test_waveform_key_ignores_scale_for_builtins():
    program = load("single_gate_rx_pi")
    pulse = PlottableProgramPulseSchedule(program)._blocks[0].pulses[0]
    invocation = pulse.instruction._0.waveform
    assert "scale" in invocation.parameters, "the RX calibration's waveform is scaled"
    assert pulse.scale != 1.0, "the stripped scale is carried on the pulse instead"

    cache = PlottableWaveformCache()

    key = WaveformKey.from_invocation(invocation, 1e9)
    assert key.builtin
    assert "scale" not in dict(key.params)
    identifier, scale = cache.cache(invocation, program.waveforms, 1e9)
    assert scale != 1.0
    assert cache.cache(invocation, program.waveforms, 1e9) == (identifier, scale)
    assert len(cache.table) == 1

    # A different sample rate is a different shape, since the samples themselves differ.
    other, _ = cache.cache(invocation, program.waveforms, 2e9)
    assert other != identifier


def test_normalization_fills_each_group_to_the_lane_fraction():
    schedule = PlottableProgramPulseSchedule(load("multiple_offset_gates_with_measures"))
    block = schedule._blocks[0]
    normalization_of = block.resolve_normalization()
    field = type(block).field_accessor(block.normalize_by)

    peaks: dict[str, float] = {}
    for pulse in block.pulses:
        record = pulse.build_record(0, 0, "label", normalization_of(pulse), block.lane_fraction)
        assert abs(record["kr"]) <= block.lane_fraction + 1e-9
        peaks[field(pulse)] = max(peaks.get(field(pulse), 0.0), abs(record["kr"]))

    assert peaks
    assert all(peak == pytest.approx(block.lane_fraction) for peak in peaks.values())

    # `with_lane_fraction` has to reach the drawn record, not just the block.
    schedule.with_lane_fraction(0.2)
    assert block.lane_fraction == 0.2
    drawn = max(
        abs(pulse.build_record(0, 0, "label", normalization_of(pulse), block.lane_fraction)["kr"])
        for pulse in block.pulses
    )
    assert drawn == pytest.approx(0.2)


def test_a_very_quiet_pulse_still_has_a_visible_height():
    block = PlottableProgramPulseSchedule(load("single_gate_rx_pi"))._blocks[0]
    pulse = block.pulses[0]
    loud = block.resolve_normalization()(pulse)

    record = pulse.build_record(0, 0, "label", loud * 1e6, block.lane_fraction)
    assert record["kr"] != 0.0, "a quiet pulse is drawn short, not flat"
    assert sig_digits(record["kr"]) <= 5


def test_lanes_run_bottom_up_in_natural_order():
    block = PlottableProgramPulseSchedule(load("multiple_offset_gates_with_measures"))._blocks[0]
    labels, lane_of = block.resolve_y_axis()

    assert labels == ["Qubit: 13", "Qubit: 12", "Qubit: 11", "Qubit: 10"]
    assert all(lane_of(event) is not None for event in block.events)
    assert lane_of(block.pulses[0]) == labels.index(block.pulses[0].qubit)


def test_swap_phases_is_drawn_on_both_frames_each_naming_the_other():
    block = PlottableProgramPulseSchedule(load("single_gate_iswap"))._blocks[0]
    updates = block.frame_updates
    swaps = [event for event in updates if event.instruction.name == "SWAP-PHASES"]

    # Block event order is not stable, so assert only on the pairing, not on which is first.
    assert len(swaps) == 2
    first, second = swaps
    assert first.frame != second.frame
    assert first.frame == second.partner_frame
    assert second.frame == first.partner_frame
    swap = first.instruction._0
    assert {first.frame, second.frame} == {swap.frame_1, swap.frame_2}

    # Every other kind of frame update names one frame and so has no partner to name.
    others = [event for event in updates if event.instruction.name != "SWAP-PHASES"]
    assert others, "the iswap calibration also shifts phase"
    assert all(event.partner_frame is None for event in others)


def test_with_y_axis_gives_a_lane_per_frame():
    schedule = PlottableProgramPulseSchedule(load("multiple_offset_gates_with_measures"))
    by_qubit, _ = schedule._blocks[0].resolve_y_axis()
    by_frame, _ = schedule.with_y_axis("Frame")._blocks[0].resolve_y_axis()

    assert len(by_frame) > len(by_qubit)
    assert all(label.startswith("Transmon-") for label in by_frame)

    with pytest.raises(ValueError, match="Invalid grouping term"):
        schedule.with_y_axis("Nonsense")._blocks[0].resolve_y_axis()


def test_colors_classify_by_operation_then_fall_back_to_the_frame():
    block = PlottableProgramPulseSchedule(load("multiple_offset_gates_with_measures"))._blocks[0]
    color_map = block.resolve_color_map()

    assert color_map["MEASURE"] != color_map["RX"]
    assert all(color.startswith("#") for color in color_map.values())

    # The classification is the point of this mode, so a hand-set color does not override it -
    # coloring by hand means naming a `color_key` (see below).
    recolored = (
        PlottableProgramPulseSchedule(load("multiple_offset_gates_with_measures"))
        .with_color_of("RX", "#123456")
        ._blocks[0]
        .resolve_color_map()
    )
    assert recolored["RX"] == color_map["RX"]


def test_color_key_groups_by_the_named_field():
    schedule = PlottableProgramPulseSchedule(load("multiple_offset_gates_with_measures"))
    color_map = schedule.with_color_key("Channel Type")._blocks[0].resolve_color_map()

    channels = {event.channel_type for event in schedule._blocks[0].colorable_events}
    assert set(color_map) == channels

    # A cycled palette means nothing by its assignment, so a caller's choice wins here.
    channel = sorted(channels)[0]
    recolored = schedule.with_color_of(channel, "#123456")._blocks[0].resolve_color_map()
    assert recolored[channel] == "#123456"


def test_hide_and_show_toggle_exactly_the_matching_events():
    schedule = PlottableProgramPulseSchedule(load("multiple_offset_gates_with_measures"))
    events = [event for block in schedule._blocks for event in block.events]

    schedule.hide("MEASURE")
    assert all(event.hidden == (event.logical_instruction_name == "MEASURE") for event in events)

    schedule.show("MEASURE")
    assert not any(event.hidden for event in events)

    # A predicate covers everything a string cannot.
    schedule.hide(lambda event: event.qubit == "Qubit: 10")
    assert {event.qubit for event in events if event.hidden} == {"Qubit: 10"}

    # A hidden event keeps its lane out of the chart entirely.
    labels, _ = schedule._blocks[0].resolve_y_axis()
    assert "Qubit: 10" not in labels


def test_hidden_pulses_do_not_reach_the_chart():
    schedule = PlottableProgramPulseSchedule(load("multiple_gates_with_measures"))
    full = schedule._blocks[0].draw().to_dict()
    hidden = schedule.hide("MEASURE")._blocks[0].draw().to_dict()
    assert len(str(hidden)) < len(str(full))


def test_max_points_per_pulse_caps_the_rendered_samples():
    schedule = PlottableProgramPulseSchedule(load("measure_ancilla_cycle"))
    block = schedule._blocks[0]

    uncapped = schedule.with_max_points_per_pulse(None)._blocks[0]
    assert uncapped.max_points_per_pulse is None
    full = max(len(record["dt"]) for record in uncapped.waveforms.build_records(max_points=None))

    capped = schedule.with_max_points_per_pulse(50)._blocks[0]
    assert capped is block
    largest = max(len(record["dt"]) for record in block.waveforms.build_records(max_points=50))
    assert largest < full
    # Both endpoints plus two extrema per block, so the cap is approximate by two samples.
    assert largest <= 52


def test_uncalibrated_gate_names_itself_in_the_error():
    program = Program.parse(load("test_blocks").to_quil() + "\nRY(0.3) 99\n")
    with pytest.raises(ValueError, match=r"RY\(0\.3\) 99"):
        PlottableProgramPulseSchedule(program)


def test_a_program_with_no_pulses_refuses_to_draw():
    program = Program.parse("DECLARE ro BIT[1]\n")
    with pytest.raises(ValueError, match="nothing to draw"):
        PlottableProgramPulseSchedule(program).draw()


def test_non_program_input_is_rejected():
    with pytest.raises(TypeError, match="Expected quil.Program"):
        PlottableProgramPulseSchedule("RX(pi) 0")


def test_disabling_normalization_draws_absolute_amplitudes():
    schedule = PlottableProgramPulseSchedule(load("multiple_offset_gates_with_measures"))
    block = schedule._blocks[0]
    assert schedule.with_normalize_by(None) is schedule
    assert block.normalize_by is None

    normalization_of = block.resolve_normalization()
    peaks = set()
    for pulse in block.pulses:
        record = pulse.build_record(0, 0, "label", normalization_of(pulse), block.lane_fraction)
        assert record["kr"] == pytest.approx(block.lane_fraction * pulse.scale, rel=1e-4)
        peaks.add(round(abs(record["kr"]), 9))

    # Normalized, every group tops out at exactly `lane_fraction`; absolute, the
    # quiet pulses stay quiet.
    assert len(peaks) > 1


@pytest.mark.parametrize(
    ("pan_y", "zoom_y", "expected_y"),
    [
        (True, True, ("default", "default")),
        (True, False, ("default", False)),
        (False, True, (False, "default")),
        (False, False, None),  # no y param at all
    ],
)
def test_pan_and_zoom_bind_to_each_axis_independently(pan_y, zoom_y, expected_y):
    # Vega-Lite takes pan (`translate`) and zoom per *param*, not per axis, so the time and lane
    # axes need one scales-bound param each. Asserting on the spec rather than the toggles is what
    # catches the two collapsing back into a single param, which silently re-couples the axes.
    schedule = (
        PlottableProgramPulseSchedule(load("test_blocks")).with_pan_y(pan_y).with_zoom_y(zoom_y)
    )
    spec = schedule._blocks[0].draw().to_dict()

    gestures = {
        encoding: (select.get("translate", "default"), select.get("zoom", "default"))
        for param in spec["params"]
        if param.get("bind") == "scales"
        for select in [param["select"]]
        for encoding in select["encodings"]
    }

    # The time axis always pans and zooms; only the lane axis is configurable.
    assert gestures.pop("x") == ("default", "default")
    assert gestures.get("y") == expected_y
