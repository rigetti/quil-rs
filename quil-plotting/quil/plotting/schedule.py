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

"""The pulse-level view of a program."""

import json
import math
from typing import Callable, Iterable, Self

import altair as alt
import numpy as np
from quil.instructions import (
    AttributeValue,
    FrameIdentifier,
    Instruction,
    Qubit,
)
from quil.program import BasicBlock, Program

from .program import PlottableBlock, PlottableProgram
from .pulse import (
    PlottableFrameUpdate,
    PlottablePulse,
    PlottablePulseEvent,
    PlottableRawCapture,
)
from .render import BLUE, GRAY, TEAL, YELLOW, gate_name_color, order_labels
from .utils import _evaluate_real
from .waveform import PlottableWaveformCache

# A distinct point shape per frame-update kind, so e.g. SET-PHASE and
# SHIFT-PHASE markers on the same lane are visually distinguishable without
# hovering. Any kind not listed (future additions) falls back to "diamond" - the
# shape every frame update used before there was more than one kind.
_FRAME_UPDATE_SHAPES = {
    "SHIFT-PHASE": "diamond",
    "SET-PHASE": "square",
    "SET-FREQUENCY": "triangle-up",
    "SHIFT-FREQUENCY": "triangle-down",
    "SET-SCALE": "cross",
    "SWAP-PHASES": "triangle-right",
}

# Instantaneous frame-state changes: no waveform, so drawn as a marker rather
# than an envelope. All five share the same payload shape - a single `.frame`
# plus a value field - so one branch in the event-building loop below handles
# them uniformly. `SwapPhases` is drawn the same way but is not listed here: it
# names two frames rather than one, so it gets its own branch. That same shared
# `.frame` is what `PlottablePulseEvent.frame` reads back off an instruction,
# which is why the swap is the only kind needing an override there.
_FRAME_UPDATE_INSTRUCTIONS = (
    Instruction.SetFrequency,
    Instruction.SetPhase,
    Instruction.SetScale,
    Instruction.ShiftFrequency,
    Instruction.ShiftPhase,
)


def _lane_order(labels: Iterable[str], order: list[str] | None) -> list[str]:
    """Lane order for a schedule: `order_labels`, reversed.

    A schedule's y scale runs upward, so lane 0 is the *bottom* lane and this
    list has to be bottom-first - the opposite of the top-to-bottom order a
    caller writes and a chart is read in.

    Args:
        labels: The lane labels to order.
        order: An explicit list of labels, or `None` for the default order.

    Returns:
        `labels`, ordered bottom to top.
    """
    # The inversion lives here alone rather than at each sort site, since
    # getting it backwards silently flips every axis.
    return list(reversed(order_labels(labels, order)))


class PlottableBlockPulseSchedule(PlottableBlock[PlottablePulseEvent]):
    """One basic block's pulse schedule, and the object that draws it.

    Built by `PlottableProgramPulseSchedule`, which holds these as `_blocks`.
    Read them to inspect a schedule's events - times, frames, amplitudes -
    without drawing anything.

    See Also:
        `PlottableProgramPulseSchedule`: the public API that configures these.
    """

    FIELDS = {
        "Qubit": lambda event: event.qubit,
        "Frame": lambda event: event.frame.name,
        "Channel Type": lambda event: event.channel_type,
        "Instruction": lambda event: event.logical_instruction_name,
    }
    """The fields a schedule can group by."""

    def __init__(
        self,
        block: BasicBlock,
        program: Program,
        instruction_name_map: list[str],
    ) -> None:
        """Schedule `block` and turn its instructions into drawable events.

        Args:
            block: One basic block of an already-expanded program.
            program: The expanded program, consulted for its frame and waveform
                definitions.
            instruction_name_map: One logical instruction name per instruction
                of `block`, from `PlottableProgramPulseSchedule.expand_program`.
        """
        super().__init__(block)

        self.y_axis: str = "Qubit"
        """The field lanes are grouped by."""

        self.width: int | None = None
        """The plot width in pixels, or `None` to size from the duration."""

        self.ns_per_pixel: float = 1.5
        """How many nanoseconds of the time axis one pixel spans initially."""

        self.min_width: int = 300
        """The narrowest a duration-sized plot is drawn, in pixels."""

        self.max_width: int = 1000
        """The widest a duration-sized plot is drawn, in pixels."""

        self.height_per_runner: int = 100
        """How tall each lane is drawn, in pixels."""

        self.lane_fraction: float = 0.45
        """The fraction of a lane a full-scale pulse spans."""

        self.normalize_by: str = "Frame"
        """The field pulse amplitudes are normalized within."""

        self.max_points_per_pulse: int | None = 500
        """Caps a pulse's rendered sample count, or `None` for no cap."""

        self.frame_update_color: str = "#0d0d36"
        """The one color every frame update is drawn in."""

        block_instructions = block.instructions
        scheduled = block.as_schedule_seconds(program)
        self.duration: float = scheduled.duration
        """The block's scheduled duration, in seconds."""

        self.waveforms = PlottableWaveformCache()
        """The distinct waveform shapes this block plays."""

        frame_to_channel_type_map = _build_channel_type_map(program)
        qubit_str_map = _build_qubit_str_map(program)

        waveform_defs = program.waveforms
        for event in scheduled.items:
            instruction = block_instructions[event.instruction_index]

            # A pulse emits a waveform; a capture demodulates the incoming
            # signal against one and writes the result to memory. Both play a
            # waveform on a frame for a known duration, so both are drawn the
            # same way - only the (optional) memory reference differs.
            if isinstance(instruction, (Instruction.Pulse, Instruction.Capture)):
                pulse = instruction._0
                memory_reference: str = ""
                if isinstance(instruction, Instruction.Capture):
                    memory_reference = instruction._0.memory_reference.to_quil_or_debug()

                sample_rate = _frame_sample_rate(program, pulse.frame)
                waveform_id, scale = self.waveforms.cache(
                    pulse.waveform,
                    waveform_defs,
                    sample_rate,
                )

                self.events.append(
                    PlottablePulse(
                        instruction=instruction,
                        logical_instruction_name=instruction_name_map[event.instruction_index],
                        start_time=event.time_span.start,
                        channel_type=frame_to_channel_type_map[pulse.frame],
                        qubit=qubit_str_map[pulse.frame],
                        waveform_id=waveform_id,
                        scale=scale,
                        memory_reference=memory_reference,
                        duration=event.time_span.duration,
                        hidden=False,
                    )
                )

            # A raw capture acquires for a fixed window with no kernel waveform
            # to demodulate against, so there is no envelope to draw - only the
            # span it occupies. The duration comes from the scheduler rather
            # than the instruction's own expression, so it is already resolved
            # to seconds on the same time base as every other event.
            elif isinstance(instruction, Instruction.RawCapture):
                raw = instruction._0
                self.events.append(
                    PlottableRawCapture(
                        instruction=instruction,
                        logical_instruction_name=instruction_name_map[event.instruction_index],
                        start_time=event.time_span.start,
                        channel_type=frame_to_channel_type_map[raw.frame],
                        qubit=qubit_str_map[raw.frame],
                        duration=event.time_span.duration,
                        memory_reference=raw.memory_reference.to_quil_or_debug() or "",
                        hidden=False,
                    )
                )

            # A swap trades phase between two frames, so it shows up on both
            # lanes: one event each, naming the other, rather than arbitrarily
            # picking one frame to draw it on. Which of the two an event is, is
            # the one thing it cannot read back off the instruction.
            elif isinstance(instruction, Instruction.SwapPhases):
                swap = instruction._0
                for second, frame in enumerate((swap.frame_1, swap.frame_2)):
                    self.events.append(
                        PlottableFrameUpdate(
                            instruction=instruction,
                            logical_instruction_name=instruction_name_map[event.instruction_index],
                            start_time=event.time_span.start,
                            channel_type=frame_to_channel_type_map[frame],
                            qubit=qubit_str_map[frame],
                            second_swap_frame=bool(second),
                            hidden=False,
                        )
                    )

            # An instantaneous frame-state change - no waveform, so no lookup
            # into the waveform cache. These kinds share `.frame` as their
            # payload's attribute name, so one branch handles them all without
            # per-type special-casing.
            elif isinstance(instruction, _FRAME_UPDATE_INSTRUCTIONS):
                payload = instruction._0
                self.events.append(
                    PlottableFrameUpdate(
                        instruction=instruction,
                        logical_instruction_name=instruction_name_map[event.instruction_index],
                        start_time=event.time_span.start,
                        channel_type=frame_to_channel_type_map[payload.frame],
                        qubit=qubit_str_map[payload.frame],
                        hidden=False,
                    )
                )

    @property
    def pulses(self) -> list[PlottablePulse]:
        """This block's pulses and captures, hidden ones included."""
        return [event for event in self.events if isinstance(event, PlottablePulse)]

    @property
    def frame_updates(self) -> list[PlottableFrameUpdate]:
        """This block's phase, frequency and scale changes."""
        return [event for event in self.events if isinstance(event, PlottableFrameUpdate)]

    @property
    def raw_captures(self) -> list[PlottableRawCapture]:
        """This block's raw captures."""
        return [event for event in self.events if isinstance(event, PlottableRawCapture)]

    @property
    def colorable_events(self) -> list[PlottablePulseEvent]:
        """The visible events that are colored, and so get a legend entry.

        Frame updates are excluded: they are markers in one fixed color, not
        amplitudes.

        Returns:
            This block's visible pulses and raw captures.
        """
        return [event for event in (*self.pulses, *self.raw_captures) if not event.hidden]

    @property
    def drawable(self) -> bool:
        """Whether this block has anything to draw."""
        return bool(self.events)

    @property
    def caption(self) -> str:
        """This block in one line, as its control-flow graph node shows it.

        An empty block still describes itself where it has a duration, so a wait
        or a delay reads as time passing rather than as nothing at all.

        Returns:
            This block's one-line summary.
        """
        if self.events:
            lanes = {event.qubit for event in self.events}
            return f"{self.duration * 1e6:.2f} µs · {len(self.events)} events · {len(lanes)} lanes"
        if self.duration:
            return f"{self.duration * 1e6:.2f} µs idle"
        return "—"

    def resolve_y_axis(
        self, labels: list[str] | None = None
    ) -> tuple[list[str], Callable[[PlottablePulseEvent], int | None]]:
        """Resolve `y_axis` into its lane labels and an event-to-lane lookup.

        Args:
            labels: Use this fixed lane set instead of the one implied by this
                block's own events - typically
                `PlottableProgramPulseSchedule._resolve_rows`'s program-wide
                labels, so a qubit sits at the same height in every block.

        Returns:
            The labels in lane order, so a label's index is its lane, and a
            callable giving an event's lane - or `None` where its value has
            no lane in `labels`.

        See Also:
            `PlottableProgramPulseSchedule.with_y_axis`
        """
        # Built from every event, not just pulses, so a
        # SHIFT-PHASE/SET-PHASE/etc. on a frame with no pulse of its own still
        # gets a lane.
        field = self.field_accessor(self.y_axis)

        if labels is None:
            labels = _lane_order(
                {field(event) for event in self.events if not event.hidden}, self.y_axis_order
            )
        lanes = {label: lane for lane, label in enumerate(labels)}
        return labels, lambda event: lanes.get(field(event))

    def _default_group_key(self, event: PlottablePulseEvent) -> str:
        """The group `event` falls into with no `color_key`.

        That is the logical instruction the event expanded out of, e.g. `RX`.

        Args:
            event: The event to classify.

        Returns:
            `event`'s group key.
        """
        return event.logical_instruction_name

    def _default_color(self, key: str, events: list[PlottablePulseEvent]) -> str:
        """Classify `key` by what the operation is, falling back on its frame.

        Args:
            key: A logical instruction name, e.g. `"RX"` or `"MEASURE"`.
            events: The events carrying `key`, for the frame-name fallback.

        Returns:
            A CSS color.
        """
        color = gate_name_color(key)
        if color is not None:
            return color

        # An unrecognized gate name - fall back on the frame's name, which the
        # circuit view has no equivalent of, so this half stays here rather
        # than being shared.
        token = events[0].frame.name.lower()
        if "charge" in token:
            return TEAL
        if "flux" in token:
            return YELLOW
        if "readout" in token:
            return BLUE
        return GRAY  # last resort

    def resolve_normalization(self) -> Callable[[PlottablePulse], float]:
        """Resolve `normalize_by` into a pulse-to-divisor lookup.

        Returns:
            A callable giving the divisor for a pulse's amplitude - the
            largest scaled IQ magnitude in its group, so that pulse fills
            its lane and quieter ones stay in proportion to it.

        See Also:
            `PlottableProgramPulseSchedule.with_normalize_by`
        """
        field = self.field_accessor(self.normalize_by)

        peaks: dict[str, float] = {}
        for pulse in self.pulses:
            if pulse.hidden:
                continue
            peak = pulse.scale * self.waveforms.table[pulse.waveform_id].peak
            group = field(pulse)
            peaks[group] = max(peaks.get(group, 0.0), peak)

        return lambda pulse: peaks.get(field(pulse), 0.0) or 1.0

    def _schedule_width(self) -> int:
        """Choose a plot-area width that keeps the time axis readable.

        A block holding a single 40 ns pulse and one running for tens of
        microseconds should not be drawn at the same width, or the short one is
        stretched until its pulses are a smear. This is what `draw` uses when
        `width` is `None`.

        Returns:
            A width in pixels, clamped to `[min_width, max_width]`.

        See Also:
            `PlottableProgramPulseSchedule.with_width`: overrides this.
            `PlottableProgramPulseSchedule.with_ns_per_pixel`: the time scale
                this sizes to.
        """
        max_time_ns = self.duration * 1e9
        if not math.isfinite(max_time_ns) or max_time_ns <= 0:
            max_time_ns = 1000.0
        return int(np.clip(max_time_ns / self.ns_per_pixel, self.min_width, self.max_width))

    def draw(self, rows: list[str] | None = None) -> alt.LayerChart:
        """Draw this block's pulse schedule, using its own settings.

        Args:
            rows: Use this fixed lane set instead of the one implied by this
                block's own events - typically
                `PlottableProgramPulseSchedule._resolve_rows`'s program-wide
                labels, so a qubit sits at the same height across every block.

        Returns:
            An `altair.LayerChart` of this block.

        See Also:
            `PlottableProgramPulseSchedule.draw`: draws the whole program,
                and is what you normally want.
        """
        width = self.width if self.width is not None else self._schedule_width()
        height_per_runner = self.height_per_runner

        # Pulse Height Normalization
        normalization_of = self.resolve_normalization()

        # Y-Axis
        y_axis_labels, lane_of = self.resolve_y_axis(labels=rows)

        fig_y_axis = alt.Axis(
            values=list(range(len(y_axis_labels))),
            labelExpr=f"{json.dumps(y_axis_labels)}[datum.value]",
            title=None,
            grid=True,
        )

        fig_y_scale = alt.Scale(domain=[-0.25, -0.25 + len(y_axis_labels)], nice=False)

        # Altair Source Data Records
        waveform_records = self.waveforms.build_records(max_points=self.max_points_per_pulse)
        pulse_records = [
            pulse.build_record(
                index=index,
                lane=lane,
                label=self._color_key_of(pulse),
                normalization=normalization_of(pulse),
                lane_fraction=self.lane_fraction,
            )
            for index, pulse in enumerate(self.pulses)
            if not pulse.hidden and (lane := lane_of(pulse)) is not None
        ]
        frame_update_records = [
            update.build_record(lane=lane)
            for update in self.frame_updates
            if not update.hidden and (lane := lane_of(update)) is not None
        ]
        raw_capture_records = [
            capture.build_record(
                lane=lane,
                label=self._color_key_of(capture),
                lane_fraction=self.lane_fraction,
            )
            for capture in self.raw_captures
            if not capture.hidden and (lane := lane_of(capture)) is not None
        ]

        # Color Map and Legend. Pulse groups and frame-update kinds share one
        # legend, which is what makes the highlight behave: `color` and `shape`
        # both encode `label`, so Vega-Lite merges the two guides into one, and
        # one guide takes one selection - so picking an entry *replaces* the
        # previous pick, across pulses and frame updates alike. Two legends
        # would mean two selections, which accumulate into a state no mark can
        # satisfy (a record carries a pulse's group or a frame update's
        # mnemonic, never both) and fade the whole chart out. The cost of
        # merging is the per-category headers, which is the trade the user
        # chose.
        #
        # Both scales therefore need the same domain, spanning both categories:
        # a pulse group's entry gets its own color and a plain dot, a frame
        # update's gets the marker color and that kind's glyph.
        palette = self.resolve_color_map()
        kinds = sorted({record["label"] for record in frame_update_records})

        legend_domain = [*palette, *kinds]
        # A bigger swatch than the (tiny, 12px) default - legend entries are
        # click targets here, not just labels, and small targets are easy to
        # miss.
        legend = alt.Legend(symbolSize=200, title=None)
        color = alt.Color(
            "label:N",
            scale=alt.Scale(
                domain=legend_domain,
                range=[*palette.values(), *[self.frame_update_color] * len(kinds)],
            ),
            legend=legend,
        )
        shape = alt.Shape(
            "label:N",
            scale=alt.Scale(
                domain=legend_domain,
                range=[
                    *["circle"] * len(palette),
                    *[_FRAME_UPDATE_SHAPES.get(kind, "diamond") for kind in kinds],
                ],
            ),
            legend=legend,
        )

        # One legend, so one selection - and every mark's opacity reads from it
        # directly. A filled area fades from `fill_opacity`; everything else
        # (strokes, frame-update markers) is drawn solid, so they share one
        # condition.
        legend_selection = alt.selection_point(fields=["label"], bind="legend")
        opacity = alt.condition(legend_selection, alt.value(1.0), alt.value(self.faded_opacity))
        area_opacity = alt.condition(
            legend_selection, alt.value(self.fill_opacity), alt.value(self.faded_opacity)
        )

        # Base Figure
        fig_x = alt.X(
            "t:Q",
            title="Time (s)",
            axis=alt.Axis(format="~s"),
            scale=alt.Scale(zero=False, nice=False),
        )
        fig = alt.Chart().encode(x=fig_x, color=color, detail="p:N")

        # Tooltip. `memory` is empty for a plain pulse and the capture's memory
        # reference for a capture - the one piece of information a capture
        # carries that a pulse does not.
        #
        # Three times, and they answer different questions: `t0`/`t1` bracket
        # the whole pulse, which is what you want when reading a schedule, while
        # `t` is the hovered sample's own time. `t` is already resolved per
        # sample by the flatten/calculate transforms below, same as the x
        # encoding itself; `t0`/`t1` are scalars on the pulse record and survive
        # the flatten unchanged, so neither needs a lookup of its own.
        base_tooltip = [
            alt.Tooltip("operation:N", title="Operation"),
            alt.Tooltip("frame:N", title="Frame"),
            alt.Tooltip("channel:N", title="Channel Type"),
            alt.Tooltip("memory:N", title="Memory"),
            alt.Tooltip("t0:Q", title="Start (s)", format=".3e"),
            alt.Tooltip("t1:Q", title="End (s)", format=".3e"),
            alt.Tooltip("t:Q", title="Time (s)", format=".3e"),
        ]

        # One filled mark per component, outlined with `line=True` rather than
        # by a second mark. The Q trace is filtered to the pulses that actually
        # have one, so a real-valued pulse does not get a flat line drawn along
        # its baseline. Each layer's tooltip adds its own raw (unscaled)
        # amplitude - "i" or "q", not the drawn "yi"/"yq", which mix in the lane
        # baseline `b` and are not a meaningful physical value - rather than
        # sharing one tooltip, so hovering the I trace of a real-valued pulse
        # doesn't show a spurious "Q: 0".
        pulse_layers = []
        for component, has_component in (("yi", None), ("yq", "hasq")):
            raw_field, raw_title = ("i", "I") if component == "yi" else ("q", "Q")
            tooltip = [*base_tooltip, alt.Tooltip(f"{raw_field}:Q", title=raw_title)]
            area = fig.mark_area(
                interpolate="linear",
                line=True,
                strokeWidth=1.5,
                aria=False,
            ).encode(
                y=alt.Y(
                    f"{component}:Q",
                    title=None,
                    axis=fig_y_axis,
                    scale=fig_y_scale,
                ),
                y2="b:Q",
                fillOpacity=area_opacity,
                strokeOpacity=opacity,
                tooltip=tooltip,
            )
            if has_component is not None:
                area = area.transform_filter(alt.datum[has_component])
            pulse_layers.append(area)

        # Lookup the pulse's waveform and scale. `legend_selection` is added to
        # the combined chart below rather than here, since both this layer and
        # the frame updates' read from it.
        layers = [
            alt.layer(*pulse_layers, data={"values": pulse_records})
            .transform_lookup(
                lookup="s",
                from_=alt.LookupData(
                    data={"values": waveform_records},
                    key="s",
                    fields=["dt", "i", "q", "hasq"],
                ),
            )
            .transform_flatten(["dt", "i", "q"])
            .transform_calculate(
                t="datum.t0 + datum.dt",
                yi="datum.i * datum.kr + datum.b",
                yq="datum.q * datum.kr + datum.b",
            )
        ]

        if frame_update_records:
            # A frame update is marked with a shape rather than a character (a
            # rotation arrow, say): the chart may be rendered without a browser,
            # by a font stack that cannot be relied on to carry one. Each kind
            # (SHIFT-PHASE, SET-PHASE, ...) gets its own shape, from the scale
            # shared with the pulse legend above.
            layers.append(
                alt.Chart({"values": frame_update_records})
                .mark_point(
                    size=45,
                    filled=True,
                    color=self.frame_update_color,
                    aria=False,
                )
                .encode(
                    x=fig_x,
                    y=alt.Y("b:Q", title=None, axis=fig_y_axis, scale=fig_y_scale),
                    shape=shape,
                    opacity=opacity,
                    tooltip=[
                        alt.Tooltip("operation:N", title="Operation"),
                        alt.Tooltip("label:N", title="Instruction"),
                        alt.Tooltip("frame:N", title="Frame"),
                        # Empty for every kind but a SWAP-PHASES, which names
                        # the frame it traded phase with - a lone swap marker
                        # means little without it.
                        alt.Tooltip("partner:N", title="Swapped With"),
                        alt.Tooltip("channel:N", title="Channel Type"),
                        alt.Tooltip("t:Q", title="Time (s)", format=".3e"),
                    ],
                )
            )

        if raw_capture_records:
            # A raw capture has no waveform to draw, so it is drawn as the span
            # it occupies: filled like a pulse's envelope, outlined so a short
            # one stays visible, and colored from the same map so it sits in the
            # shared legend alongside the pulses.
            layers.append(
                alt.Chart({"values": raw_capture_records})
                .mark_rect(strokeWidth=1.5, aria=False)
                .encode(
                    x=fig_x,
                    x2="t2:Q",
                    y=alt.Y("b:Q", title=None, axis=fig_y_axis, scale=fig_y_scale),
                    y2="b2:Q",
                    color=color,
                    stroke=color,
                    fillOpacity=area_opacity,
                    strokeOpacity=opacity,
                    tooltip=[
                        alt.Tooltip("operation:N", title="Operation"),
                        alt.Tooltip("frame:N", title="Frame"),
                        alt.Tooltip("channel:N", title="Channel Type"),
                        alt.Tooltip("memory:N", title="Memory"),
                        alt.Tooltip("t:Q", title="Start (s)", format=".3e"),
                        alt.Tooltip("t2:Q", title="End (s)", format=".3e"),
                    ],
                )
            )

        chart = (
            alt.layer(*layers)
            .add_params(legend_selection)
            .properties(
                width=width,
                height=min(height_per_runner * max(len(y_axis_labels), 1), self.max_height),
            )
            .interactive()
        )

        # `interactive()` is declared on altair's base chart, so it widens the
        # LayerChart that `layer()` returns.
        return chart  # type: ignore[return-value]


class PlottableProgramPulseSchedule(PlottableProgram[PlottableBlockPulseSchedule]):
    """A Quil program's pulse schedule to be drawn.

    Expands the program's calibrations and schedules the result, so a chart
    shows what the hardware plays. A pulse's I and Q components are drawn
    separately, and a real-valued pulse simply has no Q trace.

    The program must carry the definitions of the QPU it targets. This includes
    the `DEFCAL`s that turn its gates into pulses, the `DEFFRAME`s that give
    those pulses a sample rate, and any `DEFWAVEFORM`s they name. A gate left
    with no calibration has no pulse to draw and will raise an error.

    This object follows a builder method to construct the final image or widget.
    Configuring a chart is a chain of `with_*` methods, each returning `self`;
    {py:obj}`draw` ends the chain.

    ### Grouping fields

    Four field names run through this class. {py:obj}`with_y_axis`,
    {py:obj}`with_normalize_by`, {py:obj}`with_color_key` and the string form of
    {py:obj}`hide`/{py:obj}`show` all accept the same
    four:

    | Field              | Groups by                                     |
    | ------------------ | --------------------------------------------- |
    | `"Qubit"`          | `"Qubit: 3"`, or `"Coupler: 7"` for a coupler |
    | `"Frame"`          | the frame's name, e.g. `"q3_charge_tx"`       |
    | `"Channel Type"`   | `"charge"`, `"flux"`, `"readout"`             |
    | `"Instruction"`    | the logical instruction, e.g. `"RX"`          |

    Examples:
        A schedule drawn with its defaults, one lane per qubit:

        ```python
        # Load the program
        from quil.program import Program
        program = Program.parse(quil_text)

        # Use this library to draw it
        from quil.plotting import PlottableProgramPulseSchedule
        PlottableProgramPulseSchedule(program).draw()
        ```

        The same program grouped per frame rather than per qubit, colored by
        channel, with the flux lines left out:

        ```python
        chart = (
            PlottableProgramPulseSchedule(program)
            .with_y_axis("Frame")
            .with_color_key("Channel Type")
            .hide("flux")
            .draw()
        )
        ```

    See Also:
        {py:obj}`~quil.plotting.circuit.PlottableProgramCircuit`: the gate-level view of the
            same program.
        {py:obj}`quil.plotting.pulse.PlottablePulseEvent`: what a {py:obj}`hide`/{py:obj}`show`
            predicate is handed.
    """

    def _build_blocks(self, program: Program) -> list[PlottableBlockPulseSchedule]:
        """Expand `program`'s calibrations, schedule it, and lay out its blocks.

        Args:
            program: A Quil program including the `DEFCAL`, `DEFFRAME` and
                `DEFWAVEFORM` definitions of the QPU it targets.

        Returns:
            One block per basic block, in program order.

        Raises:
            ValueError: If a gate or measurement survives calibration expansion,
                which leaves it with no pulse to draw.
            RuntimeError: If a frame is missing a `DEFFRAME`, a waveform cannot
                be resolved to samples, or - an internal invariant - the blocks
                built from `program` do not partition its instructions.
        """
        # 1. Decompose the logical level program into primitive instructions
        expansion, instruction_name_map = self._expand_program(program)

        # 2. Parse the blocks for plottable information. `instruction_name_map`
        #    is indexed by position in the expanded program's body, but a basic
        #    block's events are indexed by position *within that block* - so
        #    each block needs the slice of the name map at its own offset, not
        #    the whole map. A block is exactly `[LABEL?] instructions
        #    [terminator?]` in the body, so a running offset over the blocks in
        #    order recovers that slice.
        blocks: list[PlottableBlockPulseSchedule] = []
        position = 0
        for block in expansion.control_flow_graph().basic_blocks():
            if block.label is not None:
                position += 1  # the LABEL, which has no event of its own
            count = len(block.instructions)
            block_name_map = instruction_name_map[position : position + count]
            position += count + (1 if block.terminator is not None else 0)

            blocks.append(PlottableBlockPulseSchedule(block, expansion, block_name_map))
        if position != len(instruction_name_map):
            raise RuntimeError(
                f"expected the blocks to partition the program's "
                f"{len(instruction_name_map)} instructions, but only accounted for {position}"
            )
        return blocks

    @staticmethod
    def _expand_program(program: Program) -> tuple[Program, list[str]]:
        """Decompose `program`'s logical instructions via its calibrations.

        The returned program keeps only instructions the scheduler can assign a
        duration to, plus the instructions that define the control-flow graph.

        Args:
            program: A Quil program with the calibrations to expand.

        Returns:
            The expanded, filtered program, and one logical instruction name
            per instruction of its body - `"RX"` for the pulses an `RX`
            calibration expanded into, and `""` where nothing maps.

        Raises:
            ValueError: If a gate or measurement survives expansion, meaning the
                program has no calibration for it.

        See Also:
            {py:obj}`quil.plotting.pulse.PlottablePulseEvent.logical_instruction_name`:
                where a name from this map ends up.
        """
        # 1. Decompose the program and retain the source map
        expanded_program, source_map = program.expand_calibrations_with_source_map()

        # 2. Convert the source map to an instruction_name_map
        instruction_name_map = [""] * len(expanded_program.body_instructions)
        for entry in source_map.entries():
            for target_index in entry.target_location():
                # Typed `int | None` because pyo3 spells exhaustion as `None`,
                # which the
                # iteration protocol turns into `StopIteration`. A yielded value
                # is an index.
                if target_index is None:
                    continue
                logical_name = program.body_instructions[entry.source_location()].name
                instruction_name_map[target_index] = logical_name

        # 3. A gate or measurement still present after expansion has no
        #    calibration to give it a pulse. Report these as errors now.
        uncalibrated = [
            instruction
            for instruction in expanded_program.body_instructions
            if isinstance(instruction, (Instruction.Gate, Instruction.Measurement))
        ]
        if uncalibrated:
            first, *rest = (instruction.to_quil_or_debug() for instruction in uncalibrated)
            others = f" (and {len(rest)} other{'s' if len(rest) != 1 else ''})" if rest else ""
            raise ValueError(
                f"cannot draw this program's pulse schedule: no calibration expands {first!r}"
                f"{others}, so there is no pulse to draw for it. A program is plotted at the "
                "pulse level, so it needs the DEFCALs of the QPU it targets."
            )

        # 4. Keep only instructions the scheduler can assign a duration to, plus
        #    the instructions that define the control-flow graph. The name map
        #    is indexed by instruction, so it has to be filtered alongside them
        #    or every name after the first drop is attributed to the wrong
        #    instruction. A whitelist rather than a blacklist of the (many)
        #    unschedulable variants, so an instruction this does not recognize
        #    is dropped rather than crashing.
        kept = [
            (name, instruction)
            for name, instruction in zip(
                instruction_name_map, expanded_program.body_instructions, strict=True
            )
            if isinstance(
                instruction,
                (
                    # Instructions with Duration
                    Instruction.Pulse,
                    Instruction.Capture,
                    Instruction.Delay,
                    Instruction.RawCapture,
                    Instruction.Fence,
                    Instruction.SetFrequency,
                    Instruction.SetPhase,
                    Instruction.SetScale,
                    Instruction.ShiftFrequency,
                    Instruction.ShiftPhase,
                    Instruction.SwapPhases,
                    # Control-flow Instructions
                    Instruction.Label,
                    Instruction.Jump,
                    Instruction.JumpWhen,
                    Instruction.JumpUnless,
                    Instruction.Halt,
                ),
            )
        ]
        instruction_name_map = [name for name, _ in kept]
        filtered_program = program.clone_without_body_instructions()
        filtered_program.add_instructions([instruction for _, instruction in kept])
        return filtered_program, instruction_name_map

    def _resolve_rows(self) -> list[str]:
        # Every block's `y_axis` was set identically by `with_y_axis`, so any
        # one of them names the field. Built from every event, not just pulses,
        # so a block whose only content is e.g. a `SHIFT-PHASE` still gets a
        # lane for it.
        drawable = self._drawable_blocks()
        if not drawable:
            return []
        field = PlottableBlockPulseSchedule.field_accessor(drawable[0].y_axis)
        labels: set[str] = set()
        for block in drawable:
            labels.update(field(event) for event in block.events if not event.hidden)
        return _lane_order(labels, drawable[0].y_axis_order)

    # -- Builder methods -------------------------------------------------------
    # The program attempts to push all rendering state to the blocks to minimize
    # state and code bloat.

    def with_y_axis(self, field: str) -> Self:
        """Define the Y-Axis, one lane will correspond a unique `field` values.

        Which field the chart's rows are grouped by. `"Qubit"`, the default,
        collapses every frame of a qubit or coupler onto one lane.

        Args:
            field: One of `"Qubit"`, `"Frame"`, `"Channel Type"` or
                `"Instruction"`. See the class's grouping-fields table.

        Returns:
            `self`, so calls chain.

        Raises:
            ValueError: If `field` is not one of the four names above.

        Examples:
            ```python
            schedule.with_y_axis("Frame").draw()
            ```

        See Also:
            {py:obj}`PlottableProgramPulseSchedule.with_y_axis_order`: the order
                those lanes appear in.
            {py:obj}`PlottableProgramPulseSchedule.with_shared_y_axis`: keep lanes
                aligned across blocks.
            {py:obj}`PlottableProgramPulseSchedule.with_height_per_runner`: how tall
                each lane is drawn.
        """
        # rejects an unknown field
        PlottableBlockPulseSchedule.field_accessor(field)
        for block in self._blocks:
            block.y_axis = field
        return self

    def with_width(self, width: int) -> Self:
        """Fix the plot area's width in pixels, overriding the automatic one.

        By default each block is sized from its own duration, so a 40 ns block
        and a 50 µs block are both readable. Setting a width instead makes every
        block the same width, which is what you want when comparing two blocks
        side by side - and means a long block is drawn at a coarser time scale
        than it would choose.

        Args:
            width: Plot-area width in pixels, excluding axis and legend.

        Returns:
            `self`, so calls chain.

        See Also:
            {py:obj}`PlottableProgramPulseSchedule.with_height_per_runner`: the
                vertical counterpart.
        """
        for block in self._blocks:
            block.width = width
        return self

    def with_ns_per_pixel(self, nanoseconds: float) -> Self:
        """Set the time scale for the duration-sized plot, defaults to 1.5.

        This is what makes a 40 ns block and a 50 µs block comparable: each is
        drawn at the same nanoseconds per pixel until it hits one of the width
        bounds. Lower it to spread every block out, raise it to pack more time
        into the same page. Ignored once {py:obj}`with_width` fixes the width
        outright.

        Args:
            nanoseconds: Nanoseconds of the time axis per pixel.

        Returns:
            `self`, so calls chain.

        See Also:
            {py:obj}`PlottableProgramPulseSchedule.with_min_width`: the bounds this is
                clamped by.
        """
        for block in self._blocks:
            block.ns_per_pixel = nanoseconds
        return self

    def with_min_width(self, pixels: int) -> Self:
        """Set the narrowest a duration-sized plot is drawn. Defaults to 300.

        The floor that stops a block holding one short pulse from collapsing to
        a sliver.

        Args:
            pixels: Plot-area width in pixels.

        Returns:
            `self`, so calls chain.

        See Also:
            {py:obj}`PlottableProgramPulseSchedule.with_max_width`: the other bound.
        """
        for block in self._blocks:
            block.min_width = pixels
        return self

    def with_max_width(self, pixels: int) -> Self:
        """Set the widest a duration-sized plot is drawn. Defaults to 1000.

        The ceiling that stops a long block from running off the page; past it a
        block is drawn at a coarser time scale than `ns_per_pixel` asks for.

        Args:
            pixels: Plot-area width in pixels.

        Returns:
            `self`, so calls chain.

        See Also:
            {py:obj}`PlottableProgramPulseSchedule.with_min_width`: the other bound.
        """
        for block in self._blocks:
            block.max_width = pixels
        return self

    def with_height_per_runner(self, pixels: int) -> Self:
        """Set how tall each lane is drawn, in pixels. Defaults to 100.

        A block's height is this times its lane count, so a program with many
        qubits gets a tall chart. Lower it to fit more lanes on screen at the
        cost of envelope detail; raise it to read a single qubit's pulse shapes
        closely.

        The total is capped, so past roughly 20 lanes at the default height the
        chart stops growing and the lanes compress instead.

        Args:
            pixels: Height per lane, in pixels.

        Returns:
            `self`, so calls chain.

        See Also:
            {py:obj}`PlottableProgramPulseSchedule.with_width`: the horizontal
                counterpart.
            {py:obj}`PlottableProgramPulseSchedule.hide`: fewer lanes, rather than
                shorter ones.
        """
        for block in self._blocks:
            block.height_per_runner = pixels
        return self

    def with_lane_fraction(self, fraction: float) -> Self:
        """Set how much of a lane a full-scale pulse spans. Defaults to 0.45.

        A pulse swings both ways about its lane, so at the default it covers at
        most 0.9 of the gap to the next lane and neighbors stay clearly apart.
        Lower it for more whitespace between lanes; raise it to make quiet
        pulses readable, accepting that past 0.5 adjacent lanes overlap - which
        is a legitimate thing to want on a sparse chart, so it is not capped.

        Amplitudes are relative either way: this is the height the loudest pulse
        of each {py:obj}`with_normalize_by` group is drawn at, not an absolute
        scale.

        Args:
            fraction: Lane fraction a full-scale pulse is drawn at.

        Returns:
            `self`, so calls chain.

        Examples:
            ```python
            schedule.with_lane_fraction(0.3).draw()
            ```

        See Also:
            {py:obj}`PlottableProgramPulseSchedule.with_height_per_runner`: how tall a
                lane is in the first place.

            {py:obj}`PlottableProgramPulseSchedule.with_normalize_by`: what a
                full-scale pulse is measured against.
        """
        for block in self._blocks:
            block.lane_fraction = fraction
        return self

    def with_normalize_by(self, field: str) -> Self:
        """Group pulses by `field`, and scale each group to its own loudest.

        Drawn pulse heights are relative, never absolute: within each group, the
        largest-amplitude pulse fills its lane and the rest stay in proportion
        to it. The field chooses what "the rest" means.

        `"Frame"`, the default, compares a pulse only against other pulses on
        the same frame, which keeps a small drive pulse readable next to a
        readout pulse hundreds of times its amplitude. Widening the group to
        `"Qubit"` or `"Channel Type"` makes amplitudes comparable across frames
        instead, at the cost of flattening the quiet ones.

        Args:
            field: One of `"Qubit"`, `"Frame"`, `"Channel Type"` or
                `"Instruction"`. See the class's grouping-fields table.

        Returns:
            `self`, so calls chain.

        Examples:
            Compare drive amplitudes across a qubit's frames:

            ```python
            schedule.with_normalize_by("Qubit").draw()
            ```

        See Also:
            {py:obj}`quil.plotting.pulse.PlottablePulse.scale`: the amplitude being
                normalized.
        """
        # rejects an unknown field
        PlottableBlockPulseSchedule.field_accessor(field)
        for block in self._blocks:
            block.normalize_by = field
        return self

    def with_max_points_per_pulse(self, max_points: int | None) -> Self:
        """Cap how many samples each pulse's envelope is drawn from.

        A waveform is sampled at its frame's rate, so a long readout pulse can
        carry tens of thousands of samples - far more than a chart can resolve,
        and enough to make a browser slow to pan. Capping trades envelope
        fidelity for a smaller, faster chart, keeping each pulse's peak
        amplitude and its start and end timing.

        Defaults to 500, which on real programs is visually indistinguishable
        from uncapped. Raise it if a slowly-varying envelope looks faceted;
        disable it when exporting a figure where exact shape matters.

        Args:
            max_points: Maximum samples per pulse, or `None` to draw every
                sample.

        Returns:
            `self`, so calls chain.

        Examples:
            ```python
            schedule.with_max_points_per_pulse(None).draw("figures/schedule.html")
            ```

        See Also:
            {py:obj}`PlottableProgramPulseSchedule.with_width`: the other lever on
                how much detail a long block shows.
        """
        for block in self._blocks:
            block.max_points_per_pulse = max_points
        return self

    def with_frame_update_color(self, frame_update_color: str) -> Self:
        """Sets the frame update marker color for the diagram."""
        for block in self._blocks:
            block.frame_update_color = frame_update_color
        return self


def _build_channel_type_map(program: Program) -> dict[FrameIdentifier, str]:
    """The hardware channel type behind every frame the program defines.

    Internal. Reads each `DEFFRAME`'s `HARDWARE-OBJECT`, giving the
    `"Channel Type"` field its values - `"charge"`, `"flux"`, `"readout"`.

    Args:
        program: The program whose frames are read.

    Returns:
        Each frame's channel type, keyed by its identifier.
    """
    channel_types = {}
    for frame_id, attributes in program.frames.get_all_frames().items():
        hardware_object = attributes["HARDWARE-OBJECT"]
        if not isinstance(hardware_object, AttributeValue.String):
            raise TypeError(
                f"{frame_id.to_quil_or_debug()}: HARDWARE-OBJECT is "
                f"{hardware_object.to_quil_or_debug()}, expected a string"
            )
        channel_types[frame_id] = json.loads(hardware_object._0)["channel_type"]
    return channel_types


def _frame_sample_rate(program: Program, frame: FrameIdentifier) -> float:
    """A frame's `SAMPLE-RATE` in Hz, which is what waveforms are sampled at.

    Internal.

    Args:
        program: The program whose `DEFFRAME`s are consulted.
        frame: The frame to look up.

    Returns:
        The frame's sample rate, in Hz.

    Raises:
        RuntimeError: If the frame has no `DEFFRAME`, or its `DEFFRAME` has no
            numeric `SAMPLE-RATE`.
    """
    attributes = program.frames.get(frame)
    if attributes is None:
        raise RuntimeError(f"no DEFFRAME for {frame!r}")
    rate = attributes.get("SAMPLE-RATE")
    if not isinstance(rate, AttributeValue.Expression):
        raise RuntimeError(f"{frame!r} has no numeric SAMPLE-RATE (got {rate!r})")
    return _evaluate_real(rate._0)


def _build_qubit_str_map(program: Program) -> dict[FrameIdentifier, str]:
    """Every frame's axis label, giving the `"Qubit"` field its values.

    Internal. A frame acting on a qubit labels as `"Qubit: 3"`; one acting on a
    coupler labels as `"Coupler: 7"`.

    Args:
        program: The program whose frames are labeled.

    Returns:
        Each frame's axis label, keyed by its identifier.
    """
    # A qubit is one that some charge or readout frame names; anything else a
    # frame acts on is a coupler. The device's own topology is not in the
    # program, so this is inferred from the frames it defines.
    qubit_set = frozenset(
        qubit._0
        for frame in program.frames.get_keys()
        if "charge" in frame.name or "readout" in frame.name
        for qubit in frame.qubits
        if isinstance(qubit, Qubit.Fixed)
    )
    return {
        frame: ",".join(
            [
                f"Qubit: {q._0}" if q._0 in qubit_set else f"Coupler: {q._0}"
                for q in frame.qubits
                if isinstance(q, Qubit.Fixed)
            ]
        )
        for frame in program.frames.get_keys()
    }
