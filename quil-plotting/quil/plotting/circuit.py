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

"""The gate-level view of a program."""

import json
from collections.abc import Iterable
from dataclasses import dataclass
from typing import Any, Self, cast

import altair as alt
from quil.instructions import Instruction, MemoryReference, Qubit
from quil.program import BasicBlock, Program
from typing_extensions import override

from .program import PlottableBlock, PlottableProgram
from .render import GRAY, gate_name_color, natural_sort_key, order_labels
from .utils import _evaluate_real, short


@dataclass(frozen=True)
class Track:
    """One row of the diagram - a qubit wire or a classical register wire.

    Instructions occupy one or more of them, which is the whole basis of the
    column packing in `PlottableBlockCircuit`.
    """

    is_register: bool
    """Whether this is a classical register's wire rather than a qubit's."""

    name: str
    """The qubit's number, or the register's Quil name."""

    # A register draws as a *single* row, but its elements are independent for
    # column packing: measurements into `ro[0]` and `ro[2]` are unrelated and
    # belong in the same column, so they must not block each other.
    index: int | None = None
    """Which element of a register this is, or `None` for a qubit."""

    @property
    def row(self) -> Self:
        """The track this one is *drawn* on.

        Register elements collapse onto one wire.

        Returns:
            This track's own drawn row.
        """
        return type(self)(self.is_register, self.name) if self.index is not None else self

    @property
    def label(self) -> str:
        """This row's axis label - `"Qubit: 3"`, or a register's bare name."""
        return self.name if self.is_register else f"Qubit: {self.name}"


@dataclass
class PlottableCircuitEvent:
    """One drawn instruction, placed on the tracks it touches at a column.

    What a {py:obj}`PlottableProgramCircuit.hide` or
    {py:obj}`PlottableProgramCircuit.show` predicate is handed, so these
    attributes are the vocabulary for filtering a circuit.
    """

    instruction: Instruction
    """The Quil instruction this was built from."""

    label: str
    """The text in the gate's box - `RZ(pi/2)`, not `RZ(pi/2) 3`."""

    tracks: tuple[Track, ...]
    """The rows this instruction occupies."""

    # Filled in by the packer rather than at construction, since it depends on
    # everything that came before it.
    column: int = 0
    """Which column it was packed into - the diagram's notion of depth."""

    hidden: bool = False
    """Whether `hide` has excluded it from the diagram."""

    target_track: Track | None = None
    """For a measurement, the register row it writes into."""

    target_reference: str | None = None
    """For a measurement, the exact element it writes, as `ro[0]`."""

    controls: tuple[Track, ...] = ()
    """The rows drawn as control dots, one per `CONTROLLED` modifier."""

    @property
    def gate(self) -> str:
        """The name this event is colored and grouped by."""
        return self.instruction.name

    @property
    def is_quantum(self) -> bool:
        """Whether this operates on qubits, not timing or classical memory."""
        # Only these are colored by gate name: the shared heuristic matches on
        # a prefix, so asking it about `SUB` would get back a one-qubit gate's
        # color (it starts with `S`). Pulse instructions are absent because a
        # circuit never holds one - `_build` rejects a Quil-T program outright,
        # and points at the pulse-schedule view instead.
        return isinstance(
            self.instruction,
            (Instruction.Gate, Instruction.Measurement, Instruction.Reset),
        )

    @property
    def quil(self) -> str:
        """This instruction's full Quil text, as the tooltip shows it."""
        return self.instruction.to_quil_or_debug()


def _memory_references(payload: object) -> list[MemoryReference]:
    """Every memory reference reachable off a classical instruction's payload.

    Covers `Move`, `Arithmetic`, `Exchange`, `Comparison` and friends.

    Args:
        payload: The instruction's `_0` payload.

    Returns:
        Every memory reference the payload's operands name.
    """
    # They all name their operands `destination`/`source`/`left`/`right`/
    # `lhs`/`rhs`, so collecting by attribute covers them without a branch per
    # instruction type.
    found = []
    for attribute in ("destination", "source", "left", "right", "lhs", "rhs"):
        value = getattr(payload, attribute, None)
        # A comparison's `rhs` is a `ComparisonOperand` wrapping either a
        # literal or a reference, so unwrap one level before the check.
        value = getattr(value, "_0", value)
        if isinstance(value, MemoryReference):
            found.append(value)
    return found


def _gate_label(instruction: Instruction, qubit_count: int) -> str:
    """A gate's label: its Quil text with the trailing qubits removed.

    Keeps the name, parameters and modifiers - `DAGGER CONTROLLED RZ(0.5)`
    - and expressions stay symbolic, so `pi/2` rather than `1.5708`.

    Args:
        instruction: The gate instruction to label.
        qubit_count: How many trailing qubit tokens to strip.

    Returns:
        The gate's Quil text with its trailing qubits removed.
    """
    # quil-rs formats parameters with no internal spaces (`RZ(theta[0]+1)`,
    # `CPHASE((2*pi)/3)`), so dropping the last `qubit_count`
    # whitespace-separated tokens leaves exactly that. `Expression` has no
    # formatter of its own, so this borrows quil-rs's.
    tokens = instruction.to_quil_or_debug().split()
    return " ".join(tokens[:-qubit_count]) if qubit_count else " ".join(tokens)


def _delay_label(payload: Any) -> str:
    """`DELAY 40 ns`, carrying the duration rather than the bare mnemonic.

    A delay's qubits sit mid-text, so they cannot be stripped the way
    `_gate_label` strips a gate's.

    Args:
        payload: The `DELAY` instruction's payload.

    Returns:
        The delay's label, with its duration rounded to a readable unit.
    """
    try:
        seconds = _evaluate_real(payload.duration)
    except RuntimeError:
        return "DELAY"
    for scale, unit in ((1e-9, "ns"), (1e-6, "µs"), (1e-3, "ms")):
        if seconds < scale * 1000:
            return f"DELAY {short(seconds / scale):g} {unit}"
    return f"DELAY {short(seconds):g} s"


def _order_tracks(tracks: Iterable[Track], order: list[str] | None) -> list[Track]:
    """Rows top to bottom, keyed on the label each row actually shows.

    Registers sink below the qubit wires by default. That is a structural
    grouping rather than a naming one, so it survives the default natural sort
    but *not* an explicit order - there the caller's answer is the answer.

    Args:
        tracks: The tracks to order. Duplicates are dropped.
        order: An explicit list of row labels, or `None` for the default order.

    Returns:
        `tracks`, deduplicated and ordered top to bottom.
    """
    # Keying on `Track.label` is what lets one `with_y_axis_order` mean the same
    # thing in both views: a caller writes the labels they can see on the axis,
    # not `Track` objects.
    unique = list(dict.fromkeys(tracks))
    if order is None:
        return sorted(unique, key=lambda track: (track.is_register, natural_sort_key(track.label)))
    # Labels are unique across tracks: a register's is its bare Quil identifier,
    # a qubit's is "Qubit: <n>", and a Quil identifier cannot contain a space or
    # a colon.
    by_label = {track.label: track for track in unique}
    return [by_label[label] for label in order_labels(by_label.keys(), order)]


class PlottableBlockCircuit(PlottableBlock[PlottableCircuitEvent]):
    """One basic block's worth of circuit, laid out into columns.

    Built by `PlottableProgramCircuit`, which holds these as `_blocks`. Read
    them to inspect a circuit's structure - its rows, its depth, the instruction
    in each column - without drawing anything.

    See Also:
        `PlottableProgramCircuit`: the public API that configures these.
    """

    FIELDS = {
        "Gate": lambda event: event.gate,
        "Qubit": lambda event: ", ".join(
            track.name for track in event.tracks if not track.is_register
        ),
    }
    """The fields a circuit can group by - `"Gate"` and `"Qubit"`."""

    def __init__(self, block: BasicBlock) -> None:
        """Turn `block`'s instructions into events and pack them into columns.

        Args:
            block: One basic block of the program.
        """
        super().__init__(block)

        self.column_width: int = 56
        """How wide each column is drawn, in pixels."""

        self.row_height: int = 44
        """How tall each row is drawn, in pixels."""

        self.box_fraction: float = 0.62
        """How wide a gate box is drawn, as a column fraction.

        Leaves a gutter, so adjacent columns read as separate operations rather
        than one run-on block.
        """

        self.classical_wire_gap: float = 0.055
        """The gap in a register's double line, as a fraction of the row height.
        """

        self.unrecognized_gate_color: str = GRAY
        """The color of a gate `gate_name_color` does not classify."""

        self.wire_color: str = "#3f3f46"
        """The color of a qubit or register wire."""

        self.control_dot_color: str = "#1c1c1c"
        """The color of a controlled gate's control dot."""

        self.column_count: int = 0
        """How many columns the block packs into - the circuit's depth."""

        # Membership only - `tracks` orders on access, since `y_axis_order` is
        # set by a builder call that necessarily lands after construction.
        self._tracks: list[Track] = []
        self._build(block.instructions)

    @property
    def tracks(self) -> list[Track]:
        """This block's rows, top to bottom."""
        return _order_tracks(self._tracks, self.y_axis_order)

    # -- Construction ----------------------------------------------------------

    def _build(self, instructions: list[Instruction]) -> None:
        """Turn instructions into events, then pack them into columns."""
        qubit_tracks: dict[str, Track] = {}
        register_tracks: dict[str, Track] = {}

        def qubit_track(qubit: Qubit) -> Track | None:
            """A track for a fixed qubit.

            `None` for a placeholder or variable qubit.

            Args:
                qubit: The qubit to look up or create a track for.

            Returns:
                The qubit's track, or `None` if it is not fixed.
            """
            if not isinstance(qubit, Qubit.Fixed):
                return None
            name = str(qubit._0)
            return qubit_tracks.setdefault(name, Track(False, name))

        def register_track(reference: MemoryReference) -> Track:
            """The packing track for one register element.

            Its drawn row is the register itself.

            Args:
                reference: The memory reference to build a track for.

            Returns:
                The element's own packing track, distinct per index.
            """
            register_tracks.setdefault(reference.name, Track(True, reference.name))
            return Track(True, reference.name, reference.index)

        # `fences` records a barrier's position in the instruction stream rather
        # than an event, since a fence draws nothing - it only constrains where
        # later columns may start.
        self._fences: list[tuple[int, tuple[Track, ...]]] = []

        for instruction in instructions:
            if isinstance(
                instruction,
                (
                    Instruction.Pulse,
                    Instruction.Capture,
                    Instruction.RawCapture,
                    Instruction.SetFrequency,
                    Instruction.ShiftFrequency,
                    Instruction.SetPhase,
                    Instruction.ShiftPhase,
                    Instruction.SetScale,
                    Instruction.SwapPhases,
                ),
            ):
                raise ValueError(
                    "Cannot draw a circuit for a program with a Quil-T instruction. It may be "
                    "better to use `PlottableProgramPulseSchedule`."
                )

            if isinstance(instruction, Instruction.Gate):
                gate = instruction._0
                tracks = tuple(filter(None, (qubit_track(q) for q in gate.qubits)))
                if not tracks:
                    continue
                # A CONTROLLED gate's leading qubits are controls, one per
                # modifier, drawn as dots rather than boxes.
                control_count = sum(
                    1 for modifier in gate.modifiers if str(modifier) == "Controlled"
                )
                self.events.append(
                    PlottableCircuitEvent(
                        instruction=instruction,
                        label=_gate_label(instruction, len(gate.qubits)),
                        tracks=tracks,
                        controls=tracks[:control_count],
                    )
                )

            elif isinstance(instruction, Instruction.Measurement):
                measurement = instruction._0
                qubit = qubit_track(measurement.qubit)
                if qubit is None:
                    continue
                target = measurement.target
                tracks = (qubit,)
                target_track = None
                if target is not None:
                    target_track = register_track(target)
                    tracks = (qubit, target_track)
                self.events.append(
                    PlottableCircuitEvent(
                        instruction=instruction,
                        label="MEASURE",
                        tracks=tracks,
                        target_track=target_track,
                        target_reference=(
                            f"{target.name}[{target.index}]" if target is not None else None
                        ),
                    )
                )

            elif isinstance(instruction, Instruction.Reset):
                # A bare RESET has no qubit and applies to all of them; it can
                # only be placed once the full qubit set is known, so it is
                # resolved after this loop.
                reset = instruction._0
                qubit = qubit_track(reset.qubit) if reset.qubit is not None else None
                self.events.append(
                    PlottableCircuitEvent(
                        instruction=instruction,
                        label="RESET",
                        tracks=(qubit,) if qubit is not None else (),
                    )
                )

            elif isinstance(instruction, Instruction.Delay):
                delay = instruction._0
                tracks = tuple(filter(None, (qubit_track(q) for q in delay.qubits)))
                if not tracks:
                    continue
                self.events.append(
                    PlottableCircuitEvent(
                        instruction=instruction,
                        label=_delay_label(delay),
                        tracks=tracks,
                    )
                )

            elif isinstance(instruction, Instruction.Fence):
                fence = instruction._0
                fenced = tuple(filter(None, (qubit_track(q) for q in fence.qubits)))
                self._fences.append((len(self.events), fenced))

            else:
                # Classical instructions land on the registers they touch.
                # Anything with no memory references and no qubits (a PRAGMA,
                # say) is not drawn.
                references = _memory_references(getattr(instruction, "_0", None))
                if not references:
                    continue
                tracks = tuple(dict.fromkeys(register_track(reference) for reference in references))
                self.events.append(
                    PlottableCircuitEvent(
                        instruction=instruction,
                        label=instruction.to_quil_or_debug(),
                        tracks=tracks,
                    )
                )

        # Rows: every qubit touched, plus only the registers something actually
        # referenced - a declared-but-unused one would be an empty wire.
        # Ordering is `tracks`' job, not this method's, since the order can
        # still change after construction.
        self._tracks = [*qubit_tracks.values(), *register_tracks.values()]

        # A bare RESET or bare FENCE means "every qubit", which is only known
        # now.
        all_qubits = tuple(_order_tracks(qubit_tracks.values(), None))
        for event in self.events:
            if not event.tracks and isinstance(event.instruction, Instruction.Reset):
                event.tracks = all_qubits
        self._fences = [(position, fenced or all_qubits) for position, fenced in self._fences]

        self._pack_columns()

    def _pack_columns(self) -> None:
        """Assign each event a column: earliest free, fences acting as barriers.

        The columns reflect the serialization the program actually asked for,
        rather than a layout policy of ours.
        """
        # Greedy left-packing over tracks - an event lands at the first column
        # where every track it touches is free, then advances those tracks past
        # it. A `FENCE` advances its qubits to the furthest column used so far,
        # so nothing after it packs back alongside anything before it.
        next_free: dict[Track, int] = {track: 0 for track in self._tracks}
        fences_by_position: dict[int, tuple[Track, ...]] = {}
        for position, fenced in self._fences:
            fences_by_position.setdefault(position, ())
            fences_by_position[position] = fences_by_position[position] + fenced

        for index, event in enumerate(self.events):
            at_index = fences_by_position.get(index)
            if at_index:
                barrier = max((next_free.get(track, 0) for track in at_index), default=0)
                for track in at_index:
                    next_free[track] = barrier

            if not event.tracks:
                continue
            column = max(next_free.get(track, 0) for track in event.tracks)
            event.column = column
            for track in event.tracks:
                next_free[track] = column + 1

        # A fence trailing the last instruction still widens the diagram, so
        # honor it too.
        trailing = fences_by_position.get(len(self.events))
        if trailing:
            barrier = max((next_free.get(track, 0) for track in trailing), default=0)
            for track in trailing:
                next_free[track] = barrier

        self.column_count = max(next_free.values(), default=0)

    # -- Views -----------------------------------------------------------------

    @property
    def visible_events(self) -> list[PlottableCircuitEvent]:
        """The events this block actually draws - not hidden, and on a row."""
        return [event for event in self.events if not event.hidden and event.tracks]

    @property
    def qubit_tracks(self) -> list[Track]:
        """This block's qubit wires, top to bottom."""
        return [track for track in self.tracks if not track.is_register]

    @property
    def register_tracks(self) -> list[Track]:
        """This block's classical register wires, top to bottom."""
        return [track for track in self.tracks if track.is_register]

    @property
    @override
    def drawable(self) -> bool:
        """Whether this block has anything to draw."""
        return bool(self.visible_events)

    @property
    @override
    def caption(self) -> str:
        """This block in one line, as its control-flow graph node shows it."""
        if not self.drawable:
            return "—"
        return (
            f"{len(self.visible_events)} instructions · "
            f"{len(self.qubit_tracks)} qubits · depth {self.column_count}"
        )

    # -- Colors ---------------------------------------------------------------

    @property
    @override
    def colorable_events(self) -> list[PlottableCircuitEvent]:
        """The visible events that earn a legend entry.

        Returns:
            Every event this block draws - a circuit colors all of them.
        """
        return self.visible_events

    @override
    def _default_group_key(self, event: PlottableCircuitEvent) -> str:
        """The group `event` falls into with no `color_key`: its gate name.

        Args:
            event: The event to classify.

        Returns:
            `event`'s group key.
        """
        return event.gate

    @override
    def _default_color(self, key: str, events: list[PlottableCircuitEvent]) -> str:
        """Classify `key` by gate name, graying out anything unrecognized.

        Unlike the pulse view there is no frame name to fall back on, so an
        unrecognized name goes gray. The `is_quantum` guard keeps the gate-name
        heuristic off classical instructions, whose names would otherwise
        prefix-match a gate - `SUB` against `S`, say.

        Args:
            key: A gate name.
            events: The events carrying `key`.

        Returns:
            A CSS color.
        """
        if not any(event.is_quantum for event in events):
            return self.unrecognized_gate_color
        return gate_name_color(key) or self.unrecognized_gate_color

    # -- Drawing ---------------------------------------------------------------

    @override
    def draw(self, rows: list[Track] | None = None) -> alt.LayerChart:
        """Draw this block's circuit, using its own settings.

        Args:
            rows: Use this fixed row set instead of the one implied by this
                block's own instructions - typically the program-wide rows from
                `PlottableProgramCircuit._resolve_rows`, so a qubit sits on the
                same row in every block.

        Returns:
            An `altair.LayerChart` of this block.

        See Also:
            `PlottableProgramCircuit.draw`: draws the whole program, and is
                what you normally want.
        """
        rows = rows if rows is not None else self.tracks
        row_of = {track: index for index, track in enumerate(rows)}
        columns = max(self.column_count, 1)

        palette = self.resolve_color_map()
        legend_selection = alt.selection_point(fields=["label"], bind="legend")
        highlighted = alt.condition(
            legend_selection, alt.value(self.fill_opacity), alt.value(self.faded_opacity)
        )
        solid = alt.condition(legend_selection, alt.value(1.0), alt.value(self.faded_opacity))
        color = alt.Color(
            "label:N",
            title=None,
            scale=alt.Scale(domain=list(palette), range=list(palette.values())),
            legend=alt.Legend(symbolSize=200),
        )

        # Axes are just the row labels; both scales are plain linear ones over
        # row/column index, which is what lets every mark below be positioned
        # arithmetically.
        row_axis = alt.Axis(
            values=list(range(len(rows))),
            labelExpr=f"{json.dumps([track.label for track in rows])}[datum.value]",
            title=None,
            grid=False,
            domain=False,
            ticks=False,
        )
        row_scale = alt.Scale(domain=[len(rows) - 0.5, -0.5], nice=False)
        column_scale = alt.Scale(domain=[-0.5, columns - 0.5], nice=False)
        # A circuit's columns are packing order, not a quantity, so the x axis
        # shows nothing.
        column_axis = alt.Axis(labels=False, ticks=False, grid=False, domain=False, title=None)

        layers = [*self._wire_layers(rows, columns, row_axis, column_axis, row_scale, column_scale)]
        layers.extend(
            self._event_layers(
                row_of, color, highlighted, solid, row_axis, column_axis, row_scale, column_scale
            )
        )

        chart = (
            alt.layer(*layers)
            .add_params(legend_selection)
            .properties(
                width=self.column_width * columns,
                height=min(self.row_height * max(len(rows), 1), self.max_height),
            )
            .configure_view(strokeWidth=0)
        )

        # altair leaves `configure_view` untyped, so the chain degrades to
        # `Any`.
        # `layer()` returns the LayerChart this is declared to give back.
        return cast(alt.LayerChart, chart)

    def _wire_layers(self, rows, columns, row_axis, column_axis, row_scale, column_scale):
        """The horizontal wires.

        One line per qubit, a conventional double line per register.

        Args:
            rows: The rows to draw wires for, top to bottom.
            columns: How many columns wide the diagram is.
            row_axis: The y-axis rows are encoded against.
            column_axis: The x-axis columns are encoded against.
            row_scale: The y-scale rows are encoded against.
            column_scale: The x-scale columns are encoded against.

        Returns:
            One chart layer per wire style drawn.
        """
        qubit_rows = [
            {"row": index, "x": -0.5, "x2": columns - 0.5, "name": track.label}
            for index, track in enumerate(rows)
            if not track.is_register
        ]
        register_rows = [
            {"row": index + offset, "x": -0.5, "x2": columns - 0.5, "name": track.label}
            for index, track in enumerate(rows)
            if track.is_register
            for offset in (-self.classical_wire_gap, self.classical_wire_gap)
        ]

        def wire(records, axis):
            return (
                alt.Chart({"values": records})
                .mark_rule(color=self.wire_color, strokeWidth=1, opacity=0.55)
                .encode(
                    x=alt.X("x:Q", axis=column_axis, scale=column_scale),
                    x2="x2:Q",
                    y=alt.Y("row:Q", axis=axis, scale=row_scale),
                )
            )

        layers = [wire(qubit_rows, row_axis)]
        if register_rows:
            layers.append(wire(register_rows, row_axis))
        return layers

    def _event_layers(
        self, row_of, color, highlighted, solid, row_axis, column_axis, row_scale, column_scale
    ):
        """The marks for the instructions themselves."""
        half = self.box_fraction / 2
        boxes, spans, dots, drops = [], [], [], []

        for event in self.visible_events:
            label_key = self._color_key_of(event)
            # The target register is reached by a drop line, so it takes no box
            # and no spine.
            box_tracks = [
                track
                for track in event.tracks
                if event.target_track is None or track.row != event.target_track.row
            ]
            drawn_rows = [row_of[t.row] for t in box_tracks if t.row in row_of]
            if not drawn_rows:
                continue
            control_rows = {row_of[t.row] for t in event.controls if t.row in row_of}
            shared = {
                "col": event.column,
                "x": event.column - half,
                "x2": event.column + half,
                "label": label_key,
                "gate": event.label,
                "quil": event.quil,
            }

            # A multi-row instruction gets a vertical spine so it reads as one
            # operation.
            if len(drawn_rows) > 1:
                spans.append({**shared, "row": min(drawn_rows), "row2": max(drawn_rows)})

            for row in drawn_rows:
                if row in control_rows:
                    dots.append({**shared, "row": row})
                else:
                    boxes.append({**shared, "row": row, "y": row - half, "y2": row + half})

            # A measurement's line onto the classical wire, captioned with the
            # exact element it writes - the wire alone says a value landed in
            # `ro`, not which of its bits.
            if event.target_track is not None and event.target_track.row in row_of:
                qubit_rows = [
                    row_of[t.row] for t in event.tracks if not t.is_register and t.row in row_of
                ]
                if qubit_rows:
                    drops.append(
                        {
                            **shared,
                            "row": min(qubit_rows),
                            "row2": row_of[event.target_track.row],
                            "target": event.target_reference or "",
                        }
                    )

        tooltip = [
            alt.Tooltip("gate:N", title="Operation"),
            alt.Tooltip("quil:N", title="Quil"),
        ]
        drop_tooltip = [
            alt.Tooltip("target:N", title="Writes"),
            alt.Tooltip("quil:N", title="Quil"),
        ]

        def positioned(records):
            return alt.Chart({"values": records}).encode(
                x=alt.X("col:Q", axis=column_axis, scale=column_scale),
                y=alt.Y("row:Q", axis=row_axis, scale=row_scale),
            )

        layers = []
        if drops:
            layers.append(
                positioned(drops)
                .mark_rule(color=self.wire_color, strokeWidth=1.25, strokeDash=[3, 2])
                .encode(y2="row2:Q", opacity=solid, tooltip=drop_tooltip)
            )
        if spans:
            layers.append(
                positioned(spans)
                .mark_rule(strokeWidth=2)
                .encode(y2="row2:Q", color=color, strokeOpacity=solid, tooltip=tooltip)
            )
        if dots:
            layers.append(
                positioned(dots)
                .mark_point(shape="circle", size=110, filled=True, color=self.control_dot_color)
                .encode(opacity=solid, tooltip=tooltip)
            )
        if boxes:
            layers.append(
                alt.Chart({"values": boxes})
                .mark_rect(cornerRadius=3, stroke=None)
                .encode(
                    x=alt.X("x:Q", axis=column_axis, scale=column_scale),
                    x2="x2:Q",
                    y=alt.Y("y:Q", axis=row_axis, scale=row_scale),
                    y2="y2:Q",
                    color=color,
                    fillOpacity=highlighted,
                    tooltip=tooltip,
                )
            )
            layers.append(
                positioned(boxes)
                .mark_text(
                    fontSize=9,
                    fontWeight="bold",
                    color="#12121a",
                    limit=self.column_width * self.box_fraction,
                )
                .encode(text="gate:N", opacity=solid, tooltip=tooltip)
            )
        return layers


class PlottableProgramCircuit(PlottableProgram[PlottableBlockCircuit]):
    """A Quil program drawn as a circuit diagram.

    The program as written: one wire per qubit, one double wire per classical
    register, and one box per gate. Instructions are packed left to right into
    the earliest column where every wire they touch is free, so a column is the
    diagram's notion of circuit depth, and a `FENCE` acts as a barrier nothing
    packs across.

    Needs no calibrations, unlike
    {py:obj}`~quil.plotting.schedule.PlottableProgramPulseSchedule` - any
    parseable program can be drawn. Reach for this view to check that a program
    says what you meant; reach for the pulse schedule to see what the hardware
    will do about it.

    Configuring a diagram is a chain of `with_*` methods, each returning `self`;
    {py:obj}`draw` ends the chain.

    ### Grouping fields

    {py:obj}`with_color_key` and the string form of
    {py:obj}`hide`/{py:obj}`show` accept two field
    names:

    | Field       | Groups by                                        |
    | ----------- | ------------------------------------------------ |
    | `"Gate"`    | the instruction's name, e.g. `"RZ"`, `"MEASURE"` |
    | `"Qubit"`   | the qubits it acts on, e.g. `"3"` or `"3, 4"`    |

    A gate has no frame and no channel type, so the pulse view's four field
    names do *not* apply here.

    Examples:
        ```python
        from quil.program import Program
        from quil.plotting import PlottableProgramCircuit

        program = Program.parse(quil_text)
        PlottableProgramCircuit(program).draw()
        ```

        Taller rows, colored per qubit rather than per gate:

        ```python
        chart = (
            PlottableProgramCircuit(program)
            .with_row_height(60)
            .with_color_key("Qubit")
            .draw()
        )
        ```

    See Also:
        {py:obj}`~quil.plotting.schedule.PlottableProgramPulseSchedule`: the pulse-level view
            of the same program.
        {py:obj}`PlottableCircuitEvent`: what a {py:obj}`hide`/{py:obj}`show` predicate is handed.
    """

    @override
    def _build_blocks(self, program: Program) -> list[PlottableBlockCircuit]:
        """Lay `program` out as a circuit, block by block.

        Args:
            program: Any parseable Quil program. Calibrations, frames and
                waveforms are not needed and not consulted.

        Returns:
            One block per basic block, in program order.
        """
        # No calibration expansion: a circuit is the program as written. That is
        # the whole difference from the pulse view, which has to expand before
        # it has anything to draw.
        return [
            PlottableBlockCircuit(block) for block in program.control_flow_graph().basic_blocks()
        ]

    @override
    def _resolve_rows(self) -> list[Track]:
        drawable = self._drawable_blocks()
        if not drawable:
            return []
        seen: dict[Track, None] = {}
        for block in drawable:
            for track in block.tracks:
                seen.setdefault(track, None)
        return _order_tracks(seen, drawable[0].y_axis_order)

    def with_column_width(self, pixels: int) -> Self:
        """Set how wide each column is drawn, in pixels. Defaults to 56.

        A diagram's width is this times its column count, so a deep circuit gets
        a wide diagram. Widen it when gate labels are being truncated - the
        label is clipped to the box, so `DAGGER CONTROLLED RZ(0.5)` needs more
        room than `H`. Narrow it to fit a deep circuit on screen.

        Args:
            pixels: Width per column, in pixels.

        Returns:
            `self`, so calls chain.

        See Also:
            {py:obj}`PlottableProgramCircuit.with_row_height`: the vertical
                counterpart.
        """
        for block in self._blocks:
            block.column_width = pixels
        return self

    def with_row_height(self, pixels: int) -> Self:
        """Set how tall each wire's row is drawn, in pixels. Defaults to 44.

        A diagram's height is this times its row count. The total is capped, so
        past roughly 45 rows at the default height the diagram stops growing and
        the rows compress instead.

        Args:
            pixels: Height per row, in pixels.

        Returns:
            `self`, so calls chain.

        See Also:
            {py:obj}`PlottableProgramCircuit.with_column_width`: the horizontal
                counterpart.
            {py:obj}`PlottableProgramCircuit.hide`: fewer rows, rather than shorter
                ones.
        """
        for block in self._blocks:
            block.row_height = pixels
        return self

    def with_box_fraction(self, fraction: float) -> Self:
        """Set a gate box's width as a column fraction. Defaults to 0.62.

        The rest of the column is gutter, which is what makes two adjacent
        operations read as two rather than one run-on block. Raise it for more
        room for a long gate label, accepting a tighter gutter; lower it to
        space a dense circuit out.

        Args:
            fraction: Column fraction a gate box spans.

        Returns:
            `self`, so calls chain.

        See Also:
            {py:obj}`PlottableProgramCircuit.with_column_width`: how wide the column is
                in the first place.
        """
        for block in self._blocks:
            block.box_fraction = fraction
        return self

    def with_classical_wire_gap(self, fraction: float) -> Self:
        """Set the gap in a register's double line. Defaults to 0.055.

        A register wire is drawn as two rules this far either side of the row,
        as a fraction of the row height - the usual notation for a classical
        wire. Raise it to tell the two lines apart on a tall row.

        Args:
            fraction: Row-height fraction each line sits off center.

        Returns:
            `self`, so calls chain.

        See Also:
            {py:obj}`PlottableProgramCircuit.with_wire_color`: what color both lines
                are drawn in.
        """
        for block in self._blocks:
            block.classical_wire_gap = fraction
        return self

    def with_unrecognized_gate_color(self, color: str) -> Self:
        """Set the color of a gate the built-in heuristic does not classify.

        Every classical instruction lands here too - only quantum operations are
        classified - so this is the color most of a control-heavy block is drawn
        in.

        Args:
            color: Any CSS color string.

        Returns:
            `self`, so calls chain.

        See Also:
            {py:obj}`PlottableProgramCircuit.with_color_of`: name one gate's color
                outright.
        """
        for block in self._blocks:
            block.unrecognized_gate_color = color
        return self

    def with_wire_color(self, color: str) -> Self:
        """Set the color of the qubit and register wires.

        Args:
            color: Any CSS color string.

        Returns:
            `self`, so calls chain.

        See Also:
            {py:obj}`PlottableProgramCircuit.with_control_dot_color`: the other piece
                of circuit furniture.
        """
        for block in self._blocks:
            block.wire_color = color
        return self

    def with_control_dot_color(self, color: str) -> Self:
        """Set the color of a controlled gate's control dot.

        Args:
            color: Any CSS color string.

        Returns:
            `self`, so calls chain.

        See Also:
            {py:obj}`PlottableProgramCircuit.with_wire_color`: the wire the dot sits
                on.
        """
        for block in self._blocks:
            block.control_dot_color = color
        return self
