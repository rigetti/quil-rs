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

"""Module containing base class definitions for program views."""

import abc
import re
from pathlib import Path
from typing import Any, Callable, Generic, Self, TypeVar

import altair as alt

from quil.instructions import Instruction
from quil.program import BasicBlock, Program

from .cfg import PlottableControlFlowGraph
from .render import BLUE, COLOR_PALETTE, EMBED_OPTIONS

EventT = TypeVar("EventT")


class PlottableBlock(abc.ABC, Generic[EventT]):
    """An abstract plottable block from a quil program.

    Rendering settings are plain attributes, set on every block at once by the
    program's `with_*` methods.
    """

    FIELDS: dict[str, Callable[[EventT], str]]
    """Every field this view can group by, mapped to a reader over one event."""

    def __init__(self, block: BasicBlock) -> None:
        """Record where `block` sits in the graph, and the default settings.

        Args:
            block: One basic block of the program.
        """
        self.label_target: str | None = (
            block.label.to_quil_or_debug() if block.label is not None else None
        )
        """The block's Quil label in `"@name"` form, or `None`."""

        self.label: str | None = (
            self.label_target[1:]
            if self.label_target and self.label_target.startswith("@")
            else self.label_target
        )
        """The block's Quil label without its `@`, or `None`."""

        self.terminator: Instruction | None = block.terminator
        """The instruction the block ends on, or `None` if it falls through."""

        self.events: list[EventT] = []
        """Everything the block can draw, hidden ones included."""

        self.y_axis_order: list[str] | None = None
        """How the block's rows are ordered, top to bottom."""

        self.color_key: str | None = None
        """The field legend groups are formed on, or `None` for the default."""

        self.color_map: dict[str, str] = {}
        """Colors set by hand, which apply only when `color_key` is set."""

        self.fill_opacity: float = 0.3
        """How solid a filled mark is drawn."""

        self.faded_opacity: float = 0.05
        """How far an unselected mark fades when a legend entry is picked."""

        self.max_height: int = 2000
        """Ceiling on the chart's drawn height, in pixels."""

    @classmethod
    def field_accessor(cls, field: str) -> Callable[[EventT], str]:
        """Return a function that accesses a groupable field from an event."""
        try:
            return cls.FIELDS[field]
        except KeyError:
            raise ValueError(
                f"Invalid grouping term, got {field}, expected one of {', '.join(cls.FIELDS)}"
            ) from None

    @property
    @abc.abstractmethod
    def drawable(self) -> bool:
        """Whether this block has anything to draw."""

    @property
    @abc.abstractmethod
    def caption(self) -> str:
        """A one-line summary, shown under the block's node in the graph."""

    @property
    @abc.abstractmethod
    def colorable_events(self) -> list[EventT]:
        """The visible events that earn a legend entry."""

    @abc.abstractmethod
    def _default_group_key(self, event: EventT) -> str:
        """The group `event` falls into when no `color_key` is set."""

    @abc.abstractmethod
    def _default_color(self, key: str, events: list[EventT]) -> str:
        """The color for `key` under the default classification.

        Args:
            key: The group key, as `_default_group_key` produced it.
            events: Every event carrying `key`.

        Returns:
            A CSS color.
        """

    @abc.abstractmethod
    def draw(self, rows: list[Any] | None = None) -> alt.LayerChart:
        """Draw this block, using its own settings.

        Args:
            rows: Draw against this row set rather than the block's own, so
                every block in a program lines up. What `_resolve_rows` returns.

        Returns:
            This block's chart.
        """

    def _matches(self, event: EventT, criterion: str | Callable[[EventT], bool]) -> bool:
        """Whether `hide`/`show`'s criterion selects `event`.

        A callable is used as a predicate directly. A plain string is shorthand
        for "this event's value on any of the groupable fields equals it" - e.g.
        `hide("flux")` for every event on a flux channel, without needing a
        lambda for the common case.

        Args:
            event: The event to test.
            criterion: The string or predicate to test it against.

        Returns:
            Whether `criterion` selects `event`.
        """
        if callable(criterion):
            return criterion(event)
        return any(accessor(event) == criterion for accessor in self.FIELDS.values())

    def _color_key_of(self, event: EventT) -> str:
        """The value `color_key` groups `event` by.

        Args:
            event: The event to classify.

        Returns:
            `event`'s group key.
        """
        if self.color_key is None:
            return self._default_group_key(event)
        return self.field_accessor(self.color_key)(event)

    def resolve_color_map(self) -> dict[str, str]:
        """Return a map from group key to color."""
        groups: dict[str, list[EventT]] = {}
        for event in self.colorable_events:
            groups.setdefault(self._color_key_of(event), []).append(event)

        if self.color_key is not None:
            return {
                key: self.color_map.get(key, COLOR_PALETTE[index % len(COLOR_PALETTE)])
                for index, key in enumerate(groups)
            }

        return {key: self._default_color(key, events) for key, events in groups.items()}


BlockT = TypeVar("BlockT", bound=PlottableBlock[Any])


class PlottableProgram(abc.ABC, Generic[BlockT]):
    """A Quil program as a plottable set of blocks."""

    def __init__(self, program: Program) -> None:
        """Parse `program` into a plottable representation.

        All of the work happens here, so a constructed view can be drawn
        repeatedly and reconfigured cheaply.

        Args:
            program: The program to draw.

        Raises:
            TypeError: If `program` is not a `quil.program.Program`.
        """
        if not isinstance(program, Program):
            expected_msg = "Expected quil.Program for `program` parameter"
            raise TypeError(f"{expected_msg}, got {type(program)}.")

        self._blocks: list[BlockT] = self._build_blocks(program)
        """The program's basic blocks as plottable objects."""

        self.shared_y_axis: bool = False
        """Whether every block draws against the program-wide row set."""

        self._cfg = PlottableControlFlowGraph(self._blocks)
        """The control-flow graph over `_blocks`."""

    @abc.abstractmethod
    def _build_blocks(self, program: Program) -> list[BlockT]:
        """Turn `program` into this view's blocks, in program order."""

    @abc.abstractmethod
    def _resolve_rows(self) -> list[Any]:
        """Resolve one row set covering every drawable block in the program.

        When {py:obj}`with_shared_y_axis` is enabled, every block draws these
        rows, in this order.

        Returns:
            The union of rows across every drawable block, in drawing order.
        """

    def _drawable_blocks(self) -> list[BlockT]:
        return [block for block in self._blocks if block.drawable]

    # -- Builder methods -------------------------------------------------------
    # The program pushes all rendering state down to the blocks, to minimize
    # state and code bloat.

    def with_shared_y_axis(self, on: bool = True) -> Self:
        """Draw every block against the program-wide row set, or only its own.

        By default this is off, so a block shows only the rows it actually uses
        and a small block stays small. Turning it on puts a qubit at the same
        height in every block, which is what makes clicking between blocks
        readable, at the cost of every block carrying idle rows.

        No effect on a single-block program.

        Args:
            on: Whether to share the row set.

        Returns:
            `self`, so calls chain.
        """
        self.shared_y_axis = on
        return self

    def with_y_axis_order(self, order: list[str] | None) -> Self:
        """Order the rows, top to bottom.

        Args:
            order: An explicit list of row labels, or `None` for the default
                natural sort. An explicit list is a *prefix*: the labels it
                names come first, in the order given, and every other row
                follows in default order - so pinning one row to the top does
                not mean enumerating all forty. A label the program does not
                contain is ignored. This reorders only; use {py:obj}`hide` to
                remove a row.

        Returns:
            `self`, so calls chain.

        See Also:
            {py:obj}`hide`: leaving a row out entirely.
        """
        for block in self._blocks:
            block.y_axis_order = order
        return self

    def with_color_key(self, field: str | None) -> Self:
        """Group the legend's colors by `field` rather than by operation.

        By default colors are classified by operation type, i.e. one-qubit gate,
        two-qubit-gate, measure, etc. This makes some assumptions.

        Args:
            field: One of this view's grouping fields - see the class
                docstring's table - or `None` to restore the default.

        Returns:
            `self`, so calls chain.

        Raises:
            ValueError: If `field` is not one of this view's grouping fields.

        See Also:
            {py:obj}`with_color_of`: override one group's color.
            {py:obj}`with_color_map`: override several at once.
        """
        if field is not None and self._blocks:
            # Validated here rather than at draw time, so a typo raises at the
            # call that contains it.
            type(self._blocks[0]).field_accessor(field)
        for block in self._blocks:
            block.color_key = field
        return self

    def with_color_map(self, color_map: dict[str, str]) -> Self:
        """Set the color of several legend groups at once.

        Accumulates: calling this twice keeps both sets of colors, and a
        repeated key takes the later value.

        Only applies when {py:obj}`with_color_key` has been set. Under the
        default classification the colors carry meaning, so they are not
        overridden.

        Args:
            color_map: Group key to CSS color. A key no group uses is ignored.

        Returns:
            `self`, so calls chain.

        See Also:
            {py:obj}`with_color_key`: required for this to take effect.
        """
        for block in self._blocks:
            block.color_map.update(color_map)
        return self

    def with_color_of(self, key: str, color: str) -> Self:
        """Set the color of one legend group.

        Only applies when {py:obj}`with_color_key` has been set - see
        {py:obj}`with_color_map`.

        Args:
            key: The group key, as the legend shows it.
            color: A CSS color.

        Returns:
            `self`, so calls chain.
        """
        for block in self._blocks:
            block.color_map[key] = color
        return self

    def with_fill_opacity(self, opacity: float) -> Self:
        """Set how solid a filled mark is drawn.

        Args:
            opacity: A CSS opacity, 0 to 1. Defaults to 0.3.

        Returns:
            `self`, so calls chain.
        """
        for block in self._blocks:
            block.fill_opacity = opacity
        return self

    def with_faded_opacity(self, opacity: float) -> Self:
        """Set how far an unselected mark fades when a legend entry is picked.

        Args:
            opacity: A CSS opacity, 0 to 1. Defaults to 0.05. Raising it toward
                1 makes a legend selection progressively harder to see.

        Returns:
            `self`, so calls chain.
        """
        for block in self._blocks:
            block.faded_opacity = opacity
        return self

    def with_max_height(self, pixels: int) -> Self:
        """Cap how tall a block's chart is drawn.

        A performance ceiling rather than a style choice: past roughly 2000px, a
        browser struggles to composite pan and zoom smoothly.

        Args:
            pixels: Maximum height. Defaults to 2000.

        Returns:
            `self`, so calls chain.
        """
        for block in self._blocks:
            block.max_height = pixels
        return self

    # -- Control-flow graph builder methods ------------------------------------
    # These configure `cfg`, the graph of blocks a multi-block program draws -
    # not the per-block circuit or pulse chart. They are no-ops on the drawing
    # of a single-block program, which never shows a graph.

    def with_cfg_node_width(self, pixels: float) -> Self:
        """Set how wide a block's node is drawn on the control-flow graph.

        Args:
            pixels: Node width. Defaults to 220.

        Returns:
            `self`, so calls chain.
        """
        self._cfg.node_width = pixels
        return self

    def with_cfg_node_height(self, pixels: float) -> Self:
        """Set how tall a block's node is drawn on the control-flow graph.

        Args:
            pixels: Node height. Defaults to 64.

        Returns:
            `self`, so calls chain.
        """
        self._cfg.node_height = pixels
        return self

    def with_cfg_row_gap(self, pixels: float) -> Self:
        """Set the vertical gap between block nodes on the control-flow graph.

        This spaces the graph's nodes apart; it has nothing to do with the rows
        of a block's own chart.

        Args:
            pixels: Distance between one node's center and the next. Defaults to
                150.

        Returns:
            `self`, so calls chain.
        """
        self._cfg.row_gap = pixels
        # The nodes were placed when the graph was built, so they have to move.
        for node in self._cfg.nodes:
            node.y = -node.index * self._cfg.row_gap
        return self

    def with_cfg_edge_bulge(self, pixels: float) -> Self:
        """Set how far a control-flow edge bulges out to the side.

        Only an edge that skips a block bulges - a jump forward, or a loop's
        back edge; an edge to the next block in order is drawn straight.

        Args:
            pixels: How far the curve bows out. Defaults to 130.

        Returns:
            `self`, so calls chain.
        """
        self._cfg.bulge = pixels
        return self

    def with_cfg_edge_samples(self, samples: int) -> Self:
        """Set how many points a bulging control-flow edge is sampled at.

        A smoothness knob for the graph's curved edges - lower is more angular
        and cheaper to draw.

        Args:
            samples: Points per curve. Defaults to 24.

        Returns:
            `self`, so calls chain.
        """
        self._cfg.curve_samples = samples
        return self

    def with_cfg_arrow_gap(self, pixels: float) -> Self:
        """Set how far short of its target a control-flow edge stops.

        The gap is what keeps an arrowhead clear of the block it points at,
        rather than overlapping its outline.

        Args:
            pixels: Clearance between the arrowhead and the node. Defaults to
                14.

        Returns:
            `self`, so calls chain.
        """
        self._cfg.arrow_gap = pixels
        return self

    def with_cfg_node_fill(self, color: str) -> Self:
        """Set the fill of a drawable block's node on the control-flow graph.

        Args:
            color: A CSS color.

        Returns:
            `self`, so calls chain.

        See Also:
            {py:obj}`with_cfg_empty_node_fill`: blocks with nothing to
                draw.
        """
        self._cfg.node_fill_drawable = color
        return self

    def with_cfg_empty_node_fill(self, color: str) -> Self:
        """Set the fill of a node whose block has nothing to draw.

        Such a node - a pure-delay block, or a loop's empty back-edge block -
        gets no link, and this color is what marks it out on the graph.

        Args:
            color: A CSS color.

        Returns:
            `self`, so calls chain.
        """
        self._cfg.node_fill_empty = color
        return self

    def with_cfg_node_stroke(self, color: str) -> Self:
        """Set the outline color of a block's node on the control-flow graph.

        Args:
            color: A CSS color.

        Returns:
            `self`, so calls chain.
        """
        self._cfg.node_stroke = color
        return self

    def with_cfg_node_title_color(self, color: str) -> Self:
        """Set the color of the block label drawn inside a graph node.

        This is the text on the node, not the chart's own title.

        Args:
            color: A CSS color.

        Returns:
            `self`, so calls chain.
        """
        self._cfg.title_color = color
        return self

    def with_cfg_node_subtitle_color(self, color: str) -> Self:
        """Set the color of the caption drawn under a graph node's label.

        Args:
            color: A CSS color.

        Returns:
            `self`, so calls chain.
        """
        self._cfg.subtitle_color = color
        return self

    def with_cfg_edge_color(self, color: str) -> Self:
        """Set the color of a forward control-flow edge.

        Args:
            color: A CSS color.

        Returns:
            `self`, so calls chain.

        See Also:
            {py:obj}`with_cfg_back_edge_color`: a loop's edge.
        """
        self._cfg.edge_color = color
        return self

    def with_cfg_back_edge_color(self, color: str) -> Self:
        """Set the color of a loop's back edge on the control-flow graph.

        An edge landing at or before the block it leaves is drawn in this color
        to make loops stand out.

        Args:
            color: A CSS color.

        Returns:
            `self`, so calls chain.
        """
        self._cfg.back_edge_color = color
        return self

    # -- Visibility ------------------------------------------------------------

    def hide(self, criterion: str | Callable[[Any], bool]) -> Self:
        """Leave out every event `criterion` selects, across every block.

        A hidden event takes no row and no legend entry, and takes no part in
        whatever the view normalizes - so hiding the readout pulses does not
        just remove them, it lets the drive pulses fill their lanes properly.

        Applied when called, not at draw time, so consecutive {py:obj}`hide` and
        {py:obj}`show` calls compose.

        Args:
            criterion: Either a string, matched against all of this view's
                grouping fields at once - `hide("flux")` catches every event on
                a flux channel, with no lambda needed - or a predicate taking
                one event and returning whether to hide it. A string matching
                nothing is a no-op.

        Returns:
            `self`, so calls chain.

        Examples:
            Drop a group, then bring one member back:

            ```python
            view.hide("flux").show("Qubit: 12")
            ```

            Anything a field cannot express takes a predicate:

            ```python
            view.hide(lambda event: event.start_time > 2e-6)
            ```

        See Also:
            {py:obj}`show`: the inverse.
        """
        for block in self._blocks:
            for event in block.events:
                if block._matches(event, criterion):
                    event.hidden = True
        return self

    def show(self, criterion: str | Callable[[Any], bool]) -> Self:
        """Bring back every event `criterion` selects, across every block.

        The inverse of {py:obj}`hide`, taking the same criterion. Events start
        out shown, so this is only useful after a {py:obj}`hide` - typically to
        carve an exception out of a broad one.

        Args:
            criterion: A string matched against this view's grouping fields, or
                a predicate over one event.

        Returns:
            `self`, so calls chain.

        See Also:
            {py:obj}`hide`: the inverse, and the fuller description of
                `criterion`.
        """
        for block in self._blocks:
            for event in block.events:
                if block._matches(event, criterion):
                    event.hidden = False
        return self

    # -- Output ----------------------------------------------------------------

    def draw(self, filename: str | Path | None = None) -> Any:
        """Draw this program's diagram, and write it to `filename` if given.

        What comes back depends on the program's shape because basic blocks are
        laid out independently and cannot share an axis with each other:

        - **One drawable block** - that block's chart.
        - **Several blocks, in a notebook** - a widget showing the control-flow
          graph. Clicking a node opens that block's chart, and a back button
          returns to the graph.
        - **Several blocks, outside a notebook** - the control-flow graph as a
          static chart, with unlinked nodes.

        The chart is interactive: scroll to zoom, drag to pan, hover a mark for
        its details, and click a legend entry to fade everything else.
        Multi-block charts also contain clickable links to walk the control
        flow.

        Writing works the same way depending on the program's shape: `filename`
        is the file written. Because several blocks cannot share one chart, a
        multi-block program also gets one file per block in a directory named
        after that file, which the graph's nodes link into. For example, if you
        call `.draw("circuit.html")` on a multi-block program:

        ```text
        circuit.html          the control-flow graph
        circuit.html.blocks/  one file per drawable block, each linking back
        ```

        Args:
            filename: Write the chart here. The suffix picks the format -
                `.html` for the interactive chart and working links, or `.svg`,
                `.png` or `.pdf` for a static figure. Missing parent directories
                are created.

        Returns:
            An `altair.LayerChart`, or an `ipywidgets.VBox` for a multi-block
            program in a notebook.

        Raises:
            ValueError: If no block in the program has anything to draw.

        Examples:
            ```python
            view.with_shared_y_axis().draw("out/circuit.html")
            ```
        """
        drawable = self._drawable_blocks()
        if not drawable:
            raise ValueError("Program has nothing to draw.")

        # Create Path
        path = Path(filename) if filename is not None else None
        if path is not None:
            path.parent.mkdir(parents=True, exist_ok=True)

        # Draw single block program simply
        if len(drawable) == 1:
            chart = drawable[0].draw()
            if path is not None:
                _write_chart(chart, path)
            return chart

        # Multiple Blocks
        rows = self._resolve_rows() if self.shared_y_axis else None

        if path is None:
            if _in_notebook():
                return self._cfg.draw_widget(lambda index: self._blocks[index].draw(rows=rows))

            # Nothing has been written, so a node has nothing to link to.
            return self._cfg.draw([None] * len(self._blocks))

        # The blocks go in a directory named after the file the graph is
        # written to, so two formats written side by side cannot collide.
        # Names are zero-padded so lexical order matches execution order, with
        # the block's label after a dot where it has one - `block-03.loop.html`.
        # A dot is the separator `tests/conftest.py`'s `plot_filename` already
        # uses, since labels and program names can both contain hyphens.
        sidecar = path.parent / f"{path.name}.blocks"
        digits = max(2, len(str(max(len(self._blocks) - 1, 0))))
        names: list[str | None] = []
        for index, block in enumerate(self._blocks):
            if not block.drawable:
                names.append(None)
                continue
            # Every run of characters unsafe in a filename becomes one `_`.
            label = f".{re.sub(r'[^A-Za-z0-9_-]+', '_', block.label)}" if block.label else ""
            names.append(f"{sidecar.name}/block-{index:0{digits}d}{label}{path.suffix}")

        graph = self._cfg.draw(names)
        _write_chart(graph, path)

        sidecar.mkdir(exist_ok=True)
        for block, name in zip(self._blocks, names, strict=True):
            if name is None:
                continue
            # A block page sits one level down, so its way back up is relative.
            _write_chart(block.draw(rows=rows), path.parent / name, back_link=f"../{path.name}")

        return graph


def _write_chart(chart: alt.LayerChart, path: Path, back_link: str | None = None) -> None:
    """Save a chart to `path`, in the format its suffix names.

    Args:
        chart: The chart to write.
        path: Destination file. Its suffix picks the format - `.html`, `.svg`,
            `.png` and `.pdf` are all supported by Altair here.
        back_link: Put a link to this relative path above the chart, labeled
            "Block index". Only meaningful for `.html`, and silently skipped
            otherwise - there is nowhere to navigate from a `.png`, so a caller
            writing images needs no special case.

    Raises:
        RuntimeError: If a back link was asked for but altair's saved page has
            no known place to put one.
    """
    chart.save(path, embed_options=EMBED_OPTIONS)

    if path.suffix == ".svg":
        # `vl-convert`'s SVG renderer resolves a relative `href` against this
        # hardcoded default base URL rather than leaving it relative - unlike
        # its HTML output, where the browser resolves `href` against the page's
        # own location. A link meant to point at a file in the output directory
        # otherwise comes out aimed at `vega-datasets` on GitHub instead.
        path.write_text(path.read_text().replace("https://vega.github.io/vega-datasets/", ""))
        return

    if back_link is None or path.suffix != ".html":
        return

    # The link is put into the saved page rather than into the chart because it
    # is a property of the page. An `href`-encoded mark would have to be a layer
    # sharing the chart's own scales, and a layered chart merges its axes into
    # one guide - the trap that silently blanked the circuit's row labels twice;
    # a `vconcat` avoids that but changes what `draw()` hands back. Same
    # reasoning as the SVG fixup above: post-process the file, leave the spec
    # alone.
    #
    # Where altair puts the chart is deterministic, because nothing here passes
    # a custom `output_div` to `save`, which is what names it.
    chart_div = '<div id="vis"></div>'
    html = path.read_text()
    if chart_div not in html:
        raise RuntimeError(
            f"cannot add a back link to {path}: altair's saved page no longer contains "
            f"{chart_div!r}, so there is no known place to put one."
        )
    anchor = (
        f'<a href="{back_link}" style="display:inline-block;margin:8px 0 14px;padding:6px 14px;'
        f"border:1px solid #1c1c1c;border-radius:6px;background:{BLUE};color:#ffffff;"
        f'font:13px/1.2 system-ui,sans-serif;text-decoration:none">&#8592; Block index</a>'
    )
    path.write_text(html.replace(chart_div, f"{anchor}\n  {chart_div}", 1))


def _in_notebook() -> bool:
    """Return if being called from a Jupyter kernel."""
    # `ZMQInteractiveShell` is the kernel behind notebook/lab/qtconsole - the
    # environments that can display a widget. A terminal IPython session is a
    # `TerminalInteractiveShell` and can display nothing but text, and a plain
    # script has no shell at all.
    from IPython import get_ipython

    shell = get_ipython()
    return shell is not None and type(shell).__name__ == "ZMQInteractiveShell"
