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

"""An interactive diagram of a program's control-flow graph."""

import math
from dataclasses import dataclass
from typing import Any, Callable, Literal

import altair as alt
import numpy as np
from quil.instructions import Instruction

from .render import BLUE, EMBED_OPTIONS, GRAY, MAGENTA


@dataclass
class PlottableCfgNode:
    """A basic block positioned as a row in the graph."""

    index: int
    """The block's position in the program."""

    title: str
    """The block's label."""

    subtitle: str
    """A caption displayed underneath the title."""

    drawable: bool
    """Whether the block has anything to draw. If not, the node has no link."""

    y: float
    """The node's vertical position on the graph."""


@dataclass
class PlottableCfgEdge:
    """One control-flow edge between two blocks, derived from a terminator."""

    source: int
    """Index of the block the edge leaves."""

    target: int
    """Index of the block the edge enters."""

    kind: Literal["fallthrough", "not-taken", "jump", "taken"]
    """How control reaches the target - a conditional jump forks into a pair."""

    back: bool
    """Whether the target is at or before the source, i.e. a loop."""

    terminator: str | None = None
    """The terminator that produced this edge, as Quil text, or `None`."""


class PlottableControlFlowGraph:
    """A program's control-flow graph as an interactable, drawable widget.

    A program with more than one basic block has no single time or column axis,
    so it is drawn as a graph of blocks instead, and each node opens that
    block's own chart. This is true for both the circuit and pulse views.

    This module is deliberately agnostic about what a block *contains*: a block
    describes itself through `caption` and `drawable`, and the caller supplies
    the per-block chart.

        Drawing settings are plain attributes, set through the owning program's
        `with_cfg_*` methods rather than on the graph itself.
    """

    def __init__(self, blocks: list[Any]) -> None:
        """Build the graph over `blocks`.

        Args:
            blocks: The program's blocks in order. Each must expose `label`,
                `label_target`, `terminator`, `caption` and `drawable`.
        """
        self.node_width: float = 220.0
        """How wide a node is drawn, in pixels."""

        self.node_height: float = 64.0
        """How tall a node is drawn, in pixels."""

        self.row_gap: float = 150.0
        """The vertical distance between one node's center and the next."""

        self.bulge: float = 130.0
        """How far a non-adjacent edge bulges to the side."""

        self.curve_samples: int = 24
        """How many points a bulging edge is sampled at."""

        self.arrow_gap: float = 14.0
        """How far short of its target an edge stops, in pixels.

        Leaves the arrowhead clear of the block instead of overlapping it.
        """

        self.node_fill_drawable: str = BLUE
        """A drawable block's fill."""

        self.node_fill_empty: str = GRAY
        """The fill of a block with nothing to draw."""

        self.node_stroke: str = "#1c1c1c"
        """A node's outline color."""

        self.title_color: str = "#ffffff"
        """The color of a node's title."""

        self.subtitle_color: str = "#d7d9ea"
        """The color of a node's subtitle."""

        self.edge_color: str = "#4a4a4a"
        """A forward edge's color."""

        self.back_edge_color: str = MAGENTA
        """A loop's back edge color."""

        label_index = {
            block.label_target: index
            for index, block in enumerate(blocks)
            if block.label_target is not None
        }

        self.nodes: list[PlottableCfgNode] = [
            self._build_node(index, block) for index, block in enumerate(blocks)
        ]
        """The graph's blocks, one per basic block."""

        self.edges: list[PlottableCfgEdge] = []
        """The graph's control-flow edges."""

        for index, block in enumerate(blocks):
            self.edges.extend(self._build_edges(index, block, len(blocks), label_index))

    def _build_node(self, index: int, block: Any) -> PlottableCfgNode:
        title = block.label if block.label else f"block {index}"
        return PlottableCfgNode(
            index=index,
            title=title,
            subtitle=block.caption,
            drawable=block.drawable,
            y=-index * self.row_gap,
        )

    def _build_edges(
        self,
        index: int,
        block: Any,
        block_count: int,
        label_index: dict[str, int],
    ) -> list[PlottableCfgEdge]:
        # `quil.program.ControlFlowGraph` exposes only the block list, not an
        # edge list, so the edges are recovered from each block's `label` and
        # `terminator` - the two fields both views' block classes already carry.
        # `None` falls through to the next block, `Jump` goes unconditionally to
        # its target, `JumpWhen`/`JumpUnless` fork into a taken edge and a
        # not-taken. fall-through, and `Halt` has no outgoing edge.
        terminator = block.terminator
        if terminator is None:
            if index + 1 < block_count:
                return [PlottableCfgEdge(index, index + 1, kind="fallthrough", back=False)]
            return []

        if isinstance(terminator, Instruction.Halt):
            return []

        key = terminator._0.target.to_quil_or_debug()
        if key not in label_index:
            raise ValueError(f"jump target {key!r} has no matching LABEL in this program's blocks")
        target = label_index[key]
        back = target <= index

        if isinstance(terminator, Instruction.Jump):
            return [
                PlottableCfgEdge(
                    index, target, kind="jump", back=back, terminator=terminator.to_quil_or_debug()
                )
            ]

        # JumpWhen / JumpUnless: a conditional fork into a taken edge and a
        # not-taken fall-through.
        quil = terminator.to_quil_or_debug()
        edges = [PlottableCfgEdge(index, target, kind="taken", back=back, terminator=quil)]
        if index + 1 < block_count:
            edges.append(
                PlottableCfgEdge(index, index + 1, kind="not-taken", back=False, terminator=quil)
            )
        return edges

    def draw(
        self,
        filenames: list[str | None],
        selection: alt.Parameter | None = None,
    ) -> alt.LayerChart:
        """Draw one node per basic block, linked to the files it names.

        A block with nothing to draw - a pure-delay block, or a loop's empty
        back-edge block - gets a node with no link, so the shape of the
        program's control flow stays visible even where there is nothing to
        plot.

        Args:
            filenames: One entry per node, in block order. A `None` entry leaves
                that node unlinked.
            selection: Attach this point selection to the nodes instead of the
                `href` links, and use it to highlight whichever node is picked.
                The two modes are mutually exclusive.

        Returns:
            A layered chart of the whole graph.

        See Also:
                `PlottableControlFlowGraph.draw_widget`: the notebook
            counterpart, which uses `selection`.
        """
        # A `selection` is what makes the graph usable inside a notebook: an
        # `href` there resolves against the notebook server, so clicking one
        # either 404s or navigates the browser away from the notebook entirely.
        named = list(zip(self.nodes, filenames, strict=True))
        linked = [pair for pair in named if pair[1] is not None]
        unlinked = [pair for pair in named if pair[1] is None]

        layers = [
            *self._draw_edges(),
            *([self._draw_nodes(unlinked, linked=False, selection=selection)] if unlinked else []),
            *([self._draw_nodes(linked, linked=True, selection=selection)] if linked else []),
            self._draw_text(
                self.nodes, "title", -6.0, self.title_color, fontSize=13, fontWeight="bold"
            ),
            self._draw_text(self.nodes, "subtitle", 12.0, self.subtitle_color, fontSize=11),
        ]

        width = self.node_width + 2 * self.bulge + 40
        height = len(self.nodes) * self.row_gap + self.node_height + 20

        # No `add_params` here: a param added to the *layer chart* binds to the
        # first named view, which is an edge line - so clicking a node would
        # never register. `_draw_nodes` attaches it to the node layer instead,
        # which is what puts the param's `views` on the rects.
        return (
            alt.layer(*layers)
            .properties(width=width, height=height, title="Control-flow graph")
            .configure_view(strokeWidth=0)
        )

    def draw_widget(self, draw_block: Callable[[int], Any]) -> Any:
        """Draw an interactive notebook widget.

        The graph is what the cell shows until something is picked, and a back
        button returns to it. A pick on a block with nothing to draw is ignored,
        matching the saved HTML where such a node gets no link; its thickened
        outline is feedback enough that the click registered.

        Args:
            draw_block: Draws block `index`'s chart. Supplied by the calling
                view, which owns how a block is rendered.

        Returns:
                An `ipywidgets.VBox` whose children are swapped between the
            graph and a block.

        See Also:
                `PlottableControlFlowGraph.draw`: the static chart, for a
            script or a saved page.
        """
        # Imported here rather than at module scope: this is the only path that
        # needs it, and pulling in the widget stack costs real time on `import
        # quil.plotting`.
        import ipywidgets

        container = ipywidgets.VBox()
        # Widgets are closed one swap late. Closing the graph chart inside its
        # own selection handler is what the deferral avoids; closing nothing
        # leaks a comm and a full vega spec per round trip, which for a busy
        # block is most of a megabyte.
        stale: list[Any] = []

        def swap(children: tuple[Any, ...]) -> None:
            for widget in stale:
                widget.close()
            stale[:] = container.children
            container.children = children

        def show_graph() -> None:
            # Rebuilt from scratch every time, which is load-bearing: a reused
            # chart keeps its frontend selection store, so re-picking the block
            # you just came back from is not a *change*, the observer never
            # fires, and the graph looks dead. A few dozen marks make this
            # cheap, and a fresh one needs no pre-picked value.
            selection = alt.selection_point(
                name="block_pick", fields=["block"], on="click", empty=False
            )
            chart = alt.JupyterChart(
                self.draw([None] * len(self.nodes), selection=selection),
                embed_options=EMBED_OPTIONS,
            )
            # altair ships no stub for `selections`, which anywidget adds
            # at runtime.
            chart.selections.observe(on_pick, names=["block_pick"])  # type: ignore[attr-defined]
            swap((chart,))

        def show_block(index: int) -> None:
            back = ipywidgets.Button(
                description="Blocks", icon="arrow-left", layout=ipywidgets.Layout(width="auto")
            )
            back.on_click(lambda _button: show_graph())
            title = self.nodes[index].title
            header = ipywidgets.HBox([back, ipywidgets.HTML(f"<b>{title}</b>")])
            swap((header, alt.JupyterChart(draw_block(index), embed_options=EMBED_OPTIONS)))

        def on_pick(change: Any) -> None:
            picked = getattr(change.new, "value", None) or []
            if not picked:
                return
            index = int(picked[0]["block"])
            if self.nodes[index].drawable:
                show_block(index)

        show_graph()
        return container

    @staticmethod
    def _xy(source: dict[str, list[dict[str, Any]]], x: str = "x:Q", y: str = "y:Q") -> alt.Chart:
        no_axis = alt.Axis(labels=False, ticks=False, grid=False, domain=False, title=None)
        scale = alt.Scale(nice=False)
        return alt.Chart(source).encode(
            x=alt.X(x, axis=no_axis, scale=scale), y=alt.Y(y, axis=no_axis, scale=scale)
        )

    def _draw_nodes(
        self,
        nodes: list[tuple[PlottableCfgNode, str | None]],
        linked: bool,
        selection: alt.Parameter | None = None,
    ) -> alt.Chart:
        records = [
            {
                "index": n.index,
                # `block` is what a selection projects on. It duplicates `index`
                # rather than reusing it because `index` is also Vega-Lite's own
                # row-index convention, and a selection keyed on that name is
                # easy to misread.
                "block": n.index,
                "x0": -self.node_width / 2,
                "x1": self.node_width / 2,
                "y0": n.y - self.node_height / 2,
                "y1": n.y + self.node_height / 2,
                "fill": self.node_fill_drawable if n.drawable else self.node_fill_empty,
                "href": filename or "",
                "title": n.title,
            }
            for n, filename in nodes
        ]
        chart = self._xy({"values": records}, x="x0:Q", y="y0:Q").mark_rect(
            cornerRadius=8,
            stroke=self.node_stroke,
            strokeWidth=1,
            opacity=1.0 if linked else 0.9,
        )
        encoding = {
            "x2": "x1:Q",
            "y2": "y1:Q",
            "color": alt.Color("fill:N", scale=None, legend=None),
        }

        if selection is not None:
            # Thicken the picked node's outline, so a click visibly registers.
            # Without this there is no feedback at all that the selection
            # changed.
            encoding["strokeWidth"] = alt.condition(selection, alt.value(3.5), alt.value(1))
            encoding["tooltip"] = alt.Tooltip("title:N", title="Block")
            chart = chart.add_params(selection)
        elif linked:
            encoding["href"] = "href:N"
            encoding["tooltip"] = alt.Tooltip("href:N", title="Open")
        else:
            encoding["tooltip"] = alt.value(None)
        return chart.encode(**encoding)

    def _draw_text(
        self, nodes: list[PlottableCfgNode], attr: str, dy: float, color: str, **mark_kwargs: Any
    ) -> alt.Chart:
        records = [
            {"index": n.index, "x": 0.0, "y": n.y, "text": getattr(n, attr), "color": color}
            for n in nodes
        ]
        return (
            self._xy({"values": records})
            .mark_text(dy=dy, **mark_kwargs)
            .encode(text="text:N", color=alt.Color("color:N", scale=None, legend=None))
        )

    def _draw_edges(self) -> list[alt.Chart]:
        edge_points: list[dict[str, Any]] = []
        arrow_points: list[dict[str, Any]] = []

        for edge_id, edge in enumerate(self.edges):
            source, target = self.nodes[edge.source], self.nodes[edge.target]
            xs, ys = self._edge_curve(source, target, edge.back)
            described = {
                "source": source.title,
                "target": target.title,
                "flow": {
                    "fallthrough": "falls through",
                    "jump": "jumps unconditionally",
                    "taken": "branch taken",
                    "not-taken": "branch not taken",
                }[edge.kind]
                + (" (loop)" if edge.back else ""),
                # The terminator verbatim, rather than a prose retelling of it:
                # which branch `JUMP-WHEN @a ro[0]` takes is the one thing a
                # reader wants off the edge, and it is already written in Quil.
                "terminator": edge.terminator or "-",
                "color": self.back_edge_color if edge.back else self.edge_color,
            }

            for order, (x, y) in enumerate(zip(xs, ys, strict=True)):
                edge_points.append(
                    {
                        "edge": edge_id,
                        "order": order,
                        "x": x,
                        "y": y,
                        "dashed": edge.kind == "not-taken",
                        **described,
                    }
                )

            # Point mark shapes default to pointing up (angle 0), rotating
            # clockwise as the angle grows - so the arrowhead's angle is the
            # clockwise bearing of the curve's own tangent at its endpoint, not
            # just whether the target is above or below: a bulging
            # (non-adjacent) edge does not arrive at its target moving purely
            # vertically.
            dx, dy = xs[-1] - xs[-2], ys[-1] - ys[-2]
            angle = math.degrees(math.atan2(dx, dy)) % 360
            arrow_points.append({"x": xs[-1], "y": ys[-1], "angle": angle, **described})

        if not edge_points:
            return []

        tooltip = [
            alt.Tooltip("source:N", title="From"),
            alt.Tooltip("target:N", title="To"),
            alt.Tooltip("flow:N", title="Flow"),
            alt.Tooltip("terminator:N", title="Terminator"),
        ]
        color = alt.Color("color:N", scale=None, legend=None)
        lines = self._xy({"values": edge_points}).encode(
            detail="edge:N", order="order:Q", color=color
        )
        return [
            lines.mark_line(strokeWidth=1.75, interpolate="linear").encode(
                strokeDash=alt.condition("datum.dashed", alt.value([5, 4]), alt.value([1, 0]))
            ),
            self._xy({"values": arrow_points})
            .mark_point(shape="triangle", size=140, filled=True, opacity=1.0, aria=False)
            .encode(color=color, angle=alt.Angle("angle:Q", scale=None), tooltip=tooltip),
            # A 1.75px line is all but unhoverable, so the tooltip rides an
            # invisible fat copy of the same path, layered last so the pointer
            # lands on it. The nodes are drawn above these, and stay clickable.
            lines.mark_line(strokeWidth=14, opacity=0.0, interpolate="linear").encode(
                tooltip=tooltip
            ),
        ]

    def _edge_curve(
        self, source: PlottableCfgNode, target: PlottableCfgNode, back: bool
    ) -> tuple[list[float], list[float]]:
        """Calculate the points of one edge's path, source node to target node.

        An edge to the next block in order draws as a straight vertical line,
        from the middle of the source's bottom edge to the middle of the
        target's top edge. Any other edge - a jump forward, or a loop's back
        edge - leaves and enters at the middle of each node's right-hand side
        and bulges out past them, so it neither runs through the nodes in
        between nor arrives at a corner. Either way the path stops an
        `arrow_gap` short of the node it points at.

        Args:
            source: The node the edge leaves.
            target: The node the edge enters.
            back: Whether this is a loop's back edge, drawn on the same side as
                a forward bulge.

        Returns:
            The edge's x and y coordinates, source to target.
        """
        if target.index == source.index + 1 and not back:
            y_start = source.y - self.node_height / 2
            return [0.0, 0.0], [y_start, target.y + self.node_height / 2 + self.arrow_gap]

        # Anchored on the two side mid-points rather than on the top and bottom
        # edges: a back edge runs upwards, so bottom-to-top would leave the
        # source heading away from its target and land on the far side of it.
        # The sine never dips below the nodes' own half-width, which is what
        # keeps the curve clear of whatever sits between the two.
        #
        # Stopping short is a nudge back along the path, so the last sample is
        # taken before `t` reaches 1 - near enough, at this distance, to read
        # off the path's speed at the end rather than its true arc length.
        rise = target.y - source.y
        stop = 1.0 - self.arrow_gap / math.hypot(math.pi * self.bulge, rise)
        t = np.linspace(0.0, stop, self.curve_samples)
        xs = (self.node_width / 2 + self.bulge * np.sin(np.pi * t)).tolist()
        ys = (source.y + rise * t).tolist()
        return xs, ys
