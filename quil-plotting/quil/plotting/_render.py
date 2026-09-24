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

"""Leaf helpers shared by the pulse-schedule and circuit views.

Internal support code with no dependencies of its own: the palette, coloring by
Quil gate name, and ordering the rows of an axis. Everything here is a pure
function or a constant, which is what lets `quil.plotting.cfg` and
`quil.plotting.program` both import it without a cycle.

The machinery those two views *share* - the builder chain, the block, writing a
chart to a file - lives in `quil.plotting.program`. Anything only one view needs
stays in that view's module.

See Also:
    `quil.plotting.program`: the base classes the views are built from.
    `quil.plotting.PlottableProgramPulseSchedule`
    `quil.plotting.PlottableProgramCircuit`
"""

import re
from typing import Any

# The whole palette, defined once. Both views, the control-flow graph and the
# saved page's back link color from these - a hex literal anywhere else is
# either chrome (an outline, a wire) or a duplicate that got away.
TEAL = "#00b5ad"
YELLOW = "#ffc504"
BLUE = "#3d47d9"
MAGENTA = "#ef476f"
GRAY = "#8a8b92"

# Cycled, in first-seen key order, whenever colors are grouped by something
# with no recognizable gate name to classify. The same colors `gate_name_color`
# hands out.
COLOR_PALETTE = (TEAL, YELLOW, BLUE, MAGENTA, GRAY)

_MULTI_QUBIT_GATES = frozenset(
    {
        "CZ",
        "CPHASE",
        "CNOT",
        "CX",
        "CCNOT",
        "CCX",
        "ISWAP",
        "SWAP",
        "PSWAP",
        "PISWAP",
        "CSWAP",
        "XY",
        "CAN",
    }
)
_ONE_QUBIT_GATES = frozenset(
    {
        "H",
        "I",
        "PHASE",
        "RX",
        "RY",
        "RZ",
        "S",
        "SX",
        "SY",
        "SZ",
        "T",
        "U",
        "X",
        "Y",
        "Z",
    }
)


def gate_name_color(name: str) -> str | None:
    """A color for a Quil gate name, or `None` if the name isn't recognized.

    Groups by what the operation *is* - reset, measurement, two-qubit gate,
    one-qubit gate - so a chart's colors carry meaning before anyone reads the
    legend.

    Args:
        name: A gate, measurement or calibration name.

    Returns:
        A hex color, or `None` if the name matches no known group.
    """
    # Returning `None` rather than a default lets each view fall back
    # differently: the pulse schedule has a frame name to guess from, a circuit
    # does not.
    #
    # Matches on a prefix because real calibration names extend the gate they
    # implement (`CZ_CYCLE_1`, `MEASURE_ANCILLA`).
    token = name.upper()
    if token.startswith("RESET"):
        return MAGENTA
    if token.startswith("MEASURE"):
        return BLUE
    if any(token.startswith(gate) for gate in _MULTI_QUBIT_GATES):
        return YELLOW
    if any(token.startswith(gate) for gate in _ONE_QUBIT_GATES):
        return TEAL
    return None


def natural_sort_key(label: str) -> tuple[Any, ...]:
    """Sort key treating digit runs as numbers, so "Qubit: 10" beats "Qubit: 9".

    A plain string sort strands every single-digit qubit below the double-digit
    ones, which is wrong on any device bigger than nine qubits
    - and equally wrong for frame names (`Transmon-9_charge_tx` before
    `Transmon-10_charge_tx`).

    Args:
        label: A row label, as shown on a chart's axis.

    Returns:
        A tuple usable as a `sorted` key.
    """
    # The tuple mixes `int` and `str`, which is only comparable because
    # `re.split` on a capturing digit group *always* alternates non-digit and
    # digit parts - so a given tuple position holds the same type for every
    # label. Do not "simplify" the split; that property is what makes this work.
    return tuple(
        int(part) if part.isdigit() else part.casefold() for part in re.split(r"(\d+)", label)
    )


def order_labels(labels: Iterable[str], order: list[str] | None) -> list[str]:
    """Row labels in top-to-bottom order - the order a chart is read in.

    An explicit list is a *prefix*: the labels it names come first, in the order
    given, and everything else follows in default order - so pinning one row to
    the top does not mean enumerating all forty, and no row can silently vanish
    (that is what `hide` is for).

    Args:
        labels: Every label the data contains; duplicates are collapsed.
        order: An explicit list of labels, or `None` for the default natural
            sort. A label the data does not contain is ignored rather than
            raising, the same way `hide("nonexistent")` matches nothing.

    Returns:
        Every unique label, in top-to-bottom order.

    See Also:
        `natural_sort_key`: the default ordering.
    """
    unique = set(labels)
    if order is None:
        return sorted(unique, key=natural_sort_key)
    # `dict.fromkeys` dedupes while keeping the caller's order, so a repeated
    # label cannot turn into two rows.
    pinned = [label for label in dict.fromkeys(order) if label in unique]
    return [*pinned, *sorted(unique - set(pinned), key=natural_sort_key)]


# vega-embed's default SVG renderer gives every mark instance its own DOM
# element. A busy program turns that into tens of thousands of DOM nodes, which
# is what makes panning/zooming laggy. The canvas renderer draws the same marks
# as pixels on one <canvas> element instead: same data, same interactivity
# (Vega's own hit-testing drives hover/tooltips either way, not the DOM), only
# ever one element. Only meaningful for HTML output - `embed_options` is
# silently unused by the svg/png/pdf export path (vl-convert), which renders
# server-side regardless of it.
EMBED_OPTIONS = {"renderer": "canvas"}
