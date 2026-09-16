---
jupytext:
  text_representation:
    extension: .md
    format_name: myst
kernelspec:
  display_name: Python 3
  name: python3
---

# Getting started

```{code-cell} python
:tags: [remove-cell]

# Charts render against the Vega runtime `conf.py` bundles into `_static` with
# `vl-convert`, so a built page needs no CDN at view time. Altair's own
# `inline` template would embed that runtime again per chart, inside a nested
# `<html>` document - this emits just the div and the embed call.
import json
import os
import tempfile
from pathlib import Path
from uuid import uuid4

import altair as alt


def render_against_page_bundle(spec):
    div = f"altair-{uuid4().hex}"
    return {
        "text/html": f'<div id="{div}"></div>'
        f'<script>vegaEmbed("#{div}", {json.dumps(spec)}).catch(console.error);</script>'
    }


alt.renderers.register("quil-docs", render_against_page_bundle)
alt.renderers.enable("quil-docs")

# Charts write into a scratch directory, so the examples below can name plain
# relative paths.
PROGRAMS = (Path.cwd() / ".." / "tests" / "programs").resolve()
os.chdir(tempfile.mkdtemp())
source = (PROGRAMS / "single_gate_rx_pi_with_measure.quil").read_text()
```

## Install

The package can be installed from PyPI with pip or your favorite python
package manager:

```bash
pip install quil-plotting
```

## Draw a circuit

```{code-cell} python
from quil.program import Program

from quil.plotting import PlottableProgramCircuit

program = Program.parse("""
DECLARE ro BIT[3]
H 0
CNOT 0 1
RZ(pi/4) 2
CZ 1 2
MEASURE 0 ro[0]
MEASURE 1 ro[1]
MEASURE 2 ro[2]
""")

PlottableProgramCircuit(program).draw()
```

One wire per qubit, one double wire per classical register, and one box per
gate. Instructions pack left to right into the earliest column where every
wire they touch is free.

## Draw a pulse schedule

The pulse-level view of a program is what the hardware will actually play.
This one resolves the program's calibrations, so the program needs them.

```{code-cell} python
from quil.plotting import PlottableProgramPulseSchedule

calibrated_program = Program.parse(source)
PlottableProgramPulseSchedule(calibrated_program).draw()
```

One lane per qubit or coupler with each pulse drawn as its own envelope.
This example is a single rx gate followed by a measurement.

## Getting the chart out

`draw()` is also responsible for saving the diagram to a file. In a Jupyter
notebook, it will display the chart in the cell. Anywhere else it will return 
an [Altair](https://altair-viz.github.io) chart object. If you, however,
pass a filename it will save the diagram to the file.

```{code-cell} python
:tags: [remove-output]

PlottableProgramCircuit(program).draw("circuit.html")
```

`.html` keeps the chart interactive; `.svg`, `.png` and `.pdf` give you a
static figure.

A program with control flow has more than one basic block, and separate blocks
do not share a time or column axis, so there is no single diagram to draw. In
that case `draw()` gives you the control-flow graph instead. The graph can be
clicked through in html or Jupyter to explore the diagrams for each block.
Writing one still writes the file you named, and puts a file per block in
a new directory beside it that the graph's nodes references:

```{code-cell} python
branching = Program.parse("""
DECLARE ro BIT[1]
H 0
MEASURE 0 ro[0]
JUMP-WHEN @correct ro[0]
JUMP @done
LABEL @correct
X 0
LABEL @done
""")

PlottableProgramCircuit(branching).draw("plots/circuit.html")

# Show the files created
for path in sorted(Path("plots").rglob("*")):
    print(path)
```

## Configuring the chart

The diagrams follow the builder pattern and can be configured easily with
an optional chain of `with_*` methods that ends with a `draw()` call:

```{code-cell} python
(
    PlottableProgramCircuit(program)
    .with_color_key("Gate")
    .with_color_of("MEASURE", "#3d47d9")
    .hide("Qubit: 2")
    .draw()
)
```

The different views take different options. For a full set of options check out
the API reference:
- [`PlottableProgramCircuit`](api/circuit.md)
- [`PlottableProgramPulseSchedule`](api/pulse-schedule.md)
