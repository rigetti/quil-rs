# Getting started

## Install

The package can be installed from PyPI wit pip or your favorite python
package manager:

```bash
pip install quil-plotting
```

## Draw a circuit

The gate-level view. It needs no calibrations, so any parseable program can be
drawn.

```python
from quil.program import Program
from quil.plotting import PlottableProgramCircuit

program = Program.parse(quil_text)

PlottableProgramCircuit(program).draw()
```

One wire per qubit, one double wire per classical register, one box per gate.
Instructions pack left to right into the earliest column where every wire they
touch is free, so a column is the diagram's notion of circuit depth.

## Draw a pulse schedule

The pulse-level view: what the hardware will actually play.
This one resolves the program's calibrations, so the program needs them.

```python
from quil.program import Program
from quil.plotting import PlottableProgramPulseSchedule

program = Program.parse(quil_text)

PlottableProgramPulseSchedule(program).draw()
```

## Getting the chart out

`draw()` does the right thing for where you are:

- **In a Jupyter notebook** it displays the chart in the cell.
- **Anywhere else** it returns an [Altair](https://altair-viz.github.io) chart
  object. Pass a filename to write it instead:

  ```python
  PlottableProgramCircuit(program).draw("circuit.html")
  ```

  `.html` keeps the chart interactive; `.svg`, `.png` and `.pdf` give you a
  static figure.

A program with control flow has more than one basic block, and separate blocks
do not share a time or column axis - so there is no single diagram to draw. In
that case `draw()` gives you the control-flow graph instead. Writing one still
writes the file you named, and puts a file per block in a sidecar directory
beside it that the graph's nodes link into:

```python
PlottableProgramCircuit(program).draw("plots/circuit.html")
```

```text
plots/circuit.html          the control-flow graph
plots/circuit.html.blocks/  one file per block, each linking back
```

## Configuring the chart

Both classes configure the same way: a chain of `with_*` methods, each
returning `self`, ended by `draw()`.

```python
(
    PlottableProgramCircuit(program)
    .with_color_key("Gate")
    .with_color_of("MEASURE", "#3d47d9")
    .hide("Qubit: 3")
    .draw()
)
```

The two classes take different options. For a full set of options check out the
API reference:
[`PlottableProgramCircuit`](api/circuit.md) and
[`PlottableProgramPulseSchedule`](api/pulse-schedule.md) in the reference.
