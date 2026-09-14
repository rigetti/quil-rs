# quil-plotting

Visualize [Quil](https://github.com/quil-lang/quil) programs as a circuit,
or as a pulse schedule.

## Install

```bash
pip install quil-plotting
```

This installs the `quil.plotting` extension into the `quil` package.

## Getting started

1. Draw a program circuit:

```python
from quil.program import Program
from quil.plotting import PlottableProgramCircuit

program = Program.parse(quil_text)

PlottableProgramCircuit(program).draw()
```

2. Draw a program's pulse schedule:

```python
from quil.program import Program
from quil.plotting import PlottableProgramPulseSchedule

program = Program.parse(quil_text)

PlottableProgramPulseSchedule(program).draw()
```

In a Jupyter notebook, `draw()` displays the chart in the cell. Outside one,
it returns an Altair chart - pass a filename to write it:

```python
PlottableProgramCircuit(program).draw("circuit.html")
```

Supports the `.html`, `.svg`, `.png` and `.pdf` filetypes. A program with control
flow is drawn as a graph of its basic blocks, and writing one also puts a file
per block in a `circuit.html.blocks/` directory that the graph links into.

These charts are highly configurable, please see the documentation or tutorial
for more information.
