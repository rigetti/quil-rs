# Quil Plotting Library

Visualization tools for [Quil](https://github.com/quil-lang/quil) programs.

The library provides two program views:

- **[`PlottableProgramCircuit`]**: Basic circuit view
- **[`PlottableProgramPulseSchedule`]**: Pulse schedule view resolved from the
    program's calibrations.

Both produce [Altair](https://altair-viz.github.io) charts, which are
interactive when drawn in a Jupyter cell or saved as HTML. They can also
be saved as SVG, PDF, or PNG. Check out the
[example notebooks](https://github.com/rigetti/quil-rs/tree/main/quil-plotting/examples)
for an interactive showcase of this packages features. See the main quil-rs
[docs](https://rigetti.github.io/quil-rs/quil.html) for more information on the main package.

```{toctree}
:maxdepth: 2

getting-started
```

```{toctree}
:maxdepth: 1
:caption: API reference

api/circuit
api/pulse-schedule
api/events
```

[`PlottableProgramCircuit`]: api/circuit.md
[`PlottableProgramPulseSchedule`]: api/pulse-schedule.md
