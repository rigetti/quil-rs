# Events

Private classes that a documented here since they are the objects a `hide()` or
`show()` predicate receives. A circuit predicate is handed a
`PlottableCircuitEvent`; a pulse-schedule predicate is handed one of the
`PlottablePulseEvent` types.

`Track` is here because a circuit event names its rows with them - `tracks`,
`target_track` and `controls` are all `Track`s - so a predicate that filters by
qubit reads its fields.

```{autodoc2-object} quil.plotting.circuit.PlottableCircuitEvent
render_plugin = "myst"
```

```{autodoc2-object} quil.plotting.circuit.Track
render_plugin = "myst"
```

```{autodoc2-object} quil.plotting.pulse.PlottablePulseEvent
render_plugin = "myst"
```

```{autodoc2-object} quil.plotting.pulse.PlottablePulse
render_plugin = "myst"
```

```{autodoc2-object} quil.plotting.pulse.PlottableFrameUpdate
render_plugin = "myst"
```

```{autodoc2-object} quil.plotting.pulse.PlottableRawCapture
render_plugin = "myst"
```
