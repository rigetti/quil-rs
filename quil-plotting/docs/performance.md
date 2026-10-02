# Improving performance

A pulse schedule of a long program can hold tens of thousands of pulses and
frame updates. Each one becomes a record in the chart, and the browser must
process every record before it shows anything. Worse, each pan or zoom then
redraws every visible pulse. There are some levers you can pull to improve
rendering performance if you experience a laggy plot.

## Draw less

Hiding events is the biggest win by far. A hidden event is left out of the
chart entirely, so it costs nothing in file size, load time, or redraw time.

Hide whole groups by name. A string matches a lane, frame, channel type, or
the gate an event came from:

```python
schedule.hide("Qubit: 12").hide("RZ")
```

Or, if you only care about one qubit or group:

```python
schedule.hide(lambda event: event.qubit != "Qubit: 12")
```

Focus on a time window with a predicate. Every event carries a `start_time` in
seconds:

```python
schedule.hide(lambda event: event.start_time > 1.6e-3)
```

Frame updates are often a third or more of a schedule's records. If you only
care about the waveforms, drop them:

```python
schedule.with_frame_updates_hidden()
```

`with_frame_updates_shown()` brings them all back.

## Smooth the pulses

By default each sample is held flat for one sample period, the staircase the
hardware actually plays. Each step costs two path segments. Smoothing joins
samples with straight lines instead, which cuts redraw time by about 15% at
the cost of hardware accuracy:

```python
schedule.with_smooth_pulses()
```

## Cap the samples per pulse

Runs of constant samples are always compressed to their endpoints, so a long
flat readout pulse is cheap. A long pulse whose samples keep changing is not,
as every sample is drawn. Capping the sample count removes intermediate samples,
but does so intellgently to minimize the effect to the overall shape.

```python
schedule.with_smooth_pulses().with_max_points_per_pulse(500)
```

Pair a cap with smoothing. A capped stepped pulse holds each kept sample
across the dropped ones after it, which looks blocky.

## Export a static figure

If you do not need to pan, zoom, or hover, save an image instead. The
rendering happens once, when the file is written, and the result opens
instantly:

```python
schedule.hide(lambda event: event.start_time > 1.6e-3).draw("schedule.png")
```

Prefer `.png`. An `.svg` or `.pdf` keeps a vector path per pulse, so it can
outweigh the interactive chart.

Static export renders in an embedded JavaScript engine with a fixed memory
limit, and a full schedule of the size above exceeds it and crashes. Hide
what you can first.
