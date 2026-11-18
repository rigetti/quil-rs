"""altair_plot module.
-------------------

This module provides an Altair (Vega-Lite) implementation of schedule visualization, as an
alternative to the plotly implementation in `plot.py`. Both render the same schedule dataframe.

Altair is useful where a static export is needed without a browser: `chart.save("schedule.svg")`
goes through `vl-convert`, which renders the chart in-process, whereas plotly's static export
requires an installed copy of Chrome. The charts remain interactive when displayed or saved as
HTML, supporting zoom, pan, an interactive legend, and hover.
"""

import json
from typing import Optional, Tuple

import altair as alt
import numpy as np
import pandas as pd
from quil.program import Program

from .dataframe import Color, add_plot_metadata, program_to_dataframe

# Below this, a pulse's imaginary component is treated as absent and its trace is not drawn.
Q_TOLERANCE = 1e-9

# The fraction of a runner's row that a full-scale pulse occupies. `add_plot_metadata` normalizes
# the IQ values to +/- 0.45, so a pulse spans at most 90% of the gap between two runners.
FILL_OPACITY = 0.3
FADED_OPACITY = 0.05

# Times are rounded to a tenth of a picosecond and normalized IQ values to six decimals; both are
# far finer than a plot can resolve, and rounding keeps the serialized chart small.
TIME_DECIMALS = 13
VALUE_DECIMALS = 6

# The time axis is scaled at 1.5 ns of schedule per pixel, as the plotly backend does, and then held
# between these bounds so that a single short pulse is not stretched across the full width and a long
# program does not run off the page. These are plot-area widths: Altair lays the axis labels and the
# legend out around the plot area, where plotly's figure width has to cover them, so they are the
# plotly bounds less its 200 pixels of margin.
NS_PER_PIXEL = 1.5
MIN_PLOT_WIDTH = 300
MAX_PLOT_WIDTH = 1000


def plot_schedule_altair(
    program: Program,
    runners: str = "Qubit",
    color_by: str = "Operation",
    runner_order: str = "Time (s)",
    exclude_readout: bool = False,
    normalize_by: Optional[str] = "Frame",
    label_by: Optional[str] = "Operation",
    width: Optional[int] = None,
    height_per_runner: int = 100,
) -> alt.LayerChart:
    """Plot the pulse schedule of a quil program with Altair.

    :param program: The quil program to plot.
    :param runners: The property to use for the y-axis.
    :param color_by: The property to use for the color of the traces.
    :param runner_order: The property to use for ordering the runners.
    :param exclude_readout: Exclude readout pulses.
    :param normalize_by: Normalize the IQ values by a property.
    :param label_by: The property to use for the legend entries.
    :param width: The width of the plot area, in pixels. Defaults to a width that keeps the time
    axis at a readable scale; see `schedule_width`.
    :param height_per_runner: The vertical space given to each runner, in pixels.
    :return: An Altair chart.

    :example:
    >>> program = Program()
    >>> program += RX(np.pi / 2, 0)
    >>> chart = plot_schedule_altair(program)
    >>> chart.save("schedule.svg")
    """
    df = program_to_dataframe(program)

    def match_column(column: str) -> str:
        """Resolve a column name against the dataframe, ignoring case."""
        return next((known for known in df.columns if known.lower() == column.lower()), column)

    runners = match_column(runners)
    color_by = match_column(color_by)
    runner_order = match_column(runner_order)
    if normalize_by is not None:
        normalize_by = match_column(normalize_by)
    if label_by is not None:
        label_by = match_column(label_by)

    df = add_plot_metadata(
        df,
        runners=runners,
        color_by=color_by,
        runner_order=runner_order,
        exclude_readout=exclude_readout,
        normalize_by=normalize_by,
        label_by=label_by,
    )

    return plot_schedule_dataframe_altair(df=df, width=width, height_per_runner=height_per_runner)


def plot_schedule_dataframe_altair(
    df: pd.DataFrame,
    width: Optional[int] = None,
    height_per_runner: int = 100,
) -> alt.LayerChart:
    """Plot the schedule data with Altair.

    :param df: The schedule dataframe, with the plotting metadata added by `add_plot_metadata`.
    See `plot_schedule_dataframe` for the columns this expects.
    :param width: The width of the plot area, in pixels. Defaults to a width that keeps the time
    axis at a readable scale; see `schedule_width`.
    :param height_per_runner: The vertical space given to each runner, in pixels.
    :return: An Altair chart.
    """
    if width is None:
        width = schedule_width(df)

    # The runner values are needed for the y-axis
    runner_df = df[["Runner", "Offset"]].drop_duplicates().sort_values("Offset", ascending=True)
    runner_offsets = [float(offset) for offset in runner_df["Offset"]]
    runner_names = [str(runner) for runner in runner_df["Runner"]]

    # Legend entries take their color from the `Color` assigned to their rows. Where a label spans
    # several colors - possible when `color_by` and `label_by` differ - the first one wins.
    palette = df[["Label", "Color"]].astype(str).drop_duplicates(subset="Label")

    pulses = df[df["Waveform"] != "shift_phase"]
    shift_phases = df[df["Waveform"] == "shift_phase"]

    samples, pulse_meta = _pulse_records(pulses)
    legend_selection = alt.selection_point(fields=["label"], bind="legend")
    color = alt.Color(
        "label:N",
        title=None,
        scale=alt.Scale(domain=list(palette["Label"]), range=list(palette["Color"])),
    )
    # Dim rather than hide the traces that are deselected in the legend, so that the schedule keeps
    # its shape while a single operation is picked out.
    opacity = alt.condition(legend_selection, alt.value(1.0), alt.value(FADED_OPACITY))
    area_opacity = alt.condition(legend_selection, alt.value(FILL_OPACITY), alt.value(FADED_OPACITY))

    # Schedules run on nanosecond timescales, so label the axis with SI prefixes ("20n") rather
    # than with the long decimals that a plain float format would give.
    time_axis = alt.Axis(format="~s")
    # Left dataless: the samples are attached once to the enclosing layer below, so that they are
    # not serialized again for every mark that draws them.
    base = alt.Chart().encode(
        x=alt.X("t:Q", title="Time (s)", axis=time_axis, scale=alt.Scale(zero=False, nice=False)),
        color=color,
        detail="p:N",
    )
    tooltip = [
        alt.Tooltip("t:Q", title="Time (s)", format=".3e"),
        alt.Tooltip("i:Q", title="I", format=".4f"),
        alt.Tooltip("q:Q", title="Q", format=".4f"),
        alt.Tooltip("operation:N", title="Operation"),
        alt.Tooltip("frame:N", title="Frame"),
        alt.Tooltip("channel:N", title="Channel Type"),
    ]

    y_axis = alt.Axis(
        values=runner_offsets,
        # Map each runner's offset back to its name. Offsets are consecutive integers from zero, so
        # they index the array of names directly.
        labelExpr=f"{json.dumps(runner_names)}[datum.value]",
        title=None,
        grid=True,
    )
    y_scale = alt.Scale(domain=[-0.25, -0.25 + len(runner_names)], nice=False)

    # One filled mark per component, outlined with `line=True` rather than by a second mark, which
    # halves the number of groups Vega has to lay out.
    pulse_layers = []
    for component, has_component in (("yi", None), ("yq", "hasq")):
        area = base.mark_area(interpolate="linear", line=True, strokeWidth=1.5).encode(
            y=alt.Y(f"{component}:Q", title=None, axis=y_axis, scale=y_scale),
            y2="b:Q",
            fillOpacity=area_opacity,
            strokeOpacity=opacity,
            tooltip=tooltip,
        )
        if has_component is not None:
            area = area.transform_filter(alt.datum[has_component])
        pulse_layers.append(area)

    # The samples are attached once, here, and shared by the marks that draw them.
    layers = [
        alt.layer(*pulse_layers, data=alt.Data(values=samples))
        .transform_lookup(
            lookup="p",
            from_=alt.LookupData(
                data=alt.Data(values=pulse_meta),
                key="p",
                fields=["b", "label", "operation", "frame", "channel", "hasq"],
            ),
        )
        # The offset that separates the runners is held per pulse, so apply it here rather than
        # baking it into every sample.
        .transform_calculate(yi="datum.i + datum.b", yq="datum.q + datum.b")
    ]

    if not shift_phases.empty:
        # The plotly implementation marks a phase shift with a `↺`. Here the chart may be rendered
        # without a browser, by a font stack that cannot be relied on to carry that glyph, so use a
        # shape instead of a character.
        layers.append(
            alt.Chart(alt.Data(values=_shift_phase_records(shift_phases)))
            # `.value`, not `str()`: `Color` mixes in `str`, and since Python 3.11 `str()` on such
            # an enum returns "Color.NAVY" rather than the colour it holds.
            .mark_point(shape="diamond", size=45, filled=True, color=Color.NAVY.value, opacity=1.0)
            .encode(
                x=alt.X("t:Q", title="Time (s)", axis=time_axis, scale=alt.Scale(zero=False, nice=False)),
                y=alt.Y("baseline:Q", title=None, axis=y_axis, scale=y_scale),
                tooltip=[
                    alt.Tooltip("t:Q", title="Time (s)", format=".3e"),
                    alt.Tooltip("frame:N", title="Frame"),
                    alt.Tooltip("channel:N", title="Channel Type"),
                ],
            )
        )

    return (
        alt.layer(*layers)
        .add_params(legend_selection)
        .properties(width=width, height=height_per_runner * max(len(runner_names), 1))
        .interactive()
    )


def schedule_width(df: pd.DataFrame) -> int:
    """Choose a plot-area width that keeps the time axis at a readable scale.

    A schedule holding a single 40 ns pulse and one running for tens of microseconds should not be
    drawn at the same width, or the short one is stretched until its pulse is a smear.

    :param df: The schedule dataframe.
    :return: The width of the plot area, in pixels.
    """
    max_time_ns = df["Time (s)"].max() * 1e9
    if np.isnan(max_time_ns):
        max_time_ns = 1000.0
    return int(np.clip(max_time_ns / NS_PER_PIXEL, MIN_PLOT_WIDTH, MAX_PLOT_WIDTH))


def _pulse_records(pulses: pd.DataFrame) -> Tuple[list, list]:
    """Shape the pulse rows into the records the chart is built from.

    The samples are split from the per-pulse metadata, which the chart joins back on with a lookup
    transform. A schedule has orders of magnitude more samples than pulses, and the metadata is
    constant along a pulse, so repeating it on every sample would dominate the size of the chart.

    :param pulses: The rows of the schedule dataframe which describe pulses.
    :return: The per-sample records and the per-pulse records.
    """
    if pulses.empty:
        return [], []

    iq = pulses["Normalized IQ"].to_numpy()
    i, q = np.real(iq), np.imag(iq)
    pulse_index = pulses["Instruction Index"].to_numpy()

    samples = pd.DataFrame(
        {
            # The values only have to survive being drawn, so round away the digits that no plot
            # can resolve rather than serializing full double precision for every sample.
            "t": np.round(pulses["Time (s)"].to_numpy(), TIME_DECIMALS),
            "i": np.round(i, VALUE_DECIMALS),
            "q": np.round(q, VALUE_DECIMALS),
            "p": pulse_index,
        }
    ).to_dict("records")

    # A pulse's imaginary trace is drawn only when that pulse has a non-zero component anywhere
    # along it, matching the plotly implementation.
    has_q = pd.Series(np.abs(q) > Q_TOLERANCE, index=pulses.index).groupby(pulse_index, observed=True).any()
    meta = (
        pd.DataFrame(
            {
                "p": pulse_index,
                "b": pulses["Offset"].to_numpy(),
                "label": pulses["Label"].astype(str).to_numpy(),
                "operation": pulses["Operation"].astype(str).to_numpy(),
                "frame": pulses["Frame"].astype(str).to_numpy(),
                "channel": pulses["Channel Type"].astype(str).to_numpy(),
            }
        )
        .drop_duplicates(subset="p")
        .assign(hasq=lambda frame: frame["p"].map(has_q))
    )
    return samples, meta.to_dict("records")


def _shift_phase_records(shift_phases: pd.DataFrame) -> list:
    """Shape the `SHIFT-PHASE` rows into records marking where each phase shift occurs.

    :param shift_phases: The rows of the schedule dataframe which describe phase shifts.
    """
    return pd.DataFrame(
        {
            "t": shift_phases["Time (s)"].to_numpy(),
            "baseline": shift_phases["Offset"].to_numpy(),
            "frame": shift_phases["Frame"].astype(str).to_numpy(),
            "channel": shift_phases["Channel Type"].astype(str).to_numpy(),
        }
    ).to_dict("records")
