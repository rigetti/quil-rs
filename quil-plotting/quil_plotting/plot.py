"""dataframe module.
-----------------

This module produces functions to visualize a schedule dataframe.

Todo:
- Handle runner ordering
"""

from typing import Optional

import numpy as np
import pandas as pd
from plotly.graph_objs import Figure, Layout, Scatter
from plotly.graph_objs.layout.xaxis import Rangeslider
from quil.program import Program

from .dataframe import Color, add_plot_metadata, program_to_dataframe


def plot_schedule(
    program: Program,
    runners: str = "Qubit",
    color_by: str = "Gate",
    runner_order: str = "Time (s)",
    exclude_readout: bool = True,
    normalize_by: Optional[str] = "Frame",
    label_by: Optional[str] = "Gate",
    slider: Optional[bool] = None,
) -> Figure:
    """Plot the pulse schedule of a quil program.

    :param program: The quil program to plot.
    :param runners: The property to use for the y-axis.
    :param color_by: The property to use for the color of the traces.
    :param runner_order: The property to use for ordering the runners.
    :param exclude_readout: Exclude readout pulses.
    :param normalize_by: Normalize the IQ values by a property.
    :return: A plotly figure.

    :example:
    >>> program = Program()
    >>> program += RX(np.pi / 2, 0)
    >>> fig = plot_schedule(program)
    >>> fig.show()
    """
    # All strings are in the `title` format
    color_by = color_by.title()
    runner_order = runner_order.title()
    if normalize_by is not None:
        normalize_by = normalize_by.title()
    if label_by is not None:
        label_by = label_by.title()

    # Construc the dataframes
    df = program_to_dataframe(program)
    df = add_plot_metadata(
        df,
        runners=runners,
        color_by=color_by,
        runner_order=runner_order,
        exclude_readout=exclude_readout,
        normalize_by=normalize_by,
        label_by=label_by,
    )

    # Produce the plot
    fig = plot_schedule_dataframe(df=df, slider=slider)
    return fig


def plot_schedule_dataframe(
    df,
    slider: Optional[bool] = None,
):
    """Plot the schedule data.

    :param df: The schedule dataframe, with the plotting metadata.
    Schedule Columns:
    'Instruction Index': int, 'Instruction': str, 'Logical Instruction': str, 'Gate': str,
    'Qubit': int, 'Pulse': str, 'Waveform': str, 'Start Time (s)': float, 'End Time (s)': float,
    'Duration (s)': float, 'Pulse Time (s)': float, 'Frame': str, 'Channel Type': str, 'IQ': complex,
    'Time (s)': float

    Plotting columns:
    'Offset': float, 'Color': str, 'Color Faded': str, 'Magnitude': float,
    'Normalized IQ': float, 'Label': str, 'Runner': str

    :param slider: Show the slider. If None, the slider will be added for schedules greater than 1.5us.
    """
    # The runner values are needed for the y-axis
    runner_df = df[["Runner", "Offset"]].drop_duplicates().sort_values("Offset", ascending=True)
    runner_values = list(runner_df["Runner"])
    runner_offsets = list(runner_df["Offset"])
    num_runners = len(runner_values)

    # Get the list of unique instructions
    # We don't care about the order
    instructions = df["Instruction Index"].drop_duplicates()

    # Trace contains the IQ pulses
    traces = []
    # Shift Phase traces are separate. They will be placed on top
    shift_phase_traces = []
    # Legend groups us used to control what appears in the legend
    legend_groups = set()
    for inst in instructions:
        # For each instruction downselect the data and get the IQ values
        pulse_df = df[df["Instruction Index"] == inst]
        ts = pulse_df["Time (s)"].to_numpy()
        I = np.real(pulse_df["Normalized IQ"].to_numpy())
        I_offset = I + pulse_df["Offset"].to_numpy()
        Q = np.imag(pulse_df["Normalized IQ"].to_numpy())
        Q_offset = Q + pulse_df["Offset"].to_numpy()

        # These metadata values are expected to be all the same within a pulse
        # We won't check that, however
        color = pulse_df["Color"].iloc[0]
        fill_color = pulse_df["Color Faded"].iloc[0]
        offset = pulse_df["Offset"].iloc[0]
        frame = pulse_df["Frame"].iloc[0]
        channel_type = pulse_df["Channel Type"].iloc[0]
        waveform = pulse_df["Waveform"].iloc[0]
        label = pulse_df["Label"].iloc[0]

        # Special cases: SHIFT-PHASE
        if waveform == "shift_phase":
            trace = Scatter(
                x=ts,
                y=[offset],
                mode="text",
                text=["↺"],
                name=label,
                hovertemplate="<br><b>Time</b>: %{x}s<br>"
                + f"<b>Frame</b>: {frame}<br><b>Channel Type</b>: {channel_type}",
                showlegend=False,
                legendgrouptitle_text="",
                legendgroup=label,
            )
            shift_phase_traces.append(trace)
            continue

        # Labels define the legend entries of the plot based on the `name` of the trace
        # Since we use a large number of traces here, the number of legend entries would be unwieldy
        # The solution is to group the traces by `label` in a legendgroup
        # The legendgroup only appears if at least one of the traces has showlegend=True
        # So we allow the first trace_I in a legendgroup to show, and hide all the others
        if label in legend_groups:
            showlegend = False
        else:
            showlegend = True
            legend_groups.add(label)

        # This trace is not seen, but provides the baseline for the fill
        trace = Scatter(
            x=[np.min(ts), np.max(ts)],
            y=np.ones(2) * offset,
            mode="lines",
            line=dict(color=color, width=0),
            name=label,
            showlegend=False,
            legendgrouptitle_text="",
            legendgroup=label,
        )
        # I (real) values
        trace_I = Scatter(
            x=ts,
            y=I_offset,
            mode="lines",
            fill="tonexty",
            line=dict(color=color, width=2),
            fillcolor=fill_color,
            name=label,
            customdata=pd.DataFrame({"I": I, "Q": Q}),
            hovertemplate="<br><b>Time</b>: %{x}s<br><b>I</b>: %{customdata[0]:.3f} + i%{customdata[1]:.3f}<br>"
            + f"<b>Frame</b>: {frame}<br><b>Channel Type</b>: {channel_type}",
            showlegend=showlegend,
            legendgrouptitle_text="",
            legendgroup=label,
        )
        # Q (imaginary) values
        trace_Q = Scatter(
            x=ts,
            y=Q_offset,
            mode="lines",
            fill="tonexty",
            line=dict(color=color, width=2),
            fillcolor=fill_color,
            name=label,
            hoverinfo="skip",
            showlegend=False,
            legendgrouptitle_text="",
            legendgroup=label,
        )
        traces.append(trace)
        traces.append(trace_I)
        # Only add the imaginary trace if some of the values are non-zero
        if np.any(np.abs(Q)) > 1e-9:
            traces.append(trace_Q)

    # Plotly places the final trace on top - so shift-phase traces go last
    traces = traces + shift_phase_traces

    # x-axis range and slider logic
    # set a default value of 1.5ns per pixel
    # this might change in the future depending on gate times
    # if the slider argument is unspecified, check the total duration
    # of the program. If larger than 1500ns (1000px), add a slider
    # and set the x-range to (0, 1500ns)
    # otherwise, we'll leave it alone and let the x-range be dynamic
    # this provides reasonable default. Consumer can change the plot
    # width afterwards with `update_layout()`
    max_time_ns = df["Time (s)"].max() * 1e9
    if np.isnan(max_time_ns):
        max_time_ns = 1000

    # Set the width and height
    # The height is 100 pixels per runner, plus 200 for margins
    height = 200 + 100 * len(runner_values)

    # The width is 200 pixels for margin plus 1.5ns per pixel, up to a maximum of 1200 pixels
    width = 200 + int(max_time_ns / 1.5)
    width = np.clip(width, 500, 1200)
    x_range = None
    if slider is None:
        if max_time_ns > width:
            slider = True
            x_range = [0, 1500e-9]
        else:
            slider = False

    layout = Layout(
        yaxis=dict(
            range=[-0.25, -0.25 + num_runners],
            tickmode="array",
            tickvals=runner_offsets,
            ticktext=[str(v) for v in runner_values],
            gridcolor=Color.GRAY,
            zerolinecolor=Color.GRAY,
        ),
        xaxis=dict(
            title="Time (s)",
            rangeslider=Rangeslider(visible=True) if slider else None,
            range=x_range,
        ),
        template="ggplot2",
        height=height,
        width=width,
    )
    fig = Figure(data=traces, layout=layout)
    return fig
