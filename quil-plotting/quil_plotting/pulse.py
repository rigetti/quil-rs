"""
Schedule plotting module.
-------------------------

This module provides functions to plot the pulse schedule of a quil program.

TODO:
- Associate pulse with originating instructions
- Decide how the legend should work
- Handle runner ordering
- Qubits vs couplers
- Use quil waveform templates
"""

import json
from typing import Dict, List, Tuple
from quil.expression import PrefixExpression, PrefixOperator, Expression
from quil.program import CalibrationSet
from quil.instructions import Gate, Qubit
from pyquil.quiltwaveforms import DragGaussianWaveform, ErfSquareWaveform, GaussianWaveform, FlatWaveform
import numpy as np
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import plotly.io as pio
from plotly.graph_objs import Figure, Layout, Scatter
from plotly.graph_objs.layout.xaxis import Rangeslider

from itertools import cycle
from typing import Optional

from quil.program import Program, ScheduleSecondsItem
from quil.instructions import Pragma, Pulse, WaveformInvocation, Waveform, ShiftPhase
from quil.waveforms import DragGaussian, ErfSquare, Gaussian

# Teal, Yellow, Magneta, Blue, Gray, Dark Blue
colors = ["#00b5ad", "#ffc504", "#ef476f", "#3d47d9", "#8a8b92", "#0d0d36"]
colors_80 = ["#33c4bd", "#ffd136", "#f26c8c", "#646ce1", "#a1a2a8", "#3d3d5e"]
colors_60 = ["#66d3ce", "#ffdc68", "#f591a9", "#8b91e8", "#b9b9be", "#6e6e86"]
colors_40 = ["#99e1de", "#ffe89b", "#f9b5c5", "#b1b5f0", "#d0d1d3", "#9e9eaf"]
colors_20 = ["#ccf0ef", "#fff3cd", "#fcdae2", "#d8daf7", "#e8e8e9", "#cfcfd7"]


def plot_schedule(
    program: Program,
    runners: str = "Qubit",
    color_by: str = "Channel Type",
    runner_order: str = "Time (s)",
    exclude_readout: bool = True,
    normalize_by: Optional[str] = "Channel Type",
    label_by: Optional[str] = None,
    slider: bool = False,
) -> Figure:
    """
    Plot the pulse schedule of a quil program.

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
    df = program_to_dataframe(program)
    fig = plot_schedule_dataframe(
        df=df,
        runners=runners,
        color_by=color_by,
        runner_order=runner_order,
        exclude_readout=exclude_readout,
        normalize_by=normalize_by,
        label_by=label_by,
        slider=slider,
    )
    return fig


def expand_with_source_mapping(program: Program, include_pragmas: bool = False) -> Tuple[Program, List[int]]:
    """
    Expand any instructions in the program which have a matching calibration, leaving the others
    unchanged.

    :param program: A quil.Program.
    :return: A quil.Program with the instructions expanded and a source map.
    The source map is a list with the length of the expanded instructions, and indicates the index
    of the logical instruction in the original program from which the expanded instruction originated.
    """
    instructions = program.body_instructions
    calibrations = program.calibrations
    expanded_program = program.clone_without_body_instructions()

    source_map = []
    expanded_instructions = []
    for logical_index, instruction in enumerate(instructions):
        expanded_instruction = calibrations.expand(instruction, [])
        if include_pragmas is False:
            expanded_instruction = [inst for inst in expanded_instruction if not isinstance(inst.inner(), Pragma)]
        source_map += [logical_index] * len(expanded_instruction)
        expanded_instructions += expanded_instruction
    expanded_program.add_instructions(expanded_instructions)
    return expanded_program, source_map


def plot_schedule_dataframe(
    df,
    runners: str = "Qubit",
    color_by: str = "Channel Type",
    runner_order: str = "Time (s)",
    exclude_readout: bool = True,
    normalize_by: Optional[str] = "Channel Type",
    label_by: Optional[str] = None,
    slider: bool = False,
):
    """
    Plot the schedule data.
    """
    df = df.copy()
    if exclude_readout is True:
        # should use the instruction name being MEASURE
        df = df.loc[~df["Frame"].str.contains("readout")].reset_index()

    categoricals = [col for col in df.columns if df[col].dtype == "category"]
    assert df[runners].dtype == "category", f"{runners} is not a categorical field. Options include: {categoricals}"
    assert df[color_by].dtype == "category", f"{color_by} is not a categorical field. Options include: {categoricals}"
    if normalize_by is not None:
        assert (
            df[normalize_by].dtype == "category"
        ), f"{normalize_by} must be a categorical field. Options include: {categoricals}"

    # Determine the order of the runners
    if runner_order in df.columns:
        sorted_df = df.sort_values(["Time (s)"])
        runner_values = sorted_df[runners].drop_duplicates()
    elif runner_order == "Topological":
        # need the root instructions
        raise NotImplementedError()
    else:
        runner_values = df[runners].drop_duplicates()
    runner_map = {runner: idx for idx, runner in enumerate(runner_values)}
    df["Offset"] = df[runners].apply(lambda runner: runner_map.get(runner, 0))

    # Calculate colors
    color_cycle, color_cycle_fill = cycle(colors), cycle(colors_40)
    color_values = df[color_by].cat.categories
    color_map = {val: next(color_cycle) for val in color_values}
    color_fill_map = {val: next(color_cycle_fill) for val in color_values}
    df["Color"] = df[color_by].apply(lambda x: color_map.get(x, "#ffffff"))
    df["Color Faded"] = df[color_by].apply(lambda x: color_fill_map.get(x, "#ffffff"))

    # Normalize
    if normalize_by is None:
        df["Normalized IQ"] = df["IQ"]
    else:
        df["Magnitude"] = np.abs(df["IQ"])
        normalization_values = df.groupby(normalize_by, observed=True)["Magnitude"].transform("max")
        df["Normalized IQ"] = 0.5 * df["IQ"] / normalization_values

    # Label
    if label_by is None:
        label_by = runners

    instructions = df["Instruction Index"].drop_duplicates()
    traces = []
    shift_phase_traces = []
    legend_groups = set()
    for inst in instructions:
        pulse_df = df[df["Instruction Index"] == inst]
        ts = pulse_df["Time (s)"].to_numpy()
        I = np.real(pulse_df["Normalized IQ"].to_numpy())
        I_offset = I + pulse_df["Offset"].to_numpy()
        Q = np.imag(pulse_df["Normalized IQ"].to_numpy())
        Q_offset = Q + pulse_df["Offset"].to_numpy()

        # These values should be consistent within a pulse
        color = pulse_df["Color"].iloc[0]
        fill_color = pulse_df["Color Faded"].iloc[0]
        offset = pulse_df["Offset"].iloc[0]
        runner_value = pulse_df[runners].iloc[0]
        frame = pulse_df["Frame"].iloc[0]
        channel_type = pulse_df["Channel Type"].iloc[0]
        waveform = pulse_df["Waveform"].iloc[0]
        label = pulse_df[label_by].iloc[0]
        logical_instruction = pulse_df["Logical Instruction"].iloc[0]

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

        # Labels are by definition, trace level
        # So we will end up with a lot of them in the legend
        # However, legend group will not show up without one
        if label in legend_groups:
            showlegend = False
        else:
            showlegend = True
            legend_groups.add(label)

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
        if np.any(np.abs(Q)) > 1e-9:
            traces.append(trace_Q)

    traces = traces + shift_phase_traces
    layout = Layout(
        yaxis=dict(
            title=runners,
            range=[-0.25, -0.25 + len(runner_values)],
            tickmode="array",
            tickvals=[index for index in range(len(runner_values))],
            ticktext=[str(v) for v in runner_values],
            gridcolor="#8a8b92",
            zerolinecolor="#8a8b92",
        ),
        xaxis=dict(
            title="Time (s)",
            rangeslider=Rangeslider(visible=True) if slider else None,
        ),
        template="ggplot2",
        height=100*len(runner_values),
        width=1400,
    )
    fig = Figure(data=traces, layout=layout)
    return fig


def program_to_dataframe(program: Program) -> pd.DataFrame:
    """Expand the schedule into a datframe."""
    # Get the instructions and the schedule
    if hasattr(program, "_program"):
        quil_program = program._program
    elif isinstance(program, Program):
        quil_program = program
    else:
        raise ValueError("Program must be a pyquil.Program or a quil.Program")
    calibrations = quil_program.calibrations
    expanded_quil_program, source_map = expand_with_source_mapping(quil_program, include_pragmas=False)
    blocks = expanded_quil_program.control_flow_graph().basic_blocks()
    assert len(blocks) == 1
    block = blocks[0]
    instructions = block.instructions()
    schedule = block.as_schedule_seconds(expanded_quil_program).items()

    # Construct the dataframe
    dataframes = []
    hardware_parameters = {}

    def label_qubit(q: int, frame_reference) -> str:
        """
        Label a qubit as a computational qubit or a coupler based on the heuristics.
        """
        # An RX gate in the calibration is the clearest sign that the device is a qubit.
        gate = Gate("RX", [Expression.from_number(complex(np.pi / 2))], [Qubit(q)], [])
        if calibrations.get_match_for_gate(gate):
            return f"Qubit {q}"
        gate = Gate("RX", [Expression.from_number(complex(np.pi))], [Qubit(q)], [])
        if calibrations.get_match_for_gate(gate):
            return f"Qubit {q}"
        if "charge" in frame_reference.name:
            return f"Qubit {q}"

        return f"Coupler {q}"

    for schedule_item in schedule:
        start_time = schedule_item.time_span.start
        end_time = schedule_item.time_span.end
        duration = schedule_item.time_span.duration
        instruction_index = schedule_item.instruction_index
        instruction = instructions[instruction_index]
        logical_instruction = quil_program.body_instructions[source_map[instruction_index]]
        if isinstance((pulse := instruction.inner()), Pulse):
            frame_reference = pulse.frame
            frame = expanded_quil_program.frames.get(frame_reference)
            qubit = ",".join(label_qubit(q.inner(), frame_reference) for q in frame_reference.qubits)
            if frame_reference.name not in hardware_parameters:
                hardware_parameters[frame_reference.name] = json.loads(frame["HARDWARE-OBJECT"].as_string())

            ts, iqs = get_iqs(pulse=pulse, waveforms=expanded_quil_program.waveforms, frame=frame)
            instruction_df = pd.DataFrame(
                {
                    "Instruction Index": instruction_index,
                    "Instruction": instruction.to_quil(),
                    "Logical Instruction": logical_instruction.to_quil(),
                    "Qubit": qubit,
                    "Pulse": pulse.to_quil(),
                    "Waveform": pulse.waveform.name,
                    "Start Time (s)": start_time,
                    "End Time (s)": end_time,
                    "Duration (s)": duration,
                    "Pulse Time (s)": ts,
                    "Frame": frame_reference.name,
                    "Channel Type": hardware_parameters[frame_reference.name]["channel_type"],
                    "IQ": iqs,
                }
            )
            dataframes.append(instruction_df)
        elif isinstance(phase_shift := instruction.inner(), ShiftPhase):
            frame_reference = phase_shift.frame
            qubit = ",".join(label_qubit(q.inner(), frame_reference) for q in frame_reference.qubits)
            frame = expanded_quil_program.frames.get(frame_reference)
            if frame_reference.name not in hardware_parameters:
                hardware_parameters[frame_reference.name] = json.loads(frame["HARDWARE-OBJECT"].as_string())

            ts, iqs = [0], [0]
            instruction_df = pd.DataFrame(
                {
                    "Instruction Index": instruction_index,
                    "Instruction": instruction.to_quil(),
                    "Logical Instruction": logical_instruction.to_quil(),
                    "Qubit": qubit,
                    "Pulse": phase_shift.to_quil(),
                    "Waveform": "shift_phase",
                    "Start Time (s)": start_time,
                    "End Time (s)": end_time,
                    "Duration (s)": duration,
                    "Pulse Time (s)": ts,
                    "Frame": frame_reference.name,
                    "Channel Type": hardware_parameters[frame_reference.name]["channel_type"],
                    "IQ": iqs,
                }
            )
            dataframes.append(instruction_df)
    df = pd.concat(dataframes)
    df["Time (s)"] = df["Pulse Time (s)"] + df["Start Time (s)"]
    df = df.sort_values(["Instruction Index", "Time (s)"])
    df = df.astype(
        {
            "Instruction Index": int,
            "Instruction": "category",
            "Logical Instruction": "category",
            "Qubit": "category",
            "Pulse": "category",
            "Waveform": "category",
            "Start Time (s)": float,
            "End Time (s)": float,
            "Duration (s)": float,
            "Pulse Time (s)": float,
            "Frame": "category",
            "Channel Type": "category",
            "IQ": complex,
        }
    )
    return df


def deoxidize(num) -> complex:
    """Remove Rust from a Number or PrefixExpression."""
    num = num.inner()
    if isinstance(num, PrefixExpression):
        if num.operator == PrefixOperator.Plus:
            return num.expression.evaluate({}, {})
        elif num.operator == PrefixOperator.Minus:
            return -1*num.expression.evaluate({}, {})
        else:
            raise ValueError()
    return num


def get_iqs(pulse: Pulse, waveforms: Dict, frame):
    waveform_reference = pulse.waveform
    name = waveform_reference.name
    parameters = pulse.waveform.parameters
    sample_rate = frame["SAMPLE-RATE"].inner().to_real()
    match name:
        case "flat":
            waveform_envelope = FlatWaveform(**parameters)
            iqs = np.asarray(waveform_envelope.samples(sample_rate))
        case "drag_gaussian":
            waveform_envelope = DragGaussianWaveform(**parameters)
            iqs = np.asarray(waveform_envelope.samples(sample_rate))
        case "gaussian":
            waveform_envelope = GaussianWaveform(**parameters)
            iqs = np.asarray(waveform_envelope.samples(sample_rate))
        case "erf_square":
            waveform_envelope = ErfSquareWaveform(**parameters)
            iqs = np.asarray(waveform_envelope.samples(sample_rate))
        case _:
            if name in waveforms:
                iqs = np.array([deoxidize(val) for val in waveforms[name].matrix])
            else:
                raise ValueError(f"Waveform {name} not in waveforms")

    ts = np.arange(0, len(iqs)) / sample_rate
    return ts, iqs
