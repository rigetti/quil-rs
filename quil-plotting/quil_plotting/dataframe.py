"""dataframe module.
-----------------

This module produces a plottable dataframe with IQ values and metadata.

Todo:
- Handle runner ordering
- Use quil waveform templates
"""

import json
from enum import Enum
from itertools import cycle
from typing import Dict, List, Optional, Tuple, Union

import numpy as np
import pandas as pd
from pyquil.quiltwaveforms import DragGaussianWaveform, ErfSquareWaveform, FlatWaveform, GaussianWaveform
from quil.expression import Expression, PrefixExpression, PrefixOperator
from quil.instructions import Gate, Pragma, Pulse, Qubit, ShiftPhase
from quil.program import Program


# color
class Color(str, Enum):
    GREEN = "#00b5ad"  # Teal
    YELLOW = "#ffc504"  # Yellow
    RED = "#ef476f"  # Magneta
    BLUE = "#3d47d9"  # Palatinate Blue
    GRAY = "#8a8b92"  # Gray
    NAVY = "#0d0d36"  # Cetacean Blue


def hex_to_rgba(hex_color: str, opacity: float = 1.0) -> str:
    """Convert a hex color to an rgba string.

    >>> hex_to_rgba("#00B5AD")
    'rgba(0,181,173,1.0)'

    :param hex_color: The hex color code.
    :param opacity: The opacity for the rgba color.
    """
    hex_color = hex_color.strip("#")
    r, g, b = tuple(int(hex_color[i : i + 2], 16) for i in (0, 2, 4))
    return f"rgba({r},{g},{b},{opacity})"


def rgba_to_hex(rgba: Union[np.ndarray, List[int]]) -> str:
    """Convert an RGBA array to a hex color string.

    :param rgba: Array or list containing RGB components on [0-255]. The alpha component is optional.
    :return: Hex color string.
    """
    if len(rgba) == 3:
        return f"#{rgba[0]:02x}{rgba[1]:02x}{rgba[2]:02x}"
    elif len(rgba) == 4:
        return f"#{rgba[0]:02x}{rgba[1]:02x}{rgba[2]:02x}{rgba[3]:02x}"
    else:
        raise ValueError("RGBA array must have 3 or 4 elements")


def expand_with_source_mapping(program: Program, include_pragmas: bool = False) -> Tuple[Program, List[int]]:
    """Expand any instructions in the program which have a matching calibration, leaving the others
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
        if expanded_instruction is None:
            expanded_instruction = [instruction]
        if include_pragmas is False:
            expanded_instruction = [inst for inst in expanded_instruction if not isinstance(inst.inner(), Pragma)]
        source_map += [logical_index] * len(expanded_instruction)
        expanded_instructions += expanded_instruction
    expanded_program.add_instructions(expanded_instructions)
    return expanded_program, source_map


def add_plot_metadata(
    df: pd.DataFrame,
    runners: str = "Qubit",
    color_by: str = "Channel Type",
    runner_order: str = "Time (s)",
    exclude_readout: bool = True,
    normalize_by: Optional[str] = "Channel Type",
    label_by: Optional[str] = None,
):
    """Add the visualization metadata to the dataframe. Introduces the columns,
    "Runner":
    "Offset":
    "Color":
    "Color Faded":
    "Normalized IQ":
    "Label":

    :param df: The IQ dataframe.
    :param color_by:
    """
    df = df.copy()
    if exclude_readout is True:
        df = df.loc[~df["Frame"].str.contains("readout")].reset_index()

    # Ensure that discrete options refer to categorical columns
    categoricals = [col for col in df.columns if df[col].dtype == "category"]
    assert df[runners].dtype == "category", f"{runners} is not a categorical field. Options include: {categoricals}"
    assert df[color_by].dtype == "category", f"{color_by} is not a categorical field. Options include: {categoricals}"
    if normalize_by is not None:
        assert (
            df[normalize_by].dtype == "category"
        ), f"{normalize_by} is not a categorical field. Options include: {categoricals}"
    if label_by is not None:
        assert (
            df[label_by].dtype == "category"
        ), f"{label_by} is not a categorical field. Options include: {categoricals}"

    # Determine the order of the runners and calculate the y-offset for the IQ values.
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
    color_cycle = cycle(Color)
    color_values = df[color_by].cat.categories
    # pre-defined colors
    color_map = {"ISWAP": Color.YELLOW, "RX": Color.GREEN, "CZ": Color.BLUE}
    # add additional colors as needed
    color_map |= {val: next(color_cycle) for val in color_values if val not in color_map}
    df["Color"] = df[color_by].apply(lambda x: hex_to_rgba(color_map.get(x, "#ffffff")))
    df["Color Faded"] = df[color_by].apply(lambda x: hex_to_rgba(color_map.get(x, "#ffffff"), 0.3))

    # Normalize
    if normalize_by is None:
        df["Normalized IQ"] = df["IQ"]
    else:
        df["Magnitude"] = np.abs(df["IQ"])
        normalization_values = df.groupby(normalize_by, observed=True)["Magnitude"].transform("max")
        df["Normalized IQ"] = 0.45 * df["IQ"] / normalization_values

    # Label
    if label_by is None:
        label_by = runners

    df["Label"] = df[label_by]
    df["Runner"] = df[runners]

    df = df.astype(
        {
            "Instruction Index": int,
            "Instruction": "category",
            "Logical Instruction": "category",
            "Gate": "category",
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
            "Offset": float,
            "Color": "category",
            "Color Faded": "category",
            "Normalized IQ": complex,
            "Label": "category",
            "Runner": "category",
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
            return -1 * num.expression.evaluate({}, {})
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


def program_to_dataframe(
    program: Program,
) -> pd.DataFrame:
    """Expand the schedule into a dataframe of IQ values and metadata.

    :param program:
    """
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
        """Label a qubit as a computational qubit or a coupler based on the heuristics."""
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

        # Get the IQs
        if isinstance((pulse := instruction.inner()), Pulse):
            frame_reference = pulse.frame
            frame = expanded_quil_program.frames.get(frame_reference)
            qubit = ",".join(label_qubit(q.inner(), frame_reference) for q in frame_reference.qubits)
            if frame_reference.name not in hardware_parameters:
                hardware_parameters[frame_reference.name] = json.loads(frame["HARDWARE-OBJECT"].as_string())

            ts, iqs = get_iqs(pulse=pulse, waveforms=expanded_quil_program.waveforms, frame=frame)
            waveform = pulse.waveform.name

        elif isinstance(pulse := instruction.inner(), ShiftPhase):
            frame_reference = pulse.frame
            qubit = ",".join(label_qubit(q.inner(), frame_reference) for q in frame_reference.qubits)
            frame = expanded_quil_program.frames.get(frame_reference)
            if frame_reference.name not in hardware_parameters:
                hardware_parameters[frame_reference.name] = json.loads(frame["HARDWARE-OBJECT"].as_string())

            ts, iqs = [0], [0]
            waveform = "shift_phase"

        else:
            continue

        instruction_df = pd.DataFrame(
            {
                "Instruction Index": instruction_index,
                "Instruction": instruction.to_quil(),
                "Logical Instruction": logical_instruction.to_quil(),
                "Gate": logical_instruction.inner().name if isinstance(logical_instruction.inner(), Gate) else "",
                "Qubit": qubit,
                "Pulse": pulse.to_quil(),
                "Waveform": waveform,
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
            "Gate": "category",
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
