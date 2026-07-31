"""dataframe module.
-----------------

This module produces a plottable dataframe with IQ values and metadata.

Todo:
- Handle runner ordering
"""

import json
from enum import Enum
from itertools import cycle
from typing import Dict, List, Optional, Set, Tuple, Union

import numpy as np
import pandas as pd
from quil.expression import Expression
from quil.instructions import Instruction, Pulse, Qubit
from quil.program import FrameSet, InstructionTarget, Program
from quil.waveform import Waveform

# Frames which only a computational qubit owns; anything else is taken to be a coupler.
QUBIT_FRAME_MARKERS = ("charge", "readout")


# color
class Color(str, Enum):
    GREEN = "#00b5ad"  # Teal
    YELLOW = "#ffc504"  # Yellow
    RED = "#ef476f"  # Magneta
    BLUE = "#3d47d9"  # Palatinate Blue
    GRAY = "#8a8b92"  # Gray
    NAVY = "#0d0d36"  # Cetacean Blue


class OperationType(str, Enum):
    """The kind of operation a pulse belongs to, which fixes the colour it is drawn in."""

    ONE_QUBIT_GATE = "1Q Gate"
    TWO_QUBIT_GATE = "2Q Gate"
    MEASUREMENT = "Measurement"
    RESET = "Reset"
    OTHER = "Other"


# Every operation of a kind is drawn in one colour, so that a schedule reads the same way from one
# program to the next.
OPERATION_TYPE_COLORS = {
    OperationType.ONE_QUBIT_GATE: Color.GREEN,
    OperationType.TWO_QUBIT_GATE: Color.YELLOW,
    OperationType.MEASUREMENT: Color.BLUE,
    OperationType.RESET: Color.RED,
    OperationType.OTHER: Color.GRAY,
}

# Operations are classified by the first token of their name, so that a calibration named for a
# cycle - `CZ_0`, `SX_DATA`, `MEASURE_ANCILLA` - is classified by the gate it implements.
OPERATION_NAME_TYPES = {
    OperationType.RESET: {"RESET"},
    OperationType.MEASUREMENT: {"MEASURE"},
    OperationType.TWO_QUBIT_GATE: {"CZ", "CPHASE", "CNOT", "CX", "CCNOT", "CCX", "ISWAP", "SWAP", "XY"},
    OperationType.ONE_QUBIT_GATE: {"H", "I", "PHASE", "RX", "RY", "RZ", "S", "SX", "SY", "SZ", "T", "U", "X", "Y", "Z"},
}

# Where the name is not recognised, the frame the pulse plays on is the next best signal: a qubit is
# driven through its charge line, coupled through flux, and measured through its resonator.
FRAME_KIND_TYPES = {
    "charge": OperationType.ONE_QUBIT_GATE,
    "flux": OperationType.TWO_QUBIT_GATE,
    "readout": OperationType.MEASUREMENT,
}


def operation_name(instruction: Instruction) -> str:
    """Name the operation that an expanded pulse came from.

    A gate carries its own name, but `MEASURE` and `RESET` are instructions in their own right
    rather than gates, so name them after the instruction. Anything else is left unnamed, and
    `classify_operation` falls back to the frame to work out what it is.

    :param instruction: The logical instruction the pulse was expanded from.
    :return: The name of the operation, or an empty string if it has none.
    """
    if isinstance(instruction, Instruction.Gate):
        return instruction._0.name
    if isinstance(instruction, Instruction.Measurement):
        return "MEASURE"
    if isinstance(instruction, Instruction.Reset):
        return "RESET"
    return ""


def classify_operation(operation: str, frame_name: str) -> str:
    """Classify the operation a pulse belongs to.

    The operation's name is the better signal, because it says what was intended: a reset driven
    through a charge line is indistinguishable from a single-qubit gate by frame alone. The frame is
    the fallback for names this does not know, which also covers the pulses of a `MEASURE`, an
    instruction that carries no gate name at all.

    :param operation: The name of the operation the pulse was expanded from, if it has one.
    :param frame_name: The name of the frame the pulse plays on.
    :return: The value of the `OperationType` the pulse belongs to. The value rather than the member
    itself, because `str` of an enum which mixes in `str` gives "OperationType.RESET" and not the
    name the column is meant to carry.
    """
    token = operation.split("_")[0].upper()
    for operation_type, names in OPERATION_NAME_TYPES.items():
        if token in names:
            return operation_type.value

    for marker, operation_type in FRAME_KIND_TYPES.items():
        if marker in frame_name:
            return operation_type.value

    return OperationType.OTHER.value


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
    :param include_pragmas: Keep `PRAGMA`s in the expanded program. They carry no duration, so a
    program containing them cannot be scheduled.
    :return: A quil.Program with the instructions expanded and a source map.
    The source map is a list with the length of the expanded instructions, and indicates the index
    of the logical instruction in the original program from which the expanded instruction originated.
    """
    expanded_program, expansion = program.expand_calibrations_with_source_map()

    # `expansion` maps each source instruction to the *range* of expanded instructions it produced.
    # Invert it in a single pass; querying it per instruction with `list_sources_for_target_index`
    # would be quadratic in the length of the program.
    source_map = [0] * len(expanded_program.body_instructions)
    for entry in expansion.entries():
        target = entry.target_location()
        if isinstance(target, InstructionTarget.Unmodified):
            target_range = range(target._0, target._0 + 1)
        else:
            target_range = target._0.range
        for target_index in target_range:
            source_map[target_index] = entry.source_location()

    if include_pragmas is False:
        kept = [
            (instruction, source_index)
            for instruction, source_index in zip(expanded_program.body_instructions, source_map, strict=True)
            if not isinstance(instruction, Instruction.Pragma)
        ]
        filtered_program = expanded_program.clone_without_body_instructions()
        filtered_program.add_instructions([instruction for instruction, _ in kept])
        expanded_program = filtered_program
        source_map = [source_index for _, source_index in kept]

    # `add_instructions` routes some instructions (`DECLARE`, for one) out of the body and into the
    # program's other fields, which would silently shift the source map out of alignment and
    # mislabel every instruction after the first one absorbed.
    if len(source_map) != len(expanded_program.body_instructions):
        raise ValueError(
            f"Source map has {len(source_map)} entries for "
            f"{len(expanded_program.body_instructions)} expanded instructions."
        )
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
        df = df.loc[~df["Frame"].str.contains("readout")].reset_index(drop=True)

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
        # Order the runners by where they first appear along the ordering column. Sorting the rows
        # and taking the first of each runner would instead depend on how ties are broken between
        # the many rows that share a timestamp, which makes the y-axis order vary with the number
        # of rows per runner rather than with the schedule.
        first_appearance = df.groupby(runners, observed=True)[runner_order].min()
        runner_values = list(first_appearance.sort_values(kind="stable").index)
    elif runner_order == "Topological":
        # need the root instructions
        raise NotImplementedError()
    else:
        runner_values = list(df[runners].drop_duplicates())
    runner_map = {runner: idx for idx, runner in enumerate(runner_values)}
    df["Offset"] = df[runners].apply(lambda runner: runner_map.get(runner, 0))

    # Calculate colors. Each value of `color_by` takes the colour of the operation type it belongs
    # to, so that every 2Q gate is drawn alike whatever its calibration happens to be called.
    # Grouping by something which is not aligned to operations spans several types - a charge frame
    # carries both its qubit's 1Q gates and the phase corrections a 2Q gate applies to it - and such
    # a value has no one type to take a colour from, so it cycles the palette instead.
    color_cycle = cycle(Color)
    types_by_value = df.groupby(color_by, observed=True)["Operation Type"].unique().to_dict()
    color_map = {}
    for value in df[color_by].cat.categories:
        types = types_by_value.get(value, ())
        color_map[value] = OPERATION_TYPE_COLORS[types[0]] if len(types) == 1 else next(color_cycle)
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
            "Operation": "category",
            "Operation Type": "category",
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


def qubit_index(qubit: Qubit) -> int:
    """Get the index of a fixed qubit.

    :param qubit: A `quil.instructions.Qubit`, which must be a fixed (concrete) qubit.
    """
    if not isinstance(qubit, Qubit.Fixed):
        raise ValueError(f"Expected a fixed qubit, got {qubit.to_quil()}")
    return qubit._0


def qubit_frame_indices(frames: FrameSet) -> Set[int]:
    """Find the device indices which are computational qubits rather than couplers.

    A computational qubit is driven through a charge line and read out through a resonator, so it
    owns a `charge` or `readout` frame; a coupler only ever carries flux. This is read from the
    program's frame definitions rather than from the frame a given pulse plays on, so that every
    pulse belonging to a qubit - its readout included - is labelled with that qubit rather than
    being split onto a row of its own.

    :param frames: The frames defined by the program.
    :return: The indices which name a qubit.
    """
    return {
        qubit._0
        for frame in frames.get_keys()
        if any(marker in frame.name for marker in QUBIT_FRAME_MARKERS)
        for qubit in frame.qubits
        if isinstance(qubit, Qubit.Fixed)
    }


def evaluate_complex(expression: Expression) -> complex:
    """Evaluate a Quil expression to a complex number.

    :param expression: A constant `quil.expression.Expression`.
    """
    return expression.evaluate({}, {})


def evaluate_real(expression: Expression) -> float:
    """Evaluate a Quil expression to a real number.

    :param expression: A constant, real-valued `quil.expression.Expression`.
    """
    value = evaluate_complex(expression)
    if value.imag != 0:
        raise ValueError(f"Expected a real-valued expression, got {value}")
    return value.real


def compress_constant_runs(ts: np.ndarray, iqs: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
    """Drop the interior samples of runs of constant IQ value.

    Waveforms are sampled at the frame's sample rate, which is typically 1 GS/s, so a frame that is
    held at a constant value - the flat top of a pulse, the zero padding of an `erf_square`, or a
    frame idling for hundreds of microseconds - costs a sample per nanosecond. When the samples are
    drawn as a line, a constant run is fully described by its first and last sample, so dropping the
    interior is visually lossless while making the row count scale with the number of *changes* in
    the waveform rather than with its duration.

    :param ts: The sample times.
    :param iqs: The IQ values, of the same length as `ts`.
    :return: The retained sample times and IQ values.
    """
    if len(iqs) <= 2:
        return ts, iqs

    # Keep a sample if it differs from either neighbour; that retains both endpoints of every
    # constant run, and every sample of a run that is only one long.
    changed = iqs[1:] != iqs[:-1]
    keep = np.empty(len(iqs), dtype=bool)
    keep[0] = keep[-1] = True
    keep[1:-1] = changed[:-1] | changed[1:]
    return ts[keep], iqs[keep]


def get_iqs(pulse: Pulse, waveforms: Dict, frame):
    """Compute the IQ values, and their sample times, for a pulse.

    Built-in waveforms are sampled by `quil` itself, which resolves the waveform template and
    applies the common `scale`, `phase`, and `detuning` parameters. Custom waveforms fall back to
    the IQ values of their `DEFWAVEFORM`.

    :param pulse: The `PULSE` instruction to sample.
    :param waveforms: The `DEFWAVEFORM` definitions of the program, keyed by name.
    :param frame: The attributes of the frame the pulse is played on.
    :return: The sample times (relative to the start of the pulse) and the IQ values.
    """
    sample_rate = frame["SAMPLE-RATE"]._0.to_real()

    # `from_quil` yields a waveform whose parameters are still Quil expressions, so evaluate them
    # down to numbers before sampling.
    waveform = Waveform.from_quil(pulse.waveform).evaluate(evaluate_real, evaluate_complex)

    builtin = waveform.as_builtin()
    if builtin is not None:
        builtin_waveform, common_parameters = builtin
        iqs = builtin_waveform.iq_values_at_sample_rate(common_parameters, sample_rate).iq_values()
    else:
        name, _ = waveform.as_custom()
        if name not in waveforms:
            raise ValueError(f"Waveform {name} not in waveforms")
        iqs = np.array([evaluate_complex(value) for value in waveforms[name].matrix], dtype=complex)

    ts = np.arange(0, len(iqs)) / sample_rate
    return ts, iqs


def program_to_dataframe(
    program: Program,
    compress: bool = True,
) -> pd.DataFrame:
    """Expand the schedule into a dataframe of IQ values and metadata.

    :param program:
    :param compress: Collapse runs of constant IQ values to their endpoints. This is lossless for
    plotting and keeps held or idling frames from costing a row per sample; see
    `compress_constant_runs`. Disable it to get every sample at the frame's sample rate.
    """
    # Get the instructions and the schedule
    if hasattr(program, "_program"):
        quil_program = program._program
    elif isinstance(program, Program):
        quil_program = program
    else:
        raise ValueError("Program must be a pyquil.Program or a quil.Program")

    expanded_quil_program, source_map = expand_with_source_mapping(quil_program, include_pragmas=False)
    blocks = expanded_quil_program.control_flow_graph().basic_blocks()
    if len(blocks) != 1:
        raise ValueError(
            f"Expected a program with a single basic block, got {len(blocks)}. A program containing "
            "control flow has no single schedule, so its blocks must be plotted individually."
        )
    block = blocks[0]
    instructions = block.instructions
    schedule = block.as_schedule_seconds(expanded_quil_program).items

    # Construct the dataframe
    dataframes = []
    hardware_parameters = {}

    qubit_indices = qubit_frame_indices(expanded_quil_program.frames)

    def label_qubit(q: int) -> str:
        """Label a device index as a computational qubit or a coupler."""
        return f"Qubit {q}" if q in qubit_indices else f"Coupler {q}"

    for schedule_item in schedule:
        start_time = schedule_item.time_span.start
        end_time = schedule_item.time_span.end
        duration = schedule_item.time_span.duration
        instruction_index = schedule_item.instruction_index
        instruction = instructions[instruction_index]
        logical_instruction = quil_program.body_instructions[source_map[instruction_index]]

        # Get the IQs
        if isinstance(instruction, (Instruction.Pulse, Instruction.ShiftPhase)):
            pulse = instruction._0
            frame_reference = pulse.frame
            frame = expanded_quil_program.frames.get(frame_reference)
            qubit = ",".join(label_qubit(qubit_index(q)) for q in frame_reference.qubits)
            if frame_reference.name not in hardware_parameters:
                hardware_parameters[frame_reference.name] = json.loads(frame["HARDWARE-OBJECT"]._0)

            if isinstance(instruction, Instruction.Pulse):
                ts, iqs = get_iqs(pulse=pulse, waveforms=expanded_quil_program.waveforms, frame=frame)
                if compress:
                    ts, iqs = compress_constant_runs(ts, iqs)
                waveform = pulse.waveform.name
            else:
                ts, iqs = [0], [0]
                waveform = "shift_phase"

        else:
            continue

        operation = operation_name(logical_instruction)
        instruction_df = pd.DataFrame(
            {
                "Instruction Index": instruction_index,
                "Instruction": instruction.to_quil(),
                "Logical Instruction": logical_instruction.to_quil(),
                "Operation": operation,
                "Operation Type": classify_operation(operation, frame_reference.name),
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

    if not dataframes:
        raise ValueError(
            "The program contains no pulses to plot. A program is plotted at the pulse level, so it "
            "needs the calibrations (`DEFCAL`) and frames (`DEFFRAME`) of the QPU it targets."
        )

    df = pd.concat(dataframes)
    df["Time (s)"] = df["Pulse Time (s)"] + df["Start Time (s)"]
    df = df.sort_values(["Instruction Index", "Time (s)"])
    df = df.astype(
        {
            "Instruction Index": int,
            "Instruction": "category",
            "Logical Instruction": "category",
            "Operation": "category",
            "Operation Type": "category",
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
