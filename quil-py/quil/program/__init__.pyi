"""
The `quil.program` module contains classes for constructing and representing a Quil program.

# Examples

## Source Mapping for Calibration Expansion

```py
import inspect
from quil.program import Program

program_text = inspect.cleandoc(
    \"\"\"
    DEFCAL X 0:
        Y 0

    DEFCAL Y 0:
        Z 0

    X 0 # This instruction is index 0
    Y 0 # This instruction is index 1
    \"\"\"
)

# First, we parse the program and expand its calibrations
program = Program.parse(program_text)
expansion = program.expand_calibrations_with_source_map()
source_map = expansion.source_map()

# This is what we expect the expanded program to be. X and Y have each been replaced by Z.
expected_program_text = inspect.cleandoc(
    \"\"\"
    DEFCAL X 0:
        Y 0

    DEFCAL Y 0:
        Z 0

    Z 0 # This instruction is index 0
    Z 0 # This instruction is index 1
    \"\"\"
)
assert expansion.program().to_quil() == Program.parse(expected_program_text).to_quil()

# In order to discover _which_ calibration led to the first Z in the resulting program, we
# can interrogate the expansion source mapping.
# 
# For instance, the X at index 0 should have been replaced with a Z at index 0.
# Here's how we can confirm that:

# First, we list the calibration expansion targets for that first instruction...
targets = source_map.list_targets_for_source_index(0)

# ...then we extract the expanded instruction.
# If the instruction had _not_ been expanded (i.e. there was no matching calibration), then `as_expanded()` would return `None`.
expanded = targets[0].as_expanded()

# This line shows how that `X 0` was expanded into instruction index 0 (only) within the expanded program.
# The end of the range is exclusive.
assert expanded.range() == range(0, 1)

# We can also map instructions in reverse: given an instruction index in the expanded program, we can find the source index.
# This is useful for understanding the provenance of instructions in the expanded program.
sources = source_map.list_sources_for_target_index(1)

# In this case, the instruction was expanded from the source program at index 1.
assert sources == [1]
```
"""

from typing import Callable, Dict, FrozenSet, List, Optional, Sequence, Set, Union, final

import numpy as np
from numpy.typing import NDArray
from typing_extensions import Self

from quil.instructions import (
    AttributeValue,
    Calibration,
    CalibrationIdentifier,
    Declaration,
    FrameIdentifier,
    Gate,
    GateDefinition,
    Instruction,
    MeasureCalibrationDefinition,
    MeasureCalibrationIdentifier,
    Measurement,
    MemoryReference,
    Qubit,
    QubitPlaceholder,
    Sharing,
    Target,
    TargetPlaceholder,
    Vector,
    Waveform,
)

@final
class Program:
    @staticmethod
    def __new__(cls) -> "Program": ...
    @property
    def body_instructions(self) -> List[Instruction]: ...
    @property
    def calibrations(self) -> CalibrationSet: ...
    @calibrations.setter
    def calibrations(self, calibration_set: CalibrationSet): ...
    @property
    def waveforms(self) -> Dict[str, Waveform]: ...
    @waveforms.setter
    def waveforms(self, waveforms: Dict[str, Waveform]): ...
    @property
    def gate_definitions(self) -> Dict[str, GateDefinition]: ...
    @gate_definitions.setter
    def gate_definitions(self, gate_definitions: Dict[str, GateDefinition]): ...
    @property
    def frames(self) -> FrameSet: ...
    @frames.setter
    def frames(self, frames: FrameSet): ...
    @property
    def memory_regions(self) -> Dict[str, MemoryRegion]: ...
    @memory_regions.setter
    def memory_regions(self, memory_regions: Dict[str, MemoryRegion]): ...
    @property
    def declarations(self) -> Dict[str, Declaration]: ...
    def dagger(self) -> "Program":
        """Creates a new conjugate transpose of the ``Program`` by reversing the order of gate instructions and applying the DAGGER modifier to each.

        Raises a ``GateError`` if any of the instructions in the program are not a ``Gate`
        """
        ...
    def expand_calibrations(self) -> "Program":
        """Expand any instructions in the program which have a matching calibration, leaving the others unchanged.

        Recurses though each instruction while ensuring there is no cycle in the expansion graph (i.e. no calibration
        expands directly or indirectly into itself)
        """
        ...
    def expand_calibrations_with_source_map(self) -> ProgramCalibrationExpansion:
        """Expand any instructions in the program which have a matching calibration, leaving the others unchanged.

        Return the expanded copy of the program and a source mapping describing the expansions made.
        """
        ...
    def into_simplified(self) -> "Program":
        """Simplify this program into a new `Program` which contains only instructions and definitions which are executed; effectively, perform dead code removal.

        Removes:
        - All calibrations, following calibration expansion
        - Frame definitions which are not used by any instruction such as `PULSE` or `CAPTURE`
        - Waveform definitions which are not used by any instruction

        When a valid program is simplified, it remains valid.
        """
        ...
    def get_used_qubits(self) -> Set[Qubit]:
        """Returns a set consisting of every Qubit that is used in the program."""
        ...
    def add_instruction(self, instruction: Instruction):
        """Add an instruction to the end of the program."""
        ...
    def add_instructions(self, instructions: Sequence[Instruction]):
        """Add a list of instructions to the end of the program."""
        ...
    @staticmethod
    def parse(input: str) -> "Program":
        """Parses the given Quil string and returns a new ``Program``.

        Raises a ``ProgramError`` if the given string isn't valid Quil.
        """
    def to_instructions(self) -> Sequence[Instruction]: ...
    def to_unitary(self, n_qubits: int) -> NDArray[np.complex128]: ...
    def copy(self) -> "Program":
        """Creates a clone of this ``Program``."""
        ...
    def clone_without_body_instructions(self) -> "Program":
        """Creates a clone of this ``Program`` with an empty body instructions list."""
    def __add__(self, rhs: Program) -> Program: ...
    def to_quil(self) -> str:
        """Attempt to convert the instruction to a valid Quil string.

        Raises an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """Convert the instruction to a Quil string.

        If any part of the instruction can't be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def filter_instructions(self, predicate: Callable[[Instruction], bool]) -> "Program":
        """Return a new ``Program`` containing only the instructions for which ``predicate`` returns ``True``."""
        ...
    def wrap_in_loop(
        self, loop_count_reference: MemoryReference, start_target: Target, end_target: Target, iterations: int
    ) -> "Program":
        """Return a copy of the `Program` wrapped in a loop that repeats ``iterations`` times.

        The loop is constructed by wrapping the body of the program in classical Quil instructions.
        The given ``loop_count_reference`` must refer to an INTEGER memory region. The value at the
        reference given will be set to ``iterations`` and decremented in the loop. The loop will
        terminate when the reference reaches 0. For this reason your program should not itself
        modify the value at the reference unless you intend to modify the remaining number of
        iterations (i.e. to break the loop).

        The given ``start_target`` and ``end_target`` will be used as the entry and exit points for
        the loop, respectively. You should provide unique `quil.instructions.Target`s that won't be
        used elsewhere in the program.

        If `iterations` is 0, then a copy of the program is returned without any changes. Raises a
        `TypeError` if `iterations` is negative.
        """
        ...
    def resolve_placeholders(self) -> None:
        """Resolve ``TargetPlaceholder``s and ``QubitPlaceholder``s within the program using default resolvers.

        The default resolver will be used to generate a unique value for that placeholder within the scope of
        the program using an auto-incrementing value (for qubit) or suffix (for target)
        while ensuring that unique value is not already in use within the program.
        """
        ...
    def resolve_placeholders_with_custom_resolvers(
        self,
        *,
        target_resolver: Optional[Callable[[TargetPlaceholder], Optional[str]]] = None,
        qubit_resolver: Optional[Callable[[QubitPlaceholder], Optional[int]]] = None,
    ):
        """Resolve ``TargetPlaceholder``s and ``QubitPlaceholder``s within the program.

        The resolved values will remain unique to that placeholder within the scope of the program.

        If you provide ``target_resolver`` and/or ``qubit_resolver``, those will be used to resolve those values respectively.
        If your resolver returns `None` for a particular placeholder, it will not be replaced but will be left as a placeholder.

        If you do not provide a resolver for a placeholder, a default resolver will be used which will generate a unique value
        for that placeholder within the scope of the program using an auto-incrementing value (for qubit) or suffix (for target)
        while ensuring that unique value is not already in use within the program.
        """
        ...
    def control_flow_graph(self) -> "ControlFlowGraph":
        """Return the `control flow graph`_ of the program.

        .. _control flow graph: https://en.wikipedia.org/wiki/Control-flow_graph
        """

class BasicBlock:
    def __new__(cls, instance: "BasicBlock") -> Self:
        """Create a new instance of a `BasicBlock` (or a subclass) using an existing instance."""

    def as_schedule_seconds(self, program: Program) -> ScheduleSeconds:
        """Return the ``ScheduleSeconds`` representing the timing of the instructions within the block.

        * Expanding each instruction within the block using the program's calibration definitions
        * Resolving the `ScheduleSeconds` of the expanded instructions
        * Mapping calibrated instructions back to the original instructions within this block, such that the
          block's instruction is represented as a timespan encompassing all of its expanded instructions

        :param program: The program containing the calibrations to be used to schedule this block. Generally,
            this should be the program from which the block was extracted.

        Important note: If the basic block contains gates, the program must contain corresponding `DEFCAL`s for those gates.
        Gates do not inherently have durations, but rather inherit them from the `PULSE`, `CAPTURE`, `DELAY`,
        and other instructions within their calibrations. Without a calibration, a gate's duration cannot be computed.

        The following example demonstrates construction of such a schedule for a simple program without explicit control
        flow (and thus with only one basic block):

        .. example-code::

            .. code-block:: python

                from quil.program import Program

                program = Program.parse("CZ 0 1; CZ 0 2")

                print(program.to_quil())

                control_flow_graph = program.control_flow_graph()
                assert control_flow_graph.has_dynamic_control_flow() == False

                basic_blocks = control_flow_graph.basic_blocks()
                assert len(basic_blocks) == 1

                schedule = blocks[0].as_schedule_seconds(program)
                print(f"Duration = {schedule.duration()}")

                print(schedule.items())


        Note: when an instruction is expanded, the "time" of that original instruction includes
        the times of all of the resulting instructions. This may cause gate times to be
        longer than a user might expect.

        To understand why, consider a program like this:

        .. example-code::

            .. code-block:: text

                # One-qubit frame
                DEFFRAME 0 "a":
                    ATTRIBUTE: 1

                # Two-qubit frame
                DEFFRAME 0 1 "b":
                    ATTRIBUTE: 1

                DEFCAL A 0:
                    PULSE 0 "a" flat(duration: 1.0)

                DEFCAL B 0 1:
                    FENCE 1
                    PULSE 0 1 "b" flat(duration: 1.0)

                A 0
                B 0 1

        `B 0` will be scheduled from time 0 to time 2, because its inner `FENCE` is scheduled for time 0.
        This may be unexpected if the user expects to see only the timing of the inner `PULSE`.
        """
    def gate_depth(self, gate_minimum_qubit_count: int) -> int:
        """Returns the length of the longest path from an initial instruction (one with no prerequisite instructions) to a final instruction (one with no dependent instructions), where the length of a path is the number of gate instructions in the path.

        :param gate_minimum_qubit_count: The minimum number of qubits in a gate for it to be counted in the depth.
        """
    def label(self) -> Optional[Target]:
        """Return the label of the block, if any. This is used to target this block in control flow."""
    def instructions(self) -> List[Instruction]:
        """Return a list of the instructions in the block, in order of definition.

        This does not include the label or terminator instructions.
        """
    def terminator(self) -> Optional[Instruction]:
        """Return the control flow terminator instruction of the block, if any.

        If this is ``None``, the implicit behavior is to "continue" to the subsequent block.
        """

@final
class CalibrationExpansion:
    def calibration_used(self) -> CalibrationSource: ...
    """The calibration which was used to expand the instruction."""
    def range(self) -> range: ...
    """The range of instructions in the expanded list which were generated by this expansion."""
    def expansions(self) -> CalibrationExpansionSourceMap: ...
    """The source map describing the nested expansions made."""

@final
class CalibrationExpansionSourceMap:
    def entries(self) -> List[CalibrationExpansionSourceMapEntry]: ...
    def list_sources_for_target_index(self, target_index: int) -> List[int]:
        """Return the locations in the source which were expanded to generate that instruction.

        This is `O(n)` where `n` is the number of first-level calibration expansions performed.
        """
        ...

    def list_sources_for_calibration_used(self, calibration_used: CalibrationSource) -> List[int]:
        """Return the locations in the source program which were expanded using a calibration.

        This is `O(n)` where `n` is the number of first-level calibration expansions performed.
        """
        ...

    def list_targets_for_source_index(self, source_index: int) -> List[CalibrationExpansion]:
        """Given a source index, return information about its expansion.

        This is `O(n)` where `n` is the number of first-level calibration expansions performed.
        """
        ...

@final
class CalibrationExpansionSourceMapEntry:
    """A description of the expansion of one instruction into other instructions.

    If present, the instruction located at `source_location` was expanded using calibrations
    into the instructions located at `target_location`.

    Note that both `source_location` and `target_location` are relative to the scope of expansion.
    In the case of a nested expansion, both describe the location relative only to that
    level of expansion and *not* the original program.

    Consider the following example:

    ```
    DEFCAL A:
        NOP
        B
        HALT


    DEFCAL B:
        NOP
        WAIT

    NOP
    NOP
    NOP
    A
    ```

    In this program, `A` will expand into `NOP`, `B`, and `HALT`. Then, `B` will expand into `NOP` and `WAIT`.
    Each level of this expansion will have its own `CalibrationExpansionSourceMap` describing the expansion.
    In the map of `B` to `NOP` and `WAIT`, the `source_location` will be `1` because `B` is the second instruction
    in `DEFCAL A`, even though `A` is the 4th instruction (index = 3) in the original program.
    """
    def source_location(self) -> int: ...
    """The instruction index within the source program's body instructions."""
    def target_location(self) -> CalibrationExpansion: ...
    """The location of the expanded instruction within the target program's body instructions."""

@final
class CalibrationSource:
    """The source of a calibration expansion.

    Can be either a calibration (`DEFCAL`) or a measure calibration (`DEFCAL MEASURE`).
    """
    def as_calibration(self) -> CalibrationIdentifier: ...
    def as_measure_calibration(self) -> MeasureCalibrationIdentifier: ...
    def is_calibration(self) -> bool: ...
    def is_measure_calibration(self) -> bool: ...
    def to_calibration(self) -> CalibrationIdentifier: ...
    def to_measure_calibration(self) -> MeasureCalibrationIdentifier: ...
    @staticmethod
    def from_calibration(inner: CalibrationIdentifier): ...
    @staticmethod
    def from_measure_calibration(inner: MeasureCalibrationIdentifier): ...
    def inner(self) -> Union[CalibrationIdentifier, MeasureCalibrationIdentifier]: ...

@final
class CalibrationSet:
    @staticmethod
    def __new__(
        cls,
        calibrations: Sequence[Calibration],
        measure_calibration_definitions: Sequence[MeasureCalibrationDefinition],
    ) -> "CalibrationSet": ...
    @property
    def calibrations(self) -> List[Calibration]: ...
    @property
    def measure_calibrations(self) -> List[MeasureCalibrationDefinition]: ...
    def expand(self, instruction: Instruction, previous_calibrations: Sequence[Instruction]) -> List[Instruction]:
        """Given an instruction, return the instructions to which it is expanded if there is a match.

        Recursively calibrate instructions, returning an error if a calibration directly or indirectly
        expands into itself.
        """
    ...

    def get_match_for_measurement(self, measurement: Measurement) -> Optional[MeasureCalibrationDefinition]:
        """Returns the last-specified ``MeasureCalibrationDefinition`` that matches the target qubit (if any), or otherwise the last-specified one that specified no qubit."""
        ...
    def get_match_for_gate(self, gate: Gate) -> Optional[Calibration]:
        """Return the final calibration which matches the gate per the QuilT specification.

        A calibration matches a gate if:
        1. It has the same name
        2. It has the same modifiers
        3. It has the same qubit count (any mix of fixed & variable)
        4. It has the same parameter count (both specified and unspecified)
        5. All fixed qubits in the calibration definition match those in the gate
        6. All specified parameters in the calibration definition match those in the gate
        """
    def __len__(self) -> int: ...
    def is_empty(self) -> bool:
        """Returns ``True`` if the ``CalibrationSet`` contains no data."""
        ...
    def insert_calibration(self, calibration: Calibration) -> Optional[Calibration]:
        """Insert another ``Calibration`` (`DEFCAL`) to the set.

        If a calibration with the same name already exists, it is overwritten and this
        function returns the previous calibration. Otherwise, None is returned.
        """
        ...
    def insert_measurement_calibration(
        self, calibration: MeasureCalibrationDefinition
    ) -> Optional[MeasureCalibrationDefinition]:
        """Add another ``MeasureCalibrationDefinition`` (`DEFCAL MEASURE`) to the set.

        If a calibration with the same name already exists, it is overwritten and this
        function returns the previous calibration. Otherwise, None is returned.
        """
    def extend(self, other: CalibrationSet):
        """Append another [`CalibrationSet`] onto this one."""
        ...
    def to_instructions(self):
        """Return the Quil instructions which describe the contained calibrations."""
        ...

@final
class MaybeCalibrationExpansion:
    """The result of having expanded a certain instruction within a program.

    Has two variants:

    - `expanded`: The instruction was expanded into other instructions, described by a `CalibrationExpansion`.
    - `int`: The instruction was not expanded and is described by an integer, the index of the instruction
        within the resulting program's body instructions.
    """
    def as_expanded(self) -> CalibrationExpansion: ...
    def as_unexpanded(self) -> int: ...
    @staticmethod
    def from_expanded(inner: CalibrationExpansion): ...
    @staticmethod
    def from_unexpanded(inner: int): ...
    def inner(self) -> Union[CalibrationExpansion, int]: ...
    def is_expanded(self) -> bool: ...
    def is_unexpanded(self) -> bool: ...
    def to_expanded(self) -> CalibrationExpansion: ...
    def to_unexpanded(self) -> int: ...

class ScheduleSecondsItem:
    """A single item within a fixed schedule, representing a single instruction within a basic block."""

    @property
    def instruction_index(self) -> int:
        """The index of the instruction within the basic block."""
    @property
    def time_span(self) -> TimeSpanSeconds:
        """The time span during which the instruction is scheduled."""

class ControlFlowGraph:
    """Representation of a control flow graph (CFG) for a Quil program.

    The CFG is a directed graph where each node is a basic block and each edge is a control flow
    transition between two basic blocks.
    """
    def __new__(cls, instance: "ControlFlowGraph") -> Self:
        """Create a new instance of a `ControlFlowGraph` (or a subclass) using an existing instance."""

    def has_dynamic_control_flow(self) -> bool:
        """Return ``True`` if the program has dynamic control flow, i.e. contains a conditional branch instruction.

        ``False`` does not imply that there is only one basic block in the program. Multiple basic blocks may have
        non-conditional control flow among them, in which the execution order is deterministic and does not depend
        on program state. This may be a sequence of basic blocks with fixed `JUMP`s or without explicit terminators.
        """
    def basic_blocks(self) -> List["BasicBlock"]:
        """Return a list of all the basic blocks in the control flow graph, in order of definition."""

class ScheduleSeconds:
    def items(self) -> List[ScheduleSecondsItem]:
        """All the items in the schedule, in unspecified order."""
    def duration(self) -> float:
        """The duration of the schedule, in seconds.

        This is the maximum of the end time of all the items.
        """

class TimeSpanSeconds:
    """Representation of a time span in seconds."""

    @property
    def start(self) -> float:
        """The start time of the time span, in seconds.

        This is relative to the start of the scheduling context (such as the basic block).
        """
    @property
    def duration(self) -> float:
        """The duration of the time span, in seconds."""
    @property
    def end(self) -> float:
        """The end time of the time span, in seconds.

        This is the sum of the start time and duration.
        """

@final
class FrameSet:
    @staticmethod
    def __new__(cls) -> "FrameSet": ...
    def get(self, identifier: FrameIdentifier) -> Optional[Dict[str, AttributeValue]]:
        """Retrieve the attributes of a frame by its identifier."""
        ...
    def get_keys(self) -> List[FrameIdentifier]:
        """Return a list of all ``FrameIdentifier``s described by this ``FrameSet``."""
        ...
    def insert(self, identifier: FrameIdentifier, attributes: Dict[str, AttributeValue]):
        """Insert a new ``FrameIdentifier``, overwriting any existing one."""
        ...
    def merge(self, other: FrameSet):
        """Merge another ``FrameSet`` into this one, overwriting any existing keys."""
    def intersection(self, identifiers: FrozenSet[FrameIdentifier]) -> "FrameSet":
        """Return a new ``FrameSet`` which describes only the given ``FrameIdentifier``s."""
        ...
    def is_empty(self) -> bool:
        """Returns ``True`` if this ``FrameSet`` defines no frames."""
        ...
    def to_instructions(self) -> List[Instruction]:
        """Return the Quil instructions which define the contained frames."""
        ...
    def get_all_frames(self) -> Dict[FrameIdentifier, Dict[str, AttributeValue]]: ...

class MemoryRegion:
    @staticmethod
    def __new__(cls, size: Vector, sharing: Optional[Sharing]) -> "MemoryRegion": ...
    @property
    def size(self) -> Vector: ...
    @size.setter
    def size(self, size: Vector): ...
    @property
    def sharing(self) -> Optional[Sharing]: ...
    @sharing.setter
    def sharing(self, sharing: Optional[Sharing]): ...

@final
class ProgramCalibrationExpansion:
    def program(self) -> Program: ...
    """The program containing the expanded instructions"""
    def source_map(self) -> ProgramCalibrationExpansionSourceMap: ...
    """The source mapping describing the expansions made"""

@final
class ProgramCalibrationExpansionSourceMap:
    def entries(self) -> List[ProgramCalibrationExpansionSourceMapEntry]: ...
    def list_sources_for_target_index(self, target_index: int) -> List[int]:
        """Return the locations in the source which were expanded to generate that instruction.

        This is `O(n)` where `n` is the number of source instructions.
        """
        ...

    def list_sources_for_calibration_used(self, calibration_used: CalibrationSource) -> List[int]:
        """Return the locations in the source program which were expanded using a calibration.

        This is `O(n)` where `n` is the number of source instructions.
        """
        ...

    def list_targets_for_source_index(self, source_index: int) -> List[MaybeCalibrationExpansion]:
        """Given a source index, return information about its expansion.

        This is `O(n)` where `n` is the number of source instructions.
        """
        ...

@final
class ProgramCalibrationExpansionSourceMapEntry:
    """A description of the possible expansion of one instruction into other instructions.

    Valid within the scope of a program's calibrations.
    """
    def source_location(self) -> int: ...
    """The instruction index within the source program's body instructions."""
    def target_location(self) -> MaybeCalibrationExpansion: ...
    """The location of the possibly-expanded instruction within the target program's body instructions."""
