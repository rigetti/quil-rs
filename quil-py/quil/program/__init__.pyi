from typing import Dict, FrozenSet, Set, final, List, Optional, Sequence

import numpy as np
from numpy.typing import NDArray
from quil.instructions import (
    AttributeValue,
    Calibration,
    Declaration,
    FrameIdentifier,
    Gate,
    GateDefinition,
    Instruction,
    MeasureCalibrationDefinition,
    Measurement,
    Qubit,
    Sharing,
    Vector,
    Waveform,
)

@final
class Program:
    @staticmethod
    def __new__(cls) -> "Program": ...
    @property
    def instructions(self) -> List[Instruction]: ...
    @property
    def calibrations(self) -> CalibrationSet: ...
    @calibrations.setter
    def calibrations(self, calibration_set: CalibrationSet): ...
    @property
    def waveforms(self) -> Dict[str, Waveform]: ...
    @waveforms.setter
    def waveforms(self, waveforms: Dict[str, Waveform]): ...
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
    @property
    def defined_gates(self) -> List[GateDefinition]: ...
    def dagger(self) -> "Program":
        """
        Creates a new conjugate transpose of the ``Program`` by reversing the order of gate
        instructions and applying the DAGGER modifier to each.

        Raises a ``GateError`` if any of the instructions in the program are not a ``Gate`
        """
        ...
    def expand_calibrations(self) -> "Program":
        """
        Expand any instructions in the program which have a matching calibration, leaving the others
        unchanged. Recurses though each instruction while ensuring there is no cycle in the expansion
        graph (i.e. no calibration expands directly or indirectly into itself)
        """
        ...
    def into_simplified(self) -> "Program":
        """
        Simplify this program into a new [`Program`] which contains only instructions
        and definitions which are executed; effectively, perform dead code removal.

        Removes:
        - All calibrations, following calibration expansion
        - Frame definitions which are not used by any instruction such as `PULSE` or `CAPTURE`
        - Waveform definitions which are not used by any instruction

        When a valid program is simplified, it remains valid.
        """
        ...
    def get_used_qubits(self) -> Set[Qubit]:
        """
        Returns a set consisting of every Qubit that is used in the program.
        """
        ...
    def add_instruction(self, instruction: Instruction):
        """
        Add an instruction to the end of the program.
        """
        ...
    def add_instructions(self, instruction: Sequence[Instruction]):
        """
        Add a list of instructions to the end of the program.
        """
        ...
    @staticmethod
    def parse(quil: str) -> "Program":
        """
        Parses the given Quil string and returns a new ``Program``.
        Raises a ``ProgramError`` if the given string isn't valid Quil.
        """
    def to_instructions(self) -> Sequence[Instruction]: ...
    def to_unitary(self) -> NDArray[np.complex_]: ...
    def clone_without_body_instructions(self) -> "Program":
        """
        Creates a clone of this ``Program`` with an empty body instructions list.
        """
    def __add__(self, rhs: Program) -> Program: ...

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
        """
        Given an instruction, return the instructions to which it is expanded if there is a match.
        Recursively calibrate instructions, returning an error if a calibration directly or indirectly
        expands into itself.
        """
    ...

    def get_match_for_measurement(self, measurement: Measurement) -> Optional[MeasureCalibrationDefinition]:
        """
        Returns the last-specified ``MeasureCalibrationDefinition`` that matches the target
        qubit (if any), or otherwise the last-specified one that specified no qubit.
        """
        ...
    def get_match_for_gate(self, gate: Gate) -> Optional[Calibration]:
        """
        Return the final calibration which matches the gate per the QuilT specification:

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
    def push_calibration(self, calibration: Calibration):
        """
        Add another gate ``Calibration`` (`DEFCAL`) to the set.
        """
        ...
    def push_measurement_calibration(self, calibration: MeasureCalibrationDefinition):
        """
        Add another ``MeasureCalibrationDefinition`` (`DEFCAL MEASURE`) to the set
        """
    def extend(self, calibration_set: CalibrationSet):
        """
        Append another [`CalibrationSet`] onto this one
        """
        ...
    def to_instructions(self):
        """
        Return the Quil instructions which describe the contained calibrations
        """
        ...

@final
class FrameSet:
    @staticmethod
    def __new__(cls) -> "FrameSet": ...
    def get(self, identifier: FrameIdentifier) -> Optional[Dict[str, AttributeValue]]:
        """
        Retrieve the attributes of a frame by its identifier
        """
        ...
    def get_keys(self) -> List[FrameIdentifier]:
        """
        Return a list of all ``FrameIdentifier``s described by this ``FrameSet``
        """
        ...
    def insert(self, identifier: FrameIdentifier, attributes: Dict[str, AttributeValue]):
        """
        Insert a new ``FrameIdentifier``, overwriting any existing one.
        """
        ...
    def merge(self, other: FrameSet):
        """
        Merge another ``FrameSet`` into this one, overwriting any existing keys.
        """
    def intersection(self, identifiers: FrozenSet[FrameIdentifier]) -> "FrameSet":
        """
        Return a new ``FrameSet`` which describes only the given ``FrameIdentifier``s
        """
        ...
    def is_empty(self) -> bool:
        """
        Returns ``True`` if this ``FrameSet`` defines no frames.
        """
        ...
    def to_instructions(self) -> List[Instruction]:
        """
        Return the Quil instructions which define the contained frames.
        """
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
