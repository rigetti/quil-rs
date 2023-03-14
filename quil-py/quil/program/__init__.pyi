from typing import final, List, Optional

@final
class Program:
    pass

from quil.instructions import (
    Calibration,
    MeasureCalibrationDefinition,
    Measurement,
    Instruction,
)

class CalibrationSet:
    @staticmethod
    def __new__(
        cls,
        calibrations: List[Calibration],
        measure_calibration_definitions: List[MeasureCalibrationDefinition],
    ) -> "CalibrationSet": ...
    @property
    def calibrations(self) -> List[Calibration]: ...
    @property
    def measure_calibrations(self) -> List[MeasureCalibrationDefinition]: ...
    def expand(
        self, instruction: Instruction, previous_calibrations: List[Instruction]
    ) -> List[Instruction]:
        """
        Given an instruction, return the instructions to which it is expanded if there is a match.
        Recursively calibrate instructions, returning an error if a calibration directly or indirectly
        expands into itself.
        """
    ...

    def get_match_for_measurement(
        self, measurement: Measurement
    ) -> Optional[MeasureCalibrationDefinition]:
        """
        Returns the last-specified ``MeasureCalibrationDefinition`` that matches the target
        qubit (if any), or otherwise the last-specified one that specified no qubit.
        """
        ...
    # def get_match_for_gate(self, gate_modifiers: List[GateModifier], gate_name: str, gate_parameters: List[Expression], gate_qubits: List[Qubit])

    def __len__(self) -> int: ...
    def is_empty(self) -> bool:
        """Returns ``True`` if the ``CalibrationSet`` contains no data."""
        ...
    def push_calibration(self):
        """
        Add another gate ``Calibration`` (`DEFCAL`) to the set.
        """
        ...
    def push_measurement_calibration(self):
        """
        Add another ``MeasureCalibrationDefinition`` (`DEFCAL MEASURE`) to the set
        """
    def extend(self):
        """
        Append another [`CalibrationSet`] onto this one
        """
        ...
    def to_instructions(self):
        """
        Return the Quil instructions which describe the contained calibrations
        """
        ...
