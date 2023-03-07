from typing import Optional

from _calibration import Calibration, MeasureCalibrationDefinition
from _gate import (
    Gate as Gate,
    GateSpecification as GateSpecification,
    GateModifier as GateModifier,
    GateDefinition as GateDefinition,
)
from _expression import Expression as Expression
from _qubit import Qubit as Qubit

class Instruction:
    """
    A Quil instruction. Each variant corresponds to a possible type of Quil instruction.

    Variants:
        ``arithmetic``: An arithmetic expression
        ``calibration_definition``: Corresponds to a `DEFCAL` instruction (not `DEFCAL MEASURE`)
        ``declaration``: Corresponds to a `DECLARE` statement
        ``gate``: A Quil quantum gate instruction
        ``halt``: Corresponds to the `HALT` instruction.
        ``measure_calibration_definition``: Corresponds to a `DEFCAL MEASURE` instruction.
        ``nop``: Corresponds to the `NOP` instruction.

    Methods (for each variant):
        ``is_*``: Returns ``True`` if the instruction is that variant, ``False`` otherwise.

        If the variant has inner data:
            ``as_*``: Returns the inner data if it is the given variant, ``None`` otherwise.
            ``to_*``: Returns the inner data if it is the given variant, raises ``ValueError`` otherwise.
            ``from_*``: Creates a new ``Instruction`` of the given variant from an instance of the inner type.

        If the variant doesn't have inner data (ie. ``halt``)
            ``new_*``: Creates a new ``Instruction`` for the variant
    """

    def is_arithmetic(self) -> bool: ...
    def is_calibration_definition(self) -> bool: ...
    def is_declaration(self) -> bool: ...
    def is_gate(self) -> bool: ...
    def is_halt(self) -> bool: ...
    def is_measure_calibration_definition(self) -> bool: ...
    def is_nop(self) -> bool: ...
    @staticmethod
    def new_halt() -> "Instruction": ...
    @staticmethod
    def new_nop() -> "Instruction": ...
    @staticmethod
    def from_calibration_definition(calibration: Calibration) -> "Instruction": ...
    @staticmethod
    def from_gate(gate: Gate) -> "Instruction": ...
    @staticmethod
    def from_measure_calibration_definition(
        measure_calibration_definition: MeasureCalibrationDefinition,
    ) -> "Instruction": ...
    def as_calibration_definition(self) -> Optional[Calibration]: ...
    def to_calibration_definition(self) -> Calibration: ...
    def as_gate(self) -> Optional[Gate]: ...
    def to_gate(self) -> Gate: ...
    def as_measure_calibration_definition(
        self,
    ) -> Optional[MeasureCalibrationDefinition]: ...
    def to_measure_calibration_definition(self) -> MeasureCalibrationDefinition: ...
