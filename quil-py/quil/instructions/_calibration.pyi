from typing import List, Optional

from . import Instruction, Expression, Qubit, GateModifier

class Calibration:
    @property
    def name(self) -> str: ...
    @property
    def parameters(self) -> List[Expression]: ...
    @property
    def qubits(self) -> List[Qubit]: ...
    @property
    def instructions(self) -> List[Instruction]: ...
    @property
    def modifiers(self) -> List[GateModifier]: ...
    @classmethod
    def __new__(
        cls,
        name: str,
        parameters: List[Expression],
        qubits: List[Qubit],
        instructions: List[Instruction],
        modifiers: List[GateModifier],
    ) -> "Calibration": ...

class MeasureCalibrationDefinition:
    @property
    def qubit(self) -> Optional[Qubit]: ...
    @property
    def parameter(self) -> str: ...
    @property
    def instructions(self) -> List[Instruction]: ...
    @classmethod
    def __new__(
        cls,
        qubit: Optional[Qubit],
        parameter: str,
        instructions: List[Instruction],
    ) -> "MeasureCalibrationDefinition": ...
