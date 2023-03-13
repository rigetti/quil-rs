from typing import Optional

from quil.instructions import MemoryReference, Qubit

class Measurement:
    @staticmethod
    def __new__(
        cls, qubit: Qubit, target: Optional[MemoryReference]
    ) -> "Measurement": ...
    @property
    def qubit(self) -> Qubit: ...
    @qubit.setter
    def qubit(self, qubit: Qubit): ...
    @property
    def target(self) -> Optional[MemoryReference]: ...
    @target.setter
    def target(self, target: Optional[MemoryReference]): ...
