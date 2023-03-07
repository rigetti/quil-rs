from enum import Enum
from typing import List

from . import Expression, Qubit

class GateModifier(Enum):
    Controlled = "CONTROLLED"
    Dagger = "DAGGER"
    Forked = "FORKED"

class Gate:
    @classmethod
    def __new__(
        cls,
        name: str,
        parameters: List[Expression],
        qubits: List[Qubit],
        modifiers: List[GateModifier],
    ) -> "Gate": ...
    @property
    def name(self) -> str: ...
    @property
    def parameters(self) -> List[Expression]: ...
    @property
    def qubits(self) -> List[Qubit]: ...
    @property
    def modifiers(self) -> List[GateModifier]: ...
    def dagger(self) -> "Gate":
        """
        Returns a copy of the gate with the ``DAGGER`` modififer added to it.
        """
        ...
    def controlled(self, control_qubit: Qubit) -> "Gate":
        """
        Returns a copy of the gate with the ``CONTROLLED`` modififer added to it.
        """
    def forked(self, fork_qubit: Qubit, alt_params: List[Expression]) -> "Gate":
        """
        Returns a copy of the gate with the ``FORKED`` modifier added to it.

        Raises a ``GateError`` if the number of provided alternate parameters don't
        equal the number of existing parameters.
        """
        ...

class GateSpecification:
    """
    A specification for a gate definition.

    Variants:
        ``matrix``: A gate specificied by a matrix of ``Expression``s representing a unitary operation.
        ``permutation``: A gate specified by a vector of integers that defines a permutation.

    Methods (for each variant):
        - is_*: Returns ``True`` if the inner type is of that variant.
        - as_*: Returns the inner data if it is the given variant, ``None`` otherwise.
        - to_*: Returns the inner data if it is the given variant, raises ``ValueError`` otherwise.
        - from_*: Creates a new ``GateSpecification`` using an instance of the inner type for the variant.
    """

    def is_matrix(self) -> bool: ...
    def is_permutation(self) -> bool: ...
    def as_matrix(self) -> Optional[List[List[Expression]]]: ...
    def to_matrix(self) -> List[List[Expression]]: ...
    def as_permutation(self) -> Optional[List[int]]: ...
    def to_permutation(self) -> List[int]: ...
    @staticmethod
    def from_matrix(matrix: [List[List[Expression]]]) -> "GateSpecification": ...
    @staticmethod
    def from_permutation(permutation: List[int]) -> "GateSpecification": ...

class GateDefinition:
    @classmethod
    def __new__(
        cls,
        name: str,
        parameters: List[str],
        specification: GateSpecification,
    ) -> "GateDefinition": ...
    @property
    def name(self) -> str: ...
    @property
    def parameters(self) -> List[str]: ...
    @property
    def specification(self) -> GateSpecification: ...
