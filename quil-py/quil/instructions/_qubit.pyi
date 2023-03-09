from typing import Optional

class Qubit:
    """
    A Qubit

    Variants:
        ``fixed``: A qubit represented as a fixed integer index.
        ``variable``: A qubit represented by a name.

    Methods (for each variant):
        - is_*: Returns ``True`` if the inner type is of that variant.
        - as_*: Returns the inner data if it is the given variant, ``None`` otherwise.
        - to_*: Returns the inner data if it is the given variant, raises ``ValueError`` otherwise.
        - from_*: Creates a new ``Qubit`` using an instance of the inner type for the variant.
    """

    def is_fixed(self) -> bool: ...
    def is_variable(self) -> bool: ...
    def as_fixed(self) -> Optional[int]: ...
    def to_fixed(self) -> int: ...
    def as_variable(self) -> Optional[str]: ...
    def to_variable(self) -> str: ...
    @staticmethod
    def from_fixed(index: int) -> "Qubit": ...
    @staticmethod
    def from_variable(name: str) -> "Qubit": ...
