from enum import Enum
from typing import Dict, List, Optional, Union, final

from quil.expression import Expression

@final
class Instruction:
    """
    A Quil instruction. Each variant corresponds to a possible type of Quil instruction.

    Variants:
        ``arithmetic``: An arithmetic expression defined by an ``Arithmetic``.
        ``binary_logic``: A binary expression defined by a ``BinaryLogic``.
        ``calibration_definition``: Corresponds to a `DEFCAL` instruction (not `DEFCAL MEASURE`)
            defined by a ``Calibration``.
        ``declaration``: Corresponds to a `DECLARE` statement defined by a ``Declaration``.
        ``frame_definition``: Corresponds to a `DEFFRAME` statement, defined by a ``FrameDefinition``.
        ``gate``: A Quil quantum gate instruction defined by a ``Gate``.
        ``halt``: Corresponds to the `HALT` instruction. No inner data.
        ``measure_calibration_definition``: Corresponds to a `DEFCAL MEASURE` instruction. Defined by a ``MeasureCalibrationDefinition``.
        ``nop``: Corresponds to the `NOP` instruction. No inner data.

    As seen above, some variants contain inner data that fully specify the instruction.
    For example, the ``gate`` variant contains a ``Gate``. This is in contrast to variants like
    ``halt`` that have no inner data because they require none to fully specify an instruction.
    This difference is important for determining which methods are available for each variant.

    Methods (for each variant):
        ``is_*``: Returns ``True`` if the instruction is that variant, ``False`` otherwise.

        If the variant has inner data (e.g. ``gate``):
            ``as_*``: Returns the inner data if it is the given variant, ``None`` otherwise.
            ``to_*``: Returns the inner data if it is the given variant, raises ``ValueError`` otherwise.
            ``from_*``: Creates a new ``Instruction`` of the given variant from an instance of the inner type.

        If the variant doesn't have inner data (e.g. ``halt``)
            ``new_*``: Creates a new ``Instruction`` for the variant.
    """

    def inner(
        self,
    ) -> Union[
        Arithmetic,
        Calibration,
        BinaryLogic,
        Declaration,
        FrameDefinition,
        Gate,
        MeasureCalibrationDefinition,
        Measurement,
    ]:
        """
        Returns the inner value of the variant. Raises a ``RuntimeError`` if inner data doesn't exist.
        """
        ...
    def is_arithmetic(self) -> bool: ...
    def is_binary_logic(self) -> bool: ...
    def is_calibration_definition(self) -> bool: ...
    def is_declaration(self) -> bool: ...
    def is_frame_definition(self) -> bool: ...
    def is_gate(self) -> bool: ...
    def is_halt(self) -> bool: ...
    def is_measure_calibration_definition(self) -> bool: ...
    def is_measurement(self) -> bool: ...
    def is_nop(self) -> bool: ...
    @staticmethod
    def new_halt() -> "Instruction": ...
    @staticmethod
    def new_nop() -> "Instruction": ...
    @staticmethod
    def from_arithmetic(arithmetic: Arithmetic) -> "Instruction": ...
    @staticmethod
    def from_binary_logic(binary_logic: BinaryLogic) -> "Instruction": ...
    @staticmethod
    def from_calibration_definition(calibration: Calibration) -> "Instruction": ...
    @staticmethod
    def from_declaration(declaration: Declaration) -> "Instruction": ...
    @staticmethod
    def from_frame_definition(frame_definition: FrameDefinition) -> "Instruction": ...
    @staticmethod
    def from_gate(gate: Gate) -> "Instruction": ...
    @staticmethod
    def from_measure_calibration_definition(
        measure_calibration_definition: MeasureCalibrationDefinition,
    ) -> "Instruction": ...
    @staticmethod
    def from_measurement(
        measurement: Measurement,
    ) -> "Instruction": ...
    def as_arithmetic(self) -> Optional[Arithmetic]: ...
    def to_arithmetic(self) -> Arithmetic: ...
    def as_declaration(self) -> Optional[Declaration]: ...
    def to_declaration(self) -> Declaration: ...
    def as_frame_definition(self) -> Optional[FrameDefinition]: ...
    def to_frame_definition(self) -> FrameDefinition: ...
    def as_binary_logic(self) -> Optional[BinaryLogic]: ...
    def to_binary_logic(self) -> BinaryLogic: ...
    def as_calibration_definition(self) -> Optional[Calibration]: ...
    def to_calibration_definition(self) -> Calibration: ...
    def as_gate(self) -> Optional[Gate]: ...
    def to_gate(self) -> Gate: ...
    def as_measure_calibration_definition(
        self,
    ) -> Optional[MeasureCalibrationDefinition]: ...
    def to_measure_calibration_definition(self) -> MeasureCalibrationDefinition: ...
    def as_measurement(
        self,
    ) -> Optional[Measurement]: ...
    def to_measurement(self) -> Measurement: ...

@final
class ArithmeticOperand:
    """
    A Quil arithmetic operand.

    Variants:
        ``literal_integer``: An integer literal.
        ``literal_real``: A real numbered literal.
        ``memory_reference``: A Quil ``MemoryReference``.

    Methods (for each variant):
        ``is_*``: Returns ``True`` if the operand is that variant, ``False`` otherwise.
        ``as_*``: Returns the inner data if it is the given variant, ``None`` otherwise.
        ``to_*``: Returns the inner data if it is the given variant, raises ``ValueError`` otherwise.
        ``from_*``: Creates a new ``ArithmeticOperand`` of the given variant from an instance of the inner type.
    """

    def inner(self) -> Union[int, float, MemoryReference]:
        """
        Returns the inner value of the variant. Raises a ``RuntimeError`` if inner data doesn't exist.
        """
        ...
    def is_literal_integer(self) -> bool: ...
    def is_literal_real(self) -> bool: ...
    def is_memory_reference(self) -> bool: ...
    def as_literal_integer(self) -> Optional[int]: ...
    def as_literal_real(self) -> Optional[float]: ...
    def as_memory_reference(self) -> Optional[MemoryReference]: ...
    def to_literal_integer(self) -> int: ...
    def to_literal_real(self) -> float: ...
    def to_memory_reference(self) -> MemoryReference: ...
    def from_literal_integer(self, literal: int) -> "ArithmeticOperand": ...
    def from_literal_real(self, literal: float) -> "ArithmeticOperand": ...
    def from_memory_reference(
        self, memory_reference: MemoryReference
    ) -> "ArithmeticOperand": ...

@final
class ArithmeticOperator(Enum):
    Add = "Add"
    Subtract = "Subtract"
    Divide = "Divide"
    Multiply = "Multiply"

class Arithmetic:
    @staticmethod
    def __new__(
        cls,
        operator: ArithmeticOperator,
        destination: ArithmeticOperand,
        source: ArithmeticOperand,
    ) -> "Arithmetic": ...
    @property
    def operator(self) -> ArithmeticOperator: ...
    @operator.setter
    def operator(self, operator: ArithmeticOperator): ...
    @property
    def destination(self) -> ArithmeticOperand: ...
    @destination.setter
    def destination(self, operand: ArithmeticOperand): ...
    @property
    def source(self) -> ArithmeticOperand: ...
    @source.setter
    def source(self, operand: ArithmeticOperand): ...

@final
class BinaryOperand:
    """
    A Quil binary operand.

    Variants:
        ``literal_integer``: An integer literal.
        ``memory_reference``: A Quil ``MemoryReference``.

    Methods (for each variant):
        ``is_*``: Returns ``True`` if the operand is that variant, ``False`` otherwise.
        ``as_*``: Returns the inner data if it is the given variant, ``None`` otherwise.
        ``to_*``: Returns the inner data if it is the given variant, raises ``ValueError`` otherwise.
        ``from_*``: Creates a new ``BinaryOperand`` of the given variant from an instance of the inner type.
    """

    def inner(self) -> Union[int, MemoryReference]:
        """
        Returns the inner value of the variant. Raises a ``RuntimeError`` if inner data doesn't exist.
        """
        ...
    def is_literal_integer(self) -> bool: ...
    def is_memory_reference(self) -> bool: ...
    def as_literal_integer(self) -> Optional[int]: ...
    def as_memory_reference(self) -> Optional[MemoryReference]: ...
    def to_literal_integer(self) -> int: ...
    def to_memory_reference(self) -> MemoryReference: ...
    def from_literal_integer(self, literal: int) -> "ArithmeticOperand": ...
    def from_memory_reference(
        self, memory_reference: MemoryReference
    ) -> "ArithmeticOperand": ...

@final
class BinaryOperator(Enum):
    And = "AND"
    Ior = "IOR"
    Xor = "XOR"

class BinaryOperands:
    @staticmethod
    def __new__(
        cls,
        memory_reference: MemoryReference,
        operand: BinaryOperand,
    ) -> "BinaryOperands": ...
    @property
    def memory_reference(self) -> MemoryReference: ...
    @memory_reference.setter
    def memory_reference(self, memory_reference: MemoryReference): ...
    @property
    def operand(self) -> BinaryOperand: ...
    @operand.setter
    def operand(self, operand: BinaryOperand): ...

class BinaryLogic:
    @staticmethod
    def __new__(
        cls,
        operator: BinaryOperator,
        operands: BinaryOperands,
    ) -> "BinaryLogic": ...
    @property
    def operator(self) -> BinaryOperator: ...
    @operator.setter
    def operator(self, operator: BinaryOperator): ...
    @property
    def operands(self) -> BinaryOperands: ...
    @operands.setter
    def operands(self, operands: BinaryOperands): ...

class Calibration:
    @staticmethod
    def __new__(
        cls,
        name: str,
        parameters: List[Expression],
        qubits: List[Qubit],
        instructions: List[Instruction],
        modifiers: List[GateModifier],
    ) -> "Calibration": ...
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

class MeasureCalibrationDefinition:
    @property
    def qubit(self) -> Optional[Qubit]: ...
    @property
    def parameter(self) -> str: ...
    @property
    def instructions(self) -> List[Instruction]: ...
    @staticmethod
    def __new__(
        cls,
        qubit: Optional[Qubit],
        parameter: str,
        instructions: List[Instruction],
    ) -> "MeasureCalibrationDefinition": ...

class Declaration:
    @staticmethod
    def __new__(
        cls,
        name: str,
        size: Vector,
        sharing: Optional[str],
    ) -> "Declaration": ...
    @property
    def name(self) -> str: ...
    @name.setter
    def name(self, name: str): ...
    @property
    def size(self) -> Vector: ...
    @size.setter
    def size(self, vector: Vector): ...
    @property
    def sharing(self) -> Optional[str]: ...
    @sharing.setter
    def sharing(self, sharing: Optional[str]): ...

class Vector:
    @staticmethod
    def __new__(cls, data_type: ScalarType, length: int) -> "Vector": ...
    @property
    def data_type(self) -> ScalarType: ...
    @data_type.setter
    def data_type(self, data_type: ScalarType): ...
    @property
    def length(self) -> int: ...
    @length.setter
    def length(self, data_type: int): ...

@final
class ScalarType(Enum):
    Bit = "BIT"
    Integer = "INTEGER"
    Octet = "OCTET"
    Real = "REAL"

@final
class AttributeValue:
    """
    A Quil instruction. Each variant corresponds to a possible type of Quil instruction.

    Variants:
        ``string``: A string attribute containing a ``str``.
        ``expression``: An expression attribute containing an ``Expression``.

    Methods (for each variant):
        ``is_*``: Returns ``True`` if the ``AttributeValue`` is that variant, ``False`` otherwise.

        ``as_*``: Returns the inner data if it is the given variant, ``None`` otherwise.
        ``to_*``: Returns the inner data if it is the given variant, raises ``ValueError`` otherwise.
        ``from_*``: Creates a new ``AttributeValue`` of the given variant from an instance of the inner type.
    """

    def inner(self) -> Union[str, Expression]:
        """
        Returns the inner value of the variant. Raises a ``RuntimeError`` if inner data doesn't exist.
        """
        ...
    def is_string(self) -> bool: ...
    def is_expression(self) -> bool: ...
    @staticmethod
    def from_string(string: str) -> "AttributeValue": ...
    @staticmethod
    def from_expression(expression: Expression) -> "AttributeValue": ...
    def as_string(self) -> Optional[str]: ...
    def to_string(self) -> str: ...
    def as_expression(self) -> Optional[Expression]: ...
    def to_expression(self) -> Expression: ...

class FrameDefinition:
    @staticmethod
    def __new__(
        cls,
        identifier: FrameIdentifier,
        attributes: Dict[str, AttributeValue],
    ) -> "FrameDefinition": ...
    @property
    def identifier(self) -> FrameIdentifier: ...
    @identifier.setter
    def identifier(self, identifier: FrameIdentifier): ...
    @property
    def attributes(self) -> Dict[str, AttributeValue]: ...
    @attributes.setter
    def attributes(self, identifier: Dict[str, AttributeValue]): ...

class FrameIdentifier:
    @staticmethod
    def __new__(cls, name: str, qubits: List[Qubit]) -> "FrameIdentifier": ...
    @property
    def name(self) -> str: ...
    @name.setter
    def name(self, name: str): ...
    @property
    def qubits(self) -> List[Qubit]: ...
    @qubits.setter
    def qubits(self, qubits: List[Qubit]): ...

class GateError(ValueError):
    """An error that may occur when performing operations on a ``Gate``"""

    ...

@final
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
        Returns a copy of the gate with the ``DAGGER`` modifier added to it.
        """
        ...
    def controlled(self, control_qubit: Qubit) -> "Gate":
        """
        Returns a copy of the gate with the ``CONTROLLED`` modifier added to it.
        """
    def forked(self, fork_qubit: Qubit, alt_params: List[Expression]) -> "Gate":
        """
        Returns a copy of the gate with the ``FORKED`` modifier added to it.

        Raises a ``GateError`` if the number of provided alternate parameters don't
        equal the number of existing parameters.
        """
        ...

@final
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

    def inner(self) -> Union[List[List[Expression]], List[int]]:
        """
        Returns the inner value of the variant. Raises a ``RuntimeError`` if inner data doesn't exist.
        """
        ...
    def is_matrix(self) -> bool: ...
    def is_permutation(self) -> bool: ...
    def as_matrix(self) -> Optional[List[List[Expression]]]: ...
    def to_matrix(self) -> List[List[Expression]]: ...
    def as_permutation(self) -> Optional[List[int]]: ...
    def to_permutation(self) -> List[int]: ...
    @staticmethod
    def from_matrix(matrix: List[List[Expression]]) -> "GateSpecification": ...
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

@final
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

    def inner(self) -> Union[int, str]:
        """
        Returns the inner value of the variant. Raises a ``RuntimeError`` if inner data doesn't exist.
        """
        ...
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

class MemoryReference:
    @staticmethod
    def __new__(cls, name: str, index: int) -> "MemoryReference": ...
    @property
    def name(self) -> str: ...
    @name.setter
    def name(self, name: str): ...
    @property
    def index(self) -> int: ...
    @index.setter
    def index(self, index: int): ...
