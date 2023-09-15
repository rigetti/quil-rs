from enum import Enum
import numpy as np
from numpy.typing import NDArray
from typing import Dict, List, Optional, Sequence, Tuple, Union, final
from typing_extensions import Self

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
        ``capture``: Corresponds to a `CAPTURE` instruction
        ``calibration``: Corresponds to a `DEFCAL` instruction.
        ``circuit_definition``: Corresponds to a `DEFCIRCUIT` instruction and its body,
            defined by a ``CircuitDefinition``.
        ``convert``: Corresponds to a `CONVERT` instruction.
        ``comparison``: Corresponds to a comparison of two `MemoryReference`s
        ``declaration``: Corresponds to a `DECLARE` statement defined by a ``Declaration``.
        ``delay``: Corresponds to a `DELAY` instruction.
        ``exchange``: Corresponds to an `EXCHANGE` instruction.
        ``fence``: Corresponds to a `FENCE` instruction.
        ``frame_definition``: Corresponds to a `DEFFRAME` statement, defined by a ``FrameDefinition``.
        ``gate``: A Quil quantum gate instruction defined by a ``Gate``.
        ``gate_definition``: A quantum gate definition defined by a ``GateDefinition``.
        ``halt``: Corresponds to the `HALT` instruction. No inner data.
        ``include``: Corresponds to an `INCLUDE` directive.
        ``jump``: Corresponds to a `JUMP` instruction
        ``jump_when``: Corresponds to a `JUMP-WHEN` instruction
        ``jump_unless``: Corresponds to a `JUMP-UNLESS` instruction
        ``label``: Corresponds to a `LABEL`
        ``load``: Corresponds to a `LOAD` instruction.
        ``measure_calibration_definition``: Corresponds to a `DEFCAL MEASURE` instruction. Defined by a ``MeasureCalibrationDefinition``.
        ``measurement``: Corresponds to a `MEASURE` instruction.
        ``move``: Corresponds to a `MOVE` instruction.
        ``nop``: Corresponds to the `NOP` instruction. No inner data.
        ``pragma``: Corresponds to a `PRAGMA` instruction.
        ``pulse``: Corresponds to a `PULSE` instruction.
        ``raw_capture``: Corresponds to a `RAW-CAPTURE` instruction.
        ``reset``: Corresponds to a `RESET` instruction.
        ``set_frequency``: Corresponds to a `SET-FREQUENCY` instruction.
        ``set_phase``: Corresponds to a `SET-PHASE` instruction.
        ``set_scale``: Corresponds to a `SET-SCALE` instruction.
        ``shift_frequency``: Corresponds to a `SHIFT-FREQUENCY` instruction.
        ``shift_phase``: Corresponds to a `SHIFT-PHASE` instruction.
        ``store``: Corresponds to a `STORE` instruction.
        ``swap_phases``: Corresponds to a `SWAP-PHASES` instruction.
        ``unary_logic``: Corresponds to a unary operation on a `MemoryReference`.
        ``waveform_definition``: A waveform defined by a ``WaveformDefinition``.
        ``wait``: Corresponds to a `WAIT` instruction. No inner data.

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

    def __new__(
        cls,
        instruction: Union[
            Arithmetic,
            Calibration,
            Capture,
            BinaryLogic,
            CircuitDefinition,
            Convert,
            Comparison,
            Declaration,
            Delay,
            Exchange,
            Fence,
            FrameDefinition,
            Gate,
            GateDefinition,
            Include,
            Jump,
            JumpWhen,
            JumpUnless,
            Label,
            Load,
            MeasureCalibrationDefinition,
            Measurement,
            Move,
            Pragma,
            Pulse,
            RawCapture,
            Reset,
            SetFrequency,
            SetPhase,
            SetScale,
            ShiftFrequency,
            ShiftPhase,
            Store,
            SwapPhases,
            UnaryLogic,
            WaveformDefinition,
        ],
    ) -> Self:
        """
        Returns a new ``Instruction`` from the given inner data.
        """
    def inner(
        self,
    ) -> Union[
        Arithmetic,
        Calibration,
        Capture,
        BinaryLogic,
        CircuitDefinition,
        Convert,
        Comparison,
        Declaration,
        Delay,
        Exchange,
        Fence,
        FrameDefinition,
        Gate,
        GateDefinition,
        Include,
        Jump,
        JumpWhen,
        JumpUnless,
        Label,
        Load,
        MeasureCalibrationDefinition,
        Measurement,
        Move,
        Pragma,
        Pulse,
        RawCapture,
        Reset,
        SetFrequency,
        SetPhase,
        SetScale,
        ShiftFrequency,
        ShiftPhase,
        Store,
        SwapPhases,
        UnaryLogic,
        WaveformDefinition,
    ]:
        """
        Returns the inner value of the variant. Raises a ``RuntimeError`` if inner data doesn't exist.
        """
        ...
    def is_arithmetic(self) -> bool: ...
    def is_binary_logic(self) -> bool: ...
    def is_calibration_definition(self) -> bool: ...
    def is_capture(self) -> bool: ...
    def is_circuit_definition(self) -> bool: ...
    def is_convert(self) -> bool: ...
    def is_comparison(self) -> bool: ...
    def is_declaration(self) -> bool: ...
    def is_delay(self) -> bool: ...
    def is_exchange(self) -> bool: ...
    def is_fence(self) -> bool: ...
    def is_frame_definition(self) -> bool: ...
    def is_gate(self) -> bool: ...
    def is_gate_definition(self) -> bool: ...
    def is_halt(self) -> bool: ...
    def is_include(self) -> bool: ...
    def is_jump(self) -> bool: ...
    def is_jump_when(self) -> bool: ...
    def is_jump_unless(self) -> bool: ...
    def is_label(self) -> bool: ...
    def is_load(self) -> bool: ...
    def is_measure_calibration_definition(self) -> bool: ...
    def is_measurement(self) -> bool: ...
    def is_move(self) -> bool: ...
    def is_nop(self) -> bool: ...
    def is_pragma(self) -> bool: ...
    def is_pulse(self) -> bool: ...
    def is_raw_capture(self) -> bool: ...
    def is_reset(self) -> bool: ...
    def is_set_frequency(self) -> bool: ...
    def is_set_phase(self) -> bool: ...
    def is_set_scale(self) -> bool: ...
    def is_shift_frequency(self) -> bool: ...
    def is_shift_phase(self) -> bool: ...
    def is_unary_logic(self) -> bool: ...
    def is_store(self) -> bool: ...
    def is_swap_phases(self) -> bool: ...
    def is_waveform_definition(self) -> bool: ...
    def is_wait(self) -> bool: ...
    @staticmethod
    def new_halt() -> "Instruction": ...
    @staticmethod
    def new_nop() -> "Instruction": ...
    @staticmethod
    def new_wait() -> "Instruction": ...
    @staticmethod
    def from_arithmetic(inner: Arithmetic) -> "Instruction": ...
    @staticmethod
    def from_binary_logic(inner: BinaryLogic) -> "Instruction": ...
    @staticmethod
    def from_calibration_definition(inner: Calibration) -> "Instruction": ...
    @staticmethod
    def from_capture(inner: Capture) -> "Instruction": ...
    @staticmethod
    def from_circuit_definition(
        inner: CircuitDefinition,
    ) -> "Instruction": ...
    @staticmethod
    def from_convert(inner: Convert) -> "Instruction": ...
    @staticmethod
    def from_comparison(inner: Comparison) -> "Instruction": ...
    @staticmethod
    def from_declaration(inner: Declaration) -> "Instruction": ...
    @staticmethod
    def from_delay(inner: Delay) -> "Instruction": ...
    @staticmethod
    def from_exchange(inner: Exchange) -> "Instruction": ...
    @staticmethod
    def from_fence(inner: Fence) -> "Instruction": ...
    @staticmethod
    def from_frame_definition(inner: FrameDefinition) -> "Instruction": ...
    @staticmethod
    def from_gate(inner: Gate) -> "Instruction": ...
    @staticmethod
    def from_gate_definition(inner: GateDefinition) -> "Instruction": ...
    @staticmethod
    def from_include(inner: Include) -> "Instruction": ...
    @staticmethod
    def from_jump(inner: Jump) -> "Instruction": ...
    @staticmethod
    def from_jump_when(inner: JumpWhen) -> "Instruction": ...
    @staticmethod
    def from_jump_unless(inner: JumpUnless) -> "Instruction": ...
    @staticmethod
    def from_label(inner: Label) -> "Instruction": ...
    @staticmethod
    def from_load(inner: Load) -> "Instruction": ...
    @staticmethod
    def from_measure_calibration_definition(
        inner: MeasureCalibrationDefinition,
    ) -> "Instruction": ...
    @staticmethod
    def from_measurement(
        inner: Measurement,
    ) -> "Instruction": ...
    @staticmethod
    def from_move(inner: Move) -> "Instruction": ...
    @staticmethod
    def from_pragma(inner: Pragma) -> "Instruction": ...
    @staticmethod
    def from_pulse(inner: Pulse) -> "Instruction": ...
    @staticmethod
    def from_raw_capture(inner: RawCapture) -> "Instruction": ...
    @staticmethod
    def from_set_frequency(inner: SetFrequency) -> "Instruction": ...
    @staticmethod
    def from_set_phase(inner: SetPhase) -> "Instruction": ...
    @staticmethod
    def from_set_scale(inner: SetScale) -> "Instruction": ...
    @staticmethod
    def from_shift_frequency(inner: ShiftFrequency) -> "Instruction": ...
    @staticmethod
    def from_shift_phase(inner: ShiftPhase) -> "Instruction": ...
    @staticmethod
    def from_unary_logic(inner: UnaryLogic) -> "Instruction": ...
    @staticmethod
    def from_store(inner: Store) -> "Instruction": ...
    @staticmethod
    def from_swap_phases(inner: SwapPhases) -> "Instruction": ...
    @staticmethod
    def from_reset(inner: Reset) -> "Instruction": ...
    @staticmethod
    def from_waveform_definition(
        inner: WaveformDefinition,
    ) -> "Instruction": ...
    def as_arithmetic(self) -> Optional[Arithmetic]: ...
    def to_arithmetic(self) -> Arithmetic: ...
    def as_binary_logic(self) -> Optional[BinaryLogic]: ...
    def to_binary_logic(self) -> BinaryLogic: ...
    def as_convert(self) -> Optional[Convert]: ...
    def to_convert(self) -> Convert: ...
    def as_comparison(self) -> Optional[Comparison]: ...
    def to_comparison(self) -> Comparison: ...
    def as_circuit_definition(self) -> Optional[CircuitDefinition]: ...
    def to_circuit_definition(self) -> CircuitDefinition: ...
    def as_calibration_definition(self) -> Optional[Calibration]: ...
    def to_calibration_definition(self) -> Calibration: ...
    def as_capture(self) -> Optional[Capture]: ...
    def to_capture(self) -> Capture: ...
    def as_declaration(self) -> Optional[Declaration]: ...
    def to_declaration(self) -> Declaration: ...
    def as_delay(self) -> Optional[Delay]: ...
    def to_delay(self) -> Delay: ...
    def as_exchange(self) -> Optional[Exchange]: ...
    def to_exchange(self) -> Exchange: ...
    def as_fence(self) -> Optional[Fence]: ...
    def to_fence(self) -> Fence: ...
    def as_gate(self) -> Optional[Gate]: ...
    def to_gate(self) -> Gate: ...
    def as_gate_definition(self) -> Optional[GateDefinition]: ...
    def to_gate_definition(self) -> GateDefinition: ...
    def as_include(self) -> Optional[Include]: ...
    def to_include(self) -> Include: ...
    def as_jump(self) -> Optional[Jump]: ...
    def to_jump(self) -> Jump: ...
    def as_jump_when(self) -> Optional[JumpWhen]: ...
    def to_jump_when(self) -> JumpWhen: ...
    def as_jump_unless(self) -> Optional[JumpUnless]: ...
    def to_jump_unless(self) -> JumpUnless: ...
    def as_label(self) -> Optional[Label]: ...
    def to_label(self) -> Label: ...
    def as_load(self) -> Optional[Load]: ...
    def to_load(self) -> Load: ...
    def as_frame_definition(self) -> Optional[FrameDefinition]: ...
    def to_frame_definition(self) -> FrameDefinition: ...
    def as_measure_calibration_definition(
        self,
    ) -> Optional[MeasureCalibrationDefinition]: ...
    def to_measure_calibration_definition(self) -> MeasureCalibrationDefinition: ...
    def as_measurement(
        self,
    ) -> Optional[Measurement]: ...
    def to_measurement(self) -> Measurement: ...
    def as_move(self) -> Optional[Move]: ...
    def to_move(self) -> Move: ...
    def as_pragma(self) -> Optional[Pragma]: ...
    def to_pragma(self) -> Pragma: ...
    def as_pulse(self) -> Optional[Pulse]: ...
    def to_pulse(self) -> Pulse: ...
    def as_raw_capture(self) -> Optional[RawCapture]: ...
    def to_raw_capture(self) -> RawCapture: ...
    def as_reset(self) -> Optional[Reset]: ...
    def to_reset(self) -> Reset: ...
    def as_set_frequency(self) -> Optional[SetFrequency]: ...
    def to_set_frequency(self) -> SetFrequency: ...
    def as_set_phase(self) -> Optional[SetPhase]: ...
    def to_set_phase(self) -> SetPhase: ...
    def as_set_scale(self) -> Optional[SetScale]: ...
    def to_set_scale(self) -> SetScale: ...
    def as_shift_frequency(self) -> Optional[ShiftFrequency]: ...
    def to_shift_frequency(self) -> ShiftFrequency: ...
    def as_shift_phase(self) -> Optional[ShiftPhase]: ...
    def to_shift_phase(self) -> ShiftPhase: ...
    def as_unary_logic(self) -> Optional[UnaryLogic]: ...
    def to_unary_logic(self) -> UnaryLogic: ...
    def as_store(self) -> Optional[Store]: ...
    def to_store(self) -> Store: ...
    def as_swap_phases(self) -> Optional[SwapPhases]: ...
    def to_swap_phases(self) -> SwapPhases: ...
    def as_waveform_definition(self) -> Optional[WaveformDefinition]: ...
    def to_waveform_definition(self) -> WaveformDefinition: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

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
    @staticmethod
    def from_literal_integer(inner: int) -> "ArithmeticOperand": ...
    @staticmethod
    def from_literal_real(inner: float) -> "ArithmeticOperand": ...
    @staticmethod
    def from_memory_reference(
        inner: MemoryReference,
    ) -> "ArithmeticOperand": ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """

@final
class ArithmeticOperator(Enum):
    Add = "Add"
    Subtract = "Subtract"
    Divide = "Divide"
    Multiply = "Multiply"
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """

class Arithmetic:
    def __new__(
        cls,
        operator: ArithmeticOperator,
        destination: ArithmeticOperand,
        source: ArithmeticOperand,
    ) -> Self: ...
    @property
    def operator(self) -> ArithmeticOperator: ...
    @operator.setter
    def operator(self, operator: ArithmeticOperator) -> None: ...
    @property
    def destination(self) -> ArithmeticOperand: ...
    @destination.setter
    def destination(self, operand: ArithmeticOperand) -> None: ...
    @property
    def source(self) -> ArithmeticOperand: ...
    @source.setter
    def source(self, operand: ArithmeticOperand) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

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
    @staticmethod
    def from_literal_integer(inner: int) -> "BinaryOperand": ...
    @staticmethod
    def from_memory_reference(
        inner: MemoryReference,
    ) -> "BinaryOperand": ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """

@final
class BinaryOperator(Enum):
    And = "AND"
    Ior = "IOR"
    Xor = "XOR"
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """

class BinaryOperands:
    def __new__(
        cls,
        memory_reference: MemoryReference,
        operand: BinaryOperand,
    ) -> Self: ...
    @property
    def memory_reference(self) -> MemoryReference: ...
    @memory_reference.setter
    def memory_reference(self, memory_reference: MemoryReference) -> None: ...
    @property
    def operand(self) -> BinaryOperand: ...
    @operand.setter
    def operand(self, operand: BinaryOperand) -> None: ...

class BinaryLogic:
    def __new__(
        cls,
        operator: BinaryOperator,
        operands: BinaryOperands,
    ) -> Self: ...
    @property
    def operator(self) -> BinaryOperator: ...
    @operator.setter
    def operator(self, operator: BinaryOperator) -> None: ...
    @property
    def operands(self) -> BinaryOperands: ...
    @operands.setter
    def operands(self, operands: BinaryOperands) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class Convert:
    def __new__(cls, destination: MemoryReference, source: MemoryReference) -> Self: ...
    @property
    def destination(self) -> MemoryReference: ...
    @destination.setter
    def destination(self, destination: MemoryReference) -> None: ...
    @property
    def source(self) -> MemoryReference: ...
    @source.setter
    def source(self, source: MemoryReference) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class Move:
    def __new__(cls, destination: MemoryReference, source: ArithmeticOperand) -> Self: ...
    @property
    def destination(self) -> MemoryReference: ...
    @destination.setter
    def destination(self, destination: MemoryReference) -> None: ...
    @property
    def source(self) -> ArithmeticOperand: ...
    @source.setter
    def source(self, source: ArithmeticOperand) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class Exchange:
    def __new__(cls, left: MemoryReference, right: MemoryReference) -> Self: ...
    @property
    def left(self) -> MemoryReference: ...
    @left.setter
    def left(self, left: MemoryReference) -> None: ...
    @property
    def right(self) -> MemoryReference: ...
    @right.setter
    def right(self, right: MemoryReference) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

@final
class ComparisonOperand:
    """
    A Quil binary operand.

    Variants:
        ``literal_integer``: An integer literal.
        ``literal_real``: A floating point literal.
        ``memory_reference``: A Quil ``MemoryReference``.

    Methods (for each variant):
        ``is_*``: Returns ``True`` if the operand is that variant, ``False`` otherwise.
        ``as_*``: Returns the inner data if it is the given variant, ``None`` otherwise.
        ``to_*``: Returns the inner data if it is the given variant, raises ``ValueError`` otherwise.
        ``from_*``: Creates a new ``BinaryOperand`` of the given variant from an instance of the inner type.
    """

    def inner(self) -> Union[int, float, MemoryReference]:
        """
        Returns the inner value of the variant.
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
    @staticmethod
    def from_literal_integer(inner: int) -> "ComparisonOperand": ...
    @staticmethod
    def from_literal_real(inner: float) -> "ComparisonOperand": ...
    @staticmethod
    def from_memory_reference(
        inner: MemoryReference,
    ) -> "ComparisonOperand": ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """

@final
class ComparisonOperator(Enum):
    Equal = "EQUAL"
    GreaterThanOrEqual = "GREATERTHANOREQUAL"
    GreaterThan = "GREATERTHAN"
    LessThanOrEqual = "GREATERTHANOREQUAL"
    LessThan = "LESSTHAN"

class Comparison:
    def __new__(
        cls,
        operator: ComparisonOperator,
        operands: Tuple[MemoryReference, MemoryReference, ComparisonOperand],
    ) -> Self: ...
    @property
    def operator(self) -> ComparisonOperator: ...
    @operator.setter
    def operator(self, operator: ComparisonOperator) -> None: ...
    @property
    def operands(
        self,
    ) -> Tuple[MemoryReference, MemoryReference, ComparisonOperand]: ...
    @operands.setter
    def operands(self, operands: Tuple[MemoryReference, MemoryReference, ComparisonOperand]) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

@final
class UnaryOperator(Enum):
    Neg = "NEG"
    Not = "NOT"
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """

class UnaryLogic:
    def __new__(cls, operator: UnaryOperator, operand: MemoryReference) -> Self: ...
    @property
    def operator(self) -> UnaryOperator: ...
    @operator.setter
    def operator(self, operator: UnaryOperator) -> None: ...
    @property
    def operand(self) -> MemoryReference: ...
    @operand.setter
    def operand(self, operand: MemoryReference) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class Calibration:
    def __new__(
        cls,
        name: str,
        parameters: Sequence[Expression],
        qubits: Sequence[Qubit],
        instructions: Sequence[Instruction],
        modifiers: Sequence[GateModifier],
    ) -> Self: ...
    @property
    def name(self) -> str: ...
    @name.setter
    def name(self, name: str) -> None: ...
    @property
    def parameters(self) -> List[Expression]: ...
    @parameters.setter
    def parameters(self, parameters: Sequence[Expression]) -> None: ...
    @property
    def qubits(self) -> List[Qubit]: ...
    @qubits.setter
    def qubits(self, qubits: Sequence[Qubit]) -> None: ...
    @property
    def instructions(self) -> List[Instruction]: ...
    @instructions.setter
    def instructions(self, instructions: Sequence[Instruction]) -> None: ...
    @property
    def modifiers(self) -> List[GateModifier]: ...
    @modifiers.setter
    def modifiers(self, modifiers: Sequence[GateModifier]) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class MeasureCalibrationDefinition:
    def __new__(
        cls,
        qubit: Optional[Qubit],
        parameter: str,
        instructions: Sequence[Instruction],
    ) -> Self: ...
    @property
    def qubit(self) -> Optional[Qubit]: ...
    @qubit.setter
    def qubit(self, qubit: Optional[Qubit]) -> None: ...
    @property
    def parameter(self) -> str: ...
    @parameter.setter
    def parameter(self, parameter: str) -> None: ...
    @property
    def instructions(self) -> List[Instruction]: ...
    @instructions.setter
    def instructions(self, instructions: Sequence[Instruction]) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class CircuitDefinition:
    def __new__(
        cls,
        name: str,
        parameters: Sequence[str],
        qubit_variables: Sequence[str],
        instructions: Sequence[Instruction],
    ) -> Self: ...
    @property
    def name(self) -> str: ...
    @name.setter
    def name(self, name: str) -> None: ...
    @property
    def parameters(self) -> List[str]: ...
    @parameters.setter
    def parameters(self, parameters: Sequence[str]) -> None: ...
    @property
    def qubit_variables(self) -> List[str]: ...
    @qubit_variables.setter
    def qubit_variables(self, qubit_variables: Sequence[str]) -> None: ...
    @property
    def instructions(self) -> List[Instruction]: ...
    @instructions.setter
    def instructions(self, instructions: Sequence[Instruction]) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class Offset:
    def __new__(
        cls,
        offset: int,
        data_type: ScalarType,
    ) -> Self: ...
    @property
    def offset(self) -> int: ...
    @offset.setter
    def offset(self, offset: int) -> None: ...
    @property
    def data_type(self) -> ScalarType: ...
    @data_type.setter
    def data_type(self, data_type: ScalarType) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """

class Sharing:
    def __new__(
        cls,
        name: str,
        offsets: Sequence[Offset],
    ) -> Self: ...
    @property
    def name(self) -> str: ...
    @name.setter
    def name(self, name: str) -> None: ...
    @property
    def offsets(self) -> List[Offset]: ...
    @offsets.setter
    def offsets(self, offsets: Sequence[Offset]) -> None: ...

class Declaration:
    def __new__(cls, name: str, size: Vector, sharing: Optional[Sharing]) -> Self: ...
    @property
    def name(self) -> str: ...
    @name.setter
    def name(self, name: str) -> None: ...
    @property
    def size(self) -> Vector: ...
    @size.setter
    def size(self, vector: Vector) -> None: ...
    @property
    def sharing(self) -> Optional[Sharing]: ...
    @sharing.setter
    def sharing(self, sharing: Optional[Sharing]) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class Vector:
    def __new__(cls, data_type: ScalarType, length: int) -> Self: ...
    @property
    def data_type(self) -> ScalarType: ...
    @data_type.setter
    def data_type(self, data_type: ScalarType) -> None: ...
    @property
    def length(self) -> int: ...
    @length.setter
    def length(self, data_type: int) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """

@final
class ScalarType(Enum):
    Bit = "BIT"
    Integer = "INTEGER"
    Octet = "OCTET"
    Real = "REAL"
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """

@final
class AttributeValue:
    """
    A frame attribute value.

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
    def from_string(inner: str) -> "AttributeValue": ...
    @staticmethod
    def from_expression(inner: Expression) -> "AttributeValue": ...
    def as_string(self) -> Optional[str]: ...
    def to_string(self) -> str: ...
    def as_expression(self) -> Optional[Expression]: ...
    def to_expression(self) -> Expression: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """

class FrameDefinition:
    def __new__(
        cls,
        identifier: FrameIdentifier,
        attributes: Dict[str, AttributeValue],
    ) -> Self: ...
    @property
    def identifier(self) -> FrameIdentifier: ...
    @identifier.setter
    def identifier(self, identifier: FrameIdentifier) -> None: ...
    @property
    def attributes(self) -> Dict[str, AttributeValue]: ...
    @attributes.setter
    def attributes(self, identifier: Dict[str, AttributeValue]) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class FrameIdentifier:
    def __new__(cls, name: str, qubits: Sequence[Qubit]) -> Self: ...
    @property
    def name(self) -> str: ...
    @name.setter
    def name(self, name: str) -> None: ...
    @property
    def qubits(self) -> List[Qubit]: ...
    @qubits.setter
    def qubits(self, qubits: Sequence[Qubit]) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """

class Capture:
    def __new__(
        cls,
        blocking: bool,
        frame: FrameIdentifier,
        memory_reference: MemoryReference,
        waveform: WaveformInvocation,
    ) -> Self: ...
    @property
    def blocking(self) -> bool: ...
    @blocking.setter
    def blocking(self, blocking: bool) -> None: ...
    @property
    def frame(self) -> FrameIdentifier: ...
    @frame.setter
    def frame(self, frame: FrameIdentifier) -> None: ...
    @property
    def memory_reference(self) -> MemoryReference: ...
    @memory_reference.setter
    def memory_reference(self, memory_reference: MemoryReference) -> None: ...
    @property
    def waveform(self) -> WaveformInvocation: ...
    @waveform.setter
    def waveform(self, waveform: WaveformInvocation) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class Pulse:
    def __new__(
        cls,
        blocking: bool,
        frame: FrameIdentifier,
        waveform: WaveformInvocation,
    ) -> Self: ...
    @property
    def blocking(self) -> bool: ...
    @blocking.setter
    def blocking(self, blocking: bool) -> None: ...
    @property
    def frame(self) -> FrameIdentifier: ...
    @frame.setter
    def frame(self, frame: FrameIdentifier) -> None: ...
    @property
    def waveform(self) -> WaveformInvocation: ...
    @waveform.setter
    def waveform(self, waveform: WaveformInvocation) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class RawCapture:
    def __new__(
        cls,
        blocking: bool,
        frame: FrameIdentifier,
        duration: Expression,
        memory_reference: MemoryReference,
    ) -> Self: ...
    @property
    def blocking(self) -> bool: ...
    @blocking.setter
    def blocking(self, blocking: bool) -> None: ...
    @property
    def frame(self) -> FrameIdentifier: ...
    @frame.setter
    def frame(self, frame: FrameIdentifier) -> None: ...
    @property
    def duration(self) -> Expression: ...
    @duration.setter
    def duration(self, duration: Expression) -> None: ...
    @property
    def memory_reference(self) -> MemoryReference: ...
    @memory_reference.setter
    def memory_reference(self, memory_reference: MemoryReference) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class SetFrequency:
    def __new__(cls, frame: FrameIdentifier, frequency: Expression) -> Self: ...
    @property
    def frame(self) -> FrameIdentifier: ...
    @frame.setter
    def frame(self, frame: FrameIdentifier) -> None: ...
    @property
    def frequency(self) -> Expression: ...
    @frequency.setter
    def frequency(self, frequency: Expression) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class SetPhase:
    def __new__(cls, frame: FrameIdentifier, phase: Expression) -> Self: ...
    @property
    def frame(self) -> FrameIdentifier: ...
    @frame.setter
    def frame(self, frame: FrameIdentifier) -> None: ...
    @property
    def phase(self) -> Expression: ...
    @phase.setter
    def phase(self, phase: Expression) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class SetScale:
    def __new__(cls, frame: FrameIdentifier, phase: Expression) -> Self: ...
    @property
    def frame(self) -> FrameIdentifier: ...
    @frame.setter
    def frame(self, frame: FrameIdentifier) -> None: ...
    @property
    def scale(self) -> Expression: ...
    @scale.setter
    def scale(self, scale: Expression) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class ShiftFrequency:
    def __new__(cls, frame: FrameIdentifier, frequency: Expression) -> Self: ...
    @property
    def frame(self) -> FrameIdentifier: ...
    @frame.setter
    def frame(self, frame: FrameIdentifier) -> None: ...
    @property
    def frequency(self) -> Expression: ...
    @frequency.setter
    def frequency(self, frequency: Expression) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class ShiftPhase:
    def __new__(cls, frame: FrameIdentifier, phase: Expression) -> Self: ...
    @property
    def frame(self) -> FrameIdentifier: ...
    @frame.setter
    def frame(self, frame: FrameIdentifier) -> None: ...
    @property
    def phase(self) -> Expression: ...
    @phase.setter
    def phase(self, phase: Expression) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class SwapPhases:
    def __new__(cls, frame_1: FrameIdentifier, frame_2: FrameIdentifier) -> Self: ...
    @property
    def frame_1(self) -> FrameIdentifier: ...
    @frame_1.setter
    def frame_1(self, frame_1: FrameIdentifier) -> None: ...
    @property
    def frame_2(self) -> FrameIdentifier: ...
    @frame_2.setter
    def frame_2(self, frame_2: FrameIdentifier) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class GateError(ValueError):
    """An error that may occur when performing operations on a ``Gate``"""

    ...

@final
class GateModifier(Enum):
    Controlled = "CONTROLLED"
    Dagger = "DAGGER"
    Forked = "FORKED"
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """

class Gate:
    def __new__(
        cls,
        name: str,
        parameters: Sequence[Expression],
        qubits: Sequence[Qubit],
        modifiers: Sequence[GateModifier],
    ) -> Self: ...
    @property
    def name(self) -> str: ...
    @name.setter
    def name(self, name: str) -> None: ...
    @property
    def parameters(self) -> List[Expression]: ...
    @parameters.setter
    def parameters(self, parameters: Sequence[Expression]) -> None: ...
    @property
    def qubits(self) -> List[Qubit]: ...
    @qubits.setter
    def qubits(self, qubits: Sequence[Qubit]) -> None: ...
    @property
    def modifiers(self) -> List[GateModifier]: ...
    @modifiers.setter
    def modifiers(self, modifiers: Sequence[GateModifier]) -> None: ...
    def dagger(self) -> Self:
        """
        Returns a copy of the gate with the ``DAGGER`` modifier added to it.
        """
        ...
    def controlled(self, control_qubit: Qubit) -> Self:
        """
        Returns a copy of the gate with the ``CONTROLLED`` modifier added to it.
        """
    def forked(self, fork_qubit: Qubit, alt_params: Sequence[Expression]) -> Self:
        """
        Returns a copy of the gate with the ``FORKED`` modifier added to it.

        Raises a ``GateError`` if the number of provided alternate parameters don't
        equal the number of existing parameters.
        """
        ...
    def to_unitary_mut(self, n_qubits: int) -> NDArray[np.complex_]:
        """
        Lift a Gate to the full `n_qubits`-qubit Hilbert space.

        Returns a ``GateError` if any of the parameters of this gate are
        non-constant, if any of the qubits are variable, if the name of this
        gate is unknown, or if there are an unexpected number of parameters.
        """
        ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

@final
class PauliGate(Enum):
    I = "I"
    X = "X"
    Y = "Y"
    Z = "Z"
    @staticmethod
    def parse(input: str) -> "PauliGate":
        """
        Parses a ``PauliGate`` from a string. Raises a ``ParseEnumError`` if the
        string isn't a valid Pauli word.
        """
        ...

class PauliTerm:
    def __new__(
        cls,
        arguments: Sequence[Tuple[PauliGate, str]],
        expression: Expression,
    ) -> Self: ...
    @property
    def arguments(self) -> List[Tuple[PauliGate, str]]: ...
    @arguments.setter
    def arguments(self, word: Sequence[Tuple[PauliGate, str]]) -> None: ...
    @property
    def expression(self) -> Expression: ...
    @expression.setter
    def expression(self, expression: Expression) -> None: ...

class PauliSum:
    def __new__(cls, arguments: Sequence[str], terms: Sequence[PauliTerm]) -> Self: ...
    @property
    def arguments(self) -> List[str]: ...
    @arguments.setter
    def arguments(self, arguments: Sequence[str]) -> None: ...
    @property
    def terms(self) -> List[PauliTerm]: ...
    @terms.setter
    def terms(self, terms: Sequence[PauliTerm]) -> None: ...

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

    def inner(self) -> Union[List[List[Expression]], List[int], PauliSum]:
        """
        Returns the inner value of the variant. Raises a ``RuntimeError`` if inner data doesn't exist.
        """
        ...
    def is_matrix(self) -> bool: ...
    def is_permutation(self) -> bool: ...
    def is_pauli_sum(self) -> bool: ...
    def as_matrix(self) -> Optional[List[List[Expression]]]: ...
    def to_matrix(self) -> List[List[Expression]]: ...
    def as_permutation(self) -> Optional[List[int]]: ...
    def to_permutation(self) -> List[int]: ...
    def as_pauli_sum(self) -> Optional[PauliSum]: ...
    def to_pauli_sum(self) -> PauliSum: ...
    @staticmethod
    def from_matrix(inner: Sequence[Sequence[Expression]]) -> "GateSpecification": ...
    @staticmethod
    def from_permutation(inner: Sequence[int]) -> "GateSpecification": ...
    @staticmethod
    def from_pauli_sum(inner: PauliSum) -> "GateSpecification": ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """

class GateDefinition:
    def __new__(
        cls,
        name: str,
        parameters: Sequence[str],
        specification: GateSpecification,
    ) -> Self: ...
    @property
    def name(self) -> str: ...
    @name.setter
    def name(self, name: str) -> None: ...
    @property
    def parameters(self) -> List[str]: ...
    @parameters.setter
    def parameters(self, parameters: Sequence[str]) -> None: ...
    @property
    def specification(self) -> GateSpecification: ...
    @specification.setter
    def specification(self, specification: GateSpecification) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

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
    def is_placeholder(self) -> bool: ...
    def as_fixed(self) -> Optional[int]: ...
    def to_fixed(self) -> int: ...
    def as_variable(self) -> Optional[str]: ...
    def to_variable(self) -> str: ...
    def as_placeholder(self) -> Optional[QubitPlaceholder]: ...
    def to_placeholder(self) -> QubitPlaceholder: ...
    @staticmethod
    def from_fixed(inner: int) -> "Qubit": ...
    @staticmethod
    def from_variable(inner: str) -> "Qubit": ...
    @staticmethod
    def from_placeholder(inner: QubitPlaceholder) -> "Qubit": ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """

class QubitPlaceholder:
    """
    A qubit that can be used as a placeholder. Must be resolved before converting
    a program to valid Quil. See ``quil.program.Program#resolve_placeholders``.
    """

    def __new__(cls) -> Self: ...
    def __lt__(self, other: QubitPlaceholder) -> bool: ...

class Reset:
    def __new__(cls, qubit: Optional[Qubit]) -> Self: ...
    @property
    def qubit(self) -> Optional[Qubit]: ...
    @qubit.setter
    def qubit(self, qubit: Optional[Qubit]) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class Delay:
    def __new__(cls, duration: Expression, frame_names: Sequence[str], qubits: Sequence[Qubit]) -> Self: ...
    @property
    def duration(self) -> Expression: ...
    @duration.setter
    def duration(self, duration: Expression) -> None: ...
    @property
    def frame_names(self) -> List[str]: ...
    @frame_names.setter
    def frame_names(self, frame_names: Sequence[str]) -> None: ...
    @property
    def qubits(self) -> List[Qubit]: ...
    @qubits.setter
    def qubits(self, qubits: Sequence[Qubit]) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class Fence:
    def __new__(cls, qubits: Sequence[Qubit]) -> Self: ...
    @property
    def qubits(self) -> List[Qubit]: ...
    @qubits.setter
    def qubits(self, qubits: Sequence[Qubit]) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

@final
class PragmaArgument:
    """

    Variants:
        ``identifier``: A Pragma argument defined by a Quil identifier
        ``integer``: A Pragma argument defined by an integer

    Methods (for each variant):
        - is_*: Returns ``True`` if the inner type is of that variant.
        - as_*: Returns the inner data if it is the given variant, ``None`` otherwise.
        - to_*: Returns the inner data if it is the given variant, raises ``ValueError`` otherwise.
        - from_*: Creates a new ``PragmaArgument`` using an instance of the inner type for the variant.
    """

    def inner(self) -> Union[str, int]:
        """
        Returns the inner value of the variant. Raises a ``RuntimeError`` if inner data doesn't exist.
        """
        ...
    def is_identifier(self) -> bool: ...
    def is_integer(self) -> bool: ...
    def as_identifier(self) -> Optional[str]: ...
    def as_integer(self) -> Optional[int]: ...
    def to_identifier(self) -> str: ...
    def to_integer(self) -> int: ...
    @staticmethod
    def from_identifier(inner: str) -> "PragmaArgument": ...
    @staticmethod
    def from_integer(inner: int) -> "PragmaArgument": ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """

class Include:
    def __new__(cls, filename: str) -> Self: ...
    @property
    def filename(self) -> str: ...
    @filename.setter
    def filename(self, filename: str) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class Pragma:
    def __new__(cls, name: str, arguments: Sequence[PragmaArgument], data: Optional[str]) -> Self: ...
    @property
    def name(self) -> str: ...
    @name.setter
    def name(self, name: str) -> None: ...
    @property
    def arguments(self) -> List[PragmaArgument]: ...
    @arguments.setter
    def arguments(self, arguments: Sequence[PragmaArgument]) -> None: ...
    @property
    def data(self) -> Optional[str]: ...
    @data.setter
    def data(self, data: Optional[str]) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class Measurement:
    def __new__(cls, qubit: Qubit, target: Optional[MemoryReference]) -> Self: ...
    @property
    def qubit(self) -> Qubit: ...
    @qubit.setter
    def qubit(self, qubit: Qubit) -> None: ...
    @property
    def target(self) -> Optional[MemoryReference]: ...
    @target.setter
    def target(self, target: Optional[MemoryReference]) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class ParseMemoryReferenceError(ValueError):
    """Errors that may occur while parsing a ``MemoryReference``"""

class MemoryReference:
    def __new__(cls, name: str, index: int) -> Self: ...
    @staticmethod
    def parse(input: str) -> "MemoryReference":
        """
        Parses a ``MemoryReference`` from a string. Raises a ``ParseMemoryReference`` error if the
        string isn't a valid Quil memory reference.
        """
        ...
    @property
    def name(self) -> str: ...
    @name.setter
    def name(self, name: str) -> None: ...
    @property
    def index(self) -> int: ...
    @index.setter
    def index(self, index: int) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """

class Load:
    def __new__(cls, destination: MemoryReference, source: str, offset: MemoryReference) -> Self: ...
    @property
    def destination(self) -> MemoryReference: ...
    @destination.setter
    def destination(self, destination: MemoryReference) -> None: ...
    @property
    def source(self) -> str: ...
    @source.setter
    def source(self, source: str) -> None: ...
    @property
    def offset(self) -> MemoryReference: ...
    @offset.setter
    def offset(self, offset: MemoryReference) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class Store:
    def __new__(cls, destination: str, offset: MemoryReference, source: ArithmeticOperand) -> Self: ...
    @property
    def destination(self) -> str: ...
    @destination.setter
    def destination(self, destination: str) -> None: ...
    @property
    def offset(self) -> MemoryReference: ...
    @offset.setter
    def offset(self, offset: MemoryReference) -> None: ...
    @property
    def source(self) -> ArithmeticOperand: ...
    @source.setter
    def source(self, source: ArithmeticOperand) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class Waveform:
    def __new__(cls, matrix: Sequence[Expression], parameters: Sequence[str]) -> Self: ...
    @property
    def matrix(self) -> List[Expression]: ...
    @matrix.setter
    def matrix(self, matrix: Sequence[Expression]) -> None: ...
    @property
    def parameters(self) -> List[str]: ...
    @parameters.setter
    def parameters(self, parameters: Sequence[str]) -> None: ...

class WaveformDefinition:
    def __new__(cls, name: str, definition: Waveform) -> Self: ...
    @property
    def name(self) -> str: ...
    @name.setter
    def name(self, name: str) -> None: ...
    @property
    def definition(self) -> Waveform: ...
    @definition.setter
    def definition(self, definition: Waveform) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class WaveformInvocation:
    def __new__(cls, name: str, parameters: Dict[str, Expression]) -> Self: ...
    @property
    def name(self) -> str: ...
    @name.setter
    def name(self, name: str) -> None: ...
    @property
    def parameters(self) -> Dict[str, Expression]: ...
    @parameters.setter
    def parameters(self, parameters: Dict[str, Expression]) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """

class Label:
    def __new__(cls, target: Target) -> Self: ...
    @property
    def target(self) -> Target: ...
    @target.setter
    def target(self, target: Target) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

@final
class Target:
    """
    Represents a Quil target.

    Variants:
        ``fixed``: A fixed target defined by a Quil identifier
        ``placeholder``: A placeholder target that can be assigned a new name at a later time.

    Methods (for each variant):
        - is_*: Returns ``True`` if the inner type is of that variant.
        - as_*: Returns the inner data if it is the given variant, ``None`` otherwise.
        - to_*: Returns the inner data if it is the given variant, raises ``ValueError`` otherwise.
        - from_*: Creates a new ``PragmaArgument`` using an instance of the inner type for the variant.
    """

    def __new__(cls, inner: Union[str, TargetPlaceholder]) -> "Target": ...
    @staticmethod
    def from_fixed(inner: str) -> "Target": ...
    @staticmethod
    def from_placeholder(inner: TargetPlaceholder) -> "Target": ...
    def is_fixed(self) -> bool: ...
    def is_placeholder(self) -> bool: ...
    def as_fixed(self) -> Optional[str]: ...
    def as_placeholder(self) -> Optional[TargetPlaceholder]: ...
    def to_fixed(self) -> str: ...
    def to_placeholder(self) -> TargetPlaceholder: ...
    def inner(self) -> Union[str, TargetPlaceholder]: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """

class TargetPlaceholder:
    """
    A placeholder target that must be assigned a fixed name before creating a program
    with valid quil.

    See ``quil.program.Program#resolve_placeholders`` for more information.
    """

    def __new__(cls, base_target: str) -> Self: ...
    @property
    def base_label(self) -> str: ...

class Jump:
    def __new__(cls, target: Target) -> Self: ...
    @property
    def target(self) -> Target: ...
    @target.setter
    def target(self, target: Target) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class JumpWhen:
    def __new__(cls, target: Target, condition: MemoryReference) -> Self: ...
    @property
    def target(self) -> Target: ...
    @target.setter
    def target(self, target: Target) -> None: ...
    @property
    def condition(self) -> MemoryReference: ...
    @condition.setter
    def condition(self, condition: MemoryReference) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""

class JumpUnless:
    def __new__(cls, target: Target, condition: MemoryReference) -> Self: ...
    @property
    def target(self) -> Target: ...
    @target.setter
    def target(self, target: Target) -> None: ...
    @property
    def condition(self) -> MemoryReference: ...
    @condition.setter
    def condition(self, condition: MemoryReference) -> None: ...
    def to_quil(self) -> str:
        """
        Attempt to convert the instruction to a valid Quil string. Raises
        an exception if the instruction can't be converted to valid Quil.
        """
        ...
    def to_quil_or_debug(self) -> str:
        """
        Convert the instruction to a Quil string. If any part of the instruction can't
        be converted to valid Quil, it will be printed in a human-readable debug format.
        """
    def __deepcopy__(self, _: Dict) -> Self:
        """Creates and returns a deep copy of the class. If the instruction contains any ``QubitPlaceholder`` or ``TargetPlaceholder``, then they will be replaced with new placeholders so resolving them in the copy will not resolve them in the original.  Should be used by passing an instance of the class to ``copy.deepcopy``"""
    def __copy__(self) -> Self:
        """Returns a shallow copy of the class."""
