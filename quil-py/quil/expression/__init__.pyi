from enum import Enum
from typing import Dict, final, Sequence, Optional, Union

from quil.instructions import MemoryReference

class EvaluationError(ValueError):
    """Error that may occur while evaluation an ``Expression``"""

class ParseExpressionError(ValueError):
    """Error that may occur while parsing an ``Expression``"""

@final
class Expression:
    """
    A Quil expression.

    Variants:
        ``address``: An address defined by a ``MemoryReference``.
        ``function_call``: A ``FunctionCall``.
        ``infix``: An ``InfixExpression``.
        ``number``: A number defined as a ``complex``.
        ``pi``: The constant `pi`. No inner data.
        ``prefix``: A ``PrefixExpression``.
        ``variable``: A variable defined as a ``str``.

    As seen above, some variants contain inner data that fully specify the expression.
    For example, the ``number`` variant contains an ``int``. This is in contrast to variants like
    ``pi`` that have no inner data because they require none to fully specify the expression.
    This difference is important for determining which methods are available for each variant.

    Methods (for each variant):
        ``is_*``: Returns ``True`` if the expression is that variant, ``False`` otherwise.

        If the variant has inner data:
            ``as_*``: returns the inner data if it is the given variant, ``None`` otherwise.
            ``to_*``: returns the inner data if it is the given variant, raises ``ValueError`` otherwise.
            ``from_*``: Creates a new ``Expression`` of the given variant from an instance of the inner type.

        If the variant doesn't have inner data (e.g. ``pi``)
            ``new_*``: Creates a new ``Expression`` for the variant
    """

    def inner(
        self,
    ) -> Union[
        MemoryReference,
        FunctionCallExpression,
        InfixExpression,
        int,
        PrefixExpression,
        str,
    ]:
        """
        Returns the inner value of the variant. Raises a ``RuntimeError`` if inner data doesn't exist.
        """
        ...
    @staticmethod
    def parse(quil: str) -> Expression:
        """
        Parses an ``Expression`` from a string. Raises a ``ParseExpressionError`` if the string
        isn't a valid Quil expression.
        """
    def is_address(self) -> bool: ...
    def is_function_call(self) -> bool: ...
    def is_infix(self) -> bool: ...
    def is_number(self) -> bool: ...
    def is_pi(self) -> bool: ...
    def is_prefix(self) -> bool: ...
    def is_variable(self) -> bool: ...
    @staticmethod
    def new_pi() -> "Expression": ...
    @staticmethod
    def from_address(memory_reference: MemoryReference) -> "Expression": ...
    @staticmethod
    def from_function_call(function_call: FunctionCallExpression) -> "Expression": ...
    @staticmethod
    def from_infix(infix: InfixExpression) -> "Expression": ...
    @staticmethod
    def from_number(number: complex) -> "Expression": ...
    @staticmethod
    def from_prefix(prefix: PrefixExpression) -> "Expression": ...
    @staticmethod
    def from_variable(variable: str) -> "Expression": ...
    def as_address(self) -> Optional[MemoryReference]: ...
    def to_address(self) -> MemoryReference: ...
    def as_function_call(self) -> Optional[FunctionCallExpression]: ...
    def to_function_call(self) -> FunctionCallExpression: ...
    def as_infix(self) -> Optional[InfixExpression]: ...
    def to_infix(self) -> InfixExpression: ...
    def as_number(self) -> Optional[complex]: ...
    def to_number(self) -> complex: ...
    def as_prefix(self) -> Optional[PrefixExpression]: ...
    def to_prefix(self) -> PrefixExpression: ...
    def as_variable(self) -> Optional[str]: ...
    def to_variable(self) -> str: ...
    def simplify(self):
        """
        Simplify the expression as much as possible, in-place.
        """
        ...
    def into_simplified(self) -> "Expression":
        """
        Return a simplified copy of the expression.
        """
    def evaluate(
        self,
        variables: Dict[str, complex],
        memory_references: Dict[str, Sequence[float]],
    ) -> complex:
        """
        Evaluate an expression, expecting that it may be fully reduced to a single complex number.
        If it cannot be reduced to a complex number, raises an ``EvaluationError``.
        """
        ...
    def substitute_variables(
        self, variable_values: Dict[str, "Expression"]
    ) -> "Expression":
        """
        Returns a copy of the expression where every matching variable in `variable_values` is
        replaced by the corresponding expression.
        """
        ...
    def to_real(self) -> float:
        """
        If this is a number with imaginary part "equal to" zero (of _small_ absolute value), return
        that number. Otherwise, raises an ``EvaluationError``
        """
    def __add__(self, other: "Expression") -> "Expression": ...
    def __sub__(self, other: "Expression") -> "Expression": ...
    def __mul__(self, other: "Expression") -> "Expression": ...
    def __truediv__(self, other: "Expression") -> "Expression": ...

class FunctionCallExpression:
    @staticmethod
    def __new__(
        cls, function: ExpressionFunction, expression: Expression
    ) -> "FunctionCallExpression": ...
    @property
    def function(self) -> ExpressionFunction: ...
    @function.setter
    def function(self, function: ExpressionFunction): ...
    @property
    def expression(self) -> Expression: ...
    @expression.setter
    def expression(self, expression: Expression): ...

class InfixExpression:
    @staticmethod
    def __new__(cls, left: Expression, operator: InfixOperator, right: Expression): ...
    @property
    def left(self) -> Expression: ...
    @left.setter
    def left(self, expression: Expression): ...
    @property
    def operator(self) -> InfixOperator: ...
    @operator.setter
    def operator(self, operator: InfixOperator): ...
    @property
    def right(self) -> Expression: ...
    @right.setter
    def right(self, expression: Expression): ...

class PrefixExpression:
    @staticmethod
    def __new__(cls, operator: PrefixOperator, expression: Expression): ...
    @property
    def operator(self) -> PrefixOperator: ...
    @operator.setter
    def operator(self, operator: PrefixOperator): ...
    @property
    def expression(self) -> Expression: ...
    @expression.setter
    def expression(self, expression: Expression): ...

@final
class ExpressionFunction(Enum):
    Cis = "CIS"
    Cosine = "COSINE"
    Exponent = "EXPONENT"
    Sine = "SINE"
    SquareRoot = "SQUAREROOT"

@final
class PrefixOperator(Enum):
    Plus = "PLUS"
    Minus = "MINUS"

@final
class InfixOperator(Enum):
    Caret = "CARET"
    Plus = "PLUS"
    Minus = "MINUS"
    Slash = "SLASH"
    Star = "STAR"
