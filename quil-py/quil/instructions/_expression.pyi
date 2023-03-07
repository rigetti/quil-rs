from typing import Optional

class Expression:
    """
    A Quil expression.

    Variants:
        ``address``: An address defined by a ``MemoryReference``.
        ``function_call``: A ``FunctionCall``.
        ``infix``: An ``InfixExpression``.
        ``number``: A number defined as an ``int``.
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

    def is_address(self) -> bool: ...
    def is_function_call(self) -> bool: ...
    def is_infix(self) -> bool: ...
    def is_number(self) -> bool: ...
    def is_pi(self) -> bool: ...
    def is_prefix(self) -> bool: ...
    def is_variable(self) -> bool: ...
    def as_number(self) -> Optional[complex]: ...
    def to_number(self) -> complex: ...
    def as_variable(self) -> Optional[str]: ...
    def to_variable(self) -> str: ...

    # TODO: Define the rest of the methods as part of building out the API for each variant
