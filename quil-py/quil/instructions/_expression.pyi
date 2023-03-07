class Expression:
    """
    A Quil expression.

    Variants:
        ``address``: A memory reference
        ``function_call``: A function call
        ``infix``: An infix expression
        ``number``: A number
        ``pi``: The constant `pi`
        ``prefix``: A prefix expression
        ``variable``: A variable

    Methods (for each variant):
        ``is_*``: Returns ``True`` if the expression is that variant, ``False`` otherwise.

        If the variant has inner data:
            ``as_*``: returns the inner data if it is the given variant, ``None`` otherwise.
            ``to_*``: returns the inner data if it is the given variant, raises ``ValueError`` otherwise.
            ``from_*``: Creates a new ``Expression`` of the given variant from an instance of the inner type.

        If the variant doesn't have inner data (ie. ``pi``)
            ``new_*``: Creates a new ``Expression`` for the variant
    """

    def is_address(self) -> bool: ...
    def is_function_call(self) -> bool: ...
    def is_infix(self) -> bool: ...
    def is_number(self) -> bool: ...
    def is_pi(self) -> bool: ...
    def is_prefix(self) -> bool: ...
    def is_variable(self) -> bool: ...
    def as_number(self) -> complex: ...
    def to_number(self) -> complex: ...
    def as_variable(self) -> str: ...
    def to_variable(self) -> str: ...

    # TODO: Define the rest of the methods as part of building out the API for each variant
