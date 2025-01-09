__all__ = [
    'IdentifierValidationError',
    'validate_identifier',
    'validate_user_identifier',
]

class IdentifierValidationError(ValueError):
    """Errors that may occur when validating a Quil identifier."""

    ...

def validate_identifier(ident: str):
    """Raises an ``IdentifierValidationError` if ident isn't a valid Quil identifier."""
    ...

def validate_user_identifier(ident: str):
    """Raises an ``IdentifierValidationError` if ident is reserved keyword or isn't a valid Quil identifier."""
    ...
