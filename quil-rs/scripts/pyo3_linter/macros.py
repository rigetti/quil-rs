"""
Protocols for parsing Rust ``macro_rules!`` invocations.
"""

from dataclasses import dataclass
from pathlib import Path
from typing import (
    Callable,
    Protocol,
    TypeAlias,
    runtime_checkable,
)
import re

from .package import Package, SubmoduleRegistry
from .reader import Line, Lines


@dataclass
class MacroContext:
    """Context passed to macro handlers during extraction."""

    path: Path
    lines: Lines
    annotated: Package
    exported: Package
    mod_context: list[str]
    registry: SubmoduleRegistry


@runtime_checkable
class MacroHandler(Protocol):
    """Protocol for handling macro invocations during item extraction.

    Implementations should:
    - Return True from `matches` if the line contains the macro they handle
    - Process the macro in `handle`, updating annotated/exported as needed
    """

    def matches(self, line: Line) -> bool:
        """Return True if this handler should process the given line."""
        ...

    def handle(self, ctx: MacroContext, module: str | None = None) -> None:
        """Process the macro invocation, updating ctx.annotated/ctx.exported.

        If this macro were matched within a `#[pymodule]` annotated function,
        then the `module` parameter will be set to the inferred name of the module.
        """
        ...


MacroHandlers: TypeAlias = list[MacroHandler]


@dataclass
class PatternHandler(MacroHandler):
    """A common type of `MacroHandler` that uses a regular expression to match lines."""

    pattern: re.Pattern
    f: Callable[[MacroContext, str | None], None]

    def matches(self, line: Line) -> bool:
        return re.search(self.pattern, line.text) is not None

    def handle(self, ctx: MacroContext, module: str | None = None):
        self.f(ctx, module)


def macro_handler(
    pattern: re.Pattern | str,
) -> Callable[[Callable[[MacroContext, str | None], None]], MacroHandler]:
    """Wrap a function that parses a macro when the current line matches the given pattern."""

    if isinstance(pattern, str):
        pattern = re.compile(pattern)

    def _macro_handler(f: Callable[[MacroContext, str | None], None]) -> MacroHandler:
        return PatternHandler(pattern, f)

    return _macro_handler
