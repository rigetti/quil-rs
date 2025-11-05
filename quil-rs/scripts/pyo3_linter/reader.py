"""
These types and functions are used to read over lines of Rust source code.
"""

from collections import deque
from dataclasses import dataclass
from itertools import chain
from pathlib import Path
from typing import Iterator, TypeAlias
from typing_extensions import Self
import logging
import re

logger = logging.getLogger(__name__)


@dataclass(frozen=True, slots=True, order=True)
class Line:
    """A line of text with the line number of the source it came from."""

    num: int
    text: str

    def __contains__(self, s: str) -> bool:
        return s in self.text

    def __iter__(self) -> Iterator:
        yield self.num
        yield self.text

    def is_blank(self) -> bool:
        return self.text.strip() == ""

    def splitat(self, index: int) -> tuple[Self, Self]:
        cls = self.__class__
        return cls(self.num, self.text[:index]), cls(self.num, self.text[index:])

    def partition(self, sep: str) -> tuple[Self, Self]:
        if (index := self.text.find(sep)) >= 0:
            return self.splitat(index)
        return self.splitat(len(self.text))


Lines: TypeAlias = Iterator[Line]


def iter_delim(
    lines: Iterator[Line],
    start="()",
    stop: str | None = None,
    /,
    first: Line | None = None,
) -> Iterator[Line]:
    """Yield lines until reading the closing delimiter.

    Lines will be yielded in the same manner as the input iterator supplies them,
    with the exception of the final line, which is split in two at the closing delimiter:
    the content before and including the closing delimiter is yielded first,
    and then any remaining content on the line is yielded after.
    Lines before and including the opening delimiter are yielded in whole,
    as this presumes you've already discovered your opening delimiter
    and are starting at that point anyway.

    If given, the initial line `first` is prepended to the `rest` of the lines,
    and generally should contain the opening delimiter.
    """
    if stop is None:
        start, stop = start

    if first is not None:
        lines = chain((first,), lines)

    reg = re.compile(rf"(?P<add>{re.escape(start)})|(?P<sub>{re.escape(stop)})")
    count = 0
    while line := next(lines, None):
        for m in reg.finditer(line.text):
            count += 1 if m.lastgroup == "add" else -1
            if count <= 0:
                yield from line.splitat(m.end())
                return
        yield line

    if count != 0 or line is not None:
        raise ValueError(
            f"mismatched delimiters: EOF when count('{start}') - count('{stop}') = {count}"
        )


def skip(
    lines: Iterator[Line],
    start: str = "()",
    stop: str | None = None,
    /,
    first: Line | None = None,
) -> Line:
    """Skip delimited text, returning only the final line, following closing delimiter."""
    return deque(iter_delim(lines, start, stop, first=first), maxlen=1).pop()


def join_lines(lines: Lines, /, first: Line | None = None, sep: str = " ") -> Line:
    """Collect lines into a single line, separated with a delimiter."""

    first = next(lines, first)
    if first is None:
        raise ValueError("no lines to iterate")
    return Line(first.num, sep.join(l.text for l in chain((first,), lines)))


MARKERS = re.compile(r"(?P<cblock>/\*)|(?P<cline>//)|(?P<macrodef>macro_rules!)")


def read_content(lines: Lines) -> Lines:
    """Iterate over lines, skipping empty lines, comments, and `macro_rules!` definitions."""

    while line := next(lines, None):
        while m := MARKERS.search(line.text):
            if m.lastgroup == "cblock":
                logger.debug(f"Skipping block comment ({line}).")
                line, rest = line.splitat(m.start())
                if not line.is_blank():
                    yield line
                line = skip(lines, "/*", "*/", first=rest)

            elif m.lastgroup == "cline":
                logger.debug(f"Skipping line comment ({line}).")
                line, _ = line.splitat(m.start())

            elif m.lastgroup == "macrodef":
                logger.debug(f"Skipping macro def ({line}).")
                line, rest = line.splitat(m.start())
                if not line.is_blank():
                    yield line
                line = skip(lines, "{}", first=rest)

        if not line.is_blank():
            yield line


def read_file(path: Path) -> Lines:
    logger.info(f"Processing {path}")
    with path.open(encoding="utf-8") as f:
        yield from read_content(
            Line(i, line.strip()) for i, line in enumerate(f, start=1)
        )
