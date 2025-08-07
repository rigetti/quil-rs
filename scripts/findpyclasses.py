"""
This script is a lint helper for our PyO3 wrappers.

Given a starting directory, it recursively searches it for *.rs files,
and attempts to extract pyo3 annotations and exports from the source files.
Afterward, it may print some messages about potential mistakes.

Note that since this works purely on the textual source,
it may make mistakes, especially among elements that share the same name.
In particular, if there are multiple `#[pyfunction]` with the same Rust name,
and at least one of them is added to a module, this script won't know which it is was.
As a result, it just prints all the functions it finds and a guess about the export.
"""

from collections import defaultdict, deque
from collections.abc import ItemsView, KeysView, MutableMapping, MutableSet, ValuesView
from dataclasses import dataclass, field, replace
from itertools import accumulate, chain
from pathlib import Path
from typing import Iterator, TypeAlias, TypeVar, TYPE_CHECKING, Self, overload

import argparse
import enum
import logging
import re

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger()


def main():
    args = get_parser().parse_args()

    if args.log_level is not None:
        logger.setLevel(args.log_level)

    annotated, exported = Package(), Package()
    for path in args.base.rglob("*.rs"):
        extract_items_from_file(path, annotated, exported)
    guess_function_modules(annotated, exported)

    if args.show_mistakes:
        print_possible_mistakes(annotated, exported)
    if args.show_package:
        print_package_info(annotated)


def get_parser() -> argparse.ArgumentParser:
    DEFAULT_PATH = Path("./src")

    parser = argparse.ArgumentParser(description="Lint quil-py source files.")

    parser.add_argument(
        "-b",
        "--base",
        metavar="PATH",
        type=Path,
        help=f"the base path to source files (default: '{DEFAULT_PATH}')",
        default=DEFAULT_PATH,
    )

    parser.add_argument(
        "-m",
        "--show-mistakes",
        action="store_true",
        default="",
        help="Show likely mistakes (default: enabled).",
    )

    parser.add_argument(
        "-M",
        "--no-show-mistakes",
        action="store_false",
        dest="show_mistakes",
        help="Don't show likely mistakes.",
    )

    parser.add_argument(
        "-p",
        "--show-package",
        action="store_true",
        help="Show package details (default: disabled).",
    )

    parser.add_argument(
        "-P",
        "--no-show-package",
        action="store_false",
        dest="show_package",
        default="",
        help="Don't show package details.",
    )

    parser.add_argument(
        "--log-level",
        help="set the logger level",
        choices=tuple(
            logging.getLevelName(level)
            for level in (logging.DEBUG, logging.INFO, logging.WARNING, logging.ERROR)
        ),
    )

    return parser


def guess_function_modules(annotated: "Package", exported: "Package"):
    """Try to match functions to modules.

    The ``pyfunction`` annotation doesn't include a ``module`` property,
    so we can only guess the module it belongs to if we can find a matching export
    (that is, a ``pymodule`` it appears to have been added to).
    """
    for func in (f for f in annotated["builtins"] if f.kind == "fn"):
        matching = (
            name
            for name, mod in exported.items()
            for item in mod
            if (item.kind == "fn" and item.rust_name == func.rust_name)
        )
        if modname := next(matching, None):
            logger.info(f"Guessing module '{modname}' for function: {func}")
            exported[modname].add(func)
        else:
            logger.warning(f"No module found for function: {func}")


def print_possible_mistakes(annotated: "Package", exported: "Package") -> bool:
    logger.debug("Checking that annotated items are added to the correct module.")
    found_issues = False

    found_exports = {"builtins", "quil", "._quil"}
    for module, anno in sorted(annotated.items()):
        if module == "builtins":
            continue

        # First check if we found the module itself.
        logger.debug(f"Looking for `#[pymodule]` matching '{module}'")
        expm = exported.get(module)
        if expm is None:
            found_issues = True
            print(
                f"Module '{module}' does not appear to be exported",
                f"  Module found in: {anno.path}",
                f"  Items in module:",
                sep="\n",
            )
            for item in anno:
                print(f"    {item} (props: {item.props}, text: {item.line})")
            print()
            continue

        parts = module.split(".")
        found_exports.add(parts[0])
        found_exports |= set(accumulate(parts, lambda parent, sub: f"{parent}.{sub}"))

        # When parsing a ``pymodule``, we know the Rust name,
        # but if it has a different Python name, we only know when we find the ``pyclass``,
        # so here we map Rust names to items, and if we can't find an item in an export,
        # we'll try to find it by Rust name in this collection,
        # and if it has a matching `kind`, we'll assume it's the same item.
        rust_to_py_anno = {item.rust_name: item for item in anno}
        logger.debug(
            f"Checking for items added to '{module}' but not annotated as such."
        )
        for item in sorted(expm):
            if (
                item not in anno
                and (anno_item := rust_to_py_anno.get(item.rust_name))
                and anno_item.kind.is_py_like(item.kind)
            ):
                replacement = replace(item, python_name=anno_item.python_name)
                logger.debug(f"Replacing {item} with {replacement} in '{module}'.")
                expm.discard(item)
                expm.add(replacement)
                item = replacement

            if not (item in anno or item.kind is Kind.Function):
                found_issues = True
                print(
                    f"  Wrong or missing module annotation for '{module}.{item.python_name}'",
                    f"     ({item})",
                    sep="\n",
                )

        logger.debug(f"Checking that items marked to belong to '{module}' are added.")
        for item in sorted(anno):
            if item.python_name.startswith("Py"):
                print(
                    f"  Suspicious name for '{module}.{item.python_name}'",
                    f"     ({item})",
                    (
                        "     "
                        "hint: did you forget a '#[pyo3(name = "
                        f'"{item.python_name.strip("Py")}" attribute?'
                    ),
                    sep="\n",
                )

            if item not in expm:
                found_issues = True
                print(
                    f"  Couldn't find export for '{module}.{item.python_name}'",
                    f"     ({item})",
                    sep="\n",
                )

            if not item.has_stub:
                found_issues = True
                print(
                    f"  Didn't find a stub annotation for '{module}.{item.python_name}'",
                    f"     ({item})",
                    sep="\n",
                )

    logger.debug(f"Found exports: {', '.join(found_exports)}")
    logger.debug(f"Expected exported modules: {', '.join(exported.keys())}")
    for module in exported.keys() - found_exports:
        found_issues = True
        print(f"No annotations found for module '{module}'")

    return found_issues


def print_package_info(annotated: "Package") -> None:
    for module, anno in sorted(annotated.items()):
        print(f"\n---- {module} Enums ----")
        for item in sorted(anno):
            if item.kind is Kind.Enumeration:
                print(
                    f"{module}.{item.python_name} | {item.rust_name} @ {item.path}:{item.line.num}"
                )

        print(f"\n---- {module} Structs ----")
        for item in sorted(anno):
            if item.kind is Kind.Struct:
                print(
                    f"{module}.{item}\n",
                    f"  {item.props}",
                    sep="",
                )
                if "frozen" in item.props and "hash" not in item.props:
                    print("  ** item is frozen, but not hash **")
                else:
                    print()


###############################################################################
# Supporting code starts here.
###############################################################################


class Kind(enum.Enum):
    Struct = "struct"
    Enumeration = "enum"
    Function = "fn"
    # When discovered in a pymodule, we don't know the Rust type.
    Class = "class"
    Error = "error"

    def is_py_like(self, py: Self) -> bool:
        match (self, py):
            case (Kind.Enumeration | Kind.Struct, Kind.Class | Kind.Error):
                return True
            case (x, y) if x is y:
                return True
        return False


class PyO3Props(dict[str, str | bool]):
    """Properties found in a PyO3 attribute, such as ``module = "name"`` or ``ord``.

    Use `parse` and `__str__` to convert to/from a string.
    You can check for the presence of a property with ordinary ``"prop" in props`` syntax,
    but you should prefer the type-specific versions which raise `TypeError`s
    if the property exists but doesn't match the expected type::

    - `gets(key)` returns a string-valued property
    - `is_(key)` checks for a boolean-valued property
    """

    def gets(self, key: str, default: str | None = None) -> str:
        if not (default is None or isinstance(default, str)):
            raise TypeError("if given, default must be str")
        if (value := super().get(key, default)) is not None and isinstance(value, str):
            return value
        if key in self:
            raise TypeError(f"expected {key} to be str, not {type(value)}")
        raise KeyError(key)

    def is_(self, key: str, default: bool = False) -> bool:
        if not (default is None or isinstance(default, bool)):
            raise TypeError("if given, default must be bool")
        if (value := super().get(key, default)) is not None and isinstance(value, bool):
            return value
        if key in self:
            raise TypeError(f"expected {key} to be bool, not {type(value)}")
        raise KeyError(key)

    @classmethod
    def parse(cls, prop_str: str) -> Self:
        """Convert props used in a ``#[whatever(arg1, arg2 = "val")]`` annotation.
        Note that the `prop_str` should just be the part between the parentheses.

        For the above example, the return value is ``{"arg1": True, "arg2": "val"}``.
        """

        result = cls()
        parts = [p.strip() for p in prop_str.split(",")]
        for part in parts:
            if "=" in part:
                key, val = map(str.strip, part.split("=", 1))
                if val.startswith('"') and val.endswith('"'):
                    val = val[1:-1]
                result[key] = val
            else:
                result[part] = True
        return result

    def __str__(self) -> str:
        """Convert back into a string."""
        return ", ".join(
            f'{k} = "{v}"' if isinstance(v, str) else f"{k}" for k, v in self.items()
        )


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


@dataclass(frozen=True, order=True)
class Item:
    """Something that can be included in a Module (e.g., a class, function, exception, etc.)."""

    rust_name: str
    python_name: str
    kind: Kind = field(compare=False, hash=False)
    path: Path = field(compare=False, hash=False)
    line: Line = field(compare=False, hash=False)
    props: PyO3Props = field(compare=False, hash=False, default_factory=PyO3Props)
    has_stub: bool = field(compare=False, hash=False, default=False)

    def __str__(self) -> str:
        name = self.rust_name
        if self.rust_name != self.python_name:
            name = f"{name} (py: {self.python_name})"
        return f"{self.kind.value} '{name}' in {self.path}@{self.line.num}"


def test_item():
    """Verify that hash and equality checks work as expected.

    Namely, we want to compare items on their Python+Rust names,
    but ignore the rest of their metadata.
    """
    i1 = Item("A", "B", "struct", Path("C"), Line(1, "struct B();"))
    i2 = Item("A", "B", "struct", Path("c"), Line(2, "struct B();"))
    i3 = Item("A", "b", "struct", Path("C"), Line(1, "struct b();"))
    assert hash(i1) == hash(i2)
    assert hash(i1) != hash(i3)
    assert i1 in {i2}
    assert i2 in {i1}
    assert i1 not in {i3}
    assert i2 not in {i3}
    assert i3 not in {i1, i2}


@dataclass
class Module(MutableSet[Item]):
    """A collection of `Item`s in the same Python module."""

    _items: set[Item] = field(default_factory=set)
    submodule: bool = True
    path: Path = Path(".")
    line_num: int = -1
    props: PyO3Props = field(default_factory=PyO3Props)

    def __contains__(self, key: object) -> bool:
        return key in self._items

    def __iter__(self) -> Iterator[Item]:
        return self._items.__iter__()

    def __len__(self) -> int:
        return self._items.__len__()

    def discard(self, value: Item) -> None:
        return self._items.discard(value)

    def add(self, item: Item) -> None:
        logger.info(f"Adding {item}")
        self._items.add(item)


_T = TypeVar("_T")


@dataclass
class Package(MutableMapping[str, Module]):
    """A collection of `Module`s."""

    _modules: defaultdict[str, Module] = field(
        default_factory=lambda: defaultdict(Module)
    )

    def __getitem__(self, name: str, /) -> Module:
        if name not in self._modules:
            logger.info(f"Creating module '{name}'.")
        return self._modules[name]

    @overload  # type: ignore
    def get(self, key: str, default: None = None, /) -> Module | None: ...
    @overload
    def get(self, key: str, default: Module, /) -> Module: ...
    @overload
    def get(self, key: str, default: _T, /) -> Module | _T: ...

    def get(self, name: str, default: _T | None = None, /) -> Module | _T | None:
        return self._modules.get(name, default)

    def __setitem__(self, name: str, module: Module, /) -> None:
        logger.info(f"Setting module '{name}'.")
        self._modules[name] = module

    def __delitem__(self, name: str, /) -> None:
        del self._modules[name]

    def __contains__(self, name: object) -> bool:
        if not isinstance(name, str):
            raise TypeError(f"Package keys must be strings, not {type(name)}")
        return name in self._modules

    def __iter__(self) -> Iterator[str]:
        return self._modules.__iter__()

    def __len__(self) -> int:
        return self._modules.__len__()

    def keys(self) -> KeysView[str]:
        return self._modules.keys()

    def values(self) -> ValuesView[Module]:
        return self._modules.values()

    def items(self) -> ItemsView[str, Module]:
        return self._modules.items()


def _meta(regex: str) -> str:
    r"""Create an RE to match content of an attribute; i.e., match ``#[{regex}]``."""
    return rf"#\[\s*?{regex}\s*?\]"


def _cfg(regex: str, cond: str = r"[^,]+") -> str:
    r"""Create an RE to match content of a configurable attribute;
    i.e., match either ``#[{regex}]`` or ``#[cfg_attr({cond}, {regex})]``.
    """
    return _meta(rf"(?:cfg_attr\s*?\({cond},\s*?{regex}\s*?\)|{regex})")


CONFIG_ATTR = r'(?:cfg_attr\(feature\s*?=\s*?"python",\s*?)?'
PYITEM_RE = re.compile(_cfg(rf"(?:pyo3::)?(?:pyclass|pyfunction)\s*(?:\((.*?)\))?"))
PYMODULE_RE = re.compile(_cfg(f"(?:pyo3::)?pymodule"))
PYMETHODS_RE = re.compile(_cfg(r"(?:pyo3::)?pymethods"))
PYO3_RE = re.compile(_cfg(r"pyo3\(([^)]+)\)"))
ITEM_RE = re.compile(
    r"(?P<kind>struct|enum|fn)\s+(?:\(\s*)?(?P<rust_name>[A-Za-z_][A-Za-z0-9_]*)"
)
ADD_ERROR_RE = re.compile(
    r'\.add\(\s*"(?P<python_name>[^"]*?)"[^<]*<(?P<rust_name>[^>]*?)>'
)
ADD_CLASS_RE = re.compile(
    r"\.add_class::<(?P<rust_name>[^>]*?)>|"
    r"\.add_function\(wrap_pyfunction!\((?P<func_name>[^>]*?),.*\)"
)
STUB_GEN_RE = re.compile(
    _cfg(
        r"gen_stub_py(class(?:(?:_complex)?_enum)?|methods|function)(?:\((\s*?[^)]+)\))?"
    )
)


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
    with path.open() as f:
        yield from read_content(
            Line(i, line.strip()) for i, line in enumerate(f, start=1)
        )


def _pyitem(
    annotated: Package, path: Path, lines: Lines, pyclass_match: re.Match
) -> tuple[Module, Item] | tuple[None, None]:
    """Process #[pyclass] and #[pyfunction] annotated items,
    adding them as annotated package details.

    On success, this returns ``(module, item)``,
    where ``module`` is the `Module` where `Item` ``item`` is added.
    """

    props = PyO3Props.parse(pyclass_match.group(1) or pyclass_match.group(2) or "")

    # Skip over additional `#[...]` lines, looking for more props.
    while (line := next(lines, None)) and line.text.strip().startswith("#["):
        line = join_lines(iter_delim(lines, "[]", first=line))
        if m := PYO3_RE.search(line.text):
            props |= PyO3Props.parse(m.group(1) or m.group(2))

    if line is None:
        logger.error(f"Unexpected EOF processing {path}")
        return None, None

    if not (item_match := ITEM_RE.search(line.text)):
        logger.error(f"Failed to extract item details for {path}@{line}")
        return None, None

    kind: Kind = Kind(item_match.group("kind"))
    rust_name: str = item_match.group("rust_name")

    if not isinstance(rust_name, str):
        logger.error(f"Couldn't find Rust name for {path}@{line}: {kind}")
        return None, None

    item = Item(
        kind=kind,
        python_name=props.gets("name", rust_name),
        rust_name=rust_name,
        path=path,
        line=line,
        props=props,
    )

    if item.python_name.startswith("Py"):
        logger.warning(f"Suspicious name for {path}@{line}: {item}")

    module = props.gets("module", "builtins")
    annotated[module].add(item)
    return annotated[module], item


def _py_source_map(annotated: Package, path: Path, line: Line, lines: Lines):
    """Process the content of the ``py_source_map!`` macro."""

    # Synthesize the match.
    pyclass_match = PYITEM_RE.search("#[pyclass]")
    assert pyclass_match is not None

    # Read the macro content.
    body = iter_delim(chain((line,), lines), "{}")
    next(body)  # skip the first
    _pyitem(annotated, path, body, pyclass_match)  # SourceMap
    _pyitem(annotated, path, body, pyclass_match)  # SourceMapEntry


def _exceptions(annotated: Package, path: Path, line: Line, lines: Lines):
    """Process the content of the ``exception!`` and ``create_exception!`` macros."""
    if not "create_exception!(" in line:
        _ = next(lines)
    _, module = next(lines)
    err_line = next(lines)
    err_name = err_line.text.strip(",")
    item = Item(
        kind=Kind.Error,
        python_name=err_name,
        rust_name=err_name,
        path=path,
        line=err_line,
        props=PyO3Props(),
    )
    mod = annotated[module.strip(",")]
    mod.add(item)
    mod.path = path


IMPL_RE = re.compile("impl\s+?(?P<name>\S+)")


def _pymethods(path: Path, lines: Lines) -> str | None:
    """Find the impl type name."""

    while (line := next(lines, None)) and line.text.strip().startswith("#["):
        skip(lines, "[]", first=line)

    if line is None or not line.text.strip().startswith("impl"):
        logger.error(f"Could not find impl line for {path}.")
        return None

    if not (
        impl := IMPL_RE.search(join_lines(iter_delim(lines, "{}", first=line)).text)
    ):
        logger.error(f"Could not find impl line for {path}.")
        return None

    return impl.group("name")


def _pymod(exported: Package, path: Path, lines: Lines):
    """Process the body of a #[pymodule] annotated function."""

    mod_line = -1
    props = PyO3Props()
    while (line := next(lines, None)) and line.text.strip().startswith("#["):
        line = join_lines(iter_delim(lines, "[]", first=line))
        if m := PYO3_RE.search(line.text):
            props |= PyO3Props.parse(m.group(1) or m.group(2))
            mod_line = line.num
            break

    if mod_line < 0:
        logger.error(f"Could not find module properties for {path}.")

    if line is None:
        return

    name = props.gets("name", "")
    module = f"{props.gets('module', '')}.{name}"
    if name == "_quil":
        module = "quil"
    exported[module].props |= props

    body = iter_delim(lines, "{}", first=line)
    while line := next(body, None):
        if "add" in line.text:
            line = join_lines(iter_delim(lines, "()", first=line))

        if add_match := ADD_ERROR_RE.search(line.text):
            python_name, rust_name = add_match.groups(("python_name", "rust_name"))
            if TYPE_CHECKING:
                assert isinstance(python_name, str)
                assert isinstance(rust_name, str)

            rust_name = rust_name[rust_name.rfind(":") + 1 :]
            item = Item(
                kind=Kind.Error,
                python_name=python_name,
                rust_name=rust_name,
                path=path,
                line=line,
            )
            exported[module].add(item)

        elif add_match := ADD_CLASS_RE.search(line.text):
            if rust_name := add_match.group("rust_name"):
                kind = Kind.Class
                python_name = rust_name
            else:
                kind = Kind.Function
                python_name = rust_name = add_match.group("func_name")

            item = Item(
                kind=kind,
                python_name=python_name,
                rust_name=rust_name,
                path=path,
                line=line,
            )
            exported[module].add(item)

        mod = exported[module]
        mod.submodule = props.is_("submodule")
        mod.path = path
        mod.line_num = mod_line


class StubKind(enum.Enum):
    Function = "function"
    Methods = "methods"
    Class = "class"
    Enumeration = "class_enum"
    ComplexEnumeration = "class_complex_enum"

    def is_like(self, kind: Kind) -> bool:
        match (kind, self):
            case (Kind.Struct, StubKind.Class):
                return True
            case (
                Kind.Enumeration,
                (StubKind.Enumeration | StubKind.ComplexEnumeration),
            ):
                return True
            case (
                Kind.Class,
                (StubKind.Class | StubKind.Enumeration | StubKind.ComplexEnumeration),
            ):
                return True
            case (Kind.Function, StubKind.Function):
                return True
        return False


@dataclass
class StubAttr:
    kind: StubKind
    module: str | None = None

    @classmethod
    def from_match(cls, stubgen_match: re.Match) -> Self:
        """Process #[gen_stub_...] annotations."""

        stub_kind = stubgen_match.group(1) or stubgen_match.group(3)
        if stub_kind is None:
            raise ValueError("Unable to determine stub kind")

        kind = StubKind(stub_kind)
        module: str | None = None

        if kind is StubKind.Function:
            props = PyO3Props.parse(stubgen_match.group(2) or stubgen_match.group(4))
            if not (module := props.gets("module")):
                logger.warning(f"Unknown module for stub {stubgen_match.group(0)}")

        return cls(kind, module)


def extract_items_from_file(path: Path, annotated: Package, exported: Package):
    """Update `annotated` and `exported` with, respectively,
    the items that are annotated to belong to a module
    and the items actually added to the module.
    """

    lines = read_file(path)
    last_stub: StubAttr | None = None
    while line := next(lines, None):
        # Handle macros:
        if "py_source_map!" in line:
            logger.info(f"Discovered SourceMap in {path}: {line}")
            _py_source_map(annotated, path, line, lines)
            continue

        elif "exception!(" in line:
            logger.info(f"Discovered Error in {path}: {line}")
            _exceptions(annotated, path, line, lines)
            continue

        # Skip non-attributes:
        if not line.text.strip().startswith("#["):
            continue

        line = join_lines(iter_delim(lines, "[]", first=line))
        logger.debug(f"Found Attribute in {path}: {line}.")

        # Note that gen_stub_* attributes must come before pyo3 attributes.
        if stubgen_match := STUB_GEN_RE.search(line.text):
            logger.info(f"Discovered stub_gen attr in {path}: {line}")
            last_stub = StubAttr.from_match(stubgen_match)
        elif "gen_stub" in line and "override_" not in line:
            logger.error(f"We're probably missing this annotation: {path} {line}")

        if pyclass_match := PYITEM_RE.search(line.text):
            logger.info(f"Discovered Item in {path}: {line}")
            module, item = _pyitem(annotated, path, lines, pyclass_match)
            if module and item and last_stub:
                if not last_stub.kind.is_like(item.kind):
                    logger.warning(
                        f"Mismatched stub {last_stub} found for {path} {item}."
                    )
                else:
                    logger.info(f"Stub annotation found for {path} {item}.")
                    module.add(replace(item, has_stub=True))
                    if last_stub.kind is StubKind.Function and last_stub.module:
                        logger.info(f"Moving fn to {last_stub.module}")
            elif item:
                logger.warning(f"No stub annotation found for {path} {item}.")

            last_stub = None
        elif ("pyclass" in line.text or "pyfunction" in line.text) and not (
            "gen_stub" in line.text or "use " in line.text
        ):
            logger.error(f"We're probably missing this annotation: {path} {line}")

        if PYMETHODS_RE.search(line.text):
            logger.info(f"Discovered impl block in {path}: {line}")
            if last_stub is None:
                logger.warning(f"No stub annotation found for {path} {line}.")
            last_stub = None
        elif "pymethods" in line.text and not (
            "gen_stub" in line.text or "use " in line.text
        ):
            logger.error(f"We're probably missing this annotation: {path} {line}")

        if PYMODULE_RE.search(line.text):
            logger.info(f"Discovered Module in {path}: {line}")
            _pymod(exported, path, chain((line,), lines))
            last_stub = None
        elif "pymodule" in line.text and not (
            "gen_stub" in line.text or "use " in line.text
        ):
            logger.error(f"We're probably missing this annotation: {path} {line}")


if __name__ == "__main__":
    main()
