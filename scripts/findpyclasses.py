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

from collections import defaultdict
from collections.abc import MutableSet
from dataclasses import dataclass, field, replace
from pathlib import Path
import re
import sys
from typing import Iterable, Iterator, TypeAlias


@dataclass(frozen=True, order=True)
class Item:
    kind: str
    python_name: str
    rust_name: str
    path: Path = field(compare=False, hash=False)
    line_num: int = field(compare=False, hash=False)


def test_item():
    i1 = Item("struct", "A", "B", Path("C"), 1)
    i2 = Item("struct", "A", "B", Path("c"), 2)
    i3 = Item("struct", "A", "b", Path("C"), 1)
    assert hash(i1) == hash(i2)
    assert hash(i1) != hash(i3)
    assert i1 in {i2}
    assert i2 in {i1}
    assert i1 not in {i3}
    assert i2 not in {i3}
    assert i3 not in {i1, i2}


@dataclass
class Module(MutableSet[Item]):
    items: set[Item] = field(default_factory=set)
    submodule: bool = True
    path: Path = Path(".")
    line_num: int = -1

    def __contains__(self, key: object) -> bool:
        return key in self.items

    def __iter__(self) -> Iterator[Item]:
        return self.items.__iter__()

    def __len__(self) -> int:
        return self.items.__len__()

    def discard(self, value: Item) -> None:
        return self.items.discard(value)

    def add(self, item: Item) -> None:
        self.items.add(item)


@dataclass
class Package:
    modules: defaultdict[str, Module] = field(
        default_factory=lambda: defaultdict(Module)
    )


PYITEM_RE = re.compile(r"#\s*\[(?:pyclass|pyfunction)\s*(?:\((.*?)\))?\]")
PYMODULE_RE = re.compile(r"#\s*\[pymodule\]")
PYO3_RE = re.compile(r"#\s*\[pyo3\s*\((.*?)\)\]")
ITEM_RE = re.compile(r"(struct|enum|fn)\s+([A-Za-z_][A-Za-z0-9_]*)")
ADD_RE = re.compile(r'\.add\(\s*"(?P<python_name>.+?)".*<(?P<rust_name>.*?)>')
ADD_CLASS_RE = re.compile(
    r"\.add_class::<(?P<class_name>[^>]*?)>|"
    r"\.add_function\(wrap_pyfunction!\((?P<func_name>[^>]*?),.*\)"
)

NumberedLine: TypeAlias = tuple[int, str]


def read_to_close(
    first: str,
    lines: Iterable[NumberedLine],
    pair="()",
    stop: str | None = None,
    sep: str = " ",
) -> str:
    """Collect lines of text between a pair of properly-nested delimiters."""

    if stop is None:
        start, stop = pair
    else:
        start = pair

    def _get_lines():
        yield first
        count = first.count(start) - first.count(stop)
        while count > 0 and (numline := next(lines, None)):
            _, line = numline
            yield line
            count += line.count(start) - line.count(stop)

    return sep.join(_get_lines())


def parse_args(arg_str: str) -> dict[str, str | bool]:
    """Convert args used in a `#[whatever(arg1, arg2 = "val")]` annotation.
    Note that the `arg_str` should just be the part between the parentheses.

    For the above example, the return value is `{"arg1": True, "arg2": "val"}`.
    """

    result: dict[str, str | bool] = {}
    parts = [p.strip() for p in arg_str.split(",")]
    for part in parts:
        if "=" in part:
            key, val = map(str.strip, part.split("=", 1))
            if val.startswith('"') and val.endswith('"'):
                val = val[1:-1]
            result[key] = val
        else:
            result[part] = True
    return result


def read_file(path: Path) -> Iterator[NumberedLine]:
    """Iterate over `(line_number, line)` tuples of a file,
    skipping content comments and `macro_rules!` definitions.
    """

    with path.open() as f:
        lines = ((i, line.strip()) for i, line in enumerate(f, start=1))
        while numline := next(lines, None):
            num, line = numline

            if "/*" in line:
                num_begin, begin = num, line[: line.find("/*")]

                cnt = line.count("/*") - line.count("*/")
                while cnt > 0 and (numline := next(lines, None)):
                    num, line = numline
                    cnt += line.count("/*") - line.count("*/")

                if not numline:
                    num, line = num_begin, begin
                elif num == num_begin:
                    line = begin + " " + line[line.rfind("*/") + 1 :]
                else:
                    yield num_begin, begin

            elif (idx := line.find("//")) >= 0:
                line = line[:idx]

            elif "macro_rules!" in line:
                _ = read_to_close(line, lines, pair="{}")
                continue

            yield num, line


def extract_items_from_file(path: Path, annotated: Package, exported: Package):
    """Update `annotated` and `exported` with, respectively,
    the items that are annotated to belong to a module
    and the items actually added to the module.
    """

    def _pyitem(lines, pyclass_match):
        """Process #[pyclass] and #[pyfunction] annotated items."""

        args = parse_args(pyclass_match.group(1) or "")
        module = args.get("module", "")
        pyname = args.get("name", "")

        # Skip over additional `#[...]` lines
        while (num_line := next(lines, None)) and num_line[1].startswith("#"):
            if m := PYO3_RE.search(num_line[1]):
                pyname = parse_args(m.group(1)).get("name", pyname)
                module = parse_args(m.group(1)).get("module", module)
        if num_line is None:
            return

        # Look for struct or enum declaration
        _, line = num_line
        item_match = ITEM_RE.search(line)
        if item_match:
            kind, rustname = item_match.groups()
            python_name = pyname or rustname
            item = Item(
                kind="func" if kind == "fn" else "class",
                python_name=python_name,
                rust_name=rustname,
                path=path,
                line_num=line_num,
            )
            if item.kind == "func":
                module = "builtins"
            annotated.modules[module].add(item)

    def _pymod(lines, mod_line: int):
        """Process the body of a #[pymodule] annotated function."""

        module = ""
        is_sub = False
        while num_line := next(lines, None):
            line_num, line = num_line
            if m := PYO3_RE.search(line):
                args = parse_args(m.group(1))
                is_sub = "submodule" in args
                module = args.get("module", module)
                name = args.get("name", "")
                if module:
                    module += "." + name
                else:
                    module = name
                mod_line = line_num
                break

        if module == "_quil":
            module = "quil"

        while (num_line := next(lines, None)) and "{" not in num_line[1]:
            continue

        line_num, line = num_line or (-1, "")
        brace_count = line.count("{") - line.count("}")
        while brace_count > 0 and (num_line := next(lines, None)):
            line_num, line = num_line
            brace_count += line.count("{") - line.count("}")

            if "add" in line:
                line = read_to_close(line, lines, "()")

            if add_match := ADD_RE.search(line):
                python_name, rust_name = add_match.groups(("python_name", "rust_name"))
                rust_name = rust_name[rust_name.rfind(":") + 1 :]
                item = Item(
                    "error",
                    python_name=python_name,
                    rust_name=rust_name,
                    path=path,
                    line_num=line_num,
                )
                exported.modules[module].add(item)

            elif add_match := ADD_CLASS_RE.search(line):
                if rust_name := add_match.group("class_name"):
                    kind = "class"
                    python_name = rust_name
                else:
                    kind = "func"
                    python_name = rust_name = add_match.group("func_name")
                item = Item(
                    kind,
                    python_name=python_name,
                    rust_name=rust_name,
                    path=path,
                    line_num=line_num,
                )
                exported.modules[module].add(item)

            mod = exported.modules[module]
            mod.submodule = is_sub
            mod.path = path
            mod.line_num = mod_line

    lines = read_file(path)
    while num_line := next(lines, None):
        line_num, line = num_line

        if "py_source_map!" in line:
            pyclass_match = PYITEM_RE.search("#[pyclass]")
            txt = read_to_close(line, lines, "{}", sep="\n")
            _lines = enumerate(txt.split("\n"), start=line_num)
            _ = next(_lines)
            _pyitem(_lines, pyclass_match)
            _ = next(_lines)
            _pyitem(_lines, pyclass_match)
            continue

        elif "exception!(" in line:
            if not "create_exception!(" in line:
                _ = next(lines)
            _, module = next(lines)
            line_num, err_name = next(lines)
            err_name = err_name.strip(",")
            item = Item(
                "error",
                python_name=err_name,
                rust_name=err_name,
                path=path,
                line_num=line_num,
            )
            mod = annotated.modules[module.strip(",")]
            mod.add(item)
            mod.path = path
            continue

        if line.startswith("#["):
            line = read_to_close(line, lines, "[]")

        if pyclass_match := PYITEM_RE.search(line):
            _pyitem(lines, pyclass_match)
        elif PYMODULE_RE.search(line):
            _pymod(lines, line_num)


def main():
    annotated, exported = Package(), Package()
    base = Path(sys.argv[1]) if len(sys.argv) > 1 else Path(".")
    for path in base.rglob("*.rs"):
        extract_items_from_file(path, annotated, exported)

    for func in (f for f in annotated.modules["builtins"].items if f.kind == "func"):
        matching = (
            name
            for name, mod in exported.modules.items()
            for item in mod
            if (item.kind == "func" and item.rust_name == func.rust_name)
        )
        if modname := next(matching, None):
            print(f"guess for export of {func}: {modname}")
            exported.modules[modname].add(func)
        else:
            print(f"no guess for export of {func}")

    for module, anno in sorted(annotated.modules.items()):
        if module == "builtins":
            continue

        rust_to_py_anno = {item.rust_name: item for item in anno}

        expm = exported.modules.get(module)
        if not expm:
            print(
                f"Module {module} is not exported;",
                f"  Module annotation: {anno.path} @ {anno.line_num}",
                f"  Items in module:",
                sep="\n",
            )
            for item in anno:
                print(f"  {item}")
            continue

        for item in sorted(anno):
            if not (
                item in expm
                or (
                    (anno_item := rust_to_py_anno.get(item.rust_name))
                    and anno_item.python_name == item.python_name
                )
            ):
                print(
                    f"{item.kind} {module}.{item.python_name} is not exported"
                    f" ({item.rust_name} @ {item.path}:{item.line_num})"
                )

        for item in sorted(expm):
            if anno_item := rust_to_py_anno.get(item.rust_name):
                item = replace(item, python_name=anno_item.python_name)

            if item not in anno and item.kind != "func":
                print(
                    f"{item.kind} {module}.{item.python_name} has wrong or missing module annotation "
                    f" ({item.rust_name} @ {item.path}:{item.line_num})"
                )


if __name__ == "__main__":
    main()
