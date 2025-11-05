"""
A lint helper for ``quil-rs``'s PyO3 wrappers.

You'd typically use this by giving the root source path to the `process_dir` function,
then pass the resulting `Package`s to the `find_possible_mistakes` function,
getting back a list of `Issue`s that you can choose to print or further process.
You can instead (or in addition) pass the `Package`s to `print_package_info`
to get pretty-printed information about the overall package structure inferred by this code.

This works purely on the textual source, so it may make mistakes,
especially among elements that share the same name.
In particular, if there are multiple ``#[pyfunction]`` with the same Rust name,
and at least one of them is added to a module, this script won't know which one it was.
With ``pyo3_stub_gen`` annotations, the script can usually make a better guess about the intent,
but without one, it just prints all the functions it finds with a guess about the export.
"""

from dataclasses import dataclass, replace
from itertools import accumulate, chain
from pathlib import Path
from typing import TYPE_CHECKING
import logging
import re

from .reader import (
    join_lines,
    iter_delim,
    read_file,
    Line,
    Lines,
    skip,
)

from .package import (
    Item,
    Kind,
    Module,
    Package,
    PackageKind,
    PyO3Props,
    StubAttr,
    StubKind,
)

logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class Issue:
    package_kind: PackageKind
    message: str


def process_dir(root: Path) -> tuple[Package, Package]:
    """Process sources recursively from the root path,
    returning the ``(annotated, exported)`` `Package`s.
    """

    annotated, exported = Package(), Package()
    for path in root.rglob("*.rs"):
        path = path.relative_to(root)
        extract_items_from_file(root, path, annotated, exported)
    guess_function_modules(annotated, exported)
    return annotated, exported


def guess_function_modules(annotated: Package, exported: Package):
    """Try to match functions to modules.

    The ``pyfunction`` annotation doesn't include a ``module`` property,
    so we can only guess the module it belongs to if we can find a matching export
    (that is, a ``pymodule`` it appears to have been added to).
    """

    builtins = annotated["builtins"]
    funcs = [f for f in builtins if f.kind is Kind.Function]
    for func in funcs:
        matching = (
            name
            for name, mod in exported.items()
            for item in mod
            if (item.kind is Kind.Function and item.rust_name == func.rust_name)
        )
        if modname := next(matching, None):
            logger.info(f"Guessing module '{modname}' for function: {func}")
            exported[modname].add(func)
            builtins.discard(func)
        else:
            logger.warning(f"No module found for function: {func}")


def find_possible_mistakes(
    annotated: Package, exported: Package, found_exports: set[str] | None = None
) -> list[Issue]:
    """Return potential mistakes between the `annotated` and `exported` ``Package``s.

    The `found_exports` parameter is a set of module names considered exported implicitly.
    If you're using this to lint code other than ``quil-rs``, you'll probably want to change them,
    but keep in mind, this linter is designed with consideration of macros used in ``quil-rs``.
    If they are not given, it uses these defaults:

    - "builtins": This is implicitly "exported".
    - "quil": This is the name of the top-level package, so it doesn't need an export.
    - "_quil": This is the name of the internal package produced by the build process,
        and its contents are exported as the top-level package within the ``__init__.py``.

    The split between the "internal" ``_quil`` and "external" ``quil`` packages
    is based on ``maturin's recommendations <https://www.maturin.rs/project_layout.html>_``,
    specifically for "Adding Python type information".
    """

    logger.debug("Checking that annotated items are added to the correct module.")
    issues: list[Issue] = []


    for method in annotated._methods:
        rust_type, _ = method.rust_name.split("::", 1)
        if (found := annotated.find_rust(rust_type)) is None:
            msg = "\n".join(
                (
                    f"Method '{method.python_name}' was not matched to a Rust type.",
                    f"   {method}",
                )
            )
            issues.append(Issue(PackageKind.Annotation, msg))
            continue

        module, item = found
        if method.python_name.startswith("Py") or method.python_name.startswith("py_"):
            msg = "\n".join(
                (
                    f"Suspicious method name for '{module}.{item.python_name}.{method.python_name}'",
                    f"     ({method})",
                    (
                        "     "
                        "hint: did you forget a '#[pyo3(name = "
                        f'"{method.python_name.lower().removeprefix("py").removeprefix("_")}"'
                        ')] attribute?'
                    ),
                )
            )
            issues.append(Issue(PackageKind.Annotation, msg))


    if found_exports is None:
        found_exports = {"builtins", "quil", "._quil"}

    for module, anno in sorted(annotated.items()):
        if module == "builtins":
            for func in (f for f in anno if f.kind is Kind.Function):
                msg = "\n".join(
                    (
                        f"Function  '{func.python_name}' does not appear to be exported",
                        f"   {func}",
                    )
                )
                issues.append(Issue(PackageKind.Annotation, msg))
            continue

        # First check if we found the module itself.
        logger.debug(f"Looking for `#[pymodule]` matching '{module}'")
        expm = exported.get(module)
        if expm is None:
            items = "\n".join(
                f"    {item} (props: {item.props}, text: {item.line})" for item in anno
            )
            msg = "\n".join(
                (
                    f"Module '{module}' does not appear to be exported",
                    f"  Module found in: {anno.path}",
                    f"  Items in module:",
                    items,
                )
            )
            issues.append(Issue(PackageKind.Annotation, msg))
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
                if alt := exported.find_rust(item):
                    hint = (
                        f"\n     (hint: item may belong to module '{alt[0]}': {alt[1]})"
                    )
                else:
                    hint = ""
                msg = "\n".join(
                    (
                        f"  Wrong or missing module annotation for '{module}.{item.python_name}'",
                        f"     ({item}){hint}",
                    )
                )
                issues.append(Issue(PackageKind.Export, msg))

        logger.debug(f"Checking that items marked to belong to '{module}' are added.")
        for item in sorted(anno):
            if item.python_name.startswith("Py") or item.python_name.startswith("py_"):
                msg = "\n".join(
                    (
                        f"  Suspicious name for '{module}.{item.python_name}'",
                        f"     ({item})",
                        (
                            "     "
                            "hint: did you forget a '#[pyo3(name = "
                            f'"{item.python_name.strip("Py")})]" attribute?'
                        ),
                    )
                )
                issues.append(Issue(PackageKind.Export, msg))

            if item not in expm:
                msg = "\n".join(
                    (
                        f"  Couldn't find export for '{module}.{item.python_name}'",
                        f"     ({item})",
                    )
                )
                issues.append(Issue(PackageKind.Export, msg))

            if item.stub_attr is None:
                msg = "\n".join(
                    (
                        f"  Didn't find a stub annotation for '{module}.{item.python_name}'",
                        f"     ({item})",
                    )
                )
                issues.append(Issue(PackageKind.Export, msg))

            elif item.stub_attr.kind is StubKind.Enumeration:
                if item.props.get("rename_all") != "SCREAMING_SNAKE_CASE":
                    msg = "\n".join(
                        (
                            (
                                f"  Simple enum '{module}.{item.python_name}' doesn't have "
                                'rename_all = "SCREAMING_SNAKE_CASE"'
                            ),
                            f"     ({item})",
                        )
                    )
                    issues.append(Issue(PackageKind.Annotation, msg))

            elif item.stub_attr.kind is StubKind.ComplexEnumeration:
                if item.rust_name not in expm._fixed_enums:
                    msg = "\n".join(
                        (
                            f"  Didn't find `fix_complex_enums!` for '{module}.{item.python_name}'",
                            f"     ({item})",
                        )
                    )
                    issues.append(Issue(PackageKind.Export, msg))

    logger.debug(f"Found exports: {', '.join(found_exports)}")
    logger.debug(f"Expected exported modules: {', '.join(exported.keys())}")
    for module in exported.keys() - found_exports:
        msg = f"No annotations found for module '{module}'"
        issues.append(Issue(PackageKind.Export, msg))

    return issues


def print_package_info(annotated: Package) -> None:
    for module, anno in sorted(annotated.items()):
        items = sorted(anno)
        all_enums = [item for item in items if item.kind is Kind.Enumeration]
        complex_enums = [
            e
            for e in all_enums
            if (stubs := e.stub_attr) and stubs.kind is StubKind.ComplexEnumeration
        ]

        enums = [e for e in all_enums if e not in complex_enums]
        errors = [item for item in items if item.kind is Kind.Error]
        structs = [item for item in items if item.kind is Kind.Struct]

        to_print = [
            ("Complex Enums", complex_enums),
            ("Enums", enums),
            ("Errors", errors),
            ("Structs", structs),
        ]

        for category, items in to_print:
            if not items:
                continue

            print(f"\n---- {module} {category} ----")
            for item in items:
                full_qual = f"{module}.{item.python_name}"
                print(
                    f"{full_qual:50} | {item.rust_name:20} | {item.path}:{item.line.num}"
                )

                if category == "Structs":
                    print("  ", item.props - {"name", "module"})


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
GETTER_SETTER_RE = re.compile(_cfg(r"(getter|setter)\(([^)]+)\)"))
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
        r"(?:(?:pyo3_stub_gen::)?derive::)?gen_stub_py(class(?:(?:_complex)?_enum)?|methods|function)(?:\((\s*?[^)]+)\))?"
    )
)

def _pymethods(annotated: Package, path: Path, lines: Lines):
    """Process the body of a ``#[pymethods]`` annotated ``impl`` block."""

    # Skip over additional `#[...]` lines.
    while (line := next(lines, None)) and line.text.strip().startswith("#["):
        line = join_lines(iter_delim(lines, "[]", first=line))

    if line is None:
        logger.error(f"Unexpected EOF processing {path}")
        return

    match line.text.split():
        case ["impl", rust_type, "{"] | ["impl", _, "for", rust_type, "{"]:
            logger.info(f"Processing impl block for {rust_type}.")
            pass
        case _:
            logger.error(f"Expected impl block, but found {line}.")
            return

    # Limit ourselves to the impl block.
    lines = iter_delim(lines, "{}")

    props = PyO3Props()
    while (line := next(lines, None)):
        if line.text.strip().startswith("#["):
            line = join_lines(iter_delim(lines, "[]", first=line))
            if m := PYO3_RE.search(line.text):
                props |= PyO3Props.parse(m.group(1) or m.group(2))
            elif m := GETTER_SETTER_RE.search(line.text):
                props["name"] = m.group(2) or m.group(4)

        if not (item_match := ITEM_RE.search(line.text)):
            continue

        kind: Kind = Kind(item_match.group("kind"))
        rust_name: str = item_match.group("rust_name")

        if not isinstance(rust_name, str):
            logger.error(f"Couldn't find Rust name for {path}@{line}: {kind}")
            return

        python_name = props.get("name") or props.get("getter")

        item = Item(
            kind=kind,
            python_name=props.gets("name", rust_name),
            rust_name=f"{rust_type}::{rust_name}",
            path=path,
            line=line,
        )
        assert item.kind is Kind.Function, str(item)
        if item.python_name.startswith("Py") or item.python_name.startswith("py_"):
            logger.warning(f"Suspicious name for {path}@{line}: {item}")

        annotated._methods.add(item)

        # Skip the actual function body.
        skip(lines, "{}", first=line)


def _pyitem(
    annotated: Package,
    path: Path,
    lines: Lines,
    pyclass_match: re.Match,
    last_stub: StubAttr | None = None,
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
        stub_attr=last_stub,
    )

    if item.python_name.startswith("Py") or item.python_name.startswith("py_"):
        logger.warning(f"Suspicious name for {path}@{line}: {item}")

    if last_stub is None:
        logger.warning(f"No stub annotation found for {path} {item}.")
    elif not last_stub.kind.is_like(item.kind):
        logger.warning(
            f"Mismatched stub {last_stub} (vs {item.kind}) found for {path} {item}."
        )
    elif last_stub.kind is StubKind.Function and last_stub.module:
        logger.info(f"Moving fn to {last_stub.module}")
        module = last_stub.module

    module = props.gets("module", "builtins")
    annotated[module].add(item)
    return annotated[module], item


def _py_source_map(annotated: Package, path: Path, line: Line, lines: Lines):
    """Process the content of the ``py_source_map!`` macro."""

    # Synthesize the match.
    pyclass_match = PYITEM_RE.search("#[pyclass]")
    assert pyclass_match is not None
    last_stub = StubAttr(StubKind.Class)

    # Read the macro content.
    body = iter_delim(chain((line,), lines), "{}")
    next(body)  # Skip the first line, which contains the opening delimiter.
    _pyitem(annotated, path, body, pyclass_match, last_stub)  # SourceMap
    _pyitem(annotated, path, body, pyclass_match, last_stub)  # SourceMapEntry


def _exceptions(annotated: Package, path: Path, line: Line, lines: Lines):
    """Process the content of the ``exception!`` and ``create_exception!`` macros."""

    body = join_lines(iter_delim(lines, "()", first=line))
    if "create_exception!(" in line:
        module, err_name, _ = body.text.split(",", maxsplit=2)
    else:
        _, module, err_name, _ = body.text.split(",", maxsplit=3)

    module = module[module.find("!") + 2 :].strip()

    item = Item(
        kind=Kind.Error,
        python_name=err_name.strip(),
        rust_name=err_name.strip(),
        path=path,
        line=line,
        stub_attr=StubAttr(kind=StubKind.Class, module=module),
    )
    annotated[module].add(item)


def _impl_instruction(exported: Package, path: Path, line: Line, lines: Lines):
    """Process the input to the ``impl_instruction!`` macro."""

    exported["quil.instructions"].update(
        Item(
            kind=Kind.Class,
            python_name=rust_name,
            rust_name=rust_name,
            path=path,
            line=line,
        )
        for name in join_lines(iter_delim(lines, "[]", first=line))
        .text.replace(" ", "")
        .removeprefix("impl_instruction!([")
        .removesuffix("]);")
        .split(",")
        if (rust_name := name.partition("[")[0].strip()) != ""
    )


def _fix_complex_enums(module: Module, line: Line, lines: Lines):
    """Process the input to the ``_fix_complex_enums!`` macro."""

    module._fixed_enums.update(
        name
        for n in (
            join_lines(iter_delim(lines, "()", first=line))
            .text.replace(" ", "")
            .removeprefix("fix_complex_enums!(")
            .removesuffix(");")
            .split(",")
        )
        if (name := n.strip()) != "" and name != "py"
    )


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
        if "fix_complex_enums!(" in line.text:
            _fix_complex_enums(exported[module], line, lines)
            continue

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


def extract_items_from_file(root: Path, path: Path, annotated: Package, exported: Package):
    """Update `annotated` and `exported` with, respectively,
    the items that are annotated to belong to a module
    and the items actually added to the module.
    """

    lines = read_file(root / path)
    last_stub: StubAttr | None = None
    while line := next(lines, None):
        # Handle macros:
        if "py_source_map!" in line:
            logger.info(f"Discovered SourceMap in {path}: {line}")
            _py_source_map(annotated, path, line, lines)
            continue

        if "impl_instruction!" in line:
            logger.info(f"Discovered impl_implementation! in {path}: {line}")
            _impl_instruction(exported, path, line, lines)
            continue

        elif re.search(r"\b(?:create_)?exception!", line.text):
            logger.info(f"Discovered Error in {path}: {line}")
            _exceptions(annotated, path, line, lines)
            continue

        # Skip non-attributes:
        if not line.text.strip().startswith("#["):
            last_stub = None
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
            _pyitem(annotated, path, lines, pyclass_match, last_stub)
        elif ("pyclass" in line.text or "pyfunction" in line.text) and not (
            "gen_stub" in line.text or "use " in line.text
        ):
            logger.error(f"We're probably missing this annotation: {path} {line}")

        if PYMETHODS_RE.search(line.text):
            logger.info(f"Discovered impl block in {path}: {line}")
            if last_stub is None:
                logger.warning(f"No stub annotation found for {path} {line}.")
            _pymethods(annotated, path, chain((line,), lines))
        elif "pymethods" in line.text and not (
            "gen_stub" in line.text or "use " in line.text
        ):
            logger.error(f"We're probably missing this annotation: {path} {line}")

        if PYMODULE_RE.search(line.text):
            logger.info(f"Discovered Module in {path}: {line}")
            _pymod(exported, path, chain((line,), lines))
        elif "pymodule" in line.text and not (
            "gen_stub" in line.text or "use " in line.text
        ):
            logger.error(f"We're probably missing this annotation: {path} {line}")
