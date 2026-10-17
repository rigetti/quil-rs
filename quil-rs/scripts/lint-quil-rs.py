"""
This script is a lint helper for our PyO3 wrappers.

Given a starting directory, it recursively searches it for ``*.rs`` files,
and attempts to extract PyO3 annotations and exports from the source files.
Afterward, it may print some messages about potential mistakes.
Run the script with ``--help`` to see its options.
"""

from collections import deque
import logging
import re
import os
import sys

logging.basicConfig(level=os.environ.get('LINTER_LOGLEVEL', 'WARNING').upper())
logger = logging.getLogger()

from pyo3_linter import (
    StubAttr,
    StubKind,
    find_possible_mistakes,
    print_package_info,
    process_dir,
    parser,
    PackageConfig,
    Item,
    Kind,
    MacroContext,
    join_lines,
    iter_delim,
    macro_handler,
    default_macro_handlers,
)


def main():
    args = parser.get_parser().parse_args()

    if args.log_level is not None:
        logger.setLevel(args.log_level)

    package_config = PackageConfig(root_module="quil", internal_module="_quil")
    annotated, exported = process_dir(
        args.base,
        package_config,
        default_macro_handlers()
        + [
            _impl_instruction,
            _define_waveforms,
            _py_instruction_singleton,
            _skip_tests,
        ],
    )

    issues = find_possible_mistakes(package_config, annotated, exported)
    if args.show_mistakes:
        for issue in issues:
            print(issue.message)

    if args.show_package:
        print_package_info(annotated)

    if issues:
        print(f"\n {len(issues)} potential issue(s) discovered.", file=sys.stderr)
        if not args.show_mistakes:
            print("  (use --show-mistakes to see)", file=sys.stderr)
        sys.exit(1)


@macro_handler(r"#\[cfg\(test\)\]")
def _skip_tests(ctx: MacroContext, module: str | None = None) -> None:
    """Skip any code that is inside a ``#[cfg(test)]`` block."""

    logger.info("Skipping #[cfg(test)] block.")
    _ = deque(iter_delim(ctx.lines, "{}"), maxlen=0)  # consume the block
    return


@macro_handler(r"define_waveforms!")
def _define_waveforms(ctx: MacroContext, module: str | None = None) -> None:
    """Process the input to the ``define_waveforms!`` macro."""

    logger.info("Processing define_waveforms! macro.")

    lines = join_lines(iter_delim(ctx.lines, "{}"))
    text = lines.text.removeprefix("define_waveforms! {").removesuffix("}")
    parts = re.finditer(r"pub\s+struct\s+(?P<name>\w+)\s*(?:(?P<fields>\{.+?\})|;)", text)

    waveform_module = ctx.annotated["quil._quil.waveform"]

    while m := next(parts, None):
        name = m.group("name")
        has_fields = m.group("fields") is not None
        rust_wrapper_name = f"Py{name}" if has_fields else name

        logger.info(f"Found waveform {name}; exporting it from the Rust type {rust_wrapper_name}")
        waveform_module.add(
            Item(
                rust_name=rust_wrapper_name,
                python_name=name,
                kind=Kind.Struct,
                path=ctx.path,
                line=lines,
                stub_attr=StubAttr(kind=StubKind.Class),
            )
        )


@macro_handler(r"impl_instruction!")
def _impl_instruction(ctx: MacroContext, module: str | None = None) -> None:
    """Process the input to the ``impl_instruction!`` macro."""

    logger.info("Processing impl_instruction! macro.")

    line = join_lines(iter_delim(ctx.lines, "[]"))
    ctx.exported["quil._quil.instructions"].update(
        Item(
            kind=Kind.Class,
            python_name=rust_name,
            rust_name=rust_name,
            path=ctx.path,
            line=line,
        )
        for name in line.text.replace(" ", "").removeprefix("impl_instruction!([").removesuffix("]);").split(",")
        if (rust_name := name.partition("[")[0].strip()) != ""
    )


@macro_handler(r"py_instruction_singleton!")
def _py_instruction_singleton(ctx: MacroContext, module: str | None = None) -> None:
    """Process the input to the ``py_instruction_singleton!`` macro."""

    logger.info("Processing py_instruction_singleton! macro.")

    # fmt: off
    _macro_pattern = (
        r"py_instruction_singleton!\s*\("
            r"(?P<var_name>[^,]+?),\s*"
            r"(?P<type_name>[^,]+?),\s*"
            r"(?P<cell_name>[^,]+?),?\s*"
        r"\)"
    )
    # fmt: on

    line = join_lines(iter_delim(ctx.lines, "()"))
    if not ((m := re.match(_macro_pattern, line.text)) and (name := m.group("type_name"))):
        logger.warning(f"Could not parse name from py_instruction_singleton! macro: {line}")
        return

    name = name.strip()

    ctx.annotated["quil._quil.instructions"].add(
        Item(
            kind=Kind.Struct,
            python_name=name,
            rust_name=name,
            path=ctx.path,
            line=line,
            stub_attr=StubAttr(kind=StubKind.Class),
        )
    )

if __name__ == "__main__":
    main()
