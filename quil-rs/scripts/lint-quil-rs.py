"""
This script is a lint helper for our PyO3 wrappers.

Given a starting directory, it recursively searches it for ``*.rs`` files,
and attempts to extract PyO3 annotations and exports from the source files.
Afterward, it may print some messages about potential mistakes.
Run the script with ``--help`` to see its options.
"""

import dataclasses
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
    Line,
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


@macro_handler(r"define_waveforms!")
def _define_waveforms(ctx: MacroContext, module: str | None = None) -> None:
    """Process the input to the ``define_waveforms!`` macro."""
    logger.info("Processing waveforms")

    lines = join_lines(iter_delim(ctx.lines, "{}"))
    text = lines.text.removeprefix("define_waveforms! {").removesuffix("}")
    parts = re.finditer(r"pub\s+struct\s+(?P<name>\w+)\s*(?:(?P<fields>\{.+?\})|;)", text)

    waveform_module = ctx.annotated["quil.waveform"]
    
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

    line = join_lines(iter_delim(ctx.lines, "[]"))
    ctx.exported["quil.instructions"].update(
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


if __name__ == "__main__":
    main()
