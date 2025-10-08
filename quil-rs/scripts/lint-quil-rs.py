"""
This script is a lint helper for our PyO3 wrappers.

Given a starting directory, it recursively searches it for ``*.rs`` files,
and attempts to extract PyO3 annotations and exports from the source files.
Afterward, it may print some messages about potential mistakes.
Run the script with ``--help`` to see its options.
"""

from pathlib import Path
import argparse
import sys
import logging

logging.basicConfig(level=logging.WARNING)
logger = logging.getLogger()

from pyo3_linter import find_possible_mistakes, print_package_info, process_dir


def main():
    args = get_parser().parse_args()

    if args.log_level is not None:
        logger.setLevel(args.log_level)

    annotated, exported = process_dir(args.base)

    issues = find_possible_mistakes(annotated, exported)
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


if __name__ == "__main__":
    main()
