from pathlib import Path
import argparse
import logging


def get_parser(default_path=Path("./src")) -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Lint quil-py source files.")

    parser.add_argument(
        "-b",
        "--base",
        metavar="PATH",
        type=Path,
        help=f"the base path to source files (default: '{default_path}')",
        default=default_path,
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
