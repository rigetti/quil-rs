"""Fixtures shared by the plotting tests."""

from pathlib import Path

import pytest

PROGRAM_FILES = sorted((Path(__file__).resolve().parent / "programs").glob("*.quil"))

# There are far too many combinations of plotting options to cover exhaustively. These are the
# default view, which stacks the qubits and colours by the operation, and one which stacks the
# individual frames and colours by the hardware channel they are played on. Between them they
# exercise both ways a runner and a colour can be chosen: by something derived from the program's
# instructions, and by something derived from the device's frames.
PLOT_OPTIONS = {
    "default": {},
    "frames-by-channel": {
        "runners": "Frame",
        "color_by": "Channel Type",
        "label_by": "Channel Type",
    },
}


def plot_filename(program_file: Path, variant: str, suffix: str = ".svg") -> str:
    """Name the plot written for a program under a given set of options.

    The variant is separated by a dot, as program names contain hyphens of their own.

    :param program_file: The program being plotted.
    :param variant: The name of the option set, as keyed in `PLOT_OPTIONS`.
    :param suffix: The file extension to use.
    """
    if variant == "default":
        return program_file.stem + suffix
    return f"{program_file.stem}.{variant}{suffix}"


@pytest.fixture(params=PROGRAM_FILES, ids=lambda path: path.stem)
def program_file(request) -> Path:
    """Each Quil program in the test corpus, in turn."""
    return request.param


@pytest.fixture(params=list(PLOT_OPTIONS), ids=list(PLOT_OPTIONS))
def plot_options(request):
    """Each named set of plotting options, in turn, as a `(name, options)` pair."""
    return request.param, PLOT_OPTIONS[request.param]
