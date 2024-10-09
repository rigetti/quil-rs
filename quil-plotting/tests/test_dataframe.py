from pathlib import Path

import pytest
from quil.program import Program

from quil_plotting import add_plot_metadata, program_to_dataframe

program_files = list((Path(__file__).resolve().parent / "programs").glob("*.quil"))


@pytest.mark.parametrize("program_file", program_files)
def test_schedule_dataframe(program_file):
    """Test that the schedule dataframe is constructed and valid."""
    with open(program_file, "r") as f:
        program = Program.parse(f.read())
    _ = program_to_dataframe(program)

    # TODO Check against reference?


@pytest.mark.parametrize("program_file", program_files)
def test_schedule_dataframe_metadata(program_file):
    """Test that plot metadata is correctly added to each dataframe."""
    with open(program_file, "r") as f:
        program = Program.parse(f.read())
    df = program_to_dataframe(program)

    _ = add_plot_metadata(df)

    # TODO Check options
    # TODO Check against reference?
