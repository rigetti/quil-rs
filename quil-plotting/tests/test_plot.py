from pathlib import Path

import pytest
from quil.program import Program

from quil_plotting import plot_schedule

program_files = list((Path(__file__).resolve().parent / "programs").glob("*.quil"))


@pytest.mark.parametrize("program_file", program_files)
def test_plot_schedule(program_file):
    """Test that the plot is produced and save it for visual inspection."""
    with open(program_file, "r") as f:
        program = Program.parse(f.read())
    fig = plot_schedule(program, slider=False)

    output_path = Path(__file__).resolve().parent / "test_plots"
    output_path.mkdir(exist_ok=True)

    name = program_file.stem
    fig.write_image(output_path / (name + ".svg"))
