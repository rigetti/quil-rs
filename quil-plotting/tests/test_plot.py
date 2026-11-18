from pathlib import Path

from conftest import plot_filename
from quil.program import Program

from quil_plotting import plot_schedule


def test_plot_schedule(program_file, plot_options):
    """Test that the plot is produced under each set of options, and save it for visual inspection."""
    variant, options = plot_options
    with open(program_file, "r") as f:
        program = Program.parse(f.read())
    fig = plot_schedule(program, slider=False, **options)

    output_path = Path(__file__).resolve().parent / "test_plots"
    output_path.mkdir(exist_ok=True)

    fig.write_image(output_path / plot_filename(program_file, variant))
