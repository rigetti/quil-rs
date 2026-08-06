from pathlib import Path

from conftest import plot_filename
from quil.program import Program

from quil_plotting import plot_schedule_altair


def test_plot_schedule_altair(program_file, plot_options):
    """Test that the chart is produced under each set of options, and save it for visual inspection."""
    variant, options = plot_options
    with open(program_file, "r") as f:
        program = Program.parse(f.read())
    chart = plot_schedule_altair(program, **options)

    output_path = Path(__file__).resolve().parent / "test_plots_altair"
    output_path.mkdir(exist_ok=True)

    # Exercises the browser-free export path, which is the reason this backend exists.
    chart.save(output_path / plot_filename(program_file, variant))
