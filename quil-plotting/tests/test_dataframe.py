from quil.program import Program

from quil_plotting import add_plot_metadata, program_to_dataframe


def test_schedule_dataframe(program_file):
    """Test that the schedule dataframe is constructed and valid."""
    with open(program_file, "r") as f:
        program = Program.parse(f.read())
    _ = program_to_dataframe(program)

    # TODO Check against reference?


def test_schedule_dataframe_metadata(program_file, plot_options):
    """Test that plot metadata is correctly added to each dataframe, under each set of options."""
    _, options = plot_options
    with open(program_file, "r") as f:
        program = Program.parse(f.read())
    df = program_to_dataframe(program)

    df = add_plot_metadata(df, **options)

    # Whichever options were used, every row has to land on a runner and be given a colour.
    assert not df["Runner"].isna().any()
    assert not df["Color"].isna().any()
    assert df["Offset"].between(0, df["Runner"].nunique() - 1).all()

    # TODO Check against reference?
