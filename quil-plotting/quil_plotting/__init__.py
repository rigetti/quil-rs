"""quil_plotting module
--------------------
This module provides plotting utilties for quil programs.

It's organized into two files. `dataframe.py` provides functions to construct a
dataframe of IQ values from a quil program. It further contains a function to
add visualization metadata to the dataframe such as color, and y-values. The dataframe
is intended to be a format that is easy to plot for multiple backends.

`plot.py` provides a reference implementation of schedule visualization using the plotly
library. The figures are interactive and can be exported as pngs or svgs.
"""

from .dataframe import add_plot_metadata, program_to_dataframe
from .plot import plot_schedule, plot_schedule_dataframe

__all__ = ["plot_schedule", "plot_schedule_dataframe", "program_to_dataframe", "add_plot_metadata"]
