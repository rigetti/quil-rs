from pathlib import Path
import sys

import pdoc

import quil  # noqa - we need to import quil for it to appear in sys.modules


if __name__ == "__main__":
    # Workaround for an incompatiblity between pdoc and how we expose the quil
    # package from pyo3. This module hierarchy isn't valid in a real Python
    # environment, but overriding module.__name__ here helps pdoc import the
    # right module internally.
    # Possibly related issue: https://github.com/mitmproxy/pdoc/issues/633
    for qualified_name, module in sys.modules.items():
        if qualified_name.startswith("quil.") and qualified_name.count(".") == 1:
            module.__name__ = qualified_name

    pdoc.pdoc("quil", "!quil.quil", output_directory=Path("docs"))
