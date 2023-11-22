from pathlib import Path
import sys

import pdoc

import quil  # noqa - we need to import quil for it to appear in sys.modules


if __name__ == "__main__":
    print(dir(sys.modules["quil"]))
    print(dir(sys.modules["quil.validation"]))
    print([k for k in sys.modules.keys() if "quil" in k])
    del sys.modules["quil.validation"]

    pdoc.pdoc("quil", "!quil.quil", output_directory=Path("docs"))
