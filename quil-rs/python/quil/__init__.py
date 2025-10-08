"""The `quil` package provides tools for constructing, manipulating, parsing, and printing [Quil](https://github.com/quil-lang/quil) programs.

⚠️ This package is still in early development and breaking changes should be expected between minor versions.
"""

# The following code exposes the _quil package contents under the namespace quil.
from . import _quil
assert isinstance(_quil.__all__, list) and all(isinstance(s, str) for s in _quil.__all__)
exec(f"from ._quil import {', '.join(_quil.__all__)}; __all__ = {_quil.__all__}")
del _quil
