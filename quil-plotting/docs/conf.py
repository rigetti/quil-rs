# Copyright 2026 Rigetti Computing
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Sphinx configuration for the quil-plotting documentation."""

import sys
from pathlib import Path

import vl_convert

# `napoleon_myst` is resolved by docutils as a plain module name, so `_ext` has
# to be importable rather than merely present.
sys.path.insert(0, str(Path(__file__).parent / "_ext"))

project = "quil-plotting"
author = "Rigetti Computing"
copyright = "2026, Rigetti Computing"

extensions = [
    "myst_nb",
    "autodoc2",
]

nb_execution_mode = "auto"
nb_execution_raise_on_error = True

# `auto_mode` off: the reference is curated in `api/`, one directive per class.
autodoc2_packages = [{"path": "../quil", "auto_mode": False}]
autodoc2_render_plugin = "myst"

# Docstrings are Google-convention with markdown bodies, which needs both
# napoleon and MyST - see `_ext/napoleon_myst.py` for why neither alone works.
autodoc2_docstring_parser_regexes = [(r".*", "napoleon_myst")]

# `undoc` keeps any undocumented member from rendering as an empty entry.
# `inherited` is deliberately absent.
autodoc2_hidden_objects = ["undoc", "dunder", "private"]

# The builder chain, `hide`/`show` and `draw` are defined on
# `program.PlottableProgram` and inherited by both entry points. autodoc2
# defaults to "direct", which renders an inherited member's signature but drops
# its docstring - so the whole public API would document itself as bare
# signatures. "all" renders the inherited docstrings too.
autodoc2_docstrings = "all"

myst_enable_extensions = [
    "colon_fence",  # the `:::{seealso}` blocks `napoleon_myst` emits
    "deflist",
    "fieldlist",  # the `:param x:` lists napoleon emits
]

# Docstring headings (`### Grouping fields`) land mid-page, below the page's
# own H1/H2, so they legitimately start deeper than H1.
suppress_warnings = ["myst.header"]

html_theme = "sphinx_rtd_theme"
html_title = "quil-plotting"

templates_path = ["_templates"]

# One copy of the Vega runtime for the whole site, bundled by `vl-convert`
# rather than fetched from a CDN, so a built page renders offline. The
# notebook's charts carry only their own spec and call into this.
_static = Path(__file__).parent / "_static"
_static.mkdir(exist_ok=True)
# Wrapped in an IIFE: the bundle declares a top-level `const _`, and a classic
# script's top-level `const` is global, so unwrapped it collides with the `_`
# the RTD theme already defines and aborts before exporting `vegaEmbed`.
(_static / "vega-bundle.js").write_text(
    f"(function () {{\n{vl_convert.javascript_bundle()}\n}})();"
)

html_static_path = ["_static"]
html_js_files = ["vega-bundle.js"]
# `jupyter_execute` is myst-nb's own output - the executed copy of every
# notebook. Left in, sphinx picks it back up as source and executes it again.
exclude_patterns = ["_build", "_ext", "jupyter_execute"]
