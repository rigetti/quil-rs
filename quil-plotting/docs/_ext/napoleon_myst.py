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

"""A docstring parser that understands both Google sections and markdown.

`autodoc2` resolves a docstring parser by module name and takes that module's
`Parser` attribute, via `docutils.parsers.get_parser_class`. This module is that
parser.

Neither half of the obvious pair works alone on this package. MyST renders the
markdown in our docstrings - the fenced examples, the grouping tables - but has
no idea what a Google `Args:` section is, so those come through as flat text.
Napoleon understands the sections but hands the body onward as reStructuredText,
where the same markdown renders as literal garbage.

So: run napoleon first to turn the sections into field lists, rewrite the few
RST directives it emits as MyST colon-fences, then parse the result as MyST. The
docstrings themselves are never touched, and `ruff`/`pydocfmt` keep enforcing
Google convention as before.
"""

import re

from myst_parser.sphinx_ import Parser as _MystParser
from sphinx.ext.napoleon import Config as _NapoleonConfig
from sphinx.ext.napoleon import GoogleDocstring as _GoogleDocstring

# Over this package's 520 docstrings, napoleon emits exactly three directives:
# `seealso`, `attribute` and `rubric`. Anything outside this set falls through
# unconverted and shows up as literal text in the page - a loud failure rather
# than a silent one, which is what we want if napoleon's output ever changes.
_DIRECTIVES = ("seealso", "attribute", "rubric")

_DIRECTIVE_RE = re.compile(
    r"^(?P<indent> *)\.\. (?P<name>" + "|".join(_DIRECTIVES) + r")::(?P<arg>.*)$"
)

# `napoleon_use_param`/`use_rtype` give `:param x:`/`:rtype:` field lists, which
# MyST parses natively once the `fieldlist` extension is on. Left as literals,
# the alternative `Parameters` blocks would need RST to render.
_NAPOLEON_CONFIG = _NapoleonConfig(
    napoleon_google_docstring=True,
    napoleon_numpy_docstring=False,
    napoleon_use_param=True,
    napoleon_use_rtype=True,
)


def _indent_of(line: str) -> int:
    return len(line) - len(line.lstrip())


def _rst_directives_to_myst(text: str) -> str:
    """Rewrite the RST directives napoleon emits as MyST colon-fences.

    Args:
        text: Napoleon's reStructuredText output.

    Returns:
        The same text, with every RST directive block rewritten as a MyST
        colon-fence and everything else left alone.
    """
    lines = text.splitlines()
    out: list[str] = []
    i = 0
    while i < len(lines):
        match = _DIRECTIVE_RE.match(lines[i])
        if match is None:
            out.append(lines[i])
            i += 1
            continue

        indent, name, arg = match["indent"], match["name"], match["arg"].strip()
        base = len(indent)
        i += 1

        # The directive's body is every following line that is blank or indented
        # past the directive itself. Trailing blank lines belong outside it.
        body: list[str] = []
        while i < len(lines) and (not lines[i].strip() or _indent_of(lines[i]) > base):
            body.append(lines[i])
            i += 1
        while body and not body[-1].strip():
            body.pop()
        while body and not body[0].strip():
            body.pop(0)

        # Dedent by the body's own minimum, not by a fixed three spaces, so that
        # nested structure inside a `See Also:` block survives intact.
        margin = min((_indent_of(b) for b in body if b.strip()), default=0)

        out.append(f"{indent}:::{{{name}}} {arg}".rstrip())
        out.extend(f"{indent}{b[margin:]}" if b.strip() else "" for b in body)
        out.append(f"{indent}:::")
        out.append("")

    return "\n".join(out)


def convert(docstring: str) -> str:
    """Turn one Google-style docstring into MyST markdown.

    Args:
        docstring: The raw docstring, Google convention.

    Returns:
        MyST markdown - field lists for the sections, colon-fences for the
        directives, and the markdown body untouched.
    """
    return _rst_directives_to_myst(str(_GoogleDocstring(docstring, _NAPOLEON_CONFIG)))


class Parser(_MystParser):
    """MyST, with Google docstring sections converted on the way in."""

    def parse(self, inputstring: str, document) -> None:  # type: ignore[no-untyped-def]
        """Parse `inputstring` as a Google-style docstring containing markdown.

        Args:
            inputstring: The docstring to parse.
            document: The docutils document to populate.
        """
        super().parse(convert(inputstring), document)
