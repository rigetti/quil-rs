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

"""Fixtures shared by the plotting tests.

Every program in `programs/` is a device dump: thousands of DEFFRAME/DEFCAL
lines wrapped around a handful of body instructions. Parsing one is cheap,
expanding it is not, so the corpus-wide tests are kept to one per view and
everything else asserts against a named program.
"""

from functools import lru_cache
from pathlib import Path

import pytest

from quil.program import Program

PROGRAMS = Path(__file__).resolve().parent / "programs"
PROGRAM_NAMES = sorted(path.stem for path in PROGRAMS.glob("*.quil"))


@lru_cache(maxsize=None)
def load(name: str) -> Program:
    """Parse one program from the corpus by stem, e.g. `load("test_blocks")`.

    Cached: a device dump takes ~16 ms to parse and several tests want the same one. Nothing
    plotting-side mutates the program it is handed, so one parse can be shared. Do not cache the
    *plottables* built from it - `hide`, `with_color_of` and friends mutate those in place, and a
    schedule cannot be deep-copied (quil's waveform objects do not pickle).

    ponytail: unbounded, so a full run holds all 47 dumps - ~350 MB. Cap `maxsize` if that ever
    matters; it costs the corpus tests their second-view cache hit.
    """
    return Program.parse((PROGRAMS / f"{name}.quil").read_text())


@pytest.fixture(params=PROGRAM_NAMES)
def program(request) -> Program:
    """Each Quil program in the test corpus, in turn."""
    return load(request.param)


def sig_digits(value: float) -> int:
    """Digits in the mantissa of `repr(value)` - what the number costs in a chart's JSON.

    The rounding helpers exist to shorten numbers, not to change them, so this is the property
    worth asserting on: a value test passes just as happily on a 17-digit result.
    """
    mantissa = repr(float(value)).split("e")[0].lstrip("-0.").replace(".", "")
    return len(mantissa.rstrip("0")) or 1
