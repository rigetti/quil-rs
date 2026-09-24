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

"""Quil program visualization library.

This library provides two program views:

- Circuit, gate-level, view captured in the `PlottableProgramCircuit` object.
- Pulse schedule view captured in the `PlottableProgramPulseSchedule` object.

Both produce [Altair](https://altair-viz.github.io) charts. These provide
interactable views when drawn in a Jupyter notebook cell or saved in an html
format. These charts support scroll-to-zoom, drag-to-pan, hover for details, and
legend selection.

See Also:
    `quil.plotting.PlottableProgramCircuit`: the gate-level view.
    `quil.plotting.PlottableProgramPulseSchedule`: the pulse-level view.
    [The Quil specification](https://github.com/quil-lang/quil): the
        language these programs are written in.
"""

try:
    import quil.program  # noqa: F401
except ModuleNotFoundError as error:  # pragma: no cover - depends on a broken install
    # `quil-plotting` installs into the `quil` package owned by the `quil`
    # distribution, so uninstalling `quil` leaves this subpackage behind with
    # nothing underneath it. pip does not remove dependents, so say plainly what
    # happened rather than failing on an arbitrary import.
    raise ModuleNotFoundError(
        "quil.plotting requires the `quil` package, but it is missing or incomplete. This "
        "usually means `quil` was uninstalled while `quil-plotting` was left in place. "
        "Reinstall it with `pip install --force-reinstall quil`."
    ) from error

from .circuit import PlottableProgramCircuit
from .schedule import PlottableProgramPulseSchedule

__all__ = ["PlottableProgramCircuit", "PlottableProgramPulseSchedule"]
