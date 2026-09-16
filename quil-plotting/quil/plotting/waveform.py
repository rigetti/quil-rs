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

"""Waveform sampling and de-duplication for the pulse-schedule view."""

from abc import ABC, abstractmethod
from collections.abc import Mapping
from dataclasses import dataclass
from functools import cached_property
from typing import Any, Self

import numpy as np
import numpy.typing as npt
from quil.instructions import Waveform, WaveformInvocation
from quil.waveform import BuiltinWaveform, CommonBuiltinParameters

import quil

from .utils import (
    _evaluate_complex,
    _evaluate_real,
    compress_constant_runs,
    decimate_to_max_points,
    round_sig,
)


class WaveformResolutionError(Exception):
    """A waveform could not be turned into samples."""


class PlottableWaveform(ABC):
    """A cached waveform shape."""

    def __init__(self, sample_rate: float) -> None:
        """Record the rate `iqs` is sampled at.

        Args:
            sample_rate: The playing frame's sample rate in Hz.
        """
        self.sample_rate = sample_rate
        """The rate `iqs` is sampled at in Hz."""

    @property
    @abstractmethod
    def iqs(self) -> npt.NDArray[np.complex128]:
        """This waveform's unit-scale IQ samples."""

    @cached_property
    def ts(self) -> npt.NDArray[np.float64]:
        """Sample times in seconds, starting at zero."""
        return np.arange(len(self.iqs)) / self.sample_rate

    @cached_property
    def has_q(self) -> bool:
        """Whether this waveform has an imaginary component worth drawing."""
        return bool(len(self.iqs) and np.abs(self.iqs.imag).max() > 1e-9)

    @cached_property
    def peak(self) -> float:
        """The largest IQ magnitude in this waveform."""
        return float(np.abs(self.iqs).max()) if len(self.iqs) else 0.0


class PlottableBuiltinWaveform(PlottableWaveform):
    """A builtin waveform sampled from its parameters.

    The pulse's scale is stripped during construction to create more
    opportunities for de-duplication.
    """

    def __init__(
        self,
        waveform: BuiltinWaveform[float, complex],
        common: CommonBuiltinParameters[float, complex],
        sample_rate: float,
    ) -> None:
        """Sample `waveform` at `sample_rate`, using `common`'s parameters.

        Args:
            waveform: The builtin waveform kind to sample.
            common: Its parameters, with `scale` already stripped.
            sample_rate: The playing frame's sample rate in Hz.

        Raises:
            WaveformResolutionError: If `common` still carries a `scale`.
        """
        if common.scale:
            raise WaveformResolutionError("Cannot cache a builtin waveform with scale parameter")

        super().__init__(sample_rate)

        self.waveform = waveform
        """The builtin waveform kind, e.g. `gaussian` or `flat`."""

        self.common = common
        """Its parameters, with `scale` already stripped."""

    @cached_property
    def iqs(self) -> npt.NDArray[np.complex128]:
        """This waveform's unit-scale IQ samples, sampled on first access."""
        try:
            samples = self.waveform.iq_values_at_sample_rate(self.common, self.sample_rate)
        except Exception as e:
            raise WaveformResolutionError(f"could not sample at {self.sample_rate} Hz: {e}") from e
        return samples.iq_values()


class PlottableCustomWaveform(PlottableWaveform):
    """A custom waveform from a DEFWAVEFORM statement."""

    def __init__(
        self,
        name: str,
        definition: Waveform,
        arguments: Mapping[str, complex],
        sample_rate: float,
    ) -> None:
        """Parse a `definition` with `arguments` bound to its parameters.

        Args:
            name: The waveform's name.
            definition: The `DEFWAVEFORM` this invocation names.
            arguments: A value for every parameter `definition` declares.
            sample_rate: The playing frame's sample rate in Hz.

        Raises:
            WaveformResolutionError: If `arguments` does not match
                `definition`'s parameters exactly.
        """
        # Process arguments
        expected = set(definition.parameters)
        supplied = set(arguments)

        missing = expected - supplied
        if missing:
            msg = f"waveform {name!r} is missing parameter(s): {sorted(missing)}"
            raise WaveformResolutionError(msg)

        extra = supplied - expected
        if extra:
            msg = f"waveform {name!r} got unexpected parameter(s): {sorted(extra)}"
            raise WaveformResolutionError(msg)

        super().__init__(sample_rate)

        # Calculate iqs
        self._iqs = np.array(
            [_evaluate_complex(entry, arguments) for entry in definition.matrix],
            dtype=np.complex128,
        )

    @property
    def iqs(self) -> npt.NDArray[np.complex128]:
        """This waveform's unit-scale IQ samples, evaluated at construction."""
        return self._iqs


@dataclass(frozen=True, kw_only=True)
class WaveformKey:
    """Identifies a waveform shape.

    Two invocations with equal keys draw the same samples up to a scale factor.
    """

    name: str
    """The waveform's name."""

    sample_rate: float
    """The playing frame's sample rate in Hz."""

    builtin: bool
    """Whether this names one of the recognized builtin waveforms."""

    params: tuple[tuple[str, complex], ...]
    """Sorted parameters, with a builtin's `scale` excluded."""

    @classmethod
    def from_invocation(cls, invocation: WaveformInvocation, sample_rate: float) -> Self:
        """Key `invocation` by its shape, ignoring a builtin's `scale`.

        Two invocations that differ only in scale share one key, and so one
        cached set of samples.

        Args:
            invocation: The waveform named by a `PULSE` or `CAPTURE`.
            sample_rate: The playing frame's sample rate in Hz.

        Returns:
            The key identifying `invocation`'s shape.
        """
        builtin = invocation.name in (
            "drag_gaussian",
            "flat",
            "gaussian",
            "erf_square",
            "hrm_gauss",
            "boxcar_kernel",
        )
        return cls(
            name=invocation.name,
            sample_rate=sample_rate,
            builtin=builtin,
            params=tuple(
                sorted(
                    (name, _evaluate_complex(expression))
                    for name, expression in invocation.parameters.items()
                    if not (builtin and name == "scale")
                )
            ),
        )


class PlottableWaveformCache:
    """The distinct waveform shapes a block plays.

    Many pulses replay the same calibration, differing only in start time, lane,
    and a complex scale, so their shape is stored once and shared.
    """

    def __init__(self) -> None:
        """Start empty; shapes are interned as the block's pulses are read."""
        self.table: list[PlottableWaveform] = []
        """The interned waveform shapes, indexed by id."""

        self.ids: dict[WaveformKey, int] = {}
        """Each interned shape's id, keyed by its `WaveformKey`."""

    def cache(
        self,
        invocation: WaveformInvocation,
        definitions: dict[str, Waveform],
        sample_rate: float,
    ) -> tuple[int, float]:
        """Intern `invocation`'s shape and return a reference to it.

        Args:
            invocation: The waveform named by a `PULSE` or `CAPTURE`.
            definitions: The program's `DEFWAVEFORM`s, by name, consulted only
                for a custom waveform.
            sample_rate: The playing frame's sample rate in Hz.

        Returns:
            The shape's id in `table`, and the scale a pulse should apply
            to it.

        Raises:
            WaveformResolutionError: If the waveform cannot be sampled,
                `invocation` cannot be parsed, or names a custom waveform with
                no `DEFWAVEFORM` in `definitions`.
        """
        key = WaveformKey.from_invocation(invocation, sample_rate)

        # Cache Hit
        if key in self.ids:
            if key.builtin:
                params = invocation.parameters
                scale = _evaluate_real(params["scale"]) if "scale" in params else 1.0
            else:
                scale = 1.0
            return (self.ids[key], scale)

        # Cache Miss Parse the actual waveform
        try:
            parsed = quil.waveform.Waveform.from_quil(invocation)
        except Exception as e:
            raise WaveformResolutionError(
                f"could not parse {invocation.to_quil_or_debug()!r}: {e}"
            ) from e
        waveform = parsed.evaluate(_evaluate_real, _evaluate_complex)

        # Process builtin waveforms
        builtin = waveform.as_builtin()
        if builtin is not None:
            builtin_waveform, common = builtin
            scale = 1.0 if common.scale is None else common.scale
            common.scale = None  # the cached shape is unscaled
            plottable_b = PlottableBuiltinWaveform(builtin_waveform, common, sample_rate)
            return (self._insert(key, plottable_b), scale)

        # Process custom waveforms
        custom = waveform.as_custom()
        if custom is not None:
            name, arguments = custom
            definition = definitions.get(name)
            if definition is None:
                raise WaveformResolutionError(
                    f"no DEFWAVEFORM for custom waveform {name!r} (known: {sorted(definitions)})"
                )
            plottable_c = PlottableCustomWaveform(name, definition, arguments, sample_rate)
            return (self._insert(key, plottable_c), 1.0)

        raise WaveformResolutionError(f"Unknown Waveform variant for {invocation.name!r}.")

    def _insert(self, key: WaveformKey, plottable: PlottableWaveform) -> int:
        """Insert the `key`, `plottable` pair into the cache."""
        key_id = len(self.table)
        self.table.append(plottable)
        self.ids[key] = key_id
        return key_id

    def build_records(self, max_points: int | None = None) -> list[dict[str, Any]]:
        """Build Altair record table for the waveforms.

        Args:
            max_points: Cap each shape's sample count at this many points. The
                default `None` applies no cap.

        Returns:
            A row per shape, carrying its sample offsets and I/Q values.
        """
        records = []
        for waveform_id, waveform in enumerate(self.table):
            ts = round_sig(waveform.ts)
            iqs = round_sig(waveform.iqs.real) + 1j * round_sig(waveform.iqs.imag)
            ts, iqs = compress_constant_runs(ts, iqs)
            if max_points is not None:
                ts, iqs = decimate_to_max_points(ts, iqs, max_points)
            records.append(
                {
                    "s": waveform_id,
                    "dt": ts.tolist(),
                    "i": iqs.real.tolist(),
                    "q": iqs.imag.tolist(),
                    "hasq": waveform.has_q,
                }
            )
        return records
