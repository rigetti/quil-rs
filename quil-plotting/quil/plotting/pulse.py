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

"""Events that occur in a pulse schedule."""

from dataclasses import dataclass
from typing import Any

from quil.instructions import FrameIdentifier, Instruction

from ._utils import short


@dataclass(kw_only=True)
class PlottablePulseEvent:
    """Something drawn on a pulse schedule diagram at a specific time.

    See Also:
        - {py:obj}`PlottablePulse`: a `PULSE` or `CAPTURE`, drawn as an
          envelope.
        - {py:obj}`PlottableFrameUpdate`: a phase, frequency or scale change.
        - {py:obj}`PlottableRawCapture`: a `RAW-CAPTURE`, drawn as a plain span.
    """

    instruction: Instruction
    """The primitive Quil-T instruction after calibration expansion."""

    logical_instruction_name: str
    """The logical instruction this expanded out of, i.e. `RX`."""

    start_time: float
    """When the event begins, in seconds, from the start of its block."""

    channel_type: str
    """The hardware channel type behind the frame."""

    qubit: str
    """The qubit the frame acts on, couplers included."""

    hidden: bool
    """Whether `hide` has excluded it from the diagram."""

    @property
    def frame(self) -> FrameIdentifier:
        """The frame the event sits on."""
        return self.instruction._0.frame  # type: ignore


@dataclass(kw_only=True)
class PlottablePulse(PlottablePulseEvent):
    """A `PULSE` or `CAPTURE`, drawn as its waveform's envelope.

    A capture's waveform is a demodulation kernel rather than an emitted
    envelope, but both occupy a frame for a known duration and draw the same.
    """

    waveform_id: int
    """Index of this pulse's shape in its block's `waveforms` cache."""

    scale: float
    """The scale applied to that shape's unit-scale samples."""

    duration: float
    """How long the pulse occupies its frame, in seconds."""

    memory_reference: str = ""
    """Where a `CAPTURE` writes its result, or empty for a plain `PULSE`."""

    def build_record(
        self,
        index: int,
        lane: int,
        label: str,
        normalization: float,
        lane_fraction: float,
    ) -> dict[str, Any]:
        """One chart row for this pulse.

        Args:
            index: A per-chart identifier, distinguishing this pulse's samples
                from those of every other pulse.
            lane: The row this pulse is drawn on.
            label: The color group this pulse belongs to.
            normalization: Divisor for {py:obj}`scale`, so the loudest pulse of
                a group fills its lane and quieter ones stay in proportion.
            lane_fraction: The fraction of a lane a full-scale pulse spans.

        Returns:
            The necessary data for Altair to plot this pulse.
        """
        return {
            "p": index,
            "s": self.waveform_id,
            "t0": short(self.start_time),
            "t1": short(self.start_time + self.duration),
            "b": lane,
            "kr": short(lane_fraction * self.scale / normalization),
            "label": label,
            "operation": self.logical_instruction_name,
            "frame": self.frame.name,
            "channel": self.channel_type,
            "memory": self.memory_reference,
        }


@dataclass(kw_only=True)
class PlottableFrameUpdate(PlottablePulseEvent):
    """An instantaneous frame-state change, drawn as a marker on its lane.

    Covers `SHIFT-PHASE`, `SET-PHASE`, `SET-FREQUENCY`, `SHIFT-FREQUENCY`,
    `SET-SCALE` and `SWAP-PHASES`. None of these plays a waveform - they change
    how a frame's carrier is generated for whatever comes next on it - so each
    is drawn as a marker whose shape names the instruction.
    """

    # A swap is drawn on both lanes it touches, so the event has to be told
    # which of them it is - the one thing its instruction cannot say.
    second_swap_frame: bool = False
    """Which of a `SWAP-PHASES`'s two frames this event was built for."""

    @property
    def frame(self) -> FrameIdentifier:
        """The frame this event sits on - for a `SWAP-PHASES`, its own half."""
        payload = self.instruction._0  # type: ignore
        if isinstance(self.instruction, Instruction.SwapPhases):
            return payload.frame_2 if self.second_swap_frame else payload.frame_1
        return payload.frame

    @property
    def partner_frame(self) -> FrameIdentifier | None:
        """The other frame a `SWAP-PHASES` traded phase with."""
        if not isinstance(self.instruction, Instruction.SwapPhases):
            return None
        payload = self.instruction._0
        return payload.frame_1 if self.second_swap_frame else payload.frame_2

    def build_record(self, lane: int) -> dict[str, Any]:
        """One chart row for this update, marking where it occurs.

        Args:
            lane: The row this update is drawn on.

        Returns:
            The necessary data for Altair to plot this update.
        """
        # `label` carries the primitive's own mnemonic (`SHIFT-PHASE`,
        # `SET-SCALE`, ...) under the same field name a pulse uses for its
        # color group, so both kinds of record share the one field the legend
        # and its selection are built on.
        return {
            "t": short(self.start_time),
            "b": lane,
            "frame": self.frame.name,
            "channel": self.channel_type,
            "operation": self.logical_instruction_name,
            "label": self.instruction.name,
            "partner": self.partner_frame.name if self.partner_frame is not None else "",
        }


@dataclass(kw_only=True)
class PlottableRawCapture(PlottablePulseEvent):
    """A `RAW-CAPTURE`, drawn as a plain block spanning its acquisition.

    There is no kernel waveform to demodulate against and so no envelope to
    draw, but it still occupies its frame for the whole window, so it is drawn
    as a filled span sized like a full-scale pulse.
    """

    duration: float
    """How long the acquisition runs, in seconds."""

    memory_reference: str
    """Where the acquired samples are written, as Quil text."""

    def build_record(self, lane: int, label: str, lane_fraction: float) -> dict[str, Any]:
        """One chart row for this capture, spanning `t` to `t2` on its lane.

        Args:
            lane: The row this capture is drawn on.
            label: The color group this capture belongs to.
            lane_fraction: The fraction of a lane a full-scale pulse spans. A
                raw capture has no envelope, so it is drawn as a block of
                exactly that height.

        Returns:
            The necessary data for Altair to plot this capture.
        """
        return {
            "t": short(self.start_time),
            "t2": short(self.start_time + self.duration),
            "b": lane,
            "b2": lane + lane_fraction,
            "label": label,
            "operation": self.logical_instruction_name,
            "frame": self.frame.name,
            "channel": self.channel_type,
            "memory": self.memory_reference,
        }
