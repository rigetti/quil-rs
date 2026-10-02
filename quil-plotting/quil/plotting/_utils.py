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

"""Numeric helper functions for the quil plotting library."""

from collections.abc import Iterable, Iterator, Mapping, Sequence

import numpy as np
import numpy.typing as npt
from quil.expression import EvaluationError, Expression
from quil.instructions import MemoryReference

MemoryMap = Mapping[str, Sequence[int] | Sequence[float]]
"""Runtime values of a program's classical memory."""


def _memory_references(expression: Expression) -> Iterator[MemoryReference]:
    """Every memory reference `expression` reads, in order, repeats included."""
    match expression:
        case Expression.Address(reference):
            yield reference
        case Expression.Infix(infix):
            yield from _memory_references(infix.left)
            yield from _memory_references(infix.right)
        case Expression.Prefix(inner) | Expression.FunctionCall(inner):
            yield from _memory_references(inner.expression)


def _evaluate_complex(
    expression: Expression,
    variables: Mapping[str, complex] | None = None,
    *,
    memory_map: MemoryMap,
    subject: str,
) -> complex:
    """Reduce a Quil expression to a complex number.

    Args:
        expression: The expression to reduce.
        variables: Values for the `%variables` it names.
        memory_map: Runtime memory values it may read. Required, even when
            empty, so no call site forgets to pass the user's map along.
        subject: What reads `expression`, e.g. `"SET-SCALE"`, named in the
            missing-memory error.

    Returns:
        The expression's value.

    Raises:
        RuntimeError: If `expression` reads memory `memory_map` does not hold,
            or names an unbound `%variable`.
    """
    try:
        return expression.evaluate(variables or {}, memory_map)
    except EvaluationError as e:
        # Parse the generic error message and produce a nice robust one.
        missing = sorted(
            {
                (reference.name, reference.index)
                for reference in _memory_references(expression)
                if reference.index >= len(memory_map.get(reference.name, ()))
            },
        )
        if missing:
            references = ", ".join(f"{name}[{index}]" for name, index in missing)
            example = ", ".join(
                f"{name!r}: [...]" for name in sorted({name for name, _ in missing})
            )
            raise RuntimeError(
                f"{subject} reads {references}, which is only known at runtime. Pass its "
                f"value with `PlottableProgramPulseSchedule(program, memory_map={{{example}}})`."
            ) from e
        raise RuntimeError(
            f"could not evaluate {expression!r} to a number: {e}. Waveform parameters "
            "must be resolvable at compile time; unbound %variables are not allowed here."
        ) from e


def _evaluate_real(
    expression: Expression,
    variables: Mapping[str, complex] | None = None,
    *,
    memory_map: MemoryMap,
    subject: str,
) -> float:
    """Reduce a Quil expression to a real number.

    See: `_evaluate_complex` for more information.

    Args:
        expression: The expression to reduce.
        variables: Values for the `%variables` it names.
        memory_map: Runtime memory values it may read.
        subject: What reads `expression`, named in the missing-memory error.

    Returns:
        The expression's real value.

    Raises:
        RuntimeError: If `_evaluate_complex` cannot reduce `expression`, or its
            value has a nonzero imaginary part.
    """
    value = _evaluate_complex(expression, variables, memory_map=memory_map, subject=subject)
    if abs(value.imag) >= 1e-16:  # account for floating point errors
        raise RuntimeError(f"expected {expression!r} to be real-valued, got {value}")
    return value.real


def round_sig(values: Iterable[float], figures: int = 5) -> npt.NDArray[np.float64]:
    """Round each value to `figures` significant figures.

    Round using significant figures rather than `np.round`'s fixed decimal
    places, since a schedule's times span several orders of magnitude, so a
    fixed decimal count either flattens the small values to zero or leaves the
    large ones unrounded. Zeros and non-finite values pass through untouched.

    Args:
        values: The numbers to round.
        figures: Significant figures to keep. Must be at least 1. The default is
            far finer than a plot can resolve, and keeps the serialized chart
            small.

    Returns:
        A new `float64` array; the input is not modified.

    Raises:
        ValueError: If `figures` is less than 1.

    See Also:
        `short`: The same rounding for a single number
    """
    if figures < 1:
        raise ValueError(f"figures must be at least 1, got {figures}")

    return np.array([short(v, figures) for v in values])


def short(value: float, figures: int = 5) -> float:
    """Round `value` to `figures` significant figures."""
    return float(f"{value:.{figures}g}")


def compress_constant_runs(
    ts: npt.NDArray[np.float64],
    iqs: npt.NDArray[np.complex128],
) -> tuple[npt.NDArray[np.float64], npt.NDArray[np.complex128]]:
    """Drop the interior samples of runs of constant IQ value.

    Waveforms are sampled at the frame's sample rate, so a stretch held at a
    constant value - the flat top of a pulse, the zero padding of an
    `erf_square`, a long readout - costs a sample per clock tick. Dropping the
    interior makes the point count scale with the number of *changes* in the
    waveform rather than with its duration. This makes the pulse more efficient
    to plot while being visually lossless.

    Args:
        ts: Sample times.
        iqs: IQ values, one per entry of `ts`.

    Returns:
        The `(ts, iqs)` pair with interior samples of constant runs removed.

    See Also:
        `decimate_to_max_points`: the lossy optimization pass for waveforms.
    """
    if len(iqs) <= 2:
        return ts, iqs

    # Keep a sample if it differs from either neighbor. That retains both
    # endpoints of every constant run, and every sample of a run only one long.
    changed = iqs[1:] != iqs[:-1]
    keep = np.empty(len(iqs), dtype=bool)
    keep[0] = keep[-1] = True
    keep[1:-1] = changed[:-1] | changed[1:]
    return ts[keep], iqs[keep]


def decimate_to_max_points(
    ts: npt.NDArray[np.float64],
    iqs: npt.NDArray[np.complex128],
    max_points: int,
) -> tuple[npt.NDArray[np.float64], npt.NDArray[np.complex128]]:
    """Cap sample count at `max_points` while keeping local extrema.

    Lossy, unlike `compress_constant_runs`. A pulse's peak amplitude and its
    start/end timing survive even at a small cap, so the envelope stays
    recognizable. A no-op if already at or under `max_points`.

    Args:
        ts: Sample times.
        iqs: IQ values, one per entry of `ts`.
        max_points: Ceiling on the returned sample count.

    Returns:
        The `(ts, iqs)` pair reduced to at most `max_points` samples.

    Raises:
        ValueError: If `max_points` is less than or equal to 1.
    """
    if max_points <= 1:
        raise ValueError(f"Expected max_points greater than 1, got {max_points}")

    if len(iqs) <= max_points:
        return ts, iqs

    n_blocks = (max_points - 2) // 2
    edges = np.linspace(0, len(iqs), n_blocks + 1, dtype=int)
    magnitudes = np.abs(iqs)

    keep = np.zeros(len(iqs), dtype=bool)
    keep[0] = keep[-1] = True
    for lo, hi in zip(edges[:-1], edges[1:], strict=True):
        block = magnitudes[lo:hi]
        keep[lo + int(np.argmin(block))] = True
        keep[lo + int(np.argmax(block))] = True

    return ts[keep], iqs[keep]
