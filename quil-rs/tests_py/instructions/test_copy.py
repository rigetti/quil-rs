import copy
from typing import Callable, Iterator

import pytest

import quil
from quil.expression import Expression
from quil.instructions import (
    CalibrationDefinition,
    CalibrationIdentifier,
    Delay,
    FrameIdentifier,
    Instruction,
    Pulse,
    Qubit,
    QubitPlaceholder,
    WaveformInvocation,
)


@pytest.fixture(params=(Qubit.Placeholder(QubitPlaceholder()), Qubit.Fixed(1), Qubit.Variable("x")))
def qubit(request: pytest.FixtureRequest) -> Qubit:
    assert isinstance(request.param, Qubit)
    return request.param


def _calibration(qubit: Qubit) -> CalibrationDefinition:
    return CalibrationDefinition(
        CalibrationIdentifier("MYCAL", [], [qubit], []),
        [Delay(Expression.Number(complex(0.5)), [], [qubit])],
    )


def _pulse(qubit: Qubit) -> Pulse:
    return Pulse(blocking=False, frame=FrameIdentifier("frame", [qubit]), waveform=WaveformInvocation("wf", {}))


@pytest.fixture(params=(_calibration, _pulse))
def make_instr(request: pytest.FixtureRequest) -> Iterator[Callable[[Qubit], Instruction]]:
    return request.param


def test_copy(qubit: Qubit, make_instr: Callable[[Qubit], Instruction]):
    instr = make_instr(qubit)

    assert copy.copy(instr) == instr

    if not isinstance(qubit, Qubit.Placeholder):
        assert copy.deepcopy(instr) == instr
    else:
        with pytest.raises(quil.PickleError, match=r"\bQubitPlaceholder\b"):
            _ = copy.deepcopy(instr)

