import copy
from typing import Any, Callable, Iterator

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


def _calibration(qubit: Qubit) -> tuple[CalibrationDefinition, Instruction]:
    calibration = CalibrationDefinition(
        CalibrationIdentifier("MYCAL", [], [qubit], []),
        [Instruction.Delay(Delay(Expression.Number(complex(0.5)), [], [qubit]))],
    )
    return calibration, Instruction.CalibrationDefinition(calibration)


def _pulse(qubit: Qubit) -> tuple[Pulse, Instruction]:
    pulse = Pulse(blocking=False, frame=FrameIdentifier("frame", [qubit]), waveform=WaveformInvocation("wf", {}))
    instr = Instruction.Pulse(pulse)
    return (pulse, instr)


@pytest.fixture(params=(_calibration, _pulse))
def make_instr(request: pytest.FixtureRequest) -> Iterator[Callable[[Qubit], tuple[Any, Instruction]]]:
    return request.param


def test_copy(qubit: Qubit, make_instr: Callable[[Qubit], tuple[Any, Instruction]]):
    inner, instr = make_instr(qubit)

    assert copy.copy(inner) == inner
    assert copy.copy(instr) == instr

    if not isinstance(qubit, Qubit.Placeholder):
        assert copy.deepcopy(inner) == inner
        assert copy.deepcopy(instr) == instr
    else:
        with pytest.raises(quil.PickleError, match=r"\bQubitPlaceholder\b"):
            _ = copy.deepcopy(inner)
        with pytest.raises(quil.PickleError, match=r"\bQubitPlaceholder\b"):
            _ = copy.deepcopy(instr)

