from copy import copy, deepcopy

from quil.expression import Expression
from quil.instructions import (
    Calibration,
    CalibrationIdentifier,
    Delay,
    FrameIdentifier,
    Instruction,
    Pulse,
    Qubit,
    QubitPlaceholder,
    WaveformInvocation,
)


def test_instruction_with_qubit():
    frame_identifier = FrameIdentifier("frame", [Qubit.placeholder(QubitPlaceholder())])
    waveform_invocation = WaveformInvocation("wf", {})
    pulse = Pulse(False, frame_identifier, waveform_invocation)

    pulse_deepcopy = deepcopy(pulse)
    assert pulse_deepcopy.blocking == pulse.blocking
    assert pulse_deepcopy.waveform == pulse.waveform
    assert pulse_deepcopy.frame.name == pulse.frame.name
    assert pulse_deepcopy.frame.qubits != pulse.frame.qubits

    instruction = Instruction(pulse)
    instruction_deepcopy = deepcopy(instruction)
    assert instruction_deepcopy != instruction


def test_instruction_with_duplicate_placeholders():
    placeholder = Qubit.placeholder(QubitPlaceholder())

    calibration = Calibration(
        CalibrationIdentifier("MYCAL", [], [placeholder], []),
        [Instruction.delay(Delay(Expression.number(complex(0.5)), [], [placeholder]))],
    )

    calibration_deepcopy = deepcopy(calibration)
    assert calibration_deepcopy != calibration

    assert calibration_deepcopy.identifier.qubits == calibration_deepcopy.instructions[0].delay().qubits
