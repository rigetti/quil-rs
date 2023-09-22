from copy import copy, deepcopy

from quil.expression import Expression
from quil.instructions import Qubit, QubitPlaceholder, FrameIdentifier, Pulse, WaveformInvocation, Instruction, Calibration, Delay

def test_instruction_with_qubit():
    frame_identifier = FrameIdentifier("frame", [Qubit.from_placeholder(QubitPlaceholder())])
    waveform_invocation = WaveformInvocation("wf", {})
    pulse = Pulse(False, frame_identifier, waveform_invocation)

    pulse_copy = copy(pulse)
    assert pulse_copy == pulse
    pulse_copy.frame.name = "renamed_frame"
    assert pulse_copy == pulse

    pulse_deepcopy = deepcopy(pulse)
    assert pulse_deepcopy.blocking == pulse.blocking
    assert pulse_deepcopy.waveform == pulse.waveform
    assert pulse_deepcopy.frame.name == pulse.frame.name
    assert pulse_deepcopy.frame.qubits != pulse.frame.qubits
    
    instruction = Instruction(pulse)
    instruction_deepcopy = deepcopy(instruction)
    assert instruction_deepcopy != instruction


def test_instruction_with_duplicate_placeholders():
    placeholder = Qubit.from_placeholder(QubitPlaceholder())

    calibration = Calibration(
            "MYCAL",
            [],
            [placeholder],
            [
                Instruction.from_delay(
                    Delay(Expression.from_number(complex(0.5)), [], [placeholder])
                )
            ],
            [])

    calibration_copy = copy(calibration)
    assert calibration_copy == calibration

    calibration_deepcopy = deepcopy(calibration)
    assert calibration_deepcopy != calibration

    assert calibration_deepcopy.qubits == calibration_deepcopy.instructions[0].to_delay().qubits
