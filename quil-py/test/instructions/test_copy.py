from copy import copy, deepcopy

from quil.instructions import Qubit, QubitPlaceholder, FrameIdentifier, Pulse, WaveformInvocation, Instruction

def test_instruction_with_qubit():
    frame_identifier = FrameIdentifier("frame", [Qubit.from_placeholder(QubitPlaceholder())])
    waveform_invocation = WaveformInvocation("wf", {})
    pulse = Pulse(False, frame_identifier, waveform_invocation)

    pulse_copy = copy(pulse)
    assert pulse_copy == pulse

    pulse_deepcopy = deepcopy(pulse)
    assert pulse_deepcopy.blocking == pulse.blocking
    assert pulse_deepcopy.waveform == pulse.waveform
    assert pulse_deepcopy.frame.name == pulse.frame.name
    assert pulse_deepcopy.frame.qubits != pulse.frame.qubits
    
    instruction = Instruction(pulse)
    instruction_deepcopy = deepcopy(instruction)
    assert instruction_deepcopy != instruction
