from quil.program import Program

quil = '''
DEFCAL seq1 0 1:
    FENCE 0 1
    NONBLOCKING PULSE 0 "rf" drag_gaussian(duration: 6.000000000000001e-08, fwhm: 1.5000000000000002e-08, t0: 3.0000000000000004e-08, anh: -190000000.0, alpha: -1.6453719598238201, scale: 0.168265925924524, phase: 0.0, detuning: 0)
    NONBLOCKING PULSE 1 "rf" drag_gaussian(duration: 6.000000000000001e-08, fwhm: 1.5000000000000002e-08, t0: 3.0000000000000004e-08, anh: -190000000.0, alpha: -1.6453719598238201, scale: 0.168265925924524, phase: 0.0, detuning: 0)
    FENCE 0 1

DEFGATE seq1() a b AS SEQUENCE:
    RX(pi/2) a
    RX(pi/2) b

DEFGATE seq2(%theta, %psi, %phi) a AS SEQUENCE:
    RZ(%theta) a
    RX(pi/2) a
    RZ(%psi) a
    RX(pi/2) a
    RZ(%phi) a

seq1 0 1
seq2(1.5707963267948966, 3.141592653589793, 0) 0
seq2(3.141592653589793, 0, 1.5707963267948966) 1
'''
program = Program.parse(quil);
calibrated_gate_names = {calibration.identifier.name for calibration in program.calibrations.calibrations}
expanded_program = program.expand_defgate_sequences(lambda name: name not in calibrated_gate_names);

expected_quil = '''
DEFCAL seq1 0 1:
    FENCE 0 1
    NONBLOCKING PULSE 0 "rf" drag_gaussian(duration: 6.000000000000001e-08, fwhm: 1.5000000000000002e-08, t0: 3.0000000000000004e-08, anh: -190000000.0, alpha: -1.6453719598238201, scale: 0.168265925924524, phase: 0.0, detuning: 0)
    NONBLOCKING PULSE 1 "rf" drag_gaussian(duration: 6.000000000000001e-08, fwhm: 1.5000000000000002e-08, t0: 3.0000000000000004e-08, anh: -190000000.0, alpha: -1.6453719598238201, scale: 0.168265925924524, phase: 0.0, detuning: 0)
    FENCE 0 1

DEFGATE seq1 a b AS SEQUENCE:
    RX(pi/2) a
    RX(pi/2) b

seq1 0 1

RZ(1.5707963267948966) 0
RX(pi/2) 0
RZ(3.141592653589793) 0
RX(pi/2) 0
RZ(0) 0

RZ(3.141592653589793) 1
RX(pi/2) 1
RZ(0) 1
RX(pi/2) 1
RZ(1.5707963267948966) 1
'''

expected_program = Program.parse(expected_quil)

assert expanded_program == expected_program
