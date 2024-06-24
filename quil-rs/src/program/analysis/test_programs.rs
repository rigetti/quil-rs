pub const QUIL_WITH_DIAMOND: &str = r"
CNOT 0 1
X 0
X 1
X 1
X 1
X 1
CNOT 1 0";

pub const QUIL_AS_TREE: &str = r"
CNOT 0 1
X 0
H 1";

pub const QUIL_AS_INVERSE_TREE: &str = r"
X 0
H 1
CNOT 0 1";

pub const QUIL_AS_LINEAR: &str = r"
X 0
X 0
X 0
X 0";

pub const QUIL_WITH_SWAP: &str = r"
X 0
X 1
SWAP 0 1
X 1
X 0";

pub const QUIL_WITH_JUMP: &str = r"
LABEL @label
X 0
X 1
JUMP @label
X 1
X 0";

pub const QUIL_WITH_JUMP_WHEN: &str = r"
LABEL @label
X 0
X 1
JUMP-WHEN @label ro
X 1
X 0";

pub const QUIL_WITH_JUMP_UNLESS: &str = r"
LABEL @label
X 0
X 1
JUMP-UNLESS @label ro
X 1
X 0";

pub const KITCHEN_SINK_QUIL: &str = "DECLARE ro BIT[1]
DEFGATE HADAMARD AS MATRIX:
\t(1/sqrt(2)),(1/sqrt(2))
\t(1/sqrt(2)),((-1)/sqrt(2))

DEFGATE RX(%theta) AS MATRIX:
\tcos((%theta/2)),((-1i)*sin((%theta/2)))
\t((-1i)*sin((%theta/2))),cos((%theta/2))

DEFGATE Name AS PERMUTATION:
\t1, 0

DEFCIRCUIT SIMPLE:
\tX 0
\tX 1

RX 0
CZ 0 1
MEASURE 0 ro[0]
DEFCAL X 0:
\tPULSE 0 \"xy\" my_waveform()

DEFCAL RX(%theta) 0:
\tPULSE 0 \"xy\" my_waveform()

DEFCAL MEASURE 0 dest:
\tDECLARE iq REAL[2]
\tCAPTURE 0 \"out\" flat(duration: 1000000, iqs: (2+3i)) iq[0]

DEFFRAME 0 \"xy\":
\tSAMPLE-RATE: 3000

DEFFRAME 0 \"xy\":
\tDIRECTION: \"rx\"
\tCENTER-FREQUENCY: 1000
\tHARDWARE-OBJECT: \"some object\"
\tINITIAL-FREQUENCY: 2000
\tSAMPLE-RATE: 3000
";
