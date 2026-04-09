# ruff: noqa: S101, D100, D103

import cmath
import textwrap
from cmath import pi
from typing import Any, Optional, TypeVar, Union

from hypothesis import given
from hypothesis import strategies as st
from hypothesis.strategies import DrawFn, SearchStrategy
from quil import waveform
from quil.expression import Expression
from quil.instructions import FrameIdentifier, Instruction, Qubit
from quil.program import Program
from quil.waveform import (
    ConcreteBuiltinWaveform,
    ConcreteCommonBuiltinParameters,
    ConcreteWaveform,
    SyntacticWaveform,
)

# Strategy combinators


def in_range(min: float, max: float) -> SearchStrategy[float]:
    return st.floats(min_value=min, max_value=max)


T = TypeVar('T')

def optional(strategy: SearchStrategy[T]) -> SearchStrategy[Optional[T]]:
    # Hypothesis says it knows better about weighted distributions than I do
    return st.one_of(st.none(), strategy)


# Strategies for `ConcreteCommonBuiltinParameters`


def duration() -> SearchStrategy[float]:
    return in_range(4e-9, 1e-3)


def scale() -> SearchStrategy[Optional[float]]:
    return optional(in_range(0, 2))


def phase() -> SearchStrategy[Optional[float]]:
    return optional(in_range(-4 * pi, 4 * pi))


def detuning() -> SearchStrategy[Optional[float]]:
    return optional(in_range(-1e10, 1e10))


# Strategies for waveform parameters


@st.composite
def unit_complex(draw: DrawFn) -> complex:
    phase = draw(st.floats(min_value=0, max_value=2 * pi, exclude_max=True))
    return cmath.exp(phase)


def time() -> SearchStrategy[float]:
    return in_range(0, 1e-3)


def anharmonicity() -> SearchStrategy[float]:
    return in_range(-1e10, 1e10)


def drag() -> SearchStrategy[float]:
    return in_range(-10, 10)


@st.composite
def common_builtin_parameters(draw: DrawFn) -> ConcreteCommonBuiltinParameters:
    t = draw(duration())
    s = draw(scale())
    ϕ = draw(phase())
    d = draw(detuning())
    common = ConcreteCommonBuiltinParameters(duration=t, scale=s, phase=ϕ, detuning=d)
    assert type(common) == ConcreteCommonBuiltinParameters
    assert common.duration == t
    assert common.scale == s
    assert common.phase == ϕ
    assert common.detuning == d
    return common


# Testing waveforms


def assert_builtin(
    # Should be a callable with the signature
    # ```
    #     (
    #       *,
    #       duration: float,
    #       scale: Optional[float],
    #       phase: Optional[float],
    #       detuning: Optional[float],
    #       **kwargs
    #     ) -> ConcreteWaveform
    # ```
    make_waveform: Any,
    common: ConcreteCommonBuiltinParameters,
    **parameters,
) -> Union[
    waveform.ConcreteFlat,
    waveform.ConcreteGaussian,
    waveform.ConcreteDragGaussian,
    waveform.ConcreteErfSquare,
    waveform.ConcreteHermiteGaussian,
    waveform.BoxcarKernel,
]:
    wf = make_waveform(
        duration=common.duration,
        scale=common.scale,
        phase=common.phase,
        detuning=common.detuning,
        **parameters,
    )
    assert type(wf) == ConcreteWaveform

    builtin_and_params = wf.as_builtin()
    assert type(builtin_and_params) == tuple
    assert len(builtin_and_params) == 2

    builtin, wf_common = builtin_and_params
    assert type(wf_common) == ConcreteCommonBuiltinParameters
    assert common == wf_common

    assert type(builtin) == ConcreteBuiltinWaveform
    return builtin.as_inner()


@given(common=common_builtin_parameters(), iq=unit_complex())
def test_flat(common: ConcreteCommonBuiltinParameters, iq: complex):
    flat = assert_builtin(ConcreteWaveform.flat, common, iq=iq)
    assert type(flat) == waveform.ConcreteFlat
    assert flat.iq == iq


@given(common=common_builtin_parameters(), fwhm=time(), t0=time())
def test_gaussian(
    common: ConcreteCommonBuiltinParameters,
    fwhm: float,
    t0: float,
):
    gaussian = assert_builtin(ConcreteWaveform.gaussian, common, fwhm=fwhm, t0=t0)
    assert type(gaussian) == waveform.ConcreteGaussian
    assert gaussian.fwhm == fwhm
    assert gaussian.t0 == t0


@given(
    common=common_builtin_parameters(),
    fwhm=time(),
    t0=time(),
    anh=anharmonicity(),
    alpha=drag(),
)
def test_drag_gaussian(
    common: ConcreteCommonBuiltinParameters,
    fwhm: float,
    t0: float,
    anh: float,
    alpha: float,
):
    drag_gaussian = assert_builtin(
        ConcreteWaveform.drag_gaussian, common, fwhm=fwhm, t0=t0, anh=anh, alpha=alpha
    )
    assert type(drag_gaussian) == waveform.ConcreteDragGaussian
    assert drag_gaussian.anh == anh
    assert drag_gaussian.alpha == alpha
    assert drag_gaussian.fwhm == fwhm
    assert drag_gaussian.t0 == t0


@given(
    common=common_builtin_parameters(),
    risetime=time(),
    pad_left=time(),
    pad_right=time(),
)
def test_erf_square(
    common: ConcreteCommonBuiltinParameters,
    risetime: float,
    pad_left: float,
    pad_right: float,
):
    erf_square = assert_builtin(
        ConcreteWaveform.erf_square,
        common,
        risetime=risetime,
        pad_left=pad_left,
        pad_right=pad_right,
    )
    assert type(erf_square) == waveform.ConcreteErfSquare
    assert erf_square.risetime == risetime
    assert erf_square.pad_left == pad_left
    assert erf_square.pad_right == pad_right


@given(
    common=common_builtin_parameters(),
    fwhm=time(),
    t0=time(),
    anh=anharmonicity(),
    alpha=drag(),
    second_order_hrm_coeff=in_range(-1, 1),
)
def test_hermite_gaussian(
    common: ConcreteCommonBuiltinParameters,
    fwhm: float,
    t0: float,
    anh: float,
    alpha: float,
    second_order_hrm_coeff: float,
):
    hermite_gaussian = assert_builtin(
        ConcreteWaveform.hermite_gaussian,
        common,
        fwhm=fwhm,
        t0=t0,
        anh=anh,
        alpha=alpha,
        second_order_hrm_coeff=second_order_hrm_coeff,
    )
    assert type(hermite_gaussian) == waveform.ConcreteHermiteGaussian
    assert hermite_gaussian.anh == anh
    assert hermite_gaussian.alpha == alpha
    assert hermite_gaussian.fwhm == fwhm
    assert hermite_gaussian.t0 == t0
    assert hermite_gaussian.second_order_hrm_coeff == second_order_hrm_coeff


@given(common=common_builtin_parameters())
def test_boxcar_kernel(common: ConcreteCommonBuiltinParameters):
    boxcar_kernel = assert_builtin(ConcreteWaveform.boxcar_kernel, common)
    assert type(boxcar_kernel) == waveform.BoxcarKernel


def test_parsed():
    program = Program.parse(
        textwrap.dedent(
            """
    PULSE 0 "tx" flat(duration: 1e-6, iq: i)
    PULSE 0 "tx" gaussian(duration: 2e-8, scale: 1+1, fwhm: 1e-8, t0: 0.5e-8)
    PULSE 0 "tx" drag_gaussian(duration: 2.6e-7, phase: pi/2, fwhm: 0.5e-7, t0: 1e-7, anh: 1_000_000, alpha: 3)
    PULSE 0 "tx" erf_square(duration: 3e-7, detuning: 123_456_789, risetime: 12e-9, pad_left: 4e-9, pad_right: 8e-9)
    PULSE 0 "tx" hrm_gauss(duration: 4e-8, scale: 0.5, detuning: -1e8, fwhm: 1.5e-8, t0: 0.75e-8, anh: -1_000_000, alpha: -3, second_order_hrm_coeff: 0.42)
    PULSE 0 "tx" boxcar_kernel(duration: 6e-8, scale: 1.5, phase: pi, detuning: 987_654_321)
    PULSE 0 "tx" special(answer: 2*(21 + 0.5i))
    """
        )
    )

    program_syntactic_pulses = []
    program_concrete_pulses = []

    for instruction in program.body_instructions:
        assert isinstance(instruction, Instruction.Pulse)
        pulse = instruction._0
        assert pulse.blocking
        assert pulse.frame == FrameIdentifier("tx", [Qubit.Fixed(0)])
        syntactic = SyntacticWaveform(pulse.waveform)
        concrete = syntactic.try_evaluate({}, {})
        program_syntactic_pulses.append(syntactic)
        program_concrete_pulses.append(concrete)

    e = Expression.parse
    explicit_syntactic_pulses = [
        SyntacticWaveform.flat(duration=1e-6, iq=e("i")),
        SyntacticWaveform.gaussian(
            duration=2e-8, scale=e("1+1"), fwhm=e("1e-8"), t0=e("0.5e-8")
        ),
        SyntacticWaveform.drag_gaussian(
            duration=2.6e-7,
            phase=e("pi/2"),
            fwhm=e("0.5e-7"),
            t0=e("1e-7"),
            anh=e("1_000_000"),
            alpha=e("3"),
        ),
        SyntacticWaveform.erf_square(
            duration=3e-7,
            detuning=e("123_456_789"),
            risetime=e("12e-9"),
            pad_left=4e-9,
            pad_right=8e-9,
        ),
        SyntacticWaveform.hermite_gaussian(
            duration=4e-8,
            scale=e("0.5"),
            detuning=e("-1e8"),
            fwhm=e("1.5e-8"),
            t0=e("0.75e-8"),
            anh=e("-1_000_000"),
            alpha=e("-3"),
            second_order_hrm_coeff=e("0.42"),
        ),
        SyntacticWaveform.boxcar_kernel(
            duration=6e-8, scale=e("1.5"), phase=e("pi"), detuning=e("987_654_321")
        ),
        SyntacticWaveform.custom("special", {"answer": e("2*(21 + 0.5i)")}),
    ]

    explicit_concrete_pulses = [
        ConcreteWaveform.flat(duration=1e-6, iq=1j),
        ConcreteWaveform.gaussian(duration=2e-8, scale=2, fwhm=1e-8, t0=0.5e-8),
        ConcreteWaveform.drag_gaussian(
            duration=2.6e-7, phase=pi / 2, fwhm=0.5e-7, t0=1e-7, anh=1_000_000, alpha=3
        ),
        ConcreteWaveform.erf_square(
            duration=3e-7,
            detuning=123_456_789,
            risetime=12e-9,
            pad_left=4e-9,
            pad_right=8e-9,
        ),
        ConcreteWaveform.hermite_gaussian(
            duration=4e-8,
            scale=0.5,
            detuning=-1e8,
            fwhm=1.5e-8,
            t0=0.75e-8,
            anh=-1_000_000,
            alpha=-3,
            second_order_hrm_coeff=0.42,
        ),
        ConcreteWaveform.boxcar_kernel(
            duration=6e-8, scale=1.5, phase=pi, detuning=987_654_321
        ),
        ConcreteWaveform.custom("special", {"answer": 42 + 1j}),
    ]

    assert program_syntactic_pulses == explicit_syntactic_pulses
    assert program_concrete_pulses == explicit_concrete_pulses

    for pulse in explicit_concrete_pulses:
        builtin_and_common = pulse.as_builtin()
        if not builtin_and_common:
            continue
        builtin, common = builtin_and_common
        assert isinstance(
            builtin.iq_values_at_sample_rate(common, 250_000_000), waveform.IqSamples
        )
