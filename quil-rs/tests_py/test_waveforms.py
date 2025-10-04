import pytest

import quil
from quil import waveforms


class TestConstructors:
    """Test that class constructors exist,
    accept the expected argument names,
    and correctly round-trip their values.
    """

    def test_boxcar_kernel(self, phase: float, scale: float, sample_count: int):
        wf = waveforms.BoxcarKernel(phase=phase, scale=scale, sample_count=sample_count)
        assert wf is not None
        assert wf.phase == phase
        assert wf.scale == scale
        assert wf.sample_count == sample_count

    def test_boxcar_kernel_bad_sample_count(self, phase: float, scale: float):
        with pytest.raises(quil.ValueError):
            _ = waveforms.BoxcarKernel(phase=phase, scale=scale, sample_count=0)

    def test_erf_square(
        self,
        duration: float,
        risetime: float,
        sample_rate: float,
        pad_left: float,
        pad_right: float,
        positive_polarity: bool,
        scale: float,
        phase: float,
        detuning: float,
    ):
        wf = waveforms.ErfSquare(
            duration=duration,
            risetime=risetime,
            sample_rate=sample_rate,
            pad_left=pad_left,
            pad_right=pad_right,
            positive_polarity=positive_polarity,
            scale=scale,
            phase=phase,
            detuning=detuning,
        )
        assert wf is not None
        assert wf.duration == duration
        assert wf.risetime == risetime
        assert wf.sample_rate == sample_rate
        assert wf.pad_left == pad_left
        assert wf.pad_right == pad_right
        assert wf.positive_polarity == positive_polarity
        assert wf.scale == scale
        assert wf.phase == phase
        assert wf.detuning == detuning

    def test_gaussian(
        self,
        duration: float,
        fwhm: float,
        t0: float,
        sample_rate: float,
        scale: float,
        phase: float,
        detuning: float,
    ):
        wf = waveforms.Gaussian(
            duration=duration,
            fwhm=fwhm,
            t0=t0,
            sample_rate=sample_rate,
            scale=scale,
            phase=phase,
            detuning=detuning,
        )
        assert wf is not None
        assert wf.duration == duration
        assert wf.fwhm == fwhm
        assert wf.t0 == t0
        assert wf.sample_rate == sample_rate
        assert wf.scale == scale
        assert wf.phase == phase
        assert wf.detuning == detuning

    def test_drag_gaussian(
        self,
        duration: float,
        fwhm: float,
        t0: float,
        anh: float,
        alpha: float,
        sample_rate: float,
        scale: float,
        phase: float,
        detuning: float,
    ):
        wf = waveforms.DragGaussian(
            duration=duration,
            fwhm=fwhm,
            t0=t0,
            anh=anh,
            alpha=alpha,
            sample_rate=sample_rate,
            scale=scale,
            phase=phase,
            detuning=detuning,
        )
        assert wf is not None
        assert wf.duration == duration
        assert wf.fwhm == fwhm
        assert wf.t0 == t0
        assert wf.anh == anh
        assert wf.alpha == alpha
        assert wf.sample_rate == sample_rate
        assert wf.scale == scale
        assert wf.phase == phase
        assert wf.detuning == detuning

    def test_hermite_gaussian(
        self,
        duration: float,
        fwhm: float,
        t0: float,
        anh: float,
        alpha: float,
        sample_rate: float,
        second_order_hrm_coeff: float,
        scale: float,
        phase: float,
        detuning: float,
    ):
        wf = waveforms.HermiteGaussian(
            duration=duration,
            fwhm=fwhm,
            t0=t0,
            anh=anh,
            alpha=alpha,
            sample_rate=sample_rate,
            second_order_hrm_coeff=second_order_hrm_coeff,
            scale=scale,
            phase=phase,
            detuning=detuning,
        )
        assert wf is not None
        assert wf.duration == duration
        assert wf.fwhm == fwhm
        assert wf.t0 == t0
        assert wf.anh == anh
        assert wf.alpha == alpha
        assert wf.sample_rate == sample_rate
        assert wf.second_order_hrm_coeff == second_order_hrm_coeff
        assert wf.scale == scale
        assert wf.phase == phase
        assert wf.detuning == detuning
