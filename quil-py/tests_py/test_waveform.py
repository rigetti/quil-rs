from quil.waveforms import ErfSquare, BoxcarKernel, apply_phase_and_detuning, DragGaussian, Gaussian, HermiteGaussian


def test_apply_phase_and_detuning():
    iq_values = [1.0, 0.0, 1.0, 0.0]
    apply_phase_and_detuning(iq_values, 0, 0, 44100)
    assert iq_values == [0, 0, 0, 0]


class TestConstrutor:
    def test_boxcar_kernel(self):
        BoxcarKernel(0.0, 0.0, 0.0)

    def test_erf_square(self):
        ErfSquare(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

    def test_gaussian(self):
        Gaussian(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

    def test_drag_gaussian(self):
        DragGaussian(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

    def test_hermite_gaussian(self):
        DragGaussian(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
