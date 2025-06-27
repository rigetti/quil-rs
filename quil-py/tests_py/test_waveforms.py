from quil import waveforms

class TestConstructors:
    def test_boxcar_kernel(self):
        waveforms.BoxcarKernel(0.0, 0.0, 0)

    def test_drag_gaussian(self):
        waveforms.DragGaussian(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

    def test_erf_square(self):
        waveforms.ErfSquare(0.0, 0.0, 0.0, 0.0, 0.0, False, 0.0, 0.0, 0.0)

    def test_gaussian(self):
        waveforms.Gaussian(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

    def test_hermite(self):
        waveforms.HermiteGaussian(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
