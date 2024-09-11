from typing import Iterable

def apply_phase_and_detuning(iq_values: Iterable[complex], phase: float, detuning: float, sample_rate: float):
    """Modulate and phase shift waveform IQ data in place."""
    ...

class BoxcarKernel:
    def __new__(self, phase_cycles: float, scale: float, sample_count: int) -> BoxcarKernel:
        """Create a new BoxcarKernel."""
        ...

    @property
    def phase(self) -> float:
        """Get the phase, in cycles."""

    @property
    def scale(self) -> float:
        """Get the scale value."""

    @property
    def sample_count(self) -> int:
        """Get the sample count."""

    def into_iq_value(self) -> complex:
        """Convert BoxcarKernel into a Complex64 value."""


class ErfSquare:
    """A waveform with a flat top and edges that are error functions (erfs)."""
    def __new__(self, duration: float, risetime: float, sample_rate: float,
                 pad_left: float, pad_right: float, positive_polarity: bool,
                 scale: float, phase: float, detuning: float) -> ErfSquare:
        ...

    @property
    def duration(self) -> float:
        """Full duration of the pulse (s)"""

    @property
    def risetime(self) -> float:
        """Slope of erf shoulders (2x FWHM of erf in s)"""

    @property
    def sample_rate(self) -> float:
        """Generate wavform samples at this rate (Hz)"""

    @property
    def pad_left(self) -> float:
        """Length of zero padding to add to beginning of pulse (s)"""

    @property
    def pad_right(self) -> float:
        """Length of zero padding to add to end of pulse (s)"""

    @property
    def positive_polarity(self) -> bool:
        """Toggle for positive/negative polarity"""

    @property
    def scale(self) -> float:
        """Scale to apply to waveform envelope"""

    @property
    def phase(self) -> float:
        """Phase shift for entire waveform"""

    @property
    def detuning(self) -> float:
        """Explicit detuning to bake into iq values"""

    def into_iq_values(self) -> list[complex]:
        """Convert ErfSquare into a list of Complex64 values."""


class Gaussian:
    """A waveform with a Gaussian shape."""
    def __new__(self, duration: float, fwhm: float, t0: float,
                 sample_rate: float, scale: float, phase: float,
                 detuning: float) -> Gaussian:
        ...

    @property
    def duration(self) -> float:
        """Full duration of the pulse (s)"""

    @property
    def fwhm(self) -> float:
        """Full width half maximum of the pulse (s)"""

    @property
    def t0(self) -> float:
        """Center/offset for pulse centroid (s)"""

    @property
    def sample_rate(self) -> float:
        """Generate waveform samples at this rate (Hz)"""

    @property
    def scale(self) -> float:
        """Scale to apply to waveform envelope"""

    @property
    def phase(self) -> float:
        """Phase shift for entire waveform"""

    @property
    def detuning(self) -> float:
        """Explicit detuning to bake into IQ valuesExplicit detuning to bake into IQ values"""

    def into_iq_values(self) -> list[complex]:
        """Convert Gaussian into a list of Complex64 values."""


class DragGaussian:
    """A waveform with a DRAG-corrected Gaussian shape.                                                             
                                                                                                                     
    This is a Gaussian shape with an additional component proportional to the time derivative of the main Gaussian pulse.
                                                                                                                         
    See Motzoi F. et al., Phys. Rev. Lett., 103 (2009) 110501. for details.                                              
    """
    def __new__(self, duration: float, fwhm: float, t0: float, anh: float,
                 alpha: float, sample_rate: float, scale: float, phase: float,
                 detuning: float) -> DragGaussian:
        ...

    @property
    def duration(self) -> float:
        """Full duration of the pulse (s)"""

    @property
    def fwhm(self) -> float:
        """Full width half maximum of the pulse (s)"""

    @property
    def t0(self) -> float:
        """Center/offset for pulse centroid (s)"""

    @property
    def anh(self) -> float:
        """Qubit anharmonicity - sets rate of evolution for the imaginary term (Hz)"""

    @property
    def alpha(self) -> float:
        """DRAG parameter - controls strength of the imaginary term"""

    @property
    def sample_rate(self) -> float:
        """Generate samples at this rate (Hz)"""

    @property
    def scale(self) -> float:
        """Scale to apply to waveform envelope"""

    @property
    def phase(self) -> float:
        """Phase shift for entire waveform"""

    @property
    def detuning(self) -> float:
        """Explicit detuning to bake into IQ valuesExplicit detuning to bake into IQ values"""

    def into_iq_values(self) -> list[complex]:
        """Convert DragGaussian into a list of Complex64 values."""
        ...


class HermiteGaussian:
    """A Hermite Gaussian waveform.

    This extends the basic DRAG pulse by adding an additional imaginary term to the pulse envelope consisting of a
    Gaussian pulse modified by the second order Hermite polynomial.

    Refer to
    "Effects of arbitrary laser or NMR pulse shapes on population inversion and coherence"
    Warren S. Warren. 81, (1984); doi: 10.1063/1.447644
    for details.
    """
    def __new__(self, duration: float, fwhm: float, t0: float, anh: float,
                 alpha: float, sample_rate: float, second_order_hrm_coeff: float,
                 scale: float, phase: float, detuning: float) -> HermiteGaussian:
        ...

    @property
    def duration(self) -> float:
        """Full duration of the pulse (s)"""

    @property
    def fwhm(self) -> float:
        """Full width half maximum of the pulse (s)"""

    @property
    def t0(self) -> float:
        """Center/offset for pulse centroid (s)"""

    @property
    def anh(self) -> float:
        """Qubit anharmonicity - sets rate of evolution for the imaginary term (Hz)"""

    @property
    def alpha(self) -> float:
        """DRAG parameter - controls strength of the imaginary term"""

    @property
    def sample_rate(self) -> float:
        """Generate samples at this rate (Hz)"""

    @property
    def second_order_hrm_coeff(self) -> float:
        """Coefficient of the second order Hermite polynomial term."""

    @property
    def scale(self) -> float:
        """Scale to apply to waveform envelope"""

    @property
    def phase(self) -> float:
        """Phase shift for entire waveform"""

    @property
    def detuning(self) -> float:
        """Explicit detuning to bake into IQ valuesExplicit detuning to bake into IQ values"""

    def into_iq_values(self) -> list[complex]:
        """Convert HermiteGaussian into a list of Complex64 values."""
        ...
