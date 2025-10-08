import math
import random
from random import Random

from pytest import fixture


@fixture
def rng(seed: int) -> Random:
    """Get an RNG for tests where the actual parameters are somewhat arbitrary,
    and instead you're testing specific behavior.

    By default, a preset seed is used each run.
    You can set that seed directly as with the following example::

        pytest --seed 62590

    Alternatively, you can run a number of random trials,
    each getting a different randomized seed.
    This run 100 tests with different random seeds::

        pytest --trials 100

    You can combine these options to test first with a specific seed,
    then with a number of random seeds.
    """
    return Random(seed)


def pytest_addoption(parser):
    """Add new options for pytest tests."""

    parser.addoption("--trials", help="set a number of random test runs to perform")
    parser.addoption("--seed", help="set a specific seed to use")


def pytest_generate_tests(metafunc):
    if "seed" in metafunc.fixturenames:
        given_seed = metafunc.config.getoption("seed")
        seeds = [int(given_seed or 62590)]

        # Create random seeds if additional trials requested.
        trials = max(0, int(metafunc.config.getoption("trials") or 0))
        if trials <= 0 and not given_seed:
            trials = 1
        seeds.extend(random.randint(0, (1 << 32) - 1) for _ in range(trials))

        metafunc.parametrize("seed", seeds)


@fixture
def phase(rng: Random) -> float:
    r"""Return a random phase between :math:`0` and :math:`2\pi`."""

    return rng.uniform(0, math.pi * 2)


@fixture
def sample_count(rng: Random) -> int:
    r"""Return a random sample count."""

    return rng.randint(1, (1 << 64) - 1)


@fixture
def duration(rng: Random) -> float:
    r"""Return a random duration."""

    return rng.uniform(0, 1e10)


@fixture
def risetime(rng: Random) -> float:
    r"""Return a random risetime."""

    return rng.uniform(0, 1e10)


@fixture
def sample_rate(rng: Random) -> float:
    r"""Return a random sample_rate."""

    return rng.uniform(0, 1e10)


@fixture
def second_order_hrm_coeff(rng: Random) -> float:
    r"""Return a random second_order_hrm_coeff."""

    return rng.uniform(0, 1e10)


@fixture
def alpha(rng: Random) -> float:
    r"""Return a random alpha."""

    return rng.uniform(0, 1e10)


@fixture
def anh(rng: Random) -> float:
    r"""Return a random anharmonicity constant."""

    return rng.uniform(0, 1e10)


@fixture
def fwhm(rng: Random) -> float:
    r"""Return a random full width at half maximum value."""

    return rng.uniform(0, 1e10)


@fixture
def t0(rng: Random) -> float:
    r"""Return a random t0."""

    return rng.uniform(0, 1e10)


@fixture
def pad_left(rng: Random) -> float:
    r"""Return a random pad_left."""

    return rng.uniform(0, 1e10)


@fixture
def pad_right(rng: Random) -> float:
    r"""Return a random pad_right."""

    return rng.uniform(0, 1e10)


@fixture
def positive_polarity(rng: Random) -> bool:
    r"""Return a random positive polarity."""

    return bool(rng.randint(0, 1))


@fixture
def scale(rng: Random) -> float:
    r"""Return a random scale."""

    return rng.uniform(0, 1e10)


@fixture
def detuning(rng: Random) -> float:
    r"""Return a random detuning."""

    return rng.uniform(0, 1e10)
