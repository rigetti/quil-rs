# Copyright 2026 Rigetti Computing
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import numpy as np
import pytest
from conftest import sig_digits

from quil.plotting.render import gate_name_color, natural_sort_key, order_labels
from quil.plotting.utils import (
    compress_constant_runs,
    decimate_to_max_points,
    round_sig,
    short,
)


def test_round_sig_keeps_precision_at_every_magnitude():
    rounded = round_sig([1234.5678, 0.00012345678, -9.87654e-9], 5)
    np.testing.assert_allclose(rounded, [1234.6, 1.2346e-4, -9.8765e-9])

    # Zeros and non-finite values pass through untouched. Exact equality, not `approx` - the
    # point of rounding is the number's length, and `approx` cannot tell 1e-300 from a
    # 16-digit neighbor of it.
    passthrough = round_sig([0.0, np.nan, np.inf, -np.inf, 1e-300], 5)
    assert passthrough[0] == 0.0
    assert np.isnan(passthrough[1])
    assert passthrough[2] == np.inf and passthrough[3] == -np.inf
    assert passthrough[4] == 1e-300

    with pytest.raises(ValueError, match="at least 1"):
        round_sig([1.0], 0)


def test_round_sig_actually_shortens_every_magnitude():
    extremes = [1234.5678, 1 / 2.4e9, 1e-300, 5e-324, 1e300, 1.797e308, -6.02214076e23]
    for value in round_sig(extremes, 5).tolist():
        if value and np.isfinite(value):
            assert sig_digits(value) <= 5, f"{value!r} is longer than the 5 figures asked for"


def test_short_matches_round_sig_on_scalars():
    for value in (1.5e-9, 123.456789e-9, 999.9994e-9, 0.0, 1e-3, -4.5e-7):
        assert short(value, 5) == round_sig([value], 5)[0], value
        assert sig_digits(short(value, 5)) <= 5


def test_compress_constant_runs_keeps_the_endpoints_of_each_run():
    ts = np.arange(8.0)
    iqs = np.array([1, 1, 1, 2, 2, 3, 3, 3], dtype=np.complex128)

    kept_ts, kept_iqs = compress_constant_runs(ts, iqs)
    assert kept_ts.tolist() == [0.0, 2.0, 3.0, 4.0, 5.0, 7.0]
    assert kept_iqs[0] == 1 and kept_iqs[-1] == 3
    assert len(kept_iqs) == 6

    # A sample differing from both neighbors is a run one long and has to survive.
    spike = np.array([0, 1, 0], dtype=np.complex128)
    assert len(compress_constant_runs(np.arange(3.0), spike)[1]) == 3

    # Nothing to drop, and nothing dropped.
    varying = np.arange(5, dtype=np.complex128)
    assert len(compress_constant_runs(np.arange(5.0), varying)[1]) == 5


def test_decimate_keeps_the_extrema_and_the_endpoints():
    ts = np.arange(10.0)
    iqs = np.array([0, 1, 5, 1, 0, 0, 2, 9, 2, 0], dtype=np.complex128)

    kept_ts, kept_iqs = decimate_to_max_points(ts, iqs, 4)
    assert kept_ts[0] == ts[0] and kept_ts[-1] == ts[-1]
    assert np.abs(kept_iqs).max() == np.abs(iqs).max()
    assert len(kept_iqs) < len(iqs)

    # A no-op at or under the cap, mirroring `compress_constant_runs`'s own guard.
    assert len(decimate_to_max_points(ts, iqs, 100)[1]) == len(iqs)


def test_decimate_never_exceeds_the_cap():
    ts = np.arange(1000.0)
    iqs = np.exp(1j * ts / 7) * (1 + np.sin(ts / 13))

    for max_points in [*range(2, 40), 100, 499, 500, 501]:
        kept = decimate_to_max_points(ts, iqs, max_points)[1]
        assert len(kept) <= max_points, f"{len(kept)} points returned for a cap of {max_points}"


def test_natural_sort_compares_digit_runs_as_numbers():
    labels = ["Qubit: 10", "Qubit: 9", "Qubit: 2"]
    assert sorted(labels, key=natural_sort_key) == ["Qubit: 2", "Qubit: 9", "Qubit: 10"]
    assert sorted(labels) != sorted(labels, key=natural_sort_key)


def test_order_labels_treats_an_explicit_list_as_a_prefix():
    labels = ["Qubit: 10", "Qubit: 9", "Qubit: 2"]

    assert order_labels(labels, None) == ["Qubit: 2", "Qubit: 9", "Qubit: 10"]
    assert order_labels(labels, ["Qubit: 10"]) == ["Qubit: 10", "Qubit: 2", "Qubit: 9"]
    # A label the data does not contain is ignored, the way `hide("nonexistent")` matches
    # nothing; a repeated one cannot turn into two rows.
    assert order_labels(labels, ["Qubit: 9", "nope", "Qubit: 9"]) == [
        "Qubit: 9",
        "Qubit: 2",
        "Qubit: 10",
    ]
    assert sorted(order_labels(labels, ["nope"])) == sorted(labels)


def test_gate_colors_group_by_what_the_operation_is():
    assert gate_name_color("MEASURE_ANCILLA") == gate_name_color("MEASURE")
    assert gate_name_color("CZ_CYCLE_1") == gate_name_color("CZ")
    assert gate_name_color("RESET") not in (gate_name_color("RX"), gate_name_color("CZ"))
    assert len({gate_name_color(name) for name in ("RESET", "MEASURE", "CZ", "RX")}) == 4
    assert gate_name_color("FOO") is None
