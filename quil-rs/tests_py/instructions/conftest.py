
from itertools import chain, zip_longest

from quil.instructions import PauliSum, PauliTerm

def _pauli_term_parts(where: str, term: PauliTerm) -> list[str]:
    """Return a list of strings representing the PauliTerm for pytest output."""
    return [
        f"  {where} Full: {term}",
        f"  {where} Arguments: {term.arguments}",
        f"  {where} Expression: {term.expression}",
    ]

def _pauli_sum_parts(where: str, s: PauliSum) -> list[str]:
    """Return a list of strings representing the PauliSum for pytest output."""
    return [
        f"  {where} Full: {s}",
        f"  {where} Arguments: {s.arguments}",
        f"  {where} Terms:",
        *[f"    {where} {t}" for t in s.terms],
    ]


def pytest_assertrepr_compare(config, op, left, right) -> list[str] | None:
    """Customize pytest output for PauliTerm and PauliSum comparisons."""

    if isinstance(left, PauliTerm) and isinstance(right, PauliTerm) and op == "==":
        return [
            f"{left} == {right}",
            # need to flatten these tuple pairs...
            *chain.from_iterable(zip_longest(
                _pauli_term_parts(" Left", left),
                _pauli_term_parts("Right", right),
                fillvalue="--",
            ))
        ]

    elif isinstance(left, PauliSum) and isinstance(right, PauliSum) and op == "==":
        return [
            f"{left} == {right}",
            *chain.from_iterable(zip_longest(
                _pauli_sum_parts(" Left", left),
                _pauli_sum_parts("Right", right),
                fillvalue="--",
            )),
        ]

    elif isinstance(left, PauliTerm) and isinstance(right, PauliSum) and op == "==":
        return [
            f"{left} == {right}",
            *chain.from_iterable(zip_longest(
                _pauli_term_parts("Term", left),
                _pauli_sum_parts(" Sum", right),
                fillvalue="--",
            )),
        ]

    elif isinstance(left, PauliSum) and isinstance(right, PauliTerm) and op == "==":
        return [
            f"{left} == {right}",
            *chain.from_iterable(zip_longest(
                _pauli_sum_parts(" Sum", left),
                _pauli_term_parts("Term", right),
                fillvalue="--",
            )),
        ]

    return None
