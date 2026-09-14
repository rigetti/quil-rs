#!/usr/bin/env python3
"""Keep `quil-plotting` pinned to the exact `quil` release it ships alongside.

`quil-plotting` installs into the `quil` package and tracks APIs that are still moving,
so the two are released together from one workflow at one version number. knope bumps
the `version` field of both (both are listed under `[packages.quil-rs]` in
`knope.toml`), but it does not rewrite the `quil==` dependency pin, which is what this
script is for.

    --sync    rewrite the pin to match `quil`; run from the knope release workflow
    --check   verify the version and the pin both match; run in CI on every PR

Standard library only: the release workflow has no Python setup step, so it runs
whatever `python3` the runner ships.
"""

import argparse
import re
import sys
from pathlib import Path

import tomllib

ROOT = Path(__file__).resolve().parents[2]
CORE = ROOT / "quil-rs" / "pyproject.toml"
PLOTTING = ROOT / "quil-plotting" / "pyproject.toml"

# knope writes versions like `0.37.0` or `0.37.0-rc.3`; PEP 440 spells the latter
# `0.37.0rc3`, and that normalized form is what has to appear in the dependency pin.
# Anything else is a shape this script has not been taught, so fail rather than emit a
# pin that silently does not match.
KNOPE_VERSION = re.compile(r"^(\d+\.\d+\.\d+)(?:-([a-z]+)\.(\d+))?$")
PIN = re.compile(r'^(?P<indent>\s*)"quil==(?P<version>[^"]+)"', re.MULTILINE)


def version(pyproject: Path) -> str:
    return tomllib.loads(pyproject.read_text())["project"]["version"]


def pep440(version: str) -> str:
    match = KNOPE_VERSION.match(version)
    if match is None:
        sys.exit(f"{CORE}: version {version!r} is not a shape this script understands")
    release, label, number = match.groups()
    return release if label is None else f"{release}{label}{number}"


def pin(text: str) -> str:
    match = PIN.search(text)
    if match is None:
        sys.exit(f"{PLOTTING}: no `quil==` pin found")
    return match.group("version")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument(
        "--sync",
        action="store_true",
        help="rewrite the pin to match `quil`",
    )
    mode.add_argument(
        "--check",
        action="store_true",
        help="verify the version and pin match",
    )
    args = parser.parse_args()

    core = version(CORE)
    expected_pin = pep440(core)
    text = PLOTTING.read_text()

    if args.sync:
        updated = PIN.sub(rf'\g<indent>"quil=={expected_pin}"', text, count=1)
        if updated != text:
            PLOTTING.write_text(updated)
            print(f"pinned quil-plotting to quil=={expected_pin}")
        else:
            print(f"already pinned to quil=={expected_pin}")
        return

    problems = []
    if (plotting := version(PLOTTING)) != core:
        problems.append(
            f"  version is {plotting!r}, expected {core!r} to match quil-rs"
        )
    if (pin_str := pin(text)) != expected_pin:
        problems.append(f"  quil pin is {pin_str!r}, expected {expected_pin!r}")
    if problems:
        sys.exit(
            f"{PLOTTING} is out of sync with {CORE}:\n"
            + "\n".join(problems)
            + "\n\nRun `python .github/scripts/plotting_version.py --sync` and commit the result."
        )
    print(f"quil-plotting {plotting} pinned to quil=={pin_str}")


if __name__ == "__main__":
    main()
