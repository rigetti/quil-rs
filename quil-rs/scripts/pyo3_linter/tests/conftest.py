from pathlib import Path

import pytest

def pytest_addoption(parser):
    parser.addoption("--generate", action="store_true", help="regenerate expected test data")

def pytest_generate_tests(metafunc: pytest.Metafunc):
    if "root" in metafunc.fixturenames:
        if (metafunc.module is None
                or not (data_dir := Path(metafunc.module.__file__).parent / "data").exists()
                or not data_dir.is_dir()):
            raise FileNotFoundError("unable to find test data directory")
        paths = [p for p in data_dir.iterdir() if p.is_dir()]
        metafunc.parametrize("root", paths, ids=[p.name for p in paths])
    if "generating" in metafunc.fixturenames:
        metafunc.parametrize("generating", (metafunc.config.getoption("generate", False),))

