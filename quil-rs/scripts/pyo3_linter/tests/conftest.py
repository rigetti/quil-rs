from pathlib import Path

import pytest

from pyo3_linter import macro_handler, MacroContext, Item, Kind, join_lines, iter_delim, default_macro_handlers, PackageConfig, MacroHandlers

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
    # if "generating" in metafunc.fixturenames:
    #     metafunc.parametrize("generating", (metafunc.config.getoption("generate", False),))


@macro_handler(r"impl_instruction!")
def _impl_instruction(ctx: MacroContext, module: str | None = None) -> None:
    """Process the input to the ``impl_instruction!`` macro."""

    line = join_lines(iter_delim(ctx.lines, "[]"))
    ctx.exported["quil.instructions"].update(
        Item(
            kind=Kind.Class,
            python_name=rust_name,
            rust_name=rust_name,
            path=ctx.path,
            line=line,
        )
        for name in line.text
            .replace(" ", "")
            .removeprefix("impl_instruction!([")
            .removesuffix("]);")
            .split(",")
        if (rust_name := name.partition("[")[0].strip()) != ""
    )

@pytest.fixture(scope="session")
def macro_handlers() -> MacroHandlers:
    return default_macro_handlers() + [_impl_instruction]

@pytest.fixture(scope="session")
def package_config() -> PackageConfig:
    return PackageConfig("quil", "_quil", "quilpy")

@pytest.fixture(scope="session")
def generating(request):
    return request.config.getoption("--generate", False)
