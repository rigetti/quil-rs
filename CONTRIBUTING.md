# Contributing to quil-rs and quil-py

Welcome to the `quil-rs` project, and thanks for contributing!

This guide is to help walk you through contributing in different ways to `quil-rs` and `quil-py`,
as well as to share some general How-Tos for development, testing, and maintenance.

## Table of Contents

- [Tooling](#tooling)
- [Instruction Module](#instruction-module)
- [`quil-py` Development](#quil-py-development)
    - [Tooling](#quil-py-tooling)
    - [Type Stubs](#type-stubs)
    - [Testing](#quil-py-testing)
    - [Documentation](#quil-py-documentation)

## Tooling

To build and test all parts of `quil-rs` and `quil-py`,
you'll need a [Rust tool chain][rust-tooling] and [Python 3.10+][python-tooling].
Many of this repository's development tasks have been simplified using [`cargo-make`][],
which you can install via Cargo:

```sh
cargo install --locked cargo-make
```

Using `cargo make` to execute tasks will install additional `cargo` plugins when necessary.
For example, we use [`cargo-hack`][] to validate different combinations of our crate features,
and running `cargo make check` will install it for you if its not yet installed.

## Instruction Module

The `instruction` module is large,
so we follow a few guidelines when adding or changing instructions to help keep
them organized and consistent in both the `quil-rs` crate and `quil` package.

For ease of development,
instruction types are grouped into submodules
so that similar instructions can be modified and viewed together
rather than having all instructions in one large file;
however, to avoid imposing potentially arbitrary
instruction categories onto the user,
the `instruction` module publicly re-exports all of its types from its root.
They are exposed to Python via the `instruction::quilpy` module's `init_submodule` function.

## `quil-py` Development 

We use a `python` feature to gate code that exists exclusively to build `quil`.
When we can, we isolate that code to a separate Rust module,
typically named `quilpy` and located near its relevant domain
(e.g. `instruction::quilpy`, `program::quilpy`, `waveform::quilpy`).
This allows us to conditionally include (or exclude) the entire module.

The `quilpy` modules export their contributions via an `init_submodule` function,
and the top-level `quilpy` module builds the final `quil` package layout,
including the `QuilError` error hierarchy found in `quilpy::errors`.

### `quil-py` Tooling 

For `quil-py` development,
we use [`poetry`][] to manage a `virtualenv` within the local directory.
We use [`maturin`][] to generate the Python package from `quil-rs`,
which `poetry` will install to the `virtualenv` when it installs development dependencies.

The simplest way to build and use a development version the `quil` package
is to first install `cargo-make` and `poetry`, then use:

```sh
cargo make install-quil
```

This builds `quil-rs`, generates Python type stubs,
creates the `quil` package, and installs it to the local `virtualenv`.
You can then use `poetry run python` to experiment with it.

You can use these additional `cargo-make` tasks:

| Task                      | Description                                                                        |
| ------------------------- | -----------                                                                        |
| `generate-stubs`          | Generate Python type stubs for `quil`                                              |
| `stubtest`                | Use [`stubtest`] to verify generated type stubs are consistent with `quil` exports |
| `pytest`                  | Use [`pytest`] to run Python-based tests against the `quil` package                |
| `package-quil`            | Build a release version of `quil` with up-to-date type stubs                       |
| `install-quil`            | Install `quil` to the local virtual environment                                    |
| `install-quil-with-stubs` | Like `install-quil`, but first regenerates type stubs                              |
| `install-quil-deps`       | Install `quil` development dependencies to the local virtual environment           |
| `document-quil`           | Generate documentation for `quil`                                                  |
| `check-api`               | Check if `quil` has breaking API changes that aren't marked as such                |
| `test-quil`               | Run all Python-related tests                                                       |
| `test-all`                | Run all Rust and Python tests                                                      |

### Type Stubs

The type stubs included in the [`python/quil`][quil-py-stubs] directory
require building the `stub_gen` binary with the `stubs` feature enabled,
then using the resulting binary to generate the stubs.
You can do this using the configured alias:

```sh
cargo generate-stubs
```

which is equivalent to

```sh
cargo run -p quil-rs --features stubs --bin stub_gen
```

### `quil-py` Testing

We use `pytest` to run Python-specific tests, found in the `tests_py` directory.
You can run them via `cargo-make`, `poetry`, or `pytest`,
depending on your specific development workflow.
The following are all essentially equivalent, depending on the environment:

```sh
# Use `cargo-make` to handle the environment setup and dependencies for you.
cargo make pytest

# Use `poetry` if you're avoiding a rebuild, but don't want to manage the environment.
poetry run pytest

# Use `pytest` directly if you're working within the virtual environment.
pytest
```

### `quil-py` Documentation

Documentation for the current release of `quil` is published [here][quil-py-docs].
You can build the documentation locally using the `document-quil` task:

```sh
cargo make document-quil
```

[`cargo-hack`]: https://github.com/taiki-e/cargo-hack
[`cargo-make`]: https://github.com/sagiegurari/cargo-make?tab=readme-ov-file#installation
[`maturin`]: https://www.maturin.rs
[`poetry`]: https://python-poetry.org/docs/#installation
[`pytest`]: https://docs.pytest.org/en/
[`stubtest`]: https://mypy.readthedocs.io/en/stable/stubtest.html
[quil-py-docs]: https://rigetti.github.io/quil-rs/quil.html
[quil-py-stubs]: https://github.com/rigetti/quil-rs/tree/main/quil-rs/python/quil
[rust-tooling]: https://www.rust-lang.org/tools/install
[python-tooling]: https://www.python.org/downloads/

