# Contributing to quil-rs and quil-py

Welcome to the `quil-rs` project, and thanks for contributing!

This guide is to help walk you through contributing in different ways to `quil-rs` and `quil-py`,
as well as share some general how-tos for development, testing, and maintenance.

## Table of Contents

[Developer Guidelines](#developer-guidelines)

- [Instruction Module](#instruction-module)

[Tips for Maintainers](#tips-for-maintainers)

- [Makefile Tasks](#makefiles-tasks)

## Developer Guidelines

### Instruction Module

The `instruction` module is large, so we follow a few guidelines when adding or changing instructions to help keep
them organized and consistent in both the `quil-rs` and `quil-py` crates.

All instruction definitions live inside of the `instruction` module. For ease of development,
instruction types are grouped into submodules so that similar instructions can be modified and viewed together
rather than having all instructions in one large file. However, to avoid imposing potentially arbritrary
instruction categories onto the user, the `instruction` module publically re-exports all of its types from
its root. 

This module structure should remain consistent between both `quil-rs` and `quil-py`. For example, the file
`src/instruction/gate.rs` exists in both `quil-rs` and `quil-py` and contains the same set of instruction
definitions (for their respective target language, of course). 

In short, an instruction should:
    1. Be organized in the equivalent `instruction` submodule for both `quil-rs` and `quil-py` crates
    2. Only be re-exported publicly from the root of the `Instruction` module

## Tips for Maintainers

### Makefile Tasks

This repo uses [cargo-make](https://github.com/sagiegurari/cargo-make) to define task flows that perform
common operations. We've extended the default flow with tasks specific to the `quil-rs` and `quil-py`
crates. You can run the default task with:

```sh
cargo make
# or
makers
```

It's worth periodically running this command as you develop to make sure there are no unexpected failures.

Check the [`cargo-make documentation`](https://github.com/sagiegurari/cargo-make#predefined-makefiles) for
more information on the predefined tasks available. The custom flows we've added are described below:

#### `stubtest-flow`

The `quil-py` crate defines `stubtest-flow`. It builds and installs the Python package, then runs
[stubtest](https://mypy.readthedocs.io/en/stable/stubtest.html) to check that the manually written type hints
are consistent with what the package exports. If any errors are reported, they should be fixed so users
of the Python package are given accurate type hints by their tooling.
