# Quil Parser & Program Builder

This library is the implementation of the [Quil spec][quil-spec] in Rust, with bindings for Python.

It serves three purposes:

1. Parse Quil programs from strings, and output programs to strings
2. Manipulate Quil programs within Rust and Python
3. Construct a dependency graph among program instructions

It should be considered unstable until the release of v1.0.

Note: For Rigetti's Python library for quantum programming with Quil, you likely want [`pyQuil`][].
This code serves as the foundation of that library,
but `pyQuil` offers higher-level abstractions and greater stability guarantees.

[quil-spec]: https://github.com/quil-lang/quil
[`pyQuil`]: https://github.com/rigetti/pyquil

## Crate Features

`quil-rs` has several optional features:

| Feature        | Description                                                        |
|----------------|--------------------------------------------------------------------|
| `graphviz-dot` | Enable plotting `ScheduledProgram`s in Graphviz dotfile format.    |
| `wasm-bindgen` | Enable compilation to `wasm32-unknown-unknown` with `wasm-bindgen` |
| `python`       | Enable Python bindings via `PyO3`                                  |
| `stubs`        | Enable type stub generation via `pyo3_stub_gen` (implies `python`) |

The `python` feature enables code to build the `quil` Python package,
which allows Python programs to use the features of `quil-rs`.
The code related to the Python package is sometimes referred to as `quil-py`,
both for historical reasons, and to distinguish it from other parts of `quil-rs`.

## Development

For more information on building, testing, or contributing to `quil-rs` and `quil-py`,
please see [the Contribution Guide][contributing.md]

[contributing.md]: https://github.com/rigetti/quil-rs/blob/main/CONTRIBUTING.md

