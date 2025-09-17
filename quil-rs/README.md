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

## Migration to `quil` v0.32.0

Prior to `v0.32.0`, this repository included a separate crate `quil-py`
which exposed the Python bindings to `quil`.
Starting at that version, we've merged that crate into `quil-rs` directly,
supported by the `python` feature flag.
With this change, we've united the version numbers for `quil-rs` and `quil`,
resulting in a large jump since the last version.

The new version comes with breaking changes, particularly for Python users,
which are summarized below and detailed in the `CHANGELOG`.
For the trouble, we now support the latest versions of `PyO3` and Python.
This comes with improvements to security, stability, and performance
and makes our codebase much easier to support moving forward.

For Rust consumers of `quil-rs`, the only breaking changes
are that the previously Unit-variants of
`Instruction` (`Halt`, `Nop`, and `Wait`) and `Expression` (`PiConstant`)
are now empty tuple variants to make them compatible with `PyO3`'s "Complex enums".
That means the following variants must have `()` appended to their usages:

```rust
// Previously you'd use this:
match expression {
    PiConstant => ...,
}

// Now you must use this:
match expression {
    PiConstant() => ...,
}
```

If you're directly using the `quil` Python package that exposes bindings to `quil-rs`,
please be aware of the following changes required to upgrade to the newest `quil`:

- We are dropping support for Python 3.9 and adding support through Python 3.13.
- We no longer wrap `quil-rs` types with an additional layer for Python interop,
    but instead directly expose them as `#[pyclass]`es; in particular, that means:
    - The `Instruction` and `Expression` classes now subclass their inner types,
        which they expose as attributes.
    - The `from_*` methods have been removed.
        Instead, simply wrap the target in the appropriately named class,
        e.g. `instr = Instruction.from_gate(some_gate)`
        becomes `instr = Instruction.Gate(some_gate)`,
        and `Expression.from_number(1.0+0.5j)`
        becomes `Expression.Number(1.0+0.5j))`.
    - Likewise, the `to_*` methods have been removed.
        If necessary, you can access it as `instr._0`,
        but typically it'll be more natural to use `match` syntax, e.g.:
        ```python
        match expr:
            case Expression.Number(n):
                print(2 * n)
            case Expression.Pi():
                print(2 * math.pi)
        ```
        Of course, the `to_quil` and `parse` methods _do_ still exist!
    - You can also use `isinstance`, but keep in mind that the `Instruction` subclasses
        are often named after the class they take as a parameter,
        but these two classes are not the same:
        ```python
        from quil.instructions import Qubit, Gate, Instruction

        gate = Gate("X", (), (Qubit.Fixed(0),), ())
        instr = Instruction.Gate(gate)

        assert isinstance(gate, Gate)
        assert isinstance(instr, Instruction)
        assert isinstance(instr, Instruction.Gate)
        assert not isinstance(instr, Gate)
        assert not isinstance(gate, Instruction.Gate)
        assert instr._0 == gate
        ```
- Classes which support `__hash__` are now immutable from Python.
    These types were always semantically immutable
    (because a Python object's hash is required to be stable throughout its lifetime),
    but now it's enforced by marking the `#[pyclass]` as `frozen`.
    If you previously were mutating these types, it was a bug, unfortunately.
    Nevertheless, removing `__hash__` constitutes a breaking change.
- The manner in which classes support the `pickle` and `copy` modules now works 
    so that `__getnewargs__` returns the appropriate arguments for the `__new__` constructor.
    This should reduce many edge cases around subclassing `quil` types,
    but may constitute a breaking change if you were relying on `__setstate__` support.
    To be clear, we do not recommend using `pickle` as a serialization format,
    but understand it has its value for its relationship `copy` and `multiprocessing`.
- The following functions are now attributes:
    - `program.BasicBlock.label`
    - `program.BasicBlock.instructions`
    - `program.BasicBlock.terminator`
    - `program.CalibrationExpansion.calibration_used`
    - `program.CalibrationExpansion.range`
    - `program.CalibrationExpansion.expansions`
    - `program.ScheduleSeconds.items`
    - `program.ScheduleSeconds.duration`
    - `program.ProgramCalibrationExpansion.program`
    - `program.ProgramCalibrationExpansion.source_map`
- Finally, these type stubs have been updated;
    they look like breaking changes when comparing the APIs,
    but in reality these were always required if you were using them as keyword arguments:
    - `instructions.SetScale.__new__`: `phase` should be `scale`
    - `instructions.MemoryReference.parse`: `input` should be `string`
    - `instructions.TargetPlaceholder.__new__`: `base_target` should be `base_label`
    - `program.CalibrationSet.__new__`: `measure_calibration_definitions` should be `measure_calibrations`

