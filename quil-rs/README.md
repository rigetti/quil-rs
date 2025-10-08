# Quil Parser & Program Builder

This library is the implementation of the [Quil spec][quil-spec] in Rust, with bindings for Python.

It serves three purposes:

1. Parse Quil programs from strings, and output programs to strings
2. Manipulate Quil programs within Rust and Python
3. Construct a dependency graph among program instructions

It should be considered unstable until the release of v1.0.

Note: For Rigetti's Python library for quantum programming with Quil, you likely want [`PyQuil`][].
This code serves as the foundation of that library,
but `PyQuil` offers higher-level abstractions and greater stability guarantees.

[quil-spec]: https://github.com/quil-lang/quil
[`PyQuil`]: https://github.com/rigetti/pyquil

## Crate Features

`quil-rs` has several optional features:

| Feature            | Description                                                           |
|--------------------|-----------------------------------------------------------------------|
| `graphviz-dot`     | Enable plotting `ScheduledProgram`s in Graphviz dotfile format.       |
| `wasm-bindgen`     | Enable compilation to `wasm32-unknown-unknown` with `wasm-bindgen`    |
| `python`           | Enable Python bindings via `PyO3`                                     |
| `stubs`            | Enable type stub generation via `pyo3_stub_gen` (implies `python`)    |

The `python` feature enables code to build the `quil` Python package,
which allows Python programs to use the features of `quil-rs`.
The code related to the Python package is sometimes referred to as `quil-py`,
both for [historical reasons][], and to distinguish it from other parts of `quil-rs`.

Note that `maturin` commands (`maturin develop`, `maturin release`, etc.)
automatically enable the `python` feature, as well as the `pyo3/extension-module` feature,
which disables linking `libpython`, and is needed for [`manylinux`][extmod] builds.

[extmod]: https://pyo3.rs/v0.25.1/building-and-distribution.html#the-extension-module-feature
[historical reasons]: #migration-to-quil-v0320

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

### Rust (`quil-rs`) Breaking Changes

For Rust consumers of `quil-rs`, there are a few breaking changes:

- [Unit enum variants are now empty tuple variants](#unit-variants)
- [`instruction::Calibration` has been renamed `instruction::CalibrationDefinition`](#instructioncalibrationdefinition)
- [`MeasureCalibrationDefinition` has new struct and constructor](#instructionmeasurecalibrationdefinition)
- [`Expression::evaluate` may require explicit types](#expressionevaluate)
- [The unused-but-public `MemoryAccess` type has been removed](#instructionmemoryaccess)

#### Unit variants

The previously `Unit`-variants of
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

#### `instruction::CalibrationDefinition`

The `Calibration` type in in the `instruction` module has been renamed `CalibrationDefinition`
to be consistent with other definition instructions, particularly `MeasureCalibrationDefinition`,
and to match its variant name within the `Instruction`s enumeration,
as is the naming convention for all other `Instruction` variants.

#### `instruction::MeasureCalibrationDefinition`

Contrary to the Quil specification, `MeasureCalibrationDefinition`
made its `qubit` optional and its `parameter` name required.
This is now fixed, making `qubit` required and the name optional;
in addition, it renames `parameter` to `target` for clarity,
in anticipation of [Named Measurements][named-measurments] support.

Note that it was always a bug to use `None` for the `qubit` parameter.

[named-measurments]: https://github.com/rigetti/quil-rs/pull/479

#### `Expression::evaluate`

The signature of `Expression::evaluate`
was generalized to make it easier to share with Python,
and this will cause an error if you were relying on type inference for these parameters, e.g.:

```rust
some_expression.evaluate(&HashMap::new(), &HashMap::new());
```

You can fix it by specifying the key type, e.g.:

```rust
some_expression.evaluate(&HashMap::<String, _>::new(), &HashMap::<&str, _>::new());
```

#### `instruction::MemoryAccess`

The `MemoryAccess` type has been removed.
This struct was not in use within `quil-rs`, but it was part of the public API.

### Python (`quil`/`quil-py`) Breaking Changes

If you're directly using the `quil` Python package that exposes bindings to `quil-rs`,
please be aware of the following changes required to upgrade to the newest `quil`:

- We are dropping support for Python 3.9 and adding support through Python 3.13.
- We no longer wrap `quil-rs` types with an additional layer for Python interop,
    but instead directly expose them as `#[pyclass]`es; in particular,
    the `from_*`, `as_*`, `to_*`, `is_*`, and `inner` methods have been removed,
    You should replace their usage with more typical Python operations (see below for examples).
    Note that `to_quil` and `parse` are still there for converting to/from `str`s.
- The following functions are now `@property` attributes (so you no longer call them),
    making them consistent with other `getter`s already in `quil`:
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
- Classes which implement `__hash__` are now immutable from Python,
    and conversely, classes which are mutable from Python
    no longer implement `__hash__` methods.
    In Python, [a class should not be both hashable and mutable][python-hash][^note-hash].
    We now enforce this, usually by making the type immutable so we can implement `__hash__`,
    though in some cases we removed `__hash__` to allow the type to be mutable.
- The manner in which classes support the `pickle` and `copy` modules now works
    so that `__getnewargs__` returns the appropriate arguments for the `__new__` constructor.
    This should reduce many edge cases around subclassing `quil` types,
    but may constitute a breaking change if you were relying on `__setstate__` support.
    To be clear, we do not recommend using `pickle` as a serialization format,
    but understand it has its value for its relationship to `copy` and `multiprocessing`.
- Finally, some type stubs had incorrect parameter names, but now have been updated.
    They look like breaking changes when comparing the APIs,
    but in reality these were already required if you were using them as keyword arguments:
    - `instructions.SetScale.__new__`: `phase` should be `scale`
    - `instructions.MemoryReference.parse`: `input` should be `string`
    - `instructions.TargetPlaceholder.__new__`: `base_target` should be `base_label`
    - `program.CalibrationSet.__new__`: `measure_calibration_definitions` should be `measure_calibrations`

[python-hash]: https://docs.python.org/3/reference/datamodel.html#object.__hash__
[^note-hash]: If it implements `__hash__`, it should implement `__eq__`,
    but if it implements `__eq__` and is mutable, it should not implement `__hash__`.
    Thus, a class that implements `__hash__` should not be mutable.
    Put another way, a Python object's hash is required to be stable throughout its lifetime.
    This is actually the same rule as in Rust, but in safe Rust you can only violate it
    through a logic bug involving interior mutability.

#### Specific Examples

Instead of using `from_*`, just use the target class's constructor directly.
For example:

```diff python
from quil.instructions import Instruction, Gate, Qubit

some_gate = Gate(
    "X",
    parameters=(),
-   qubits=[Qubit.from_fixed(0)],
+   qubits=[Qubit.Fixed(0)],
    modifiers=()
)

- instr = Instruction.from_gate(some_gate)
+ instr = Instruction.Gate(some_gate)

- expr = Expression.from_number(1.0+0.5j)
+ expr = Expression.Number(1.0+0.5j)
```

Replace `is_*`, `to_*`, `as_*`, and `inner` with `match`.
Here's an example of extracting `inner` elements:

```python
def foo(x):
    match x:
        case Instruction.Gate(gate):
            return gate.name
        case Instruction.Halt():
            return "Halt"
        case Expression.Number(n):
            return n * 2
        case _:
            return "something else"
```

If needed, you can replace `inner` with `_0`, usually paired with an `isinstance` check.
Keep in mind that enumerated subclasses are often named after the class they take as a parameter.
The following `assert`s are all valid:

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
