
# Purpose of Migration

The primary purpose of our changes to `quil-rs` and related repositories
was to upgrade PyO3. Doing so allowed us to:

- support versions of Python beyond 3.12
- reduce technical debt across repositories
- integrate new build tools to improve code quality

In addition, we used the opportunity to improve the Python ergonomics in the library,
though we still consider this a work in progress. 

## PyQuil Migration

The intent is for PyQuil to use the latest library versions of `quil-rs` and friends.

At present, the `v5` branch of PyQuil supports them,
though at present it only includes the bare minimum changes necessary
and makes no specific effort to address compatibility specifically.

Currently, there is work in progress to further improve the ergonomics
and to upgrade PyQuil accordingly.

The current migration path for `quil-rs` users
is [documented in the `quil-rs` repository][migration-guide].
Many of these changes are mechanical in nature.
PyQuil users are insulated from most of these changes,
provided they are not directly manipulating wrapped `quil` objects.

[migration-guide]: https://github.com/rigetti/quil-rs/tree/main/quil-rs#migration-to-quil-v0320

Still, some of the details may apply to PyQuil users,
as there are changes we included that are necessarily incompatible with earlier versions:

### Python (`quil`/`quil-py`) Breaking Changes

If you're directly using the `quil` Python package that exposes bindings to `quil-rs`,
please be aware of the following changes required to upgrade to the newest `quil`:

- We dropped support for Python 3.9.
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
- Some type stubs had incorrect parameter names, but now have been updated.
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

