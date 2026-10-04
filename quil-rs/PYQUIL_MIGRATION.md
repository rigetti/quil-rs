
## Three categories of changes

- Immutable classes will break code that attempts to modify hashable objects in-place.
- Some `isinstance` checks may need to be rewritten.
- Some constructors signatures have been updated in a way that requires changes.

### `isinstance` Checks

PyQuil versions of `quil` classes have been migrated to their `quil` equivalents.

PyQuil subclasses of `quil` classes (e.g., `ClassicalAdd`, `ClassicalAnd`, `ClassicalDiv`, etc.)
have been redefined as deprecated functions
that return the correct `quil` class with the appropriate parameters:

This:
  `isinstance(inst, ClassicalAdd)`
must now be written as
  `isinstance(inst, ArithmeticBinaryOp) and inst.op == ArithmeticOp.ADD`

- Constructing them will issue a deprecation warning.
- Some `isinstance` checks will now be flagged by static type checkers and will fail when called.

- `MemoryReference` is no longer a subclass of `Expression`.
- This:
    `isinstance(expr, MemoryReference)`
  continues to work, but is comparing against the `Instruction` subclass `MemoryReference`.
  If you want to know if `expr` is an `Expression` around a `MemoryReference`, you should write:
    `isinstance(expr, Expression.Address)`
- `MemoryReference` implements all the `__add__`, `__sub__`, etc. methods
  and returns an `Expression`, so this still works:
    `MemoryReference("ro", 0) + 1`
  and you get an `Expression.Address` around the `MemoryReference`.

### Immutable classes will break code that attempts to modify hashable objects in-place.

All `instruction` classes are now immutable.

This:

```python
gate = Gate("X", qubits=[0])
gate.qubits[0] = 1
```

will now raise an `AttributeError`, and must be rewritten as:

```python
gate = Gate("X", qubits=[0])
gate = Gate(gate.name, qubits[1], params=gate.params)
```

### In a few cases, the PyQuil constructor needed breaking changes

- The `nonblocking` parameter of `Capture`, `RawCapture`, and `Pulse` is now keyword-only.
  This:
    `Capture(qubit, memory_reference, True)`
  must now be written as
    `Capture(qubit, memory_reference, nonblocking=True)`
  or preferably as
    `Capture(qubit, memory_reference, blocking=False)`

-----




















# Migrating from PyQuil v4 to `quil` (Python bindings)

This document collects the class-by-class and method-by-method changes needed
to migrate code written against PyQuil v4's `quilatom`/`quilbase` classes to
the current `quil` Python package (backed by `quil-rs`). It complements the
higher-level ["Migration to `quil` v0.32.0"](README.md#migration-to-quil-v0320)
section of the README, which covers broader, structural changes (dropped
`from_*`/`to_*`/`is_*`/`inner` helpers, `@property` conversions, hashability
rules, pickling, etc.). The notes below focus on the differences in
individual constructors, method signatures, and behaviors.

Unless noted otherwise, "PyQuil v4" refers to the old `pyquil.quilatom` /
`pyquil.quilbase` classes, and the "new" behavior refers to the classes now
exposed by `quil.instructions` (and, in one case, `quil.instructions.Frame`
at the Rust level for users of the `python` feature in `quil-rs` directly).

## General

- **Frozen classes.** `Frame` and `Waveform` are now frozen (immutable) from
  Python.
- **`FormalArgument`/`Parameter`.** PyQuil v4's `quilatom.FormalArgument`
  class (and, similarly, `Parameter`) has not been replaced with a dedicated
  type. These are wrappers around a `name: str` with special formatting
  rules; in Rust they correspond to the `Qubit::Variable` and
  `Expression::Variable` variants respectively, and are otherwise just
  `str`s from Python. Anywhere a `FormalArgument` was required, you can pass
  a plain `str` instead. We may reconsider adding an explicit type back for
  backwards compatibility and to allow more restrictive parameter typing.
- **`unpack_classical_reg`.** PyQuil's `unpack_classical_reg` helper used to
  convert `MemoryReference`, `(str, int)`, or `[str, int]` into a
  `MemoryReference`. Now, any place that requires a `MemoryReference` can
  extract one directly from those same forms, as well as from a
  `DeclarationAt` or a `Declaration` (or its `Instruction` wrapper).

## Renamed classes

Several classical/arithmetic instruction classes were renamed to drop the
`Classical`/PyQuil-specific prefixes and be consistent with their Quil
instruction names. In each case, the constructor signature was also
simplified to `(destination, source)` (or similar), rather than exposing
`left`/`right` (or an `operator`/`op` class attribute on subclasses):

| PyQuil v4 name                 | New name       | Notes |
| ------------------------------ | -------------- | ----- |
| `ArithmeticBinaryOp`           | `Arithmetic`   | Constructor was `Arithmetic(left, right)`, with `operator` as a class attribute on subclasses. |
| `LogicalBinaryOp` (+subclasses)| `BinaryLogic`  | Constructor took `left`/`right`, exposed as (now removed) mutable properties; `left` corresponds to `destination`, `right` to `source`. `operator` was a class attribute (`op`) on subclasses. |
| `ClassicalComparison` (+subclasses) | `Comparison` | Constructor was `(target, left, right)`, with `operator` as a class attribute on subclasses (e.g. `ClassicalEqual`, `ClassicalLessThan`). |
| `ClassicalConvert`              | `Convert`     | Constructor was `(left, right)`. |
| `ClassicalLoad`                 | `Load`        | Constructor was `(target, left, right)`. |
| `ClassicalMove`                 | `Move`        | Constructor was `Arithmetic(left, right)` (i.e. shared PyQuil's `Arithmetic` constructor shape). |
| `ClassicalStore`                 | `Store`      | Constructor was `(target, left, right)`. |
| `UnaryClassicalInstruction`      | `UnaryLogic` | Constructor just took `target`; `op` was a class attribute. |
| `Declare`                        | `Declaration` | See [`Declaration`/`MemoryReference`](#declarationmemoryreference) below. |

## `Declaration`/`MemoryReference`

- `Declaration`'s constructor (`__new__`) has been adjusted to match PyQuil
  v4's `Declare` constructor.
- `MemoryReference.offset` has been renamed `index`. The old `offset` getter
  still works but is deprecated and issues a `PyDeprecationWarning`.
- `MemoryReference.declared_size` has been removed. It was only used for
  pretty-printing and can't be inferred from parsing, so it always returns
  `None` now (with a `PyDeprecationWarning`).

## `Gate`

- The `params` constructor keyword argument has been renamed `parameters`.
  `params` is still accepted for backwards compatibility.

## `CalibrationDefinition`

- PyQuil v4's constructor was `(name, parameters, qubits, instrs,
  modifiers)`. The old positional/keyword form is still accepted for
  compatibility, but issues a deprecation warning suggesting you use the new
  `from_parts` constructor instead.

## `CircuitDefinition`

- The constructor keyword `qubit_variables` was renamed just `qubits` in
  PyQuil v4's version. The new constructor also no longer requires inputs be
  wrapped in `Parameter`/`FormalArgument` instances&mdash;plain `str`s are
  accepted directly.

## `MeasureCalibrationDefinition`

- The constructor now matches the PyQuil v4 signature, except that the
  `instrs` parameter has been renamed `instructions` (the old name is still
  accepted for compatibility). Direct `quil` users can also migrate to the
  new `from_calibration` constructor.

## `Label` / `LabelPlaceholder` / `TargetPlaceholder`

In `quil-rs`, `Label` represents a Quil `LABEL` instruction, and its
behavior is driven by the `Target` enum (`Target.Fixed(str)` or
`Target.Placeholder(TargetPlaceholder)`). PyQuil v4 had separate
`quilatom.Label` and `quilatom.LabelPlaceholder` classes, each of which
really just wrapped a `Target` (the former always `Fixed`, the latter always
`Placeholder`)&mdash;neither was the actual `quil.Label` type. In the new
version, `LabelPlaceholder` is gone; use `Label` directly and construct it
from an existing `Target`, from a `str`, or from `None`, letting the
constructor build the right `Target` variant for you. To explicitly control
the `TargetPlaceholder`'s `base_label`, pass a `str` along with the
keyword-only `placeholder=True` argument.

Migration table for old usages:

| Deprecated Usage                   | Updated Usage                           |
| ----------------------------------- | ---------------------------------------- |
| `Label("jump-target")`              | No change needed                         |
| `Label(label_name="jump-target")`   | `Label(target="jump-target")`            |
| `LabelPlaceholder()`                | `Label()`                                |
| `LabelPlaceholder("L")`             | `Label("L", placeholder=True)`           |
| `LabelPlaceholder(prefix="L")`      | `Label(target="L", placeholder=True)`    |
| `LabelPlaceholder(placeholder=p)`   | `Label(target=p)`                        |

Note: `LabelPlaceholder("L", placeholder=p)` was technically legal in v4,
but the `prefix` argument was silently ignored, so it was equivalent to just
passing `placeholder=p`. Since `placeholder` is now repurposed as a `bool`
in the new `Label` constructor, that call pattern is no longer valid and
will raise a `TypeError`.

Additionally, `TargetPlaceholder` (the type replacing `LabelPlaceholder`)
uses `base_label` in place of the old `prefix` parameter. It still accepts
the deprecated keyword-only `prefix` and `placeholder` parameters to ease
the transition, but new code should prefer `Label("L", placeholder=True)`.

## `Reset` / `ResetQubit`

PyQuil v4 had both a `Reset` class and a functionally-equivalent
`ResetQubit` class (which just required the `Qubit` argument). In the new
version, `ResetQubit` is simply an alias for `Reset`.

## Pulse/Frame instructions

- **`Pulse`**: PyQuil v4 took `nonblocking=False` instead of `blocking`. The
  new constructor defaults to `blocking=True`, so callers that didn't
  specify either argument see no behavior change. Callers that passed
  `nonblocking` as a keyword argument get a deprecation warning but
  consistent behavior. Callers that passed `nonblocking` positionally now
  get an error, since the parameter is keyword-only (to avoid silently
  changing behavior on upgrade).
- **`Capture`**: PyQuil v4 took `kernel` and `memory_region` (in that order)
  and `nonblocking` (defaulting to `False`) instead of `waveform`,
  `memory_reference`, and `blocking`.
- **`RawCapture`**: PyQuil v4 took `memory_region` instead of
  `memory_reference`, and `nonblocking` (defaulting to `False`) instead of
  `blocking`.
- **`SetFrequency`**: PyQuil v4 took `freq` instead of `frequency`. `freq` is
  still accepted (as a deprecated keyword-only alias).
- **`ShiftFrequency`**: Same change as `SetFrequency`&mdash;`freq` is now
  `frequency`.
- **`SwapPhases`**: PyQuil v4's signature was `(frameA, frameB)`; the new
  signature is `(frame_1, frame_2)`. The old names are still accepted as
  deprecated keyword-only aliases.

## `Delay`

PyQuil v4's constructor looked roughly like:

```python
Delay(frames: list[FrameIdentifier], qubits: list[QubitDesignator], duration: float)
```

while the older `quil` form looked roughly like:

```python
Delay(duration: Expression, frame_names: list[str], qubits: list[Qubit])
```

Quil's spec indicates the order of qubits in a `DEFFRAME` is significant,
but it isn't clear whether order within a `DELAY` is significant. PyQuil's
constructor, when given `FrameIdentifier`s, would extract their `qubits` and
merge them with any `qubits` passed to the constructor, potentially
reordering or adding qubits that weren't originally given. In practice, most
usage came through the `DelayFrames` and `DelayQubits` subclasses (which
only accepted a list of frames or a list of qubits, respectively).

The new signature is more flexible and consistent with what Quil's `DELAY`
instruction actually requires: a `duration` and at least one `Qubit` are
required, and frame names are optional. An alternative constructor is
available to explicitly reproduce the `FrameIdentifier`-based behavior.

Key impacts:

- Users who called the `Delay` constructor directly in PyQuil v4 will get
  errors, but users of `DelayFrames`/`DelayQubits` (the more common usage)
  will see no behavioral change.
- `quil` users will also get constructor errors, as the parameter order for
  `frame_names` and `qubits` has been swapped; the type differences should
  make mismatches obvious.
- PyQuil v4 required `duration` be a `float`. The new constructor accepts
  arbitrary `Expression`s (and anything convertible to one, including
  `float`s). Unlike v4, the property getter no longer coerces the result
  back into a `float`&mdash;use `frame.duration.to_real()` instead.
- PyQuil v4's `DelayFrames` and `DelayQubits` were separate classes; the new
  version only has a single, more flexible `Delay` class. Since there's no
  corresponding `Instruction` variant for the old subclasses, code that
  relied on `isinstance` checks against `DelayFrames`/`DelayQubits` will now
  fail those checks (deliberately, to reduce risk of unexpected behavior).

## `FrameDefinition`

- **Constructor.** This differs from both the PyQuil v4 and prior `quil`
  versions. The initial `identifier` argument is now required and must be
  given positionally. You can optionally provide an `attributes` dictionary
  (values are taken mostly as-is), plus additional/overriding attribute
  values as keyword arguments (which take precedence over `attributes`).
  PyQuil v4's signature took a `frame: FrameIdentifier` plus a specific list
  of otherwise-optional attributes, which could be given positionally; since
  that usage is hard to read, the new signature is more explicit.
- **`get_attribute`.** PyQuil v4 only accepted/returned `str | float | None`.
  Now, arbitrary `Expression`s are allowed (as Quil permits), so callers
  should deconstruct the resulting `AttributeValue` (e.g. via a `match`
  statement) and handle it appropriately.
- **`__getitem__`/`__setitem__`.** PyQuil v4 raised `AttributeError`; the new
  version raises `KeyError`. As with `get_attribute`, values are now
  `AttributeValue`s rather than only `str | float | None`.
- **Mutating `identifier`.** PyQuil v4 allowed reassigning the `identifier`
  frame of a `FrameDefinition` (and replacing the whole `attributes`
  dictionary). Both are removed since they seem like bad ideas compared to
  constructing a new instance, consistent with the other frozen instruction
  types. (For now, the underlying `attributes` dictionary is still mutable
  in place, but this may become read-only in the future.)

## `FrameIdentifier`

The `FrameIdentifier` corresponds to PyQuil's `quilatom.Frame`. Its
constructor parameter order is swapped relative to PyQuil's
`quilatom.Frame`.

## `PauliTerm` / `PauliSum`

- **`__eq__` no longer compares across types.** PyQuil v4's `PauliTerm.__eq__`
  allowed comparing a `PauliTerm` to a `PauliSum`, violating Python's rule
  that objects which compare equal must have the same hash. This bug is
  fixed by no longer supporting that comparison. If you need to compare a
  `PauliTerm` to a `PauliSum`, wrap the `PauliTerm` in a single-term
  `PauliSum` first. (`PauliSum.__eq__` has the same caveat.)
- **Result rearrangement/simplification.** Unlike earlier `quil-rs`
  versions, the constructors may rearrange and/or simplify the arguments of
  `PauliTerm`s and `PauliSum`s. The result is always logically equivalent,
  but may produce different Quil output, and hence different downstream
  compilation results. This is analogous to how compiler optimizations and
  physical device constraints already affect observable program behavior.
- **`Qubit::Placeholder` now rejected.** PyQuil v4 accepted
  `Qubit::Placeholder` instances and silently converted them to a `str`,
  producing invalid Quil identifiers. This was a bug; placeholders now
  explicitly raise an error.
- **Integer/`Qubit::Fixed` arguments are now prefixed.** PyQuil v4 converted
  integers and `Qubit::Fixed` instances directly into `str`s (e.g.
  `Qubit::Fixed(0)` became `"0"`), which is not a valid Quil identifier. The
  new behavior prefixes such values with `q` (e.g. `"q0"`) to produce valid
  identifiers. This can cause conflicts if you mix string and integer
  arguments yourself&mdash;avoiding that mix is now your responsibility (as
  is avoiding other invalid identifiers passed directly as strings).
- **Constructing from a list of pairs is deprecated.** You can still
  construct a `PauliTerm` from a sequence of `(operator, qubit)` pairs, but
  this now issues a deprecation warning in favor of the new
  `PauliTerm.from_list` static method:

  ```python
  # This will raise a deprecation warning:
  term = PauliTerm([(PauliGate.X, 0), (PauliGate.Y, "q"), ("I", None)], 1.5)
  # Prefer this form:
  term = PauliTerm.from_list([(PauliGate.X, 0), (PauliGate.Y, "q"), ("I", None)], 1.5)
  ```

- **`PauliTerm.id()` behavior changes:**
  - Previously, an empty `arguments` list returned an empty string in some
    versions and `"I"` in others (the latter change also introduced a
    `sort_ops=True` argument, with a stated intent to change defaults later).
    Now, both the empty-arguments case and the identity case consistently
    return `"I"`, and `sort_ops` defaults to `False`.
  - A new keyword-only `delimiter` argument controls the string used to join
    each operator-argument pair. It defaults to the empty string for
    backwards compatibility.
  - When `delimiter` is empty (the default/original behavior) and an
    argument itself contains `X`, `Y`, `Z`, or `I`, the output would
    previously have been ambiguous. Such arguments are now wrapped in
    parentheses to disambiguate, which means the output no longer exactly
    matches old behavior in that edge case.
  - The original class mainly targeted integer qubit indices (not valid
    Quil identifiers). The constructor now generally forces such values into
    valid identifiers by prefixing them with `q`, so output that used to
    look like `X0Y1` now looks like `Xq0Yq1`.
  - The companion parsing method has been updated to handle both the old
    unambiguous output and the new, more general output.
- **`PauliSum` constructor argument order.** PyQuil v4's constructor took
  `(arguments, terms)`. The preferred new order is `(terms, arguments)`;
  the old order is still accepted but issues a deprecation warning.
  `arguments` are inferred from the `PauliTerm`s if not given.
- **`__repr__` replaced.** `PauliSum.__repr__` is now the Rust-derived
  default (consistent with other classes in this crate); the original
  `__repr__` behavior is preserved as `__str__` instead.
- **`get_qubits` return type.** `PauliSum.get_qubits` used to return
  `Qubit`s; it now returns `str`s.

## `Pragma`

- PyQuil v4 accepted `Qubit | FormalArgument | int | str` items for `args`.
  Now, only integers and identifiers (`str`) can be given directly as
  `PragmaArgument`s.
- PyQuil v4's constructor took `command` and `freeform_string`; the new
  constructor uses `name` and `data` instead.

## `WaveformDefinition`

PyQuil v4's `DefWaveform` took `parameters` (a list of `Parameter`s, here
just their names as `str`s) and `entries` directly, building the underlying
`quil_rs.Waveform` internally. The new constructor no longer accepts those;
use `WaveformDefinition.quil` to construct one directly from a `name` and a
`definition` (a `Waveform`).

## Rust (`quil-rs` with the `python` feature)

Most of the above notes describe Python-visible behavior. Rust callers of
the underlying types (e.g. via the `python` feature) should additionally be
aware of:

- `FrameIdentifier::new`'s parameter order is swapped relative to PyQuil's
  `quilatom.Frame` (see [`FrameIdentifier`](#frameidentifier) above).
- `Frame` and `Waveform` are now frozen/immutable (see [General](#general)).
- `PauliTerm`'s `__new__`, exposed to Python, is used directly by Rust
  callers relying on the `python` feature and should be reviewed for the
  constructor changes described above.
