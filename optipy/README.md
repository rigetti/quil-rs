# Optional PyO3 Features

This crate provides a procedural macro that removes [`pyo3`][] related macros.
It can be applied to a module using `cfg_attr(not(feature = "some-gate"), optipy)`.

## Why is this needed?

To use PyO3 effectively,
you're expected to wrap items with the procedural macros `#[pyclass]` and `#[pymodule]`.
You can feature-gate their application via e.g. `#[cfg_attr(feature = "some-gate", pyclass)]`,
and that will work just fine in many cases.
It goes wrong, however, if you need to use any of `PyO3`'s attributes in that scope,
such as applying `#[getter]` to a method or `#[pyo3(name = "SomethingElse")]` to an `enum` variant,
as you can't use `cfg_attr` on those items. See the following issues and PRs for more information:

- https://github.com/PyO3/pyo3/issues/780
- https://github.com/PyO3/pyo3/issues/1003
- https://github.com/PyO3/pyo3/pull/2786 

So this crate takes [the suggestion][pr-suggestion] from the last PR listed above,
and strips `pyo3` attributes. 

[`pyo3`]: https://github.com/PyO3/
[pr-suggestion]: https://github.com/PyO3/pyo3/pull/2786#issuecomment-1331207264

## What about stubs?

Since we also use `pyo3_stub_gen` to generate Python stub files,
this macro strips those attributes, too.
By default, the macro strips both.
To keep the `PyO3` attributes while stripping only the `pyo3_stub_gen` attributes,
you can write the attribute as `strip_pyo3(only_stubs)`.

The assumption is that if you don't want the `PyO3` attributes,
you have no need for the stub-related attributes, either.
On the other hand, the stub-related attributes are only needed when generating stubs,
so we really only want to include them when running the stub generating binary,
and thus we have a separate feature for them.
