"""
These types are used for tracking the package/modules/items annotated and exported by Rust code.
"""

from collections import defaultdict
from collections.abc import ItemsView, KeysView, MutableMapping, MutableSet, ValuesView
from dataclasses import dataclass, field
from pathlib import Path
from typing_extensions import Self
from typing import (
    Iterable,
    Iterator,
    TypeVar,
    overload,
)
import enum
import logging
import re

from .reader import Line

logger = logging.getLogger(__name__)

class PackageKind(enum.Enum):
    Annotation = "Annotation"
    Export = "Export"


class Kind(enum.Enum):
    """The sorts of things that can be annotated."""

    Struct = "struct"
    Enumeration = "enum"
    Function = "fn"
    # When discovered in a pymodule, we don't know the Rust type.
    Class = "class"
    Error = "error"

    def is_py_like(self, py: Self, /) -> bool:
        """Return ``True`` if the Python-argument is "like" this one; that is,::
        - they have the same kind, or
        - this is a struct or enum and the argument is a class or error.

        This is useful for comparing annotated Rust items (where we know the Rust item kind)
        to exported Python items (where we know the Python item kind).
        """
        match (self, py):
            case (Kind.Enumeration | Kind.Struct, Kind.Class | Kind.Error):
                return True
            case (x, y) if x is y:
                return True
        return False


class StubKind(enum.Enum):
    """A ``gen_stub_*`` attribute."""

    Function = "function"
    Methods = "methods"
    Class = "class"
    Enumeration = "class_enum"
    ComplexEnumeration = "class_complex_enum"

    def is_like(self, kind: Kind) -> bool:
        match (kind, self):
            case (Kind.Struct, StubKind.Class):
                return True
            case (
                Kind.Enumeration,
                (StubKind.Enumeration | StubKind.ComplexEnumeration),
            ):
                return True
            case (
                Kind.Class,
                (StubKind.Class | StubKind.Enumeration | StubKind.ComplexEnumeration),
            ):
                return True
            case (Kind.Function, StubKind.Function):
                return True
        return False


@dataclass
class StubAttr:
    """Represents information found with a ``pyo3_stub_gen`` annotation."""

    kind: StubKind
    module: str | None = None

    @classmethod
    def from_match(cls, stubgen_match: re.Match) -> Self:
        """Process #[gen_stub_...] annotations."""

        stub_kind = stubgen_match.group(1) or stubgen_match.group(3)
        if stub_kind is None:
            raise ValueError("Unable to determine stub kind")

        kind = StubKind(stub_kind)
        module: str | None = None

        if kind is StubKind.Function:
            props = PyO3Props.parse(stubgen_match.group(2) or stubgen_match.group(4))
            if not (module := props.gets("module")):
                logger.warning(f"Unknown module for stub {stubgen_match.group(0)}")

        return cls(kind, module)


class PyO3Props(dict[str, str | bool]):
    """Properties found in a PyO3 attribute, such as ``module = "name"`` or ``ord``.

    Use `parse` and `__str__` to convert to/from a string.
    You can check for the presence of a property with ordinary ``"prop" in props`` syntax,
    but you should prefer the type-specific versions which raise `TypeError`s
    if the property exists but doesn't match the expected type::

    - `gets(key)` returns a string-valued property
    - `is_(key)` checks for a boolean-valued property
    """

    def gets(self, key: str, default: str | None = None) -> str:
        if not (default is None or isinstance(default, str)):
            raise TypeError("if given, default must be str")
        if (value := super().get(key, default)) is not None and isinstance(value, str):
            return value
        if key in self:
            raise TypeError(f"expected {key} to be str, not {type(value)}")
        raise KeyError(key)

    def is_(self, key: str, default: bool = False) -> bool:
        if not (default is None or isinstance(default, bool)):
            raise TypeError("if given, default must be bool")
        if (value := super().get(key, default)) is not None and isinstance(value, bool):
            return value
        if key in self:
            raise TypeError(f"expected {key} to be bool, not {type(value)}")
        raise KeyError(key)

    @classmethod
    def parse(cls, prop_str: str) -> Self:
        """Convert props used in a ``#[whatever(arg1, arg2 = "val")]`` annotation.
        Note that the `prop_str` should just be the part between the parentheses.

        For the above example, the return value is ``{"arg1": True, "arg2": "val"}``.
        """

        result = cls()
        parts = [p.strip() for p in prop_str.split(",")]
        for part in parts:
            if "=" in part:
                key, val = map(str.strip, part.split("=", 1))
                if val.startswith('"') and val.endswith('"'):
                    val = val[1:-1]
                if key:
                    result[key] = val
            elif part:
                result[part] = True
        return result

    def __str__(self) -> str:
        """Convert back into a string."""
        return ", ".join(
            f'{k} = "{v}"' if isinstance(v, str) else str(k)
            for k, v in sorted(self.items())
        )

    def __sub__(self, keys: set[str]) -> Self:
        cls = self.__class__
        return cls((k, v) for k, v in self.items() if k not in keys)


@dataclass(frozen=True, order=True)
class Item:
    """Something that can be included in a Module (e.g., a class, function, exception, etc.)."""

    rust_name: str
    python_name: str
    kind: Kind = field(compare=False, hash=False)
    path: Path = field(compare=False, hash=False)
    line: Line = field(compare=False, hash=False)
    props: PyO3Props = field(compare=False, hash=False, default_factory=PyO3Props)
    stub_attr: StubAttr | None = field(compare=False, hash=False, default=None)

    def __str__(self) -> str:
        name = self.rust_name
        if self.rust_name != self.python_name:
            name = f"{name} (py: {self.python_name})"
        return f"{self.kind.value} '{name}' in {self.path}@{self.line.num}"

    def has_stub(self) -> bool:
        return self.stub_attr is not None


@dataclass
class Module(MutableSet[Item]):
    """A collection of `Item`s in the same Python module."""

    _items: set[Item] = field(default_factory=set)
    submodule: bool = True
    path: Path = Path(".")
    line_num: int = -1
    props: PyO3Props = field(default_factory=PyO3Props)
    _fixed_enums: set[str] = field(default_factory=set)

    def __contains__(self, key: object) -> bool:
        return key in self._items

    def __iter__(self) -> Iterator[Item]:
        return self._items.__iter__()

    def __len__(self) -> int:
        return self._items.__len__()

    def update(self, items: Iterable[Item]) -> None:
        return self._items.update(items)

    def discard(self, value: Item) -> None:
        return self._items.discard(value)

    def add(self, item: Item) -> None:
        logger.info(f"Adding {item}")
        self._items.add(item)

    def find_rust(self, item: Item | str) -> Item | None:
        """Return an `item` in this `Module` that has the same Rust name as the given `item`."""
        target = item.rust_name if isinstance(item, Item) else item
        return next((i for i in self if i.rust_name == target), None)

_T = TypeVar("_T")

@dataclass
class Package(MutableMapping[str, Module]):
    """A collection of `Module`s."""

    _modules: defaultdict[str, Module] = field(
        default_factory=lambda: defaultdict(Module)
    )

    # This tracks ``rust_type_name -> methods`` for `impl` blocks as we discover them,
    # for when we don't yet know which Python module the implemented type belongs to.
    _methods: Module = field(default_factory=Module)

    def find_rust(self, item: Item | str) -> tuple[str, Item] | None:
        """Return the name of a `Module` and an `Item` within it
        that has the same Rust name as the given `item`,
        but is distinct from the given `item.`

        If the given `item` is a `str` instead of an actual `Item`,
        the search is instead for an `Item` with that Rust name,
        and distinctness does not apply.

        The `Item`-argument form is useful for finding an `Item`
        that has been mistakenly placed in multiple modules,
        while `str`-argument form is useful for associating something with an `Item`
        when its module isn't immediately available (i.e., in an `impl` block).
        """
        return next(
            (
                (name, found)
                for name, module in self._modules.items()
                if (found := module.find_rust(item)) is not None
                and (isinstance(item, str) or id(found) != id(item))
            ),
            None,
        )

    def __getitem__(self, name: str, /) -> Module:
        if name not in self._modules:
            logger.info(f"Creating module '{name}'.")
        return self._modules[name]

    @overload  # type: ignore
    def get(self, key: str, default: None = None, /) -> Module | None: ...
    @overload
    def get(self, key: str, default: Module, /) -> Module: ...
    @overload
    def get(self, key: str, default: _T, /) -> Module | _T: ...

    def get(self, name: str, default: _T | None = None, /) -> Module | _T | None:
        return self._modules.get(name, default)

    def __setitem__(self, name: str, module: Module, /) -> None:
        logger.info(f"Setting module '{name}'.")
        self._modules[name] = module

    def __delitem__(self, name: str, /) -> None:
        del self._modules[name]

    def __contains__(self, name: object) -> bool:
        if not isinstance(name, str):
            raise TypeError(f"Package keys must be strings, not {type(name)}")
        return name in self._modules

    def __iter__(self) -> Iterator[str]:
        return self._modules.__iter__()

    def __len__(self) -> int:
        return self._modules.__len__()

    def keys(self) -> KeysView[str]:
        return self._modules.keys()

    def values(self) -> ValuesView[Module]:
        return self._modules.values()

    def items(self) -> ItemsView[str, Module]:
        return self._modules.items()

