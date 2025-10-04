// Some text at the top of the module.

use std::importable::item;

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::{gen_stub_pyclass_complex_enum, gen_stub_pymethods};

#[derive(Clone, Debug, thiserror::Error, PartialEq, Eq)]
pub enum RustError {
    #[error(transparent)]
    SomeError(#[from] AnotherError),
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass_complex_enum)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.instructions", eq, frozen)
)]
pub enum EnumA {
    InstA(InstA),
    InstB(InstB),
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass_complex_enum)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.instructions", eq, frozen)
)]
pub enum EnumB {
    InstC(InstC),
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass_enum)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.instructions", eq, frozen, rename_all = "SCREAMING_SNAKE_CASE")
)]
pub enum SimpleEnum {
    Variant1,
    Variant2,
    Variant3,
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "quil.instructions", subclass, get_all, set_all, eq)
)]
pub struct InstA {
    pub field: u64,
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(module = "instructions", subclass, get_all, set_all, eq)
)]
pub struct InstB {
    pub field: u64,
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(feature = "python", pyo3::pyclass)]
pub struct InstC {
    pub field: u64,
}
