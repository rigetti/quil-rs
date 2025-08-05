//! This binary is used to generate Python stub files (type hints) for the `quil` package.
//! For more information on why this exists as a separate binary rather than a build script,
//! see the [`pyo3-stub-gen`][] documentation.
//!
//! [`pyo3-stub-gen`]: https://github.com/Jij-Inc/pyo3-stub-gen
#[cfg(feature = "stubs")]
use pyo3_stub_gen::Result;

#[cfg(feature = "stubs")]
fn main() -> Result<()> {
    let stub = quil_rs::quilpy::stub_info()?;
    stub.generate()?;
    Ok(())
}

#[cfg(not(feature = "stubs"))]
fn main() {
    eprintln!("Executing this binary only makes sense with the --stubs feature enabled.");
}
