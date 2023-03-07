use pyo3::prelude::*;
use rigetti_pyo3::create_init_submodule;

pub mod instruction;
pub mod program;

create_init_submodule! {
    submodules: [ "instructions": instruction::init_submodule, "program": program::init_submodule ],
}

#[pymodule]
fn quil(py: Python<'_>, m: &PyModule) -> PyResult<()> {
    init_submodule("quil", py, m)?;
    Ok(())
}

pub fn init_quil_submodule(name: &str, py: Python<'_>, m: &PyModule) -> PyResult<()> {
    init_submodule(name, py, m)?;
    Ok(())
}
