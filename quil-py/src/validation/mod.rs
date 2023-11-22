use pyo3::{pyfunction, PyResult};

use rigetti_pyo3::create_init_submodule;

pub mod identifier;

create_init_submodule! {
    funcs: [dummy],
    submodules: ["identifier": identifier::init_submodule],
}

#[pyfunction]
fn dummy() -> PyResult<()> {
    Ok(())
}
