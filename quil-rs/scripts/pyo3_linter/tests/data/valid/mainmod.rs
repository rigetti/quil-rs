use pyo3::{
    prelude::*,
    types::{PyDict, PyList, PyTuple, PyType, PyTypeMethods},
    wrap_pymodule,
};

#[cfg(feature = "stubs")]
use pyo3_stub_gen::define_stub_info_gatherer;

use crate::submod;

#[pymodule]
#[pyo3(name = "_quil")]
fn init_module(m: &Bound<'_, PyModule>) -> PyResult<()> {
    let py = m.py();

    m.add_wrapped(wrap_pymodule!(submod::init_submodule))?;

    m.add("SomeError", py.get_type::<SomeError>())?;

    let sys = PyModule::import(py, "sys")?;
    let sys_modules: Bound<'_, PyDict> = sys.getattr("modules")?.downcast_into()?;
    sys_modules.set_item("quil.instructions", m.getattr("instructions")?)?;
    Ok(())
}
