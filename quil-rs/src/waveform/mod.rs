use pyo3::prelude::*;

use crate::impl_repr;
pub(crate) mod templates;

pub use templates::*;

#[pymodule]
#[pyo3(name = "waveforms", module = "quil", submodule)]
pub(crate) fn init_submodule(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<BoxcarKernel>()?;
    m.add_class::<ErfSquare>()?;
    m.add_class::<Gaussian>()?;
    m.add_class::<DragGaussian>()?;
    m.add_class::<HermiteGaussian>()?;
    m.add_function(wrap_pyfunction!(py_apply_phase_and_detuning, m)?)?;
    Ok(())
}

impl_repr!(BoxcarKernel);
impl_repr!(DragGaussian);
impl_repr!(ErfSquare);
impl_repr!(Gaussian);
impl_repr!(HermiteGaussian);
