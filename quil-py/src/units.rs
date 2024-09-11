use quil_rs::units::{Cycles, Radians};
use rigetti_pyo3::py_wrap_type;

py_wrap_type! {
    #[derive(Debug)]
    PyCycles(Cycles<f64>) as "Cycles"
}

py_wrap_type! {
    #[derive(Debug)]
    PyRadians(Radians<f64>) as "Radians"
}
