use pyo3::exceptions::PyValueError;
use quil_rs::program::scheduling::{
    ComputedScheduleError, ComputedScheduleItem, ScheduleSeconds, Seconds, TimeSpan,
};
use rigetti_pyo3::{
    impl_repr, py_wrap_error, py_wrap_type, pyo3::prelude::*, wrap_error, PyWrapper,
};

use crate::impl_eq;

wrap_error!(RustComputedScheduleError(ComputedScheduleError));
py_wrap_error!(
    quil,
    RustComputedScheduleError,
    PyComputedScheduleError,
    PyValueError
);
py_wrap_type! {
    #[pyo3(subclass)]
    #[derive(Debug, PartialEq)]
    PyScheduleSeconds(ScheduleSeconds) as "ScheduleSeconds"
}

impl_repr!(PyScheduleSeconds);
impl_eq!(PyScheduleSeconds);

#[pymethods]
impl PyScheduleSeconds {
    pub fn items(&self) -> Vec<PyScheduleSecondsItem> {
        self.as_inner()
            .items()
            .iter()
            .map(PyScheduleSecondsItem::from)
            .collect()
    }

    pub fn duration(&self) -> f64 {
        self.as_inner().duration().0
    }
}

py_wrap_type! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass)]
    PyScheduleSecondsItem(ComputedScheduleItem<Seconds>) as "ScheduleSecondsItem"
}

impl_repr!(PyScheduleSecondsItem);
impl_eq!(PyScheduleSecondsItem);

#[pymethods]
impl PyScheduleSecondsItem {
    #[getter]
    pub fn time_span(&self) -> PyTimeSpanSeconds {
        (&self.as_inner().time_span).into()
    }

    #[getter]
    pub fn instruction_index(&self) -> usize {
        self.as_inner().instruction_index
    }
}

py_wrap_type! {
    #[derive(Debug, PartialEq)]
    #[pyo3(subclass)]
    PyTimeSpanSeconds(TimeSpan<Seconds>) as "TimeSpanSeconds"
}

impl_repr!(PyTimeSpanSeconds);
impl_eq!(PyTimeSpanSeconds);

#[pymethods]
impl PyTimeSpanSeconds {
    #[getter]
    pub fn start(&self) -> f64 {
        self.as_inner().start_time().0
    }

    #[getter]
    pub fn duration(&self) -> f64 {
        self.as_inner().duration().0
    }

    #[getter]
    pub fn end(&self) -> f64 {
        self.as_inner().end().0
    }
}
