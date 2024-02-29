use pyo3::exceptions::PyValueError;
use quil_rs::program::scheduling::{
    ComputedScheduleError, ComputedScheduleItem, ScheduleSeconds, ScheduledBasicBlock,
    ScheduledBasicBlockOwned, Seconds, TimeSpan,
};
use rigetti_pyo3::{
    impl_repr, py_wrap_error, py_wrap_type, pyo3::prelude::*, wrap_error, PyWrapper, ToPythonError,
};

use super::PyProgram;

py_wrap_type! {
    PyScheduledBasicBlock(ScheduledBasicBlockOwned) as "ScheduledBasicBlock"
}

impl_repr!(PyScheduledBasicBlock);

wrap_error!(RustComputedScheduleError(ComputedScheduleError));
py_wrap_error!(
    quil,
    RustComputedScheduleError,
    PyComputedScheduleError,
    PyValueError
);

impl PyScheduledBasicBlock {
    pub fn as_schedule_seconds(&self, program: &PyProgram) -> PyResult<PyScheduleSeconds> {
        ScheduledBasicBlock::from(&self.0)
            .as_schedule_seconds(&program.0)
            .map(Into::into)
            .map_err(RustComputedScheduleError::from)
            .map_err(RustComputedScheduleError::to_py_err)
    }
}

py_wrap_type! {
    PyScheduleSeconds(ScheduleSeconds) as "ScheduleSeconds"
}

impl_repr!(PyScheduleSeconds);

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
    PyScheduleSecondsItem(ComputedScheduleItem<Seconds>) as "ScheduleSecondsItem"
}

impl_repr!(PyScheduleSecondsItem);

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
    PyTimeSpanSeconds(TimeSpan<Seconds>) as "TimeSpanSeconds"
}

impl_repr!(PyTimeSpanSeconds);

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
