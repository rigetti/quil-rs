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
    pub fn as_fixed_schedule(&self, program: &PyProgram) -> PyResult<PyFixedSchedule> {
        ScheduledBasicBlock::from(&self.0)
            .as_schedule_seconds(&program.0)
            .map(Into::into)
            .map_err(RustComputedScheduleError::from)
            .map_err(RustComputedScheduleError::to_py_err)
    }
}

py_wrap_type! {
    PyFixedSchedule(ScheduleSeconds) as "FixedSchedule"
}

impl_repr!(PyFixedSchedule);

#[pymethods]
impl PyFixedSchedule {
    pub fn items(&self) -> Vec<PyFixedScheduleItem> {
        self.as_inner()
            .items()
            .iter()
            .map(PyFixedScheduleItem::from)
            .collect()
    }

    pub fn duration(&self) -> f64 {
        self.as_inner().duration().0
    }
}

py_wrap_type! {
    PyFixedScheduleItem(ComputedScheduleItem<Seconds>) as "FixedScheduleItem"
}

impl_repr!(PyFixedScheduleItem);

#[pymethods]
impl PyFixedScheduleItem {
    #[getter]
    pub fn time_span(&self) -> PyFixedTimeSpan {
        (&self.as_inner().time_span).into()
    }

    #[getter]
    pub fn instruction_index(&self) -> usize {
        self.as_inner().instruction_index
    }
}

py_wrap_type! {
    PyFixedTimeSpan(TimeSpan<Seconds>) as "FixedTimeSpan"
}

impl_repr!(PyFixedTimeSpan);

#[pymethods]
impl PyFixedTimeSpan {
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
