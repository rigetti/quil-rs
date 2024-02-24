use quil_rs::program::scheduling::{
    ComputedScheduleItem, FixedSchedule, ScheduledBasicBlock, ScheduledBasicBlockOwned, Seconds,
    TimeSpan,
};
use rigetti_pyo3::{py_wrap_type, pyo3::prelude::*};

use super::PyProgram;

py_wrap_type! {
    PyScheduledBasicBlock(ScheduledBasicBlockOwned) as "ScheduledBasicBlock"
}

impl PyScheduledBasicBlock {
    pub fn as_fixed_schedule(&self, program: &PyProgram) -> PyFixedSchedule {
        ScheduledBasicBlock::from(&self.0)
            .as_fixed_schedule(&program.0)
            .expect("todo handle error")
            .into()
    }
}

py_wrap_type! {
    PyFixedSchedule(FixedSchedule) as "FixedSchedule"
}

#[pymethods]
impl PyFixedSchedule {
    pub fn items(&self) -> Vec<PyFixedScheduleItem> {
        self.0
            .items()
            .iter()
            .map(PyFixedScheduleItem::from)
            .collect()
    }

    pub fn duration(&self) -> f64 {
        self.0.duration().0
    }
}

py_wrap_type! {
    PyFixedScheduleItem(ComputedScheduleItem<Seconds>) as "FixedScheduleItem"
}

#[pymethods]
impl PyFixedScheduleItem {
    #[getter]
    pub fn time_span(&self) -> PyFixedTimeSpan {
        (&self.0.time_span).into()
    }

    #[getter]
    pub fn instruction_index(&self) -> usize {
        self.0.instruction_index
    }
}

py_wrap_type! {
    PyFixedTimeSpan(TimeSpan<Seconds>) as "FixedTimeSpan"
}

#[pymethods]
impl PyFixedTimeSpan {
    #[getter]
    pub fn start(&self) -> f64 {
        self.0.start_time().0
    }

    #[getter]
    pub fn duration(&self) -> f64 {
        self.0.duration().0
    }
}
