pub(crate) mod graph;
pub(crate) mod schedule;

#[cfg(feature = "graphviz-dot")]
pub(crate) mod graphviz_dot;

pub use graph::{
    DependencyGraph, ExecutionDependency, MemoryAccessType, ScheduleError, ScheduleErrorVariant,
    ScheduleResult, ScheduledBasicBlock, ScheduledBasicBlockOwned, ScheduledGraphNode,
    ScheduledProgram,
};

pub use schedule::{
    ComputedScheduleError, ComputedScheduleItem, Schedule, ScheduleSeconds, Seconds, TimeSpan,
};
