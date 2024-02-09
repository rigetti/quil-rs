pub(crate) mod graph;

#[cfg(feature = "graphviz-dot")]
pub(crate) mod graphviz_dot;

pub use graph::{ScheduledBasicBlock, ScheduledProgram};
