//! A Schedule represents a flattening of the [`DependencyGraph`] into a linear sequence of
//! instructions, with each instruction assigned a start time and duration.

use std::collections::HashMap;

use itertools::Itertools;
use petgraph::{
    visit::{EdgeFiltered, Topo},
    Direction,
};

use crate::{
    instruction::{
        AttributeValue, Capture, Delay, Instruction, Pulse, RawCapture, WaveformInvocation,
    },
    quil::Quil,
    Program,
};

use super::{ExecutionDependency, ScheduledBasicBlock, ScheduledGraphNode};

#[derive(Clone, Default, PartialEq, PartialOrd)]
pub struct Seconds(pub f64);

impl std::ops::Add<Seconds> for Seconds {
    type Output = Seconds;

    fn add(self, rhs: Seconds) -> Self::Output {
        Self(rhs.0 + self.0)
    }
}

// todo: rather than Default, it should require Time::zero. For primitives those are the same,
// but not so for Expression
#[derive(Debug, Default)]
pub struct Schedule<Time: Default> {
    items: Vec<ComputedScheduleItem<Time>>,
    /// The total duration of the block. This is the end time of the schedule when it starts at Time::zero()
    duration: Time,
}

impl<Time: Default> Schedule<Time> {
    pub fn duration(&self) -> &Time {
        &self.duration
    }

    pub fn items(&self) -> &[ComputedScheduleItem<Time>] {
        self.items.as_ref()
    }
}

pub type FixedSchedule = Schedule<Seconds>;

#[derive(Debug)]
pub struct ComputedScheduleItem<Time> {
    pub time_span: TimeSpan<Time>,
    pub instruction_index: usize,
}

#[derive(Debug, thiserror::Error)]
pub enum ComputedScheduleError {
    #[error("unknown duration for instruction {}", instruction.to_quil_or_debug())]
    UnknownDuration { instruction: Instruction },

    #[error("internal error: invalid dependency graph")]
    InvalidDependencyGraph,
}

pub type ComputedScheduleResult<T> = Result<T, ComputedScheduleError>;

/// Represents a span of time, for some unit of time
#[derive(Debug)]
pub struct TimeSpan<Time> {
    /// The inclusive start time of the described item
    start_time: Time,

    /// The described item's continuous duration
    duration: Time,
}

impl<'p> ScheduledBasicBlock<'p> {
    // todo create more restricted subtype of instruction for ScheduledBasicBlock so we don't have
    // to handle unreachable instructions here
    /// Return the duration of a scheduled Quil instruction:
    ///
    /// * For PULSE and CAPTURE, this is the duration of the waveform at the frame's sample rate
    /// * For DELAY and RAW-CAPTURE, it's the named duration
    /// * For supporting instructions like SET-*, SHIFT-*, and FENCE, it's 0
    ///
    /// Return `None` for other instructions.
    fn get_fixed_instruction_duration(
        program: &Program,
        instruction: &Instruction,
    ) -> Option<Seconds> {
        match instruction {
            Instruction::Capture(Capture { waveform, .. })
            | Instruction::Pulse(Pulse { waveform, .. }) => {
                Self::get_fixed_waveform_duration(program, instruction, waveform)
            }
            Instruction::Delay(Delay { duration, .. })
            | Instruction::RawCapture(RawCapture { duration, .. }) => {
                duration.to_real().ok().map(Seconds)
            }
            Instruction::Fence(_)
            | Instruction::SetFrequency(_)
            | Instruction::SetPhase(_)
            | Instruction::SetScale(_)
            | Instruction::ShiftFrequency(_)
            | Instruction::ShiftPhase(_)
            | Instruction::SwapPhases(_) => Some(Seconds(0.0)),
            // Todo should this tolerate classical instructions? how about reset?
            _ => None,
        }
    }

    /// Return the duration of a Quil waveform:
    ///
    /// If the waveform is defined in the program with `DEFWAVEFORM`, the duration is the sample count
    /// divided by the sample rate.
    ///
    /// Otherwise, it's the `duration` parameter of the waveform invocation. This relies on the assumption that
    /// all template waveforms in use have such a parameter in units of seconds.
    fn get_fixed_waveform_duration(
        program: &Program,
        instruction: &Instruction,
        waveform_invocation: &WaveformInvocation,
    ) -> Option<Seconds> {
        if let Some(definition) = program.waveforms.get(&waveform_invocation.name) {
            let sample_count = definition.matrix.len();
            let common_sample_rate =
                program
                    .get_frames_for_instruction(instruction)
                    .and_then(|frames| {
                        frames
                            .used
                            .into_iter()
                            .filter_map(|frame| {
                                program
                                    .frames
                                    .get(frame)
                                    .and_then(|frame_definition| {
                                        frame_definition.get("SAMPLE-RATE")
                                    })
                                    .map(|sample_rate_expression| match sample_rate_expression {
                                        AttributeValue::String(_) => todo!("handle error"),
                                        AttributeValue::Expression(expression) => expression,
                                    })
                                    .and_then(|expression| expression.to_real().ok())
                            })
                            .all_equal_value()
                            .ok()
                    });

            common_sample_rate
                .map(|sample_rate| sample_count as f64 / sample_rate)
                .map(Seconds)
        } else {
            waveform_invocation
                .parameters
                .get("duration")
                .and_then(|v| v.to_real().ok())
                .map(Seconds)
        }
    }

    /// Compute the flattened schedule for this [`ScheduledBasicBlock`] in terms of seconds,
    /// using a default built-in calculation for the duration of scheduled instructions.
    pub fn as_fixed_schedule(&self, program: &Program) -> ComputedScheduleResult<FixedSchedule> {
        self.as_schedule(program, Self::get_fixed_instruction_duration)
    }

    /// Compute the flattened schedule for this [`ScheduledBasicBlock`] using a user-provided
    /// closure for computation of instruction duration.
    ///
    /// Return an error if the schedule cannot be computed from the information provided.
    pub fn as_schedule<F, Time: Clone + Default + PartialOrd + std::ops::Add<Time, Output = Time>>(
        &self,
        program: &'p Program,
        get_duration: F,
    ) -> ComputedScheduleResult<Schedule<Time>>
    where
        F: Fn(&'p Program, &'p Instruction) -> Option<Time>,
    {
        let mut schedule = Schedule::default();
        let mut end_time_by_instruction_index = HashMap::<usize, Time>::new();

        let graph_filtered = EdgeFiltered::from_fn(&self.graph, |(_, _, dependencies)| {
            dependencies.contains(&ExecutionDependency::Scheduled)
        });
        let mut topo = Topo::new(&graph_filtered);

        while let Some(instruction_node) = topo.next(&graph_filtered) {
            // todo also handle the end node so as to capture the whole schedule duration
            if let ScheduledGraphNode::InstructionIndex(index) = instruction_node {
                let instruction = *self
                    .basic_block()
                    .instructions()
                    .get(index)
                    .expect("todo error");
                let duration = get_duration(program, instruction).ok_or(
                    ComputedScheduleError::UnknownDuration {
                        instruction: instruction.clone(),
                    },
                )?;

                let latest_previous_instruction_scheduler_end_time = self
                    .graph
                    .edges_directed(instruction_node, Direction::Incoming)
                    .filter_map(|(source, _, dependencies)| {
                        if dependencies.contains(&ExecutionDependency::Scheduled) {
                            match source {
                                ScheduledGraphNode::BlockStart => Ok(Some(Time::default())),
                                ScheduledGraphNode::InstructionIndex(previous_index) => {
                                    end_time_by_instruction_index
                                        .get(&previous_index)
                                        .cloned()
                                        .ok_or(ComputedScheduleError::InvalidDependencyGraph)
                                        .map(Some)
                                }
                                ScheduledGraphNode::BlockEnd => unreachable!(),
                            }
                        } else {
                            Ok(None)
                        }
                        .transpose()
                    })
                    .collect::<Result<Vec<Time>, _>>()?
                    .into_iter()
                    // this implementation allows us to require PartialOrd instead of Ord (required for `.max()`),
                    // which is convenient for f64
                    .fold(Time::default(), |acc, el| if el > acc { el } else { acc });

                let start_time = latest_previous_instruction_scheduler_end_time;
                let end_time = start_time.clone() + duration.clone();
                if schedule.duration < end_time {
                    schedule.duration = end_time.clone();
                }

                end_time_by_instruction_index.insert(index, end_time);
                schedule.items.push(ComputedScheduleItem {
                    time_span: TimeSpan {
                        start_time,
                        duration,
                    },
                    instruction_index: index,
                });
            }
        }

        Ok(schedule)
    }
}
