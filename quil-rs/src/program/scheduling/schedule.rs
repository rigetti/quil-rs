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

#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub struct Seconds(pub f64);

impl std::ops::Add<Seconds> for Seconds {
    type Output = Seconds;

    fn add(self, rhs: Seconds) -> Self::Output {
        Self(rhs.0 + self.0)
    }
}

impl std::ops::Sub<Seconds> for Seconds {
    type Output = Seconds;

    fn sub(self, rhs: Seconds) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

pub trait Zero: PartialEq + Sized {
    fn zero() -> Self;

    fn is_zero(&self) -> bool {
        self == &Self::zero()
    }
}

impl Zero for Seconds {
    fn zero() -> Self {
        Self(0.0)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Schedule<TimeUnit> {
    items: Vec<ComputedScheduleItem<TimeUnit>>,
    /// The total duration of the block. This is the end time of the schedule when it starts at `TimeUnit::zero()`
    duration: TimeUnit,
}

impl<TimeUnit> Schedule<TimeUnit> {
    pub fn duration(&self) -> &TimeUnit {
        &self.duration
    }

    pub fn items(&self) -> &[ComputedScheduleItem<TimeUnit>] {
        self.items.as_ref()
    }

    pub fn into_items(self) -> Vec<ComputedScheduleItem<TimeUnit>> {
        self.items
    }
}

impl<TimeUnit: Clone + PartialOrd + std::ops::Add<TimeUnit, Output = TimeUnit> + Zero>
    From<Vec<ComputedScheduleItem<TimeUnit>>> for Schedule<TimeUnit>
{
    fn from(items: Vec<ComputedScheduleItem<TimeUnit>>) -> Self {
        let duration = items
            .iter()
            .map(|item| item.time_span.start_time.clone() + item.time_span.duration.clone())
            .fold(TimeUnit::zero(), |acc, el| if el > acc { el } else { acc });
        Self { items, duration }
    }
}

impl<TimeUnit: Zero> Default for Schedule<TimeUnit> {
    fn default() -> Self {
        Self {
            items: Default::default(),
            duration: TimeUnit::zero(),
        }
    }
}

pub type ScheduleSeconds = Schedule<Seconds>;

#[derive(Clone, Debug, PartialEq)]
pub struct ComputedScheduleItem<TimeUnit> {
    pub time_span: TimeSpan<TimeUnit>,
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
#[derive(Clone, Debug, PartialEq)]
pub struct TimeSpan<TimeUnit> {
    /// The inclusive start time of the described item
    pub start_time: TimeUnit,

    /// The described item's continuous duration
    pub duration: TimeUnit,
}

impl<TimeUnit> TimeSpan<TimeUnit> {
    pub fn start_time(&self) -> &TimeUnit {
        &self.start_time
    }

    pub fn duration(&self) -> &TimeUnit {
        &self.duration
    }
}

impl<TimeUnit: Clone + std::ops::Add<TimeUnit, Output = TimeUnit>> TimeSpan<TimeUnit> {
    pub fn end(&self) -> TimeUnit {
        self.start_time.clone() + self.duration.clone()
    }
}

impl<
        TimeUnit: Clone
            + PartialOrd
            + std::ops::Add<TimeUnit, Output = TimeUnit>
            + std::ops::Sub<TimeUnit, Output = TimeUnit>,
    > TimeSpan<TimeUnit>
{
    pub(crate) fn union(self, rhs: Self) -> Self {
        let start_time = if rhs.start_time < self.start_time {
            rhs.start_time.clone()
        } else {
            self.start_time.clone()
        };

        let self_end_time = self.start_time.clone() + self.duration;
        let rhs_end_time = rhs.start_time + rhs.duration;
        let end_time = if self_end_time < rhs_end_time {
            rhs_end_time
        } else {
            self_end_time
        };

        Self {
            duration: end_time - start_time.clone(),
            start_time,
        }
    }
}

impl<'p> ScheduledBasicBlock<'p> {
    /// Return the duration of a scheduled Quil instruction:
    ///
    /// * For PULSE and CAPTURE, this is the duration of the waveform at the frame's sample rate
    /// * For DELAY and RAW-CAPTURE, it's the named duration
    /// * For supporting instructions like SET-*, SHIFT-*, and FENCE, it's 0
    ///
    /// Return `None` for other instructions.
    pub(crate) fn get_instruction_duration_seconds(
        program: &Program,
        instruction: &Instruction,
    ) -> Option<Seconds> {
        match instruction {
            Instruction::Capture(Capture { waveform, .. })
            | Instruction::Pulse(Pulse { waveform, .. }) => {
                Self::get_waveform_duration_seconds(program, instruction, waveform)
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
    fn get_waveform_duration_seconds(
        program: &Program,
        instruction: &Instruction,
        WaveformInvocation { name, parameters }: &WaveformInvocation,
    ) -> Option<Seconds> {
        if let Some(definition) = program.waveforms.get(name) {
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
                                    .and_then(|sample_rate_expression| match sample_rate_expression
                                    {
                                        AttributeValue::String(_) => None,
                                        AttributeValue::Expression(expression) => Some(expression),
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
            // Per the Quil spec, all waveform templates have a "duration"
            // parameter, and "erf_square" also has "pad_left" and "pad_right".
            // We explicitly choose to be more flexible here, and allow any
            // built-in waveform templates to have "pad_*" parameters, as well
            // as allow "erf_square" to omit them.
            let parameter = |parameter_name| {
                parameters
                    .get(parameter_name)
                    .and_then(|v| v.to_real().ok())
                    .map(Seconds)
            };
            Some(
                parameter("duration")?
                    + parameter("pad_left").unwrap_or(Seconds::zero())
                    + parameter("pad_right").unwrap_or(Seconds::zero()),
            )
        }
    }

    /// Compute the flattened schedule for this [`ScheduledBasicBlock`] in terms of seconds,
    /// using a default built-in calculation for the duration of scheduled instructions.
    pub fn as_schedule_seconds(
        &self,
        program: &Program,
    ) -> ComputedScheduleResult<ScheduleSeconds> {
        self.as_schedule(program, Self::get_instruction_duration_seconds)
    }

    /// Compute the flattened schedule for this [`ScheduledBasicBlock`] using a user-provided
    /// closure for computation of instruction duration.
    ///
    /// Return an error if the schedule cannot be computed from the information provided.
    pub fn as_schedule<
        F,
        TimeUnit: Clone + PartialOrd + std::ops::Add<TimeUnit, Output = TimeUnit> + Zero,
    >(
        &self,
        program: &'p Program,
        get_duration: F,
    ) -> ComputedScheduleResult<Schedule<TimeUnit>>
    where
        F: Fn(&'p Program, &'p Instruction) -> Option<TimeUnit>,
    {
        let mut schedule = Schedule::default();
        let mut end_time_by_instruction_index = HashMap::<usize, TimeUnit>::new();

        let graph_filtered = EdgeFiltered::from_fn(&self.graph, |(_, _, dependencies)| {
            dependencies.contains(&ExecutionDependency::Scheduled)
        });
        let mut topo = Topo::new(&graph_filtered);

        while let Some(instruction_node) = topo.next(&graph_filtered) {
            if let ScheduledGraphNode::InstructionIndex(index) = instruction_node {
                let instruction = *self
                    .basic_block()
                    .instructions()
                    .get(index)
                    .ok_or_else(|| ComputedScheduleError::InvalidDependencyGraph)?;
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
                                ScheduledGraphNode::BlockStart => Ok(Some(TimeUnit::zero())),
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
                    .collect::<Result<Vec<TimeUnit>, _>>()?
                    .into_iter()
                    // this implementation allows us to require PartialOrd instead of Ord (required for `.max()`),
                    // which is convenient for f64
                    .fold(TimeUnit::zero(), |acc, el| if el > acc { el } else { acc });

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

#[cfg(test)]
mod tests {
    use core::panic;
    use std::str::FromStr;

    use crate::{instruction::InstructionHandler, program::scheduling::TimeSpan, Program};

    #[rstest::rstest]
    #[case("CAPTURE 0 \"a\" flat(duration: 1.0) ro", Some(1.0))]
    #[case("DELAY 0 \"a\" 1.0", Some(1.0))]
    #[case("FENCE", Some(0.0))]
    #[case("PULSE 0 \"a\" flat(duration: 1.0)", Some(1.0))]
    #[case("RAW-CAPTURE 0 \"a\" 1.0 ro", Some(1.0))]
    #[case("RESET", None)]
    #[case("SET-FREQUENCY 0 \"a\" 1.0", Some(0.0))]
    #[case("SET-PHASE 0 \"a\" 1.0", Some(0.0))]
    #[case("SET-SCALE 0 \"a\" 1.0", Some(0.0))]
    #[case("SHIFT-FREQUENCY 0 \"a\" 1.0", Some(0.0))]
    #[case("SHIFT-PHASE 0 \"a\" 1.0", Some(0.0))]
    #[case("SWAP-PHASES 0 \"a\" 0 \"b\"", Some(0.0))]
    fn instruction_duration_seconds(
        #[case] input_program: &str,
        #[case] expected_duration: Option<f64>,
    ) {
        let empty_program = Program::new();
        let program = Program::from_str(input_program)
            .map_err(|e| e.to_string())
            .unwrap();
        let instruction = program.into_instructions().remove(0);
        let duration =
            crate::program::scheduling::ScheduledBasicBlock::get_instruction_duration_seconds(
                &empty_program,
                &instruction,
            );
        assert_eq!(
            expected_duration.map(crate::program::scheduling::Seconds),
            duration
        );
    }

    #[rstest::rstest]
    #[case(
        r#"FENCE
FENCE
FENCE
"#,
        Ok(vec![0.0, 0.0, 0.0])
    )]
    #[case(
        r#"DEFFRAME 0 "a":
    SAMPLE-RATE: 1e9
PULSE 0 "a" flat(duration: 1.0)
PULSE 0 "a" flat(duration: 1.0)
PULSE 0 "a" flat(duration: 1.0)
"#,
        Ok(vec![0.0, 1.0, 2.0])
    )]
    #[case(
        r#"DEFFRAME 0 "a":
    SAMPLE-RATE: 1e9
PULSE 0 "a" erf_square(duration: 1.0, pad_left: 0.2, pad_right: 0.3)
PULSE 0 "a" erf_square(duration: 0.1, pad_left: 0.7, pad_right: 0.7)
PULSE 0 "a" erf_square(duration: 0.5, pad_left: 0.6, pad_right: 0.4)
FENCE
"#,
        Ok(vec![0.0, 1.5, 3.0, 4.5])
    )]
    #[case(
        r#"DEFFRAME 0 "a":
    SAMPLE-RATE: 1e9
DEFFRAME 0 "b":
    SAMPLE-RATE: 1e9
NONBLOCKING PULSE 0 "a" flat(duration: 1.0)
NONBLOCKING PULSE 0 "b" flat(duration: 10.0)
FENCE
PULSE 0 "a" flat(duration: 1.0)
FENCE
PULSE 0 "a" flat(duration: 1.0)
"#,
        Ok(vec![0.0, 0.0, 10.0, 10.0, 11.0, 11.0])
    )]
    #[case(
        r#"DEFFRAME 0 "a":
    SAMPLE-RATE: 1e9
DEFFRAME 0 "b":
    SAMPLE-RATE: 1e9
DELAY 0 "a" 1.0
SET-PHASE 0 "a" 1.0
SHIFT-PHASE 0 "a" 1.0
SWAP-PHASES 0 "a" 0 "b"
SET-FREQUENCY 0 "a" 1.0
SHIFT-FREQUENCY 0 "a" 1.0
SET-SCALE 0 "a" 1.0
FENCE
PULSE 0 "a" flat(duration: 1.0)
"#,
        Ok(vec![0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0])
    )]
    #[case("RESET", Err(()))]
    fn schedule_seconds(#[case] input_program: &str, #[case] expected_times: Result<Vec<f64>, ()>) {
        let program: Program = input_program.parse().unwrap();
        let block: crate::program::analysis::BasicBlock = (&program).try_into().unwrap();
        let mut handler = InstructionHandler::default();
        let scheduled_block =
            crate::program::scheduling::ScheduledBasicBlock::build(block, &program, &mut handler)
                .unwrap();
        match (
            scheduled_block.as_schedule_seconds(&program),
            expected_times,
        ) {
            (Ok(schedule), Ok(expected_times)) => {
                let times = schedule
                    .items()
                    .iter()
                    .map(|item| item.time_span.start_time.0)
                    .collect::<Vec<_>>();
                assert_eq!(expected_times, times);
            }
            (Err(_), Err(_)) => {}
            (Ok(schedule), Err(_)) => {
                let times = schedule
                    .items()
                    .iter()
                    .map(|item| item.time_span.start_time.0)
                    .collect::<Vec<_>>();
                panic!("expected error, got {:?}", times);
            }
            (Err(error), Ok(_)) => {
                panic!("expected success, got error: {error}")
            }
        }
    }

    #[rstest::rstest]
    #[case::identical((0, 10), (0, 10), (0, 10))]
    #[case::adjacent((0, 1), (1, 1), (0, 2))]
    #[case::disjoint((0, 10), (20, 10), (0, 30))]
    #[case::disjoint_reverse((20, 10), (0, 10), (0, 30))]
    fn time_span_union(
        #[case] a: (usize, usize),
        #[case] b: (usize, usize),
        #[case] expected: (usize, usize),
    ) {
        let a = TimeSpan {
            start_time: a.0,
            duration: a.1,
        };
        let b = TimeSpan {
            start_time: b.0,
            duration: b.1,
        };
        let expected = TimeSpan {
            start_time: expected.0,
            duration: expected.1,
        };
        assert_eq!(expected, a.union(b));
    }
}
