//! Utilities for working with Graphviz dot format files

// Copyright 2021 Rigetti Computing
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

use dot_writer::{Attributes, DotWriter, Shape, Style};

use crate::{
    instruction::Target,
    program::{
        analysis::BasicBlockTerminator,
        scheduling::graph::{
            ExecutionDependency, MemoryAccessType, ScheduledBasicBlock, ScheduledGraphNode,
        },
    },
    quil::Quil,
};

use super::graph::ScheduledProgram;

impl ScheduledBasicBlock<'_> {
    /// Given a [`dot_writer::Scope`] representing a subgraph/cluster, write the timing graph for this block into it.
    /// Uses the `node_prefix` argument for namespacing so that node IDs remain unique within the overall graph.
    fn write_dot_format(&self, cluster: &mut dot_writer::Scope, node_prefix: &str) {
        self.graph.nodes().for_each(|node| {
            let node_id = get_node_id(&node, node_prefix);
            match &node {
                ScheduledGraphNode::BlockEnd => {
                    cluster
                        .node_named(node_id)
                        .set_shape(Shape::Circle)
                        .set_label("end");
                }
                ScheduledGraphNode::BlockStart => {
                    cluster
                        .node_named(node_id)
                        .set_shape(Shape::Circle)
                        .set_label("start");
                }
                ScheduledGraphNode::InstructionIndex(index) => {
                    cluster
                        .node_named(node_id)
                        .set_shape(Shape::Rectangle)
                        .set_label(&escape_label(&format!(
                            "[{}] {}",
                            index,
                            self.instructions().get(*index).unwrap().to_quil_or_debug()
                        )));
                }
            };
            self.graph.edges(node).for_each(|(src, dest, edge)| {
                let source = get_node_id(&src, node_prefix);
                let target = get_node_id(&dest, node_prefix);
                let mut labels = edge
                    .iter()
                    .map(|dependency| match dependency {
                        ExecutionDependency::AwaitMemoryAccess(access_type) => match access_type {
                            MemoryAccessType::Read => "await read",
                            MemoryAccessType::Write => "await write",
                            MemoryAccessType::Capture => "await capture",
                        },
                        ExecutionDependency::Scheduled => "timing",
                        ExecutionDependency::StableOrdering => "ordering",
                    })
                    .collect::<Vec<&str>>();

                // We sort them so that graph output is deterministic; iterating over the set
                // without sorting would cause flaky tests.
                labels.sort_unstable();
                let label = labels.join("\n");
                cluster
                    .edge(source, target)
                    .attributes()
                    .set_label(label.as_str());
            })
        });
    }
}

#[derive(Debug, PartialEq)]
enum DotFormatBlockLabel<'p> {
    Unlabeled { index: usize },
    Labeled(&'p Target),
}

impl<'p> DotFormatBlockLabel<'p> {
    fn from_label_or_index(label: Option<&'p Target>, index: usize) -> Self {
        label
            .map(DotFormatBlockLabel::Labeled)
            .unwrap_or(DotFormatBlockLabel::Unlabeled { index })
    }
}

impl<'p> From<&'p Target> for DotFormatBlockLabel<'p> {
    fn from(target: &'p Target) -> Self {
        DotFormatBlockLabel::Labeled(target)
    }
}

impl From<usize> for DotFormatBlockLabel<'_> {
    fn from(index: usize) -> Self {
        DotFormatBlockLabel::Unlabeled { index }
    }
}

impl std::fmt::Display for DotFormatBlockLabel<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DotFormatBlockLabel::Unlabeled { index } => write!(f, "block_{index}"),
            DotFormatBlockLabel::Labeled(label) => write!(f, "{}", label.to_quil_or_debug()),
        }
    }
}

impl ScheduledProgram<'_> {
    /// Return a DOT format string (as bytes) for use with Graphviz.
    ///
    /// This outputs a `digraph` object with a `subgraph` for each block to inform the layout engine.
    /// Each `subgraph` ID is prefixed with `cluster_` which instructs some supporting layout engines
    /// to enclose the subgraph with a border. This improves readability of the graph.
    ///
    /// Lines on the graph indicate scheduling dependencies within blocks and control flow among blocks.
    /// Each node representing an instruction is labeled with the contents of that instruction.
    pub fn get_dot_format(&self) -> Vec<u8> {
        let mut output_bytes = Vec::new();

        {
            let mut writer = DotWriter::from(&mut output_bytes);
            writer.set_pretty_print(true);
            let mut digraph = writer.digraph();

            let blocks = self.basic_blocks();
            println!("blocks: {blocks:?}");
            let mut iter = blocks.iter().enumerate().peekable();
            if let Some((index, first_block)) = iter.peek() {
                let block_node_label = first_block
                    .basic_block()
                    .label()
                    .map(DotFormatBlockLabel::from)
                    .unwrap_or(DotFormatBlockLabel::from(*index));
                digraph.edge(
                    "entry",
                    get_node_id(
                        &ScheduledGraphNode::BlockStart,
                        &block_node_label.to_string(),
                    ),
                );
            }
            digraph.node_named("entry").set_label("Entry Point");

            while let Some((index, scheduled_basic_block)) = iter.next() {
                let block_node_label = scheduled_basic_block
                    .basic_block()
                    .label()
                    .map(DotFormatBlockLabel::from)
                    .unwrap_or(DotFormatBlockLabel::from(index));
                let string_label = block_node_label.to_string();
                {
                    let mut cluster = digraph.cluster();
                    cluster.set_label(&string_label);
                    cluster.node_attributes().set_style(Style::Filled);

                    scheduled_basic_block.write_dot_format(&mut cluster, &string_label);
                }

                let next_index_and_block = iter.peek();
                let terminator_source_label =
                    get_node_id(&ScheduledGraphNode::BlockEnd, &string_label);
                match &scheduled_basic_block.terminator() {
                    BasicBlockTerminator::ConditionalJump {
                        condition,
                        target,
                        jump_if_condition_zero,
                    } => {
                        let (condition_met, condition_unmet) = if *jump_if_condition_zero {
                            ("== 0", "!= 0")
                        } else {
                            ("!= 0", "== 0")
                        };
                        digraph
                            .edge(
                                &terminator_source_label,
                                get_node_id(
                                    &ScheduledGraphNode::BlockStart,
                                    &DotFormatBlockLabel::from(*target).to_string(),
                                ),
                            )
                            .attributes()
                            .set_label(
                                format!("if {} {}", condition.to_quil_or_debug(), condition_met)
                                    .as_str(),
                            );
                        if let Some((next_index, next_block)) = next_index_and_block {
                            let next_block_prefix = DotFormatBlockLabel::from_label_or_index(
                                next_block.label(),
                                *next_index,
                            )
                            .to_string();
                            digraph
                                .edge(
                                    &terminator_source_label,
                                    get_node_id(
                                        &ScheduledGraphNode::BlockStart,
                                        &next_block_prefix,
                                    ),
                                )
                                .attributes()
                                .set_label(
                                    format!(
                                        "if {} {}",
                                        condition.to_quil_or_debug(),
                                        condition_unmet
                                    )
                                    .as_str(),
                                );
                        };
                    }
                    BasicBlockTerminator::Jump { target } => {
                        digraph
                            .edge(
                                &terminator_source_label,
                                get_node_id(
                                    &ScheduledGraphNode::BlockStart,
                                    &DotFormatBlockLabel::from(*target).to_string(),
                                ),
                            )
                            .attributes()
                            .set_label("always");
                    }
                    BasicBlockTerminator::Continue => {
                        if let Some((next_index, next_block)) = next_index_and_block {
                            let node_label: DotFormatBlockLabel<'_> =
                                DotFormatBlockLabel::from_label_or_index(
                                    next_block.label(),
                                    *next_index,
                                );

                            digraph
                                .edge(
                                    &terminator_source_label,
                                    get_node_id(
                                        &ScheduledGraphNode::BlockStart,
                                        &node_label.to_string(),
                                    ),
                                )
                                .attributes()
                                .set_label("always");
                        };
                    }
                    BasicBlockTerminator::Halt => {}
                }
            }
        }

        output_bytes
    }
}

/// Escape a string for safe use as a Graphviz node ID or label
fn escape_label(original: &str) -> String {
    original.replace('\\', "\\\\").replace('"', "\\\"")
}

/// Return a string to be used as the node ID within the graph text.
/// `prefix` parameter allows namespacing for uniqueness.
fn get_node_id(node: &ScheduledGraphNode, prefix: &str) -> String {
    match node {
        ScheduledGraphNode::BlockEnd => {
            format!("\"{prefix}_end\"")
        }
        ScheduledGraphNode::BlockStart => {
            format!("\"{prefix}_start\"")
        }
        ScheduledGraphNode::InstructionIndex(index) => {
            format!("\"{prefix}_{index}\"")
        }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    pub(crate) use graph::build_dot_format_snapshot_test_case;
    mod graph {

        use crate::instruction::InstructionHandler;
        use crate::program::Program;

        /// Build a test case which compiles the input program, builds the dot-format string from the program,
        /// and then compares that to a "correct" snapshot of that dot format. This makes diffs easy to compare and
        /// understand; if a test is failing, you can copy the snapshot contents out to your preferred Graphviz
        /// viewer to help understand why.
        ///
        /// NOTE: because this relies on direct string comparison, it will be brittle against changes in the way
        /// that the `get_dot_format` method works. If _all_ or _most_ of these tests are failing, examine the
        /// diffs closely to determine if it's only a matter of reformatting.
        macro_rules! build_dot_format_snapshot_test_case {
            ($name: ident, $input: expr) => {
                crate::program::scheduling::graphviz_dot::tests::build_dot_format_snapshot_test_case!(
                    $name,
                    $input,
                    &mut InstructionHandler::default(),
                );
            };
            ($name: ident, $input:expr, $handler: expr $(,)?) => {
                #[test]
                fn $name() {
                    use crate::program::scheduling::graph::ScheduledProgram;
                    const FRAME_DEFINITIONS: &'static str = "
DEFFRAME 0 \"rf\":
    INITIAL-FREQUENCY: 1e6
DEFFRAME 1 \"rf\":
    INITIAL-FREQUENCY: 1e6
DEFFRAME 2 \"rf\":
    INITIAL-FREQUENCY: 1e6
DEFFRAME 0 \"ro_rx\":
    INITIAL-FREQUENCY: 1e6
DEFFRAME 0 \"ro_tx\":
    INITIAL-FREQUENCY: 1e6
DEFFRAME 0 1 \"cz\":
    INITIAL-FREQUENCY: 1e6
";

                    let program = format!("{}\n{}", FRAME_DEFINITIONS, $input)
                        .parse::<Program>()
                        .unwrap();
                    let scheduled_program =
                        ScheduledProgram::from_program(&program, $handler).unwrap();

                    for block in scheduled_program.basic_blocks() {
                        let graph = block.get_dependency_graph();
                        assert!(
                            !petgraph::algo::is_cyclic_directed(graph),
                            "cycle in graph: {:?}",
                            graph
                        );
                    }

                    let dot_format_bytes = scheduled_program.get_dot_format();
                    let dot_format = String::from_utf8_lossy(&dot_format_bytes);

                    insta::assert_snapshot!(dot_format);
                }
            };
        }

        pub(crate) use build_dot_format_snapshot_test_case;

        build_dot_format_snapshot_test_case!(
            single_instruction,
            "PULSE 0 \"rf\" test(duration: 1e6)"
        );

        build_dot_format_snapshot_test_case!(
            single_dependency,
            "
PULSE 0 \"rf\" test(duration: 1e6)
PULSE 0 \"rf\" test(duration: 1e6)
"
        );

        build_dot_format_snapshot_test_case!(
            chained_pulses,
            "
PULSE 0 \"rf\" test(duration: 1e6)
PULSE 0 \"rf\" test(duration: 1e6)
PULSE 0 \"rf\" test(duration: 1e6)
PULSE 0 \"rf\" test(duration: 1e6)
PULSE 0 \"rf\" test(duration: 1e6)
"
        );

        build_dot_format_snapshot_test_case!(
            different_frames_blocking,
            "
PULSE 0 \"rf\" test(duration: 1e6)
PULSE 1 \"rf\" test(duration: 1e6)
PULSE 2 \"rf\" test(duration: 1e6)
"
        );

        build_dot_format_snapshot_test_case!(
            different_frames_nonblocking,
            "
NONBLOCKING PULSE 0 \"rf\" test(duration: 1e6)
NONBLOCKING PULSE 1 \"rf\" test(duration: 1e6)
NONBLOCKING PULSE 2 \"rf\" test(duration: 1e6)
"
        );

        build_dot_format_snapshot_test_case!(
            blocking_pulses_wrap_nonblocking,
            "
PULSE 0 \"rf\" test(duration: 1e6)
NONBLOCKING PULSE 0 \"ro_tx\" test(duration: 1e6)
PULSE 0 \"rf\" test(duration: 1e6)
FENCE 0
FENCE 0
"
        );

        build_dot_format_snapshot_test_case!(
            blocking_pulses_after_nonblocking,
            "
NONBLOCKING PULSE 0 \"ro_tx\" test(duration: 1e6)
PULSE 0 \"rf\" test(duration: 1e6)
PULSE 0 \"ro_rx\" test(duration: 1e6)
"
        );

        build_dot_format_snapshot_test_case!(
            blocking_2q_pulse,
            "
PULSE 0 \"rf\" test(duration: 1e-6)
PULSE 1 \"rf\" test(duration: 1e-6)
PULSE 0 1 \"cz\" test(duration: 1e-6)
"
        );

        build_dot_format_snapshot_test_case!(
            fence_all_with_nonblocking_pulses,
            "
NONBLOCKING PULSE 0 \"rf\" test(duration: 1e6)
NONBLOCKING PULSE 1 \"rf\" test(duration: 1e6)
FENCE
NONBLOCKING PULSE 0 \"rf\" test(duration: 1e6)
NONBLOCKING PULSE 1 \"rf\" test(duration: 1e6)
"
        );

        build_dot_format_snapshot_test_case!(fence_all, "FENCE");

        build_dot_format_snapshot_test_case!(
            fence_wrapper,
            "
FENCE
NONBLOCKING PULSE 0 1 \"cz\" test(duration: 1e-6)
NONBLOCKING PULSE 1 \"rf\" test(duration: 1e-6)
FENCE 1
"
        );

        build_dot_format_snapshot_test_case!(
            fence_one_wrapper,
            r#"
FENCE 0
NONBLOCKING PULSE 0 "rf" flat(iq: 1, duration: 4e-7)
FENCE 0
"#
        );

        build_dot_format_snapshot_test_case!(
            jump,
            "DECLARE ro BIT
LABEL @first-block
PULSE 0 \"rf\" test(duration: 1e6)
JUMP-UNLESS @third-block ro[0]
LABEL @second-block
PULSE 0 \"rf\" test(duration: 1e6)
LABEL @third-block
PULSE 0 \"rf\" test(duration: 1e6)
"
        );

        build_dot_format_snapshot_test_case!(
            active_reset_single_frame,
            "DECLARE ro BIT
LABEL @measure
NONBLOCKING PULSE 0 \"ro_tx\" test(duration: 1e6)
NONBLOCKING CAPTURE 0 \"ro_rx\" test(duration: 1e6) ro
JUMP-WHEN @end ro[0]
LABEL @feedback
PULSE 0 \"rf\" test(duration: 1e6)
JUMP @measure
LABEL @end
"
        );

        build_dot_format_snapshot_test_case!(
            labels_only,
            "LABEL @a
LABEL @b
LABEL @c
"
        );

        // assert that read and write memory dependencies are expressed correctly
        build_dot_format_snapshot_test_case!(
            simple_memory_access,
            "DECLARE a INTEGER
DECLARE b INTEGER
MOVE a 1
MOVE b 2
ADD a b
"
        );

        // assert that a block "waits" for a capture to complete
        build_dot_format_snapshot_test_case!(
            simple_capture,
            "DECLARE ro BIT
CAPTURE 0 \"ro_rx\" test ro"
        );

        // assert that a block "waits" for a capture to complete even with a pulse after it
        build_dot_format_snapshot_test_case!(
            pulse_after_capture,
            "DECLARE ro BIT
CAPTURE 0 \"ro_rx\" test ro
PULSE 0 \"rf\" test"
        );

        // assert that a block "waits" for a capture to complete even with a pulse after it
        build_dot_format_snapshot_test_case!(
            pulse_after_set_frequency,
            r#"DECLARE ro BIT
SET-FREQUENCY 0 "rf" 3e9
PULSE 0 "rf" test"#
        );

        // assert that a block "waits" for a capture to complete
        build_dot_format_snapshot_test_case!(
            parametric_pulse,
            "DECLARE ro BIT
DECLARE param REAL
PULSE 0 \"rf\" test(a: param[0])
CAPTURE 0 \"ro_rx\" test(a: param[0]) ro"
        );

        // Assert that all pulses following a capture block on that capture, until the next capture
        build_dot_format_snapshot_test_case!(
            parametric_pulses_using_capture_results,
            "DECLARE ro BIT
DECLARE param REAL
CAPTURE 0 \"ro_rx\" test(a: param[0]) ro
NONBLOCKING PULSE 0 \"rf\" test(a: ro[0])
NONBLOCKING PULSE 1 \"rf\" test(a: ro[0])
CAPTURE 0 \"ro_rx\" test(a: param[0]) ro
NONBLOCKING PULSE 0 \"rf\" test(a: ro[0])
NONBLOCKING PULSE 1 \"rf\" test(a: ro[0])"
        );

        build_dot_format_snapshot_test_case!(
            multiple_classical_instructions,
            "DECLARE ro INTEGER[2]\nMOVE ro[0] 1\nMOVE ro[1] 0\nADD ro[0] 5\nSUB ro[1] ro[0]"
        );
    }
}
