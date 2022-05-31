//! Utilities for working with Graphviz dot format files

/**
 * Copyright 2021 Rigetti Computing
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 **/
use dot_writer::{Attributes, DotWriter, Shape, Style};

use crate::program::graph::{
    BlockTerminator, ExecutionDependency, InstructionBlock, MemoryAccessType, ScheduledGraphNode,
    ScheduledProgram,
};

impl InstructionBlock {
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
                            self.instructions.get(*index).unwrap()
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
                        ExecutionDependency::ReferenceFrame => "frame",
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

impl ScheduledProgram {
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

            let mut iter = self.blocks.iter().peekable();
            if let Some((first_label, _)) = iter.peek() {
                digraph.edge(
                    "entry",
                    get_node_id(&ScheduledGraphNode::BlockStart, first_label),
                );
            }
            digraph.node_named("entry").set_label("Entry Point");

            while let Some((label, block)) = iter.next() {
                let node_prefix = label;
                {
                    let mut cluster = digraph.cluster();
                    cluster.set_label(label);
                    cluster.node_attributes().set_style(Style::Filled);

                    block.write_dot_format(&mut cluster, node_prefix);
                }

                let next_block_label = iter.peek().map(|(next_label, _)| (*next_label).clone());
                let terminator_source_label =
                    get_node_id(&ScheduledGraphNode::BlockEnd, node_prefix);
                match &block.terminator {
                    BlockTerminator::Conditional {
                        condition,
                        target,
                        jump_if_condition_true,
                    } => {
                        let equality_operators = if *jump_if_condition_true {
                            ("==", "!=")
                        } else {
                            ("!=", "==")
                        };
                        digraph
                            .edge(
                                &terminator_source_label,
                                get_node_id(&ScheduledGraphNode::BlockStart, target),
                            )
                            .attributes()
                            .set_label(
                                format!("if {} {} 0", condition, equality_operators.0).as_str(),
                            );
                        if let Some(next_label) = next_block_label {
                            digraph
                                .edge(
                                    &terminator_source_label,
                                    get_node_id(&ScheduledGraphNode::BlockStart, &next_label),
                                )
                                .attributes()
                                .set_label(
                                    format!("if {} {} 0", condition, equality_operators.1).as_str(),
                                );
                        };
                    }
                    BlockTerminator::Unconditional { target } => {
                        digraph
                            .edge(
                                &terminator_source_label,
                                get_node_id(&ScheduledGraphNode::BlockStart, target),
                            )
                            .attributes()
                            .set_label("always");
                    }
                    BlockTerminator::Continue => {
                        if let Some(next_label) = next_block_label {
                            digraph
                                .edge(
                                    &terminator_source_label,
                                    get_node_id(&ScheduledGraphNode::BlockStart, &next_label),
                                )
                                .attributes()
                                .set_label("always");
                        };
                    }
                    BlockTerminator::Halt => {}
                }
            }
        }

        output_bytes
    }
}

/// Escape a string for safe use as a Graphviz node ID or label
fn escape_label(original: &str) -> String {
    original.replace('\"', "\\\"")
}

/// Return a string to be used as the node ID within the graph text.
/// `prefix` parameter allows namespacing for uniqueness.
fn get_node_id(node: &ScheduledGraphNode, prefix: &str) -> String {
    match node {
        ScheduledGraphNode::BlockEnd => {
            format!("\"{}_end\"", prefix)
        }
        ScheduledGraphNode::BlockStart => {
            format!("\"{}_start\"", prefix)
        }
        ScheduledGraphNode::InstructionIndex(index) => {
            format!("\"{}_{}\"", prefix, index)
        }
    }
}
