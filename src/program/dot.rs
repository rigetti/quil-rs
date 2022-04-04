// Utilities for working with Graphviz dot format files

use crate::program::graph::{
    BlockTerminator, ExecutionDependency, MemoryAccessType, ScheduledGraphNode,
};

use super::graph::{InstructionBlock, ScheduledProgram};

impl InstructionBlock {
    /// Write a DOT-formatted string to the provided writer for use with GraphViz.
    /// This output can be used within a `subgraph` or at the top level of a `digraph`.
    ///
    /// Parameters:
    ///
    /// * line_prefix: The prefix for each new line in the output. This can be used to indent this
    ///   output for readability in a larger definition.
    /// * element_prefix: The prefix for each graph element (node and edge). This can be used to
    ///   namespace this block when used with other blocks which may have conflicting labels.
    pub fn write_dot_format(
        &self,
        f: &mut std::fmt::Formatter,
        line_prefix: &str,
        element_prefix: &str,
    ) -> std::fmt::Result {
        self.graph.nodes().try_for_each(|node| {
            match &node {
                ScheduledGraphNode::BlockEnd => {
                    writeln!(
                        f,
                        "{}\"{}end\" [ label=end, shape=circle ]",
                        line_prefix, element_prefix
                    )
                }
                ScheduledGraphNode::BlockStart => {
                    writeln!(
                        f,
                        "{}\"{}start\" [ label=start, shape=circle ]",
                        line_prefix, element_prefix
                    )
                }
                ScheduledGraphNode::InstructionIndex(index) => {
                    write!(
                        f,
                        "{}\"{}{}\" [label=\"",
                        line_prefix, element_prefix, index
                    )?;
                    write_escaped(f, &format!("{}", self.instructions.get(*index).unwrap()))?;
                    writeln!(f, "\"]")
                }
            }?;
            self.graph.edges(node).try_for_each(|(src, dest, edge)| {
                match &src {
                    ScheduledGraphNode::BlockEnd => {
                        write!(f, "{}\"{}end\"", line_prefix, element_prefix)
                    }
                    ScheduledGraphNode::BlockStart => {
                        write!(f, "{}\"{}start\"", line_prefix, element_prefix)
                    }
                    ScheduledGraphNode::InstructionIndex(index) => {
                        write!(f, "{}\"{}{}\"", line_prefix, element_prefix, index)
                    }
                }?;
                write!(f, " -> ")?;
                match &dest {
                    ScheduledGraphNode::BlockEnd => write!(f, "\"{}end\"", element_prefix),
                    ScheduledGraphNode::BlockStart => {
                        write!(f, "\"{}start\"", element_prefix)
                    }
                    ScheduledGraphNode::InstructionIndex(index) => {
                        write!(f, "\"{}{}\"", element_prefix, index)
                    }
                }?;
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
                writeln!(f, " [ label=\"{}\" ]", label)
            })
        })?;
        Ok(())
    }
}

impl ScheduledProgram {
    /// Write a DOT format string to the provided writer for use with Graphviz.
    ///
    /// This outputs a `digraph` object with a `subgraph` for each block to inform the layout engine.
    /// Each `subgraph` ID is prefixed with `cluster_` which instructs some supporting layout engines
    /// to enclose the subgraph with a border. This improves readability of the graph.
    ///
    /// Lines on the graph indicate scheduling dependencies within blocks and control flow among blocks.
    /// Each node representing an instruction is labeled with the contents of that instruction.
    pub fn write_dot_format(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "digraph {{")?;

        let mut iter = self.blocks.iter().peekable();

        writeln!(f, "\tentry [label=\"Entry Point\"]")?;

        if let Some((first_label, _)) = iter.peek() {
            writeln!(f, "\tentry -> \"{}_start\"", first_label)?;
        }

        while let Some((label, block)) = iter.next() {
            writeln!(f, "\tsubgraph \"cluster_{}\" {{", label)?;
            writeln!(f, "\t\tlabel=\"{}\"", label)?;
            writeln!(f, "\t\tnode [ style=\"filled\" ]")?;

            let line_prefix = "\t\t";
            // let element_prefix = format!("b{}_", index);
            let element_prefix = format!("{}_", label);

            block.write_dot_format(f, line_prefix, &element_prefix)?;
            writeln!(f, "\t}}")?;

            let next_block_label = iter.peek().map(|(next_label, _)| (*next_label).clone());
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
                    writeln!(
                        f,
                        "\"{}_end\" -> \"{}_start\" [label=\"if {} {} 0\"]",
                        label, target, condition, equality_operators.0,
                    )?;
                    if let Some(next_label) = next_block_label {
                        writeln!(
                            f,
                            "\"{}_end\" -> \"{}_start\" [label=\"if {} {} 0\"]",
                            label, next_label, condition, equality_operators.1
                        )?;
                    };
                }
                BlockTerminator::Unconditional { target } => {
                    writeln!(
                        f,
                        "\"{}_end\" -> \"{}_start\" [label=\"always\"]",
                        label, target
                    )?;
                }
                BlockTerminator::Continue => {
                    if let Some(next_label) = next_block_label {
                        writeln!(
                            f,
                            "\"{}_end\" -> \"{}_start\" [label=\"always\"]",
                            label, next_label
                        )?;
                    };
                }
                BlockTerminator::Halt => {}
            }
        }
        writeln!(f, "}}")
    }
}

/// Escape strings for use as DOT format quoted ID's
fn write_escaped(f: &mut std::fmt::Formatter, s: &str) -> std::fmt::Result {
    for c in s.chars() {
        write_char(f, c)?;
    }
    Ok(())
}

/// Escape a single character for use within a DOT format quoted ID.
fn write_char(f: &mut std::fmt::Formatter, c: char) -> std::fmt::Result {
    use std::fmt::Write;
    match c {
        '"' | '\\' => f.write_char('\\')?,
        // \l is for left justified linebreak
        '\n' => return f.write_str("\\l"),
        _ => {}
    }
    f.write_char(c)
}
