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

//! Welcome to the Rust implementation of the
//! [Quil quantum programming language](https://github.com/quil-lang/quil).
//!
//! Within this crate you'll find:
//!
//! * Builder utilities for Quil [programs], [instructions], and [expressions]
//! * A [parser] and [serializer] for converting Quil to and from text strings
//! * A [constructor for timing graphs], for understanding and debugging Quil-T
//!   pulse control programs
//!
//! This crate is still early in its development and does not fully support all
//! Quil features, nor claim a stable API. Prior to `v1.0`, minor-version changes
//! are considered breaking changes. Please pin your versions when needed, and
//! closely follow the
//! [changelog](https://github.com/rigetti/quil-rust/releases) when upgrading.
//!
//! [constructor for timing graphs]: crate::program::graph::ScheduledProgram#method.get_dot_format
//! [expressions]: crate::expression::Expression
//! [instructions]: crate::instruction::Instruction
//! [parser]: crate::program::Program#method.from_str
//! [programs]: crate::program::Program
//! [serializer]: crate::program::Program#method.to_string

pub mod expression;
mod hash;
pub mod instruction;
mod macros;
pub(crate) mod parser;
pub mod program;
pub mod quil;
pub mod reserved;
pub mod validation;

pub use program::Program;
