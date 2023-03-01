# Contributing to quil-rs and quil-py

Welcome to the `quil-rs` project, and thanks for contributing!

This guide is to help walk you through contributing in different ways to `quil-rs` and `quil-py`,
as well as share some general how-tos for development, testing, and maintenance.

## Table of Contents

[Developer Guidelines](#developer-guidelines)

- [Instruction Module](#instruction-module)

## Developer Guidelines

### Instruction Module

The `instruction` module is large, so we follow a few guidelines when adding or changing instructions to help keep
them organized and consistent in both the `quil-rs` and `quil-py` crates.

All instruction definitions live inside of the `instruction` module. For ease of development,
instruction types are grouped into submodules so that similar instructions can be modified and viewed together
rather than having all instructions in one large file. However, to avoid imposing potentially arbritrary
instruction categories onto the user, the `instruction` module publically re-exports all of its types from
its root. 

This module structure should remain consistent between both `quil-rs` and `quil-py`. For example, the file
`src/instruction/gate.rs` exists in both `quil-rs` and `quil-py` and contains the same set of instruction
definitions (for their respective target language, of course). 

In short, an instruction should:
    1. Be organized in the equivalent `instruction` submodule for both `quil-rs` and `quil-py` crates
    2. Only be re-exported publically from the root of the `Instruction` module

