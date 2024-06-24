# Quil Parser & Program Builder

This library is the implementation of the [Quil spec](https://github.com/quil-lang/quil) in Rust.

It serves three purposes:

1. Parse Quil programs from strings, and output programs to strings
2. Manipulate Quil programs within Rust
3. Construct a dependency graph among program instructions

It should be considered unstable until the release of v1.0.

## Crate Features

| Feature      | Description                                                        |   |   |   |
|--------------|--------------------------------------------------------------------|---|---|---|
| graphviz-dot | Enable plotting `ScheduledProgram`s in Graphviz dotfile format.    |   |   |   |
| wasm-bindgen | Enable compilation to `wasm32-unknown-unknown` with `wasm-bindgen` |   |   |   |


## Testing

When testing this crate, you should run with the `--all-features` flag to ensure all tests are executed.

```sh
cargo test --all-features
```