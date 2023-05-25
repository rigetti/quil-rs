#! /bin/bash

# Get current version of quil-rs
PACKAGE_VERSION=$(cargo metadata --format-version 1 --no-deps | grep -o '"name":"quil-rs","version":"[^"]*' | cut -d '"' -f 8)

# Write it to `quil-py`s Cargo.toml
# `awk -i inplace` would work on linux, but not on macos. This command is compatible with both.
awk -v ver="$PACKAGE_VERSION" '/^quil-rs = /{$0="quil-rs = { path = \"../quil-rs\", version = \"" ver "\" }"}1' quil-py/Cargo.toml > quil-py/Cargo.toml.tmp
mv quil-py/Cargo.toml.tmp quil-py/Cargo.toml

# Validate that the new version of Cargo.toml is valid
cargo check --manifest-path=quil-py/Cargo.toml
