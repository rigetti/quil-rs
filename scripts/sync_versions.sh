#! /bin/bash
# This script gets the current version of quil-rs from its manifest,
# and uses it to update the dependency specification of quil-rs in quil-cli's manifset.

# Get current version of quil-rs
PACKAGE_VERSION=$(cargo metadata --format-version 1 --no-deps | grep -o '"name":"quil-rs","version":"[^"]*' | cut -d '"' -f 8)

patch_cargo_toml() {
    # `awk -i inplace` would work on linux, but not on macos. This command is compatible with both.
    awk -v ver="$PACKAGE_VERSION" '/^quil-rs = /{$0="quil-rs = { path = \"../quil-rs\", version = \"" ver "\" }"}1' "$1"
}

# Write it to `quil-cli`s Cargo.toml
patch_cargo_toml quil-cli/Cargo.toml > quil-cli/Cargo.toml.tmp
mv quil-cli/Cargo.toml.tmp quil-cli/Cargo.toml

# Validate that the new version of Cargo.toml is valid
cargo check --manifest-path=quil-cli/Cargo.toml
