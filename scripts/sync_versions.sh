#! /bin/bash
# This script gets the current version of quil-rs from its manifest, and uses it to update the dependency specification
# of quil-rs in quil-py's manifset.
# Requires that `jq` and `cargo-edit` are installed.

COMMON_VERSION=$(cargo metadata --format-version 1 --no-deps | jq '.packages[] | select(.name=="quil-rs") | .version' -r)
# Specifying the @VERSION here is required for cargo upgrade to ignore the `path` field for the package.
cargo upgrade --package quil-rs@$COMMON_VERSION -i allow --recursive false --offline
