#!/bin/sh

### This script is called by Semantic Release after determining a new version
### for the package and prior to pushing it back to GitHub

set -ex

[[ $1 == "" ]] && echo "usage: $0 <new version string>" && exit 1

# We use sed to bump the version to avoid having to install or use any other tools
sed -i.bak -E "s/^version = \".+\"$/version = \"$1\"/" Cargo.toml
rm Cargo.toml.bak
