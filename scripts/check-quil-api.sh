#!/usr/bin/env bash
# Determines if `griffe` and `knope` agree about whether this is a breaking change for `quil`.
# Exits with status 1 if `griffe` says it's a breaking change, but `knope` doesn't know that.
# Exits with status 0 if they both or only `knope` reports this is a breaking change.
#
# This uses `poetry` to run `griffe`, and must be executed from the project's root directory.

set -u

# `griffe` needs to run at the project root, but `poetry` needs `-P` to find `pyproject.toml`.
# This adds both the locations for the `quil` package both before and after the crate merge.
# `griffe` doesn't mind if the path doesn't exist, but if you compare tags across the merge,
# it won't find the `quil` package at all, and it'll fail with a obscure error.
poetry run -P quil-rs -- \
  griffe check \
    --search quil-rs/python \
    --search quil-py \
    quil

if [[ ! $? ]]; then
  echo "griffe does not report any breaking changes"
  exit 0
fi

# Now check if `knope` knows this has "Breaking Changes".
#
# This just looks for a line mentioning what will get added to the `quil-rs/CHANGELOG.md`,
# and if it exists, looks for the line `### Breaking Changes`.
# If it finds both, it should be a breaking change for `quil-rs`.
# If it doesn't find the one of those lines, or finds other changes before `Breaking Changes`,
# then either there are no breaking changes, or they aren't breaking changes for `quil-rs`.
knope --dry-run release | awk -f <(cat <<-'EOF'
  BEGIN { is_breaking = 0; }
  /^Would add the following to .*\/CHANGELOG.md: *$/ { is_quilrs = ($6 ~ /quil-rs/); }
  /^### Breaking Changes$/ && is_quilrs { is_breaking = 1; }
  END { exit is_breaking; }
EOF
)

if [[ ! $? ]]; then
  echo "griffe says this is a breaking change for the quil API, but knope does not know that!"
  exit 1
fi
