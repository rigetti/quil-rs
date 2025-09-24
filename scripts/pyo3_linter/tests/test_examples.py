import json
from pathlib import Path
from typing import Iterable

from pyo3_linter import process_dir, find_possible_mistakes, Issue, Package, PackageKind


def package_to_dict(package: Package) -> dict:
    return {
        name: {
            "submodule": mod.submodule,
            "props": mod.props,
            "items": [
                {
                    "rust_name": item.rust_name,
                    "python_name": item.python_name,
                    "kind": item.kind.name,
                    "props": item.props,
                    "stub_attr": {
                        "kind": item.stub_attr.kind.name,
                        "module": item.stub_attr.module,
                    } if item.stub_attr else None,
                }
                for item in sorted(mod)
            ]
        }
        for name, mod in package.items()
    }


def issues_to_list(issues: Iterable[Issue]) -> list:
    return [
        {
            "package_kind": issue.package_kind.name,
            "message": issue.message,
        }
        for issue in issues
    ]


def list_to_issues(issues: list[dict]) -> set[Issue]:
    return {
        Issue(package_kind = PackageKind(issue["package_kind"]), message=issue["message"])
        for issue in issues
    }


def _save_generated(data, paths):
    for d, p in zip(data, paths):
        with open(p, "w") as f:
            json.dump(d, f, indent=2)


def test_find_mistakes(root: Path, generating: bool):
    """Processes files under the `root` and compares the results to expected output,
    stored in JSON files under that root directory.
    """

    annotated, exported = process_dir(root)
    issues = find_possible_mistakes(annotated, exported)

    # These are dicts/lists so they'll be valid JSON and easier to manipulate/check.
    got_data, expected_paths = list(zip(*(
        ( package_to_dict(annotated), (root / "expected-annotated.json") ),
        ( package_to_dict(exported), (root / "expected-exported.json") ),
        ( issues_to_list(issues), (root / "expected-issues.json") ),
    )))

    if generating:
        _save_generated(got_data, expected_paths)
        expected_data = got_data
    else:
        for p in expected_paths:
            assert p.exists() and p.is_file(), f"missing comparison file for {root.name}"
        expected_data = [ json.loads(p.read_text()) for p in expected_paths ]

    for got, expected in zip(got_data, expected_data):
        if isinstance(got, list):
            # Compare as sets, because order isn't important.
            got = list_to_issues(got)
            expected = list_to_issues(expected)
        assert got == expected

