import json
from dataclasses import asdict
from pathlib import Path
import pytest

from pyo3_linter import process_dir, find_possible_mistakes, Package, Issue

def pytest_generate_tests(metafunc: pytest.Metafunc):
    if "root" in metafunc.fixturenames:
        paths = [p for p in Path("tests/data").iterdir() if p.is_dir()]
        metafunc.parametrize("root", paths, ids=[p.name for p in paths])


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

def issues_to_list(issues: list[Issue]) -> list:
    return [
        {
            "package_kind": issue.package_kind.name,
            "message": issue.message,
        }
        for issue in issues
    ]


def test_find_mistakes(root: Path):
    """Processes files under the `root` and compares the results to expected output,
    stored in JSON files under that root directory.
    """

    expected_paths = [
        (root / "expected-annotated.json"),
        (root / "expected-exported.json"),
        (root / "expected-issues.json"),
    ]
    for p in expected_paths:
        assert p.exists() and p.is_file(), f"missing comparison file for {root.name}"

    expected_annotated = json.loads(expected_paths[0].read_text())
    expected_exported = json.loads(expected_paths[1].read_text())
    expected_issues = json.loads(expected_paths[2].read_text())

    annotated, exported = process_dir(root)
    issues = find_possible_mistakes(annotated, exported)

    got_annotated = package_to_dict(annotated)
    got_exported = package_to_dict(exported)
    got_issues = issues_to_list(issues)

    assert got_annotated == expected_annotated
    assert got_exported == expected_exported
    assert got_issues == expected_issues

