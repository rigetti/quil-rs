from pathlib import Path

from pyo3_linter import Item, Line

def test_item():
    """Verify that hash and equality checks work as expected.

    Namely, we want to compare items on their Python+Rust names,
    but ignore the rest of their metadata.
    """
    i1 = Item("A", "B", "struct", Path("C"), Line(1, "struct B();"))
    i2 = Item("A", "B", "struct", Path("c"), Line(2, "struct B();"))
    i3 = Item("A", "b", "struct", Path("C"), Line(1, "struct b();"))
    assert hash(i1) == hash(i2)
    assert hash(i1) != hash(i3)
    assert i1 in {i2}
    assert i2 in {i1}
    assert i1 not in {i3}
    assert i2 not in {i3}
    assert i3 not in {i1, i2}



