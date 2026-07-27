import pytest

from quil.instructions import Label, Target, TargetPlaceholder

class TestLabel:
    def test_placeholder_mismatch(self):
        with pytest.raises(ValueError):
            Label(Target.Fixed("x"), placeholder=True)
        with pytest.raises(ValueError):
            Label(placeholder=False)

    def test_from_existing(self):
        t = Target.Placeholder(TargetPlaceholder("base"))
        assert Label(t).target == t
        assert Label(t, placeholder=None).target == t

    def test_fixed(self):
        fixed = "my_label"
        assert Label(fixed).target == Target.Fixed(fixed)
        assert Label(fixed, placeholder=None).target == Target.Fixed(fixed)
        assert Label(fixed, placeholder=False).target == Target.Fixed(fixed)

    def test_placeholder(self):
        assert isinstance(Label().target, Target.Placeholder)
        assert isinstance(Label(placeholder=None).target, Target.Placeholder)
        assert isinstance(Label(placeholder=True).target, Target.Placeholder)

    def test_prefixed_placeholder(self):
        base = "base"
        match (target := Label(base, placeholder=True).target):
            case Target.Placeholder(p):
                assert p.base_label == base
            case _:
                assert False, f"{target=} is not a placeholder"
