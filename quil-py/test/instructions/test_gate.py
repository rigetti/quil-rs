from quil.expression import Expression
from quil.instructions import PauliTerm, PauliGate


class TestPauliTerm:
    def test_new(self):
        pt = PauliTerm([(PauliGate.X, "a")], Expression.new_pi())
        assert pt.arguments == [(PauliGate.X, "a")]
        pt.arguments = [(PauliGate.Y, "b")]
        assert pt.arguments == [(PauliGate.Y, "b")]
