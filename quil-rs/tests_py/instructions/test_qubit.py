from quil.instructions import Gate, Qubit, QubitPlaceholder, FixedQubit, FormalArgument

class TestConstructor:
    """Test that we construct `Qubit`s in several different ways.

    We allow users to construct `Qubit`s directly by passing "obvious" values,
    such as an integer to create a `Qubit.Fixed` instance,
    and we accept those same things in other places that `Qubit`s are expected,
    automatically creating the `Qubit` instance when necessary.
    """

    def test_fixed(self):
        g = Gate("X", (), (1, Qubit(2), FixedQubit(3)))
        for q in g.qubits:
            assert isinstance(q, Qubit)
            assert isinstance(q, FixedQubit)
            assert q == FixedQubit(q.index)

    def test_variable(self):
        g = Gate("X", (), ("x", Qubit("y"), FormalArgument("z")))
        for q in g.qubits:
            assert isinstance(q, Qubit)
            assert isinstance(q, FormalArgument)
            assert q == FormalArgument(q.name)

    def test_placeholder(self):
        g = Gate("X", (), (Qubit(), Qubit(None), QubitPlaceholder()))
        for q in g.qubits:
            assert isinstance(q, Qubit)
            assert isinstance(q, QubitPlaceholder)
