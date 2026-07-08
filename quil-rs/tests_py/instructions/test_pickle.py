import pickle
from quil.instructions import Gate, MemoryReference, Nop, Qubit, Halt, Wait

class TestPickle:
    def test_gate(self):
        g = Gate("H", (), (Qubit(0),))
        p = pickle.dumps(g)
        g2 = pickle.loads(p)
        assert g == g2

    def test_halt(self):
        h = Halt
        p = pickle.dumps(h)
        q = pickle.loads(p)
        assert q == h

    def test_nop(self):
        h = Nop
        p = pickle.dumps(h)
        q = pickle.loads(p)
        assert q == h

    def test_wait(self):
        h = Wait
        p = pickle.dumps(h)
        q = pickle.loads(p)
        assert q == h


class TestParse:
    def test_memory_ref(self):
        input_str = "ro[0]"
        ref = MemoryReference.parse(input_str)
        assert isinstance(ref, MemoryReference)
        assert ref.index == 0
        assert ref.name == "ro"
        assert ref.to_quil() == input_str
