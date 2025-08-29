import pickle
from quil.instructions import Instruction, Gate, MemoryReference, Qubit

class TestPickle:
    def test_gate(self):
        q0 = Qubit.Fixed(0)
        g = Gate("H", [], [q0], [])

        p = pickle.dumps(g)
        g2 = pickle.loads(p)
        assert g == g2

        i = Instruction.Gate(g)
        p2 = pickle.dumps(i)
        i2 = pickle.loads(p2)
        assert i == i2

    def test_halt(self):
        h = Instruction.Halt()
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
