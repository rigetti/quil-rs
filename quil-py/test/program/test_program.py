import pickle

from quil.program import Program


def test_pickle():
    program = Program.parse("DECLARE ro BIT[6]")
    pickled = pickle.dumps(program)
    unpickled = pickle.loads(pickled)
    assert program == unpickled
