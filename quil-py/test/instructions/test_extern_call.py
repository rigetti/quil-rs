import pytest

from quil.instructions import (
    Call,
    CallArgument,
    Declaration,
    ExternParameter,
    ExternParameterType,
    ExternSignature,
    Instruction,
    MemoryReference,
    Pragma,
    PragmaArgument,
    ScalarType,
    Vector,
)
from quil.program import Program


@pytest.mark.parametrize(
    "return_argument",
    [CallArgument.from_identifier("real"), CallArgument.from_memory_reference(MemoryReference("real", 1))],
)
def test_extern_call_instructions(return_argument: CallArgument):
    p = Program()
    extern_signature = ExternSignature(
        return_type=ScalarType.Real,
        parameters=[
            ExternParameter(
                name="a",
                mutable=False,
                data_type=ExternParameterType.from_variable_length_vector(ScalarType.Integer),
            )
        ],
    )
    pragma = Pragma(
        "EXTERN",
        [PragmaArgument.from_identifier("foo")],
        f'"{extern_signature.to_quil()}"',
    )
    p.add_instruction(Instruction(pragma))
    real_declaration = Declaration(name="real", size=Vector(data_type=ScalarType.Real, length=3), sharing=None)
    p.add_instruction(Instruction(real_declaration))
    integer_declaration = Declaration(name="integer", size=Vector(data_type=ScalarType.Integer, length=3), sharing=None)
    p.add_instruction(Instruction(integer_declaration))
    call = Call(
        name="foo",
        arguments=[
            return_argument,
            CallArgument.from_identifier(integer_declaration.name),
        ],
    )
    p.add_instruction(Instruction(call))

    parsed_program = Program.parse(p.to_quil())
    assert p == parsed_program


def test_extern_call_quil():
    input = """PRAGMA EXTERN foo "OCTET (params : mut REAL[3])"
DECLARE reals REAL[3]
DECLARE octets OCTET[3]
CALL foo octets[1] reals
"""
    program = Program.parse(input)
    assert program == Program.parse(program.to_quil())
