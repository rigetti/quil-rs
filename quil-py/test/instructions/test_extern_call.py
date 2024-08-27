import pytest

from quil.instructions import (
    Call,
    CallArgument,
    Declaration,
    ExternDefinition,
    ExternParameter,
    ExternParameterType,
    ExternSignature,
    Instruction,
    MemoryReference,
    ReservedPragma,
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
    extern_definition = ExternDefinition(
        name="test",
        signature=ExternSignature(
            return_type=ScalarType.Real,
            parameters=[
                ExternParameter(
                    name="a",
                    mutable=False,
                    data_type=ExternParameterType.from_variable_length_vector(ScalarType.Integer),
                )
            ],
        ),
    )
    p.add_instruction(Instruction(ReservedPragma.from_extern_definition(extern_definition)))
    real_declaration = Declaration(name="real", size=Vector(data_type=ScalarType.Real, length=3), sharing=None)
    p.add_instruction(Instruction(real_declaration))
    integer_declaration = Declaration(name="integer", size=Vector(data_type=ScalarType.Integer, length=3), sharing=None)
    p.add_instruction(Instruction(integer_declaration))
    call = Call(
        name=extern_definition.name,
        arguments=[
            return_argument,
            CallArgument.from_identifier(integer_declaration.name),
        ],
    )
    p.add_instruction(Instruction(call))

    parsed_program = Program.parse(p.to_quil())
    assert p == parsed_program

    p.resolve_call_instructions()


def test_extern_call_quil():
    input = """DECLARE reals REAL[3]
DECLARE octets OCTET[3]
PRAGMA EXTERN foo "OCTET (params : mut REAL[3])"
CALL foo octets[1] reals
"""
    program = Program.parse(input)
    program.resolve_call_instructions()
