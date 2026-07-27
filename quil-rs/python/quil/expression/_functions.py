"""
This module exposes Python functions for use as `Expression` objects.
"""

import numpy as np
from quil._quil.instructions import GateDefinition, GateSpecification
from quil._quil.expression import Expression, ExpressionFunction, FunctionCallExpression
from quil._quil.expression import ExpressionLike

def fn(f: ExpressionFunction, x: Expression | str | int | float | complex) -> Expression:
    if isinstance(x, str):
        x = Expression.Variable(x)
    elif not isinstance(x, Expression):
        x = Expression.Number(x)
    return Expression.FunctionCall(FunctionCallExpression(f, x))

def qcos(x: ExpressionLike) -> Expression: return fn(ExpressionFunction.COSINE, x)
def qsin(x) -> Expression: return fn(ExpressionFunction.SINE, x)


