"""
Check that code generation produces valid Ada sources when a dynamic variable
has the same name as a local variable.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode
from langkit.expressions import DynamicVariable, Var, langkit_property

from utils import build


neg = DynamicVariable("neg", T.Int)


class FooNode(ASTNode):
    pass


class Example(FooNode):
    # Code generation used to create a local variable called "Neg" in Ada to
    # store the result of the unary operator, causing a name clash conflict
    # with...

    # ... the "Neg" formal for this property that comes from the "neg"
    # argument.

    @langkit_property(public=True)
    def f1(i=T.Int, neg=T.Int):
        base = Var(-neg)
        return i + base

    # ... the "Neg" formal for this property that comes from the "neg" dynamic
    # variable.

    @langkit_property(public=True, dynamic_vars=[neg])
    def f2(i=T.Int):
        base = Var(-neg)
        return i + base


build(lkt_file="expected_concrete_syntax.lkt", types_from_lkt=True)
print("Compilation was successful")
