"""
Check that generated code is compilable when local variables use names that
could conflict with generated code entities.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode
from langkit.expressions import Var, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    @langkit_property(public=True, return_type=T.Int)
    def prop(ints=T.Int.array):
        get = Var(ints.at(0))
        return get + ints.at(1)


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    types_from_lkt=True,
)
print("Done")
