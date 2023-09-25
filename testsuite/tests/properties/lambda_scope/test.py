"""
Check that scopes for lambda arguments are properly handled.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True)
    def prop(l=T.Example.list):
        # Nest two lambda expressions that define arguments with the same name
        # (v).
        return l.map(lambda v: v.values.find(lambda v: v == 1))


class Example(FooNode):

    @langkit_property()
    def values():
        return [1]


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    types_from_lkt=True,
)
print("Done")
