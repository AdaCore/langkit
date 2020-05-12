"""
Test AST node synthetization and a basic use of it in the Python API.
"""

from langkit.dsl import ASTNode, Field, synthetic
from langkit.expressions import New, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Literal(FooNode):
    token_node = True


class Name(FooNode):
    token_node = True


@synthetic
class SynthNode(FooNode):
    name = Field(type=Name)
    items = Field(type=Literal.list)


class LiteralSequence(FooNode):
    name = Field()
    items = Field()

    @langkit_property(memoized=True)
    def new_node():
        return New(SynthNode, name=Self.name, items=Self.items)

    @langkit_property(public=True)
    def prop():
        return Self.new_node.as_bare_entity


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
