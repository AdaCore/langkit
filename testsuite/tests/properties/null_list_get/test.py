"""
Test that trying to get an element out of a null AST list is properly turned
into a Property_Error.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import No, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property()
    def at_bare(n=T.Example.list, i=T.Int):
        return n.at(i)

    @langkit_property()
    def at_entity(n=T.Example.list.entity, i=T.Int):
        return n.at(i)

    @langkit_property(public=True)
    def run_bare():
        return Self.at_bare(No(T.Example.list), 1).as_bare_entity

    @langkit_property(public=True)
    def run_entity():
        return Self.at_entity(No(T.Example.list.entity), 1)


class Example(FooNode):
    pass


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
