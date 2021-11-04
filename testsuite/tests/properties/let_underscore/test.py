"""
Check that "underscode variables" (to be ignored) work as expected.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import Let, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class BarNode(FooNode):

    @langkit_property(external=True, return_type=T.BigInt,
                      uses_entity_info=False, uses_envs=False)
    def foo(i=T.Int):
        pass

    @langkit_property(public=True)
    def prop():
        return Let(lambda _=Self.foo(1): Self.foo(2))


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              types_from_lkt=True)
print('Done')
