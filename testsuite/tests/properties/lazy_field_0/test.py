"""
Check that code emission for lazy fields is correct.
"""

from langkit.dsl import ASTNode
from langkit.expressions import lazy_field

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    @lazy_field(public=True, activate_tracing=True)
    def my_field():
        return 42


build_and_run(lkt_file='expected_concrete_syntax.lkt', ada_main='main.adb')
print('Done')
