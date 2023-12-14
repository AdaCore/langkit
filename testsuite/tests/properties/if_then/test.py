"""
Test that doing If(...).then(...) compiles properly. It used not to.
"""

from langkit.dsl import ASTNode
from langkit.expressions import If, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    @langkit_property(public=True)
    def foo():
        return If(True, Self, Self).then(lambda s: s).as_bare_entity


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
