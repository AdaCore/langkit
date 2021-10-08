"""
Test that the String type works as expected in the property DSL and in the
generated APIs.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import String, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property(public=True)
    def identity(s=T.String):
        return s

    @langkit_property(public=True)
    def extend(s=T.String):
        return s.concat(String("foo"))

    @langkit_property(public=True)
    def newline():
        return String("hello\nworld")


build_and_run(lkt_file='expected_concrete_syntax.lkt',
              ada_main="main.adb",
              py_script='main.py')
print('Done')
