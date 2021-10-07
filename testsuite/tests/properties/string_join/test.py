"""
Check that String.array's "join" operation works as expected.
"""

from langkit.dsl import ASTNode, Struct, T, UserField
from langkit.expressions import langkit_property

from utils import build_and_run


class Str(Struct):
    value = UserField(type=T.String)


class FooNode(ASTNode):
    pass


class Example(FooNode):
    @langkit_property(public=True, return_type=T.String)
    def join(strings=T.Str.array, separator=T.String):
        return separator.join(strings.map(lambda s: s.value))


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
