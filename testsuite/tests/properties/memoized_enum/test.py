"""
Check that memoization with enumeration arguments work as expected.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode, Enum, EnumValue
from langkit.expressions import langkit_property

from utils import build_and_run


class MyEnum(Enum):
    a = EnumValue()
    b = EnumValue()


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True

    @langkit_property(public=True, memoized=True)
    def id_bool(b=T.Bool):
        return b

    @langkit_property(public=True, memoized=True)
    def id_my_enum(e=T.MyEnum):
        return e


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
