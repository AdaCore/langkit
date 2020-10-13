"""
Test that the introspection API works as expected for arrays introspection.
"""

from langkit.dsl import ASTNode, Enum, EnumValue
from langkit.expressions import langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class E1(Enum):
    a = EnumValue()
    b = EnumValue()
    c = EnumValue()


class E2(Enum):
    x = EnumValue()
    y = EnumValue(is_default=True)
    z = EnumValue()


class Example(FooNode):

    @langkit_property(public=True)
    def id1(e=E1):
        return e

    @langkit_property(public=True)
    def id2(e=E2):
        return e


build_and_run(lkt_file='expected_concrete_syntax.lkt', ada_main=['main.adb'])
print('Done')
