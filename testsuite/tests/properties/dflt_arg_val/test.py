from langkit.dsl import ASTNode, Bool, Enum, EnumValue, T
from langkit.expressions import No, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Color(Enum):
    red = EnumValue()
    green = EnumValue()
    blue = EnumValue()


class Example(FooNode):

    # Test default argument for a simple type: boolean
    @langkit_property(public=True)
    def prop1(arg=(Bool, True)):
        return arg

    # Test default argument for a simple type: enum
    @langkit_property(public=True)
    def prop2(arg=(Color, Color.red)):
        return arg

    # Test default argument for public entities
    @langkit_property(public=True)
    def prop3(arg=(T.FooNode.entity, No(T.FooNode.entity))):
        return arg


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              types_from_lkt=True)
print('')
print('Done')
