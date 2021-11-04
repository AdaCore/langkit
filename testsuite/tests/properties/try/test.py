from langkit.dsl import ASTNode, T
from langkit.expressions import PropertyError, Self, Try, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property(return_type=T.Bool)
    def failing_property():
        return PropertyError(T.Bool)

    @langkit_property(public=True, return_type=T.Bool)
    def failsafe_property():
        return Try(Self.failing_property, False)

    @langkit_property(public=True, return_type=T.Bool)
    def failsafe_property_2():
        return Try(Self.failing_property)


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              types_from_lkt=True)
print('Done')
